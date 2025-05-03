(** strategy_grpc.ml - gRPC Clients for Strategy Engine *)

open Strategy_grpc_intf (* Include interface *)
open Lwt.Infix (* For Lwt binds *)
open Grpc_lwt (* gRPC Lwt client *)
open Grpc_lwt.Client.Result.Infix (* For Result Lwt binds *)
open Timestamps_proto.Google.Protobuf (* For Timestamp *)
open Indicator_data_pb (* For IndicatorValue *)
open Indicator_data_grpc (* For IndicatorService client stub *)
open Ml_prediction_pb (* For PredictionValue *)
open Ml_prediction_grpc (* For PredictionService client stub *)
open Market_data_pb (* For OrderRequest, OrderResponse, MarketDataService client stub *)
open Market_data_grpc (* For MarketDataService client stub *)
open Logs (* For logging *)
open Result (* For Result type *)
open Base.Continue_or_stop (* Used by Grpc_lwt.Client.Stream.iter *)
open Lwt_unix (* For sleep *)

(** The main gRPC client environment type. Holds client connections/stubs. *)
type t = {
  indicator_chan: Grpc_lwt.Client.channel;
  prediction_chan: Grpc_lwt.Client.channel;
  order_execution_chan: Grpc_lwt.Client.channel;
}

(* Helper to create a gRPC channel *)
let create_channel address =
  info (fun m -> m "Creating gRPC channel to %s" address);
  let uri = Uri.of_string ("grpc://" ^ address) in (* Assuming insecure for now *)
  Grpc_lwt.Client.open_uri uri

(** Create a new gRPC client environment (channels only). *)
let create ~indicator_addr ~prediction_addr ~ingestion_addr =
  let indicator_chan = create_channel indicator_addr in
  let prediction_chan = create_channel prediction_addr in
  let ingestion_chan = create_channel ingestion_addr in
  {
    indicator_chan;
    prediction_chan;
    order_execution_chan;
  }

(* Helper to handle a streaming RPC and feed data into the data manager *)
let rec handle_streaming_rpc ~channel ~rpc_method ~subscribe_request ~data_mgr ~add_data_fn ~stream_name =
  info (fun m -> m "Starting gRPC client stream '%s'" stream_name);
  Lwt.catch (fun () ->
    (* Use Grpc_lwt.Client.call_server_streaming for non-blocking stream *)
    Grpc_lwt.Client.call_server_streaming channel
      ~rpc_method:rpc_method
      ~req:subscribe_request
      ()
    >>= fun stream_result ->
    match stream_result with
    | Ok (stream, status) ->
        (* Status can be checked here if needed, but usually stream finishes first *)
        info (fun m -> m "gRPC stream '%s' established. Processing messages..." stream_name);
        (* Iterate over the stream and process messages *)
        Grpc_lwt.Client.Stream.iter (fun data ->
          add_data_fn data_mgr data >>= fun () ->
          Lwt.return Continue (* Continue processing the stream *)
        ) stream >>= fun () -> (* Wait for stream iteration to complete *)
        (* Stream finished iterating (either closed by server or error) *)
        info (fun m -> m "gRPC client stream '%s' iteration finished." stream_name);
        (* The stream is done, check the final status *)
        let final_status = Lwt_main.run status in (* Blocking call to get final status *)
        info (fun m -> m "gRPC client stream '%s' final status: %s" stream_name (Grpc_status.show final_status));
        (* If the stream finishes, attempt to reconnect after a delay *)
        Lwt_unix.sleep 5.0 >>= fun () -> (* Wait for 5 seconds before retrying *)
        warning (fun m -> m "Attempting to reconnect '%s' stream..." stream_name);
        handle_streaming_rpc ~channel ~rpc_method ~subscribe_request ~data_mgr ~add_data_fn ~stream_name (* Retry connection *)

    | Error (`Grpc_status (status, msg)) ->
        error (fun m -> m "gRPC client stream '%s' failed to establish with status %s: %s" stream_name (Grpc_status.show status) msg);
        (* If connection fails, attempt to reconnect after a delay *)
        Lwt_unix.sleep 5.0 >>= fun () -> (* Wait before retrying *)
        warning (fun m -> m "Attempting to reconnect '%s' stream after error..." stream_name);
        handle_streaming_rpc ~channel ~rpc_method ~subscribe_request ~data_mgr ~add_data_fn ~stream_name (* Retry connection *)
    | Error (`Other err) ->
        error (fun m -> m "gRPC client stream '%s' failed to establish with unexpected error: %s" stream_name (Printexc.to_string err));
        (* If connection fails, attempt to reconnect after a delay *)
        Lwt_unix.sleep 5.0 >>= fun () -> (* Wait before retrying *)
        warning (fun m -> m "Attempting to reconnect '%s' stream after exception..." stream_name);
        handle_streaming_rpc ~channel ~rpc_method ~subscribe_request ~data_mgr ~add_data_fn ~stream_name (* Retry connection *)
  ) (fun e ->
    (* Catch exceptions during the Lwt flow *)
    error (fun m -> m "Exception in gRPC client stream '%s' loop: %s" stream_name (Printexc.to_string e));
    Lwt_unix.sleep 5.0 >>= fun () -> (* Wait before retrying *)
    warning (fun m -> m "Attempting to reconnect '%s' stream after exception..." stream_name);
    handle_streaming_rpc ~channel ~rpc_method ~subscribe_request ~data_mgr ~add_data_fn ~stream_name (* Retry connection *)
  )


(** Start the client task to subscribe to indicator updates. *)
let start_indicator_subscription grpc_client_env data_mgr =
  let open IndicatorService in
  let rpc_method = "/indicator_data.IndicatorService/SubscribeToIndicators" in (* Full RPC method name *)
  let request = IndicatorSubscriptionRequest.create () in (* Empty request subscribes to all by default *)
  handle_streaming_rpc
    ~channel:grpc_client_env.indicator_chan
    ~rpc_method:rpc_method
    ~subscribe_request:request
    ~data_mgr:data_mgr
    ~add_data_fn:Strategy_data.add_indicator_value
    ~stream_name:"IndicatorStream"


(** Start the client task to subscribe to prediction updates. *)
let start_prediction_subscription grpc_client_env data_mgr =
   let open PredictionService in
   let rpc_method = "/ml_prediction.PredictionService/SubscribeToPredictions" in (* Full RPC method name *)
   let request = PredictionSubscriptionRequest.create () in (* Empty request subscribes to all by default *)
   handle_streaming_rpc
     ~channel:grpc_client_env.prediction_chan
     ~rpc_method:rpc_method
     ~subscribe_request:request
     ~data_mgr:data_mgr
     ~add_data_fn:Strategy_data.add_prediction_value
     ~stream_name:"PredictionStream"


(* Counter for generating unique client order IDs *)
let order_id_counter = ref 0
let order_id_mutex = Lwt_mutex.create ()

(* Generate a unique client order ID *)
let generate_client_order_id () =
  Lwt_mutex.with_lock order_id_mutex (fun () ->
    let id = !order_id_counter in
    incr order_id_counter;
    Lwt.return (Printf.sprintf "strategy-%d-%d" (Unix.getpid ()) id) (* Example: include PID and counter *)
  )


(** Send a trade order request. Returns Result Lwt.t. *)
let send_order grpc_client_env exchange symbol side otype quantity price =
  let open MarketDataService in
  let rpc_method = "/market_data.MarketDataService/ExecuteOrder" in (* Full RPC method name *)
  let* client_order_id = generate_client_order_id () in
  let request = OrderRequest.create
    ~client_order_id:client_order_id
    ~exchange:exchange
    ~symbol:symbol
    ~side:side
    ~type_:otype (* 'type' is a reserved keyword, use type_ *)
    ~quantity:quantity
    ~price:price
    ()
  in
  info (fun m -> m "Sending order: %s %s %s @ %s qty %s (ID: %s)"
            (Market_data_pb.OrderSide.show side) (Market_data_pb.OrderType.show otype)
            symbol price quantity client_order_id);

  Lwt.catch (fun () ->
    (* Use Grpc_lwt.Client.call for unary call *)
    Grpc_lwt.Client.call grpc_client_env.order_execution_chan
      ~rpc_method:rpc_method
      ~req:request
      ()
    >>= fun response_result ->
    match response_result with
    | Ok (response, status) ->
        (* Wait for the final status *)
        let final_status = Lwt_main.run status in (* Blocking call to get final status *)
        info (fun m -> m "Order response for %s (Exchange ID: %s): Status=%s, Msg=%s (gRPC Status: %s)"
              client_order_id response.OrderResponse.exchange_order_id response.OrderResponse.status response.OrderResponse.message (Grpc_status.show final_status));
        (* Note: The DSL interpreter currently does NOT receive this response.
           A real system needs an order state manager and potentially a subscription
           to Order Updates from the ingestion service. *)
        Lwt.return (Ok ()) (* Indicate the RPC call itself was successful *)
    | Error (`Grpc_status (status, msg)) ->
        error (fun m -> m "gRPC status error sending order %s: Status=%s, Msg=%s" client_order_id (Grpc_status.show status) msg);
        Lwt.return (Error (`GrpcError (Printf.sprintf "gRPC status error sending order: %s (%s)" msg (Grpc_status.show status))))
    | Error (`Other err) ->
        error (fun m -> m "Unexpected error sending order %s: %s" client_order_id (Printexc.to_string err));
        Lwt.return (Error (`GrpcError (Printf.sprintf "Unexpected error sending order: %s" (Printexc.to_string err))))
  ) (fun e ->
    error (fun m -> m "Exception caught sending order %s: %s" client_order_id (Printexc.to_string e));
    Lwt.return (Error (`GrpcError (Printf.sprintf "Exception sending order: %s" (Printexc.to_string e))))
  )