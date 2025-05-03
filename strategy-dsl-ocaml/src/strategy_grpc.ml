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
open Market_data_pb (* For OrderRequest, OrderResponse, OrderUpdate, MarketDataService client stub *)
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

(* Helper to create a gRPC channel with retry logic *)
let rec create_channel_with_retry address attempt =
  let delay = min (5.0 *. (float_of_int (1 lsl attempt))) 60.0 in (* Exponential backoff up to 60s *)
  info (fun m -> m "Attempt %d: Creating gRPC channel to %s (retrying in %.1f s)" attempt address delay);
  Lwt.catch (fun () ->
    let uri = Uri.of_string ("grpc://" ^ address) in (* Assuming insecure for now *)
    Grpc_lwt.Client.open_uri uri >>= fun channel ->
    info (fun m -> m "Successfully created gRPC channel to %s" address);
    Lwt.return channel
  ) (fun e ->
    error (fun m -> m "Failed to create gRPC channel to %s: %s. Retrying..." address (Printexc.to_string e));
    let* () = Lwt_unix.sleep delay in
    create_channel_with_retry address (attempt + 1)
  )


(** Create a new gRPC client environment (channels only) with retry. *)
let create ~indicator_addr ~prediction_addr ~ingestion_addr =
  let indicator_chan = create_channel_with_retry indicator_addr 1 in
  let prediction_chan = create_channel_with_retry prediction_addr 1 in
  let ingestion_chan = create_channel_with_retry ingestion_addr 1 in
  (* We need to wait for channels to be created before returning the struct *)
  let* indicator_chan = indicator_chan in
  let* prediction_chan = prediction_chan in
  let* ingestion_chan = ingestion_chan in
  Lwt.return {
    indicator_chan;
    prediction_chan;
    order_execution_chan = ingestion_chan;
  }


(* Helper to handle a streaming RPC and feed data into the data manager *)
let rec handle_streaming_rpc ~channel_lwt ~rpc_method ~subscribe_request ~data_mgr ~add_data_fn ~stream_name =
  let* channel = channel_lwt in (* Wait for channel to be available *)
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
        let* () = Grpc_lwt.Client.Stream.iter (fun data ->
          let* () = add_data_fn data_mgr data in
          Lwt.return Continue (* Continue processing the stream *)
        ) stream in (* Wait for stream iteration to complete *)
        (* Stream finished iterating (either closed by server or error) *)
        info (fun m -> m "gRPC client stream '%s' iteration finished." stream_name);
        (* The stream is done, check the final status *)
        let final_status = Lwt_main.run status in (* Blocking call to get final status *)
        info (fun m -> m "gRPC client stream '%s' final status: %s" stream_name (Grpc_status.show final_status));
        (* If the stream finishes, attempt to reconnect after a delay *)
        Lwt_unix.sleep 5.0 >>= fun () -> (* Wait for 5 seconds before retrying *)
        warning (fun m -> m "Attempting to reconnect '%s' stream..." stream_name);
        handle_streaming_rpc ~channel_lwt:(Lwt.return channel) ~rpc_method ~subscribe_request ~data_mgr ~add_data_fn ~stream_name (* Retry connection *)

    | Error (`Grpc_status (status, msg)) ->
        error (fun m -> m "gRPC client stream '%s' failed to establish with status %s: %s" stream_name (Grpc_status.show status) msg);
        (* If connection fails, attempt to reconnect after a delay *)
        Lwt_unix.sleep 5.0 >>= fun () -> (* Wait before retrying *)
        warning (fun m -> m "Attempting to reconnect '%s' stream after error..." stream_name);
        handle_streaming_rpc ~channel_lwt:(Lwt.return channel) ~rpc_method ~subscribe_request ~data_mgr ~add_data_fn ~stream_name (* Retry connection *)
    | Error (`Other err) ->
        error (fun m -> m "gRPC client stream '%s' failed to establish with unexpected error: %s" stream_name (Printexc.to_string err));
        (* If connection fails, attempt to reconnect after a delay *)
        Lwt_unix.sleep 5.0 >>= fun () -> (* Wait before retrying *)
        warning (fun m -> m "Attempting to reconnect '%s' stream after exception..." stream_name);
        handle_streaming_rpc ~channel_lwt:(Lwt.return channel) ~rpc_method ~subscribe_request ~data_mgr ~add_data_fn ~stream_name (* Retry connection *)
  ) (fun e ->
    (* Catch exceptions during the Lwt flow *)
    error (fun m -> m "Exception in gRPC client stream '%s' loop: %s" stream_name (Printexc.to_string e));
    Lwt_unix.sleep 5.0 >>= fun () -> (* Wait before retrying *)
    warning (fun m -> m "Attempting to reconnect '%s' stream after exception..." stream_name);
    handle_streaming_rpc ~channel_lwt:(Lwt.return channel) ~rpc_method ~subscribe_request ~data_mgr ~add_data_fn ~stream_name (* Retry connection *)
  )


(** Start the client task to subscribe to indicator updates. *)
let start_indicator_subscription grpc_client_env data_mgr =
  let open IndicatorService in
  let rpc_method = "/indicator_data.IndicatorService/SubscribeToIndicators" in (* Full RPC method name *)
  let request = IndicatorSubscriptionRequest.create () in (* Empty request subscribes to all by default *)
  handle_streaming_rpc
    ~channel_lwt:(Lwt.return grpc_client_env.indicator_chan) (* Pass channel as Lwt.t *)
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
     ~channel_lwt:(Lwt.return grpc_client_env.prediction_chan) (* Pass channel as Lwt.t *)
     ~rpc_method:rpc_method
     ~subscribe_request:request
     ~data_mgr:data_mgr
     ~add_data_fn:Strategy_data.add_prediction_value
     ~stream_name:"PredictionStream"

(** Start the client task to subscribe to order status updates. *)
let start_order_update_subscription grpc_client_env data_mgr =
  let open MarketDataService in
  let rpc_method = "/market_data.MarketDataService/SubscribeToOrderUpdates" in (* Full RPC method name *)
  let request = OrderUpdateRequest.create () in (* Empty request subscribes to all by default *)
  handle_streaming_rpc
    ~channel_lwt:(Lwt.return grpc_client_env.order_execution_chan) (* Pass channel as Lwt.t *)
    ~rpc_method:rpc_method
    ~subscribe_request:request
    ~data_mgr:data_mgr
    ~add_data_fn:Strategy_data.add_order_update
    ~stream_name:"OrderUpdateStream"


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
              client_order_id response.OrderResponse.exchange_order_id (Market_data_pb.OrderStatus.show response.OrderResponse.status) response.OrderResponse.message (Grpc_status.show final_status));
        (* The response payload provides the *initial* status (e.g., ACKNOWLEDGED, REJECTED).
           Subsequent status *updates* (e.g., FILLED) come via the SubscribeToOrderUpdates stream.
           The DSL interpreter currently does NOT receive this response directly;
           it could potentially query the DataManager for the latest status for a given ID if needed. *)
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