(** strategy_grpc.mli - gRPC Clients for Strategy Engine *)

open Lwt.Infix (* For Lwt binds *)
open Grpc_lwt (* gRPC Lwt client *)
open Timestamps_proto.Google.Protobuf (* For Timestamp *)
open Indicator_data_pb (* For IndicatorValue *)
open Indicator_data_grpc (* For IndicatorService client stub *)
open Ml_prediction_pb (* For PredictionValue *)
open Ml_prediction_grpc (* For PredictionService client stub *)
open Market_data_pb (* For OrderRequest, OrderResponse, MarketDataService client stub *)
open Market_data_grpc (* For MarketDataService client stub *)
open Logs (* For logging *)
open Result (* For Result type *)

(** The main gRPC client environment type. Holds client connections/stubs. *)
type t = {
  indicator_chan: Grpc_lwt.Client.channel;
  prediction_chan: Grpc_lwt.Client.channel;
  order_execution_chan: Grpc_lwt.Client.channel;
}

(** Create a new gRPC client environment (channels only). *)
val create : indicator_addr:string -> prediction_addr:string -> ingestion_addr:string -> t

(** Start the client task to subscribe to indicator updates. *)
val start_indicator_subscription : t -> Strategy_data.t -> unit Lwt.t

(** Start the client task to subscribe to prediction updates. *)
val start_prediction_subscription : t -> Strategy_data.t -> unit Lwt.t

(** Send a trade order request. Returns Result Lwt.t. *)
val send_order : t -> string -> string -> Market_data_pb.OrderSide.t -> Market_data_pb.OrderType.t -> string -> string -> (unit, [> `GrpcError of string | `ExecutionError of string]) result Lwt.t