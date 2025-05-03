(** strategy_health_check.mli - HTTP Health Check Server *)

open Lwt.Infix (* For Lwt binds *)
open Strategy_data (* Need DataManager for checks *)
open Strategy_engine (* Need Engine for checks *)
open Strategy_grpc (* Need gRPC clients for checks *)

(** Start the HTTP health check server. Returns unit Lwt.t. *)
val start_server : listen_address:string -> Strategy_data.t -> Strategy_engine.t -> Strategy_grpc.t -> unit Lwt.t