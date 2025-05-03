(** strategy_data.mli - Data Management for Strategy Engine *)

open Lwt.Infix (* For Lwt binds *)
open Lwt_mutex (* For mutexes *)
open Lwt_condition (* For condition variables *)
open Timestamps_proto.Google.Protobuf (* For Timestamp *)
open Indicator_data_pb (* For IndicatorValue *)
open Ml_prediction_pb (* For PredictionValue *)
open Market_data_pb (* For Trade, Quote, OrderBookUpdate *)
open Batteries.Deque (* Use Deque for history *)

(** Type for a market data item *)
type market_data_item = {
  timestamp: Timestamp.t;
  value: string; (* String representation of price or relevant value *)
  source: string; (* e.g., "trade", "quote", "order_book_update" *)
}

(** Type for an indicator data item *)
type indicator_data_item = {
  timestamp: Timestamp.t;
  value: string; (* String representation of indicator value *)
  indicator_name: string;
}

(** Type for a prediction data item *)
type prediction_data_item = {
  timestamp: Timestamp.t;
  value: string; (* String representation of prediction value *)
  prediction_type: string;
}


(** The main data manager type. Stores recent data streams per symbol. *)
type t

(** Create a new data manager instance. *)
val create : max_history_size:int -> t

(** Add a market data event to the manager. Returns unit Lwt.t. *)
val add_market_data : t -> market_data_pb.MarketDataEvent.t -> unit Lwt.t

(** Add an indicator value to the manager. Returns unit Lwt.t. *)
val add_indicator_value : t -> Indicator_data_pb.IndicatorValue.t -> unit Lwt.t

(** Add a prediction value to the manager. Returns unit Lwt.t. *)
val add_prediction_value : t -> Ml_prediction_pb.PredictionValue.t -> unit Lwt.t

(** Get the latest market data item for a symbol. Returns (timestamp, value, source) option Lwt.t. *)
val get_latest_market_data : t -> string -> (Timestamp.t * string * string) option Lwt.t

(** Get the latest indicator value for a specific indicator on a symbol. Returns (timestamp, value) option Lwt.t. *)
val get_latest_indicator : t -> string -> string -> (Timestamp.t * string * string) option Lwt.t

(** Get the previous indicator value for a specific indicator on a symbol (1 period ago). Returns (timestamp, value) option Lwt.t. *)
val get_previous_indicator : t -> string -> string -> (Timestamp.t * string * string) option Lwt.t

(** Get the latest prediction value for a specific type on a symbol. Returns (timestamp, value) option Lwt.t. *)
val get_latest_prediction : t -> string -> string -> (Timestamp.t * string * string) option Lwt.t

(** Get the previous prediction value for a specific type on a symbol (1 period ago). Returns (timestamp, value) option Lwt.t. *)
val get_previous_prediction : t -> string -> string -> (Timestamp.t * string * string) option Lwt.t


(** Get a list of symbols currently tracked by the data manager. *)
val get_tracked_symbols : t -> string list Lwt.t

(** Get a list of known indicator names seen by the data manager. *)
val get_known_indicator_names : t -> string list Lwt.t

(** Get a list of known prediction types seen by the data manager. *)
val get_known_prediction_names : t -> string list Lwt.t

(** Wait for new data notification and return symbols that have new data. *)
val wait_for_new_data : t -> string list Lwt.t