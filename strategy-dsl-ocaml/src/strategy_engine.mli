(** strategy_engine.mli - Strategy Execution Engine *)

open Lwt.Infix (* For Lwt binds *)
open Strategy_compiler (* Need compiled strategy type *)
open Strategy_data (* Need DataManager *)
open Strategy_grpc (* Need gRPC Clients *)
open Strategy_config (* Need Config types *)
open Strategy_ast (* Need AST types *)

(** Type for a loaded and compiled strategy *)
type loaded_strategy = {
  name: string;
  compiled_logic: compiled_strategy; (* The executable function *)
  mutable state: Strategy_compiler.exec_env; (* Mutable state for this strategy instance *)
  (* Add other metadata: parameters, indicators/predictions required *)
}

(** The main strategy engine type. Manages loaded strategies and executes them on data updates. *)
type t

(** Create a new strategy engine instance. *)
val create : Strategy_data.t -> Strategy_grpc.t -> Strategy_config.t -> t

(** Load strategies from files based on configuration. Returns unit Lwt.t. *)
val load_strategies : t -> unit Lwt.t

(** Start the main execution loop. This loop waits for new data and runs strategies. Returns unit Lwt.t. *)
val start_execution_loop : t -> unit Lwt.t

(** Get a list of names of currently loaded strategies. Returns string list Lwt.t. *)
val get_loaded_strategy_names : t -> string list Lwt.t

(** Stop the execution loop gracefully. *)
val stop_execution_loop : t -> unit Lwt.t