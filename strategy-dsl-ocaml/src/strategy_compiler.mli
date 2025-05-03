(** strategy_compiler.mli - Compiler (Interpreter) for the Strategy DSL AST *)

open Strategy_ast (* Need AST types *)
open Strategy_data (* Need data types *)
open Strategy_grpc (* Need gRPC client for order execution *)
open Lwt.Infix (* For Lwt binds *)

(** Type for the execution environment. Maps names (for indicators, predictions, variables) to their *runtime values*. *)
type exec_env = (string, Strategy_ast.value) Hashtbl.t

(** Type for a compiled strategy. It's a function that takes the current execution environment,
    the DataManager, the gRPC client environment, and the current symbol, and returns
    the updated execution environment and a list of actions to perform (wrapped in Lwt). *)
type compiled_strategy = exec_env -> Strategy_data.t -> Strategy_grpc.t -> string -> (exec_env * Strategy_ast.action list) Lwt.t

(** Exception raised during execution errors. *)
exception ExecutionError of string

(** Compile (Interpret) a strategy AST into an executable function. *)
val compile_strategy : strategy -> compiled_strategy

(** Execute a compiled strategy using the latest data from the data manager.
    Handles setting up the initial execution environment based on data. *)
val execute_compiled_strategy : compiled_strategy -> Strategy_data.t -> Strategy_grpc.t -> string -> exec_env -> (exec_env * Strategy_ast.action list) Lwt.t