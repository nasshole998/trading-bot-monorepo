(** strategy_engine.ml - Strategy Execution Engine *)

open Strategy_engine_intf (* Include interface *)
open Strategy_ast (* Need AST types *)
open Strategy_lexer (* Need lexer *)
open Strategy_parser (* Need parser *)
open Strategy_typecheck (* Need type checker *)
open Strategy_compiler (* Need compiler *)
open Strategy_data (* Need DataManager *)
open Strategy_grpc (* Need gRPC Clients *)
open Strategy_config (* Need Config types *)
open Lwt.Infix (* For Lwt binds *)
open Logs (* For logging *)
open Result.Infix (* For Result binds *)
open Batteries.Hashtbl (* For Hashtbl.copy *)


(** Internal structure for a loaded and compiled strategy *)
type loaded_strategy = {
  name: string;
  compiled_logic: compiled_strategy; (* The executable function *)
  mutable state: Strategy_compiler.exec_env; (* Mutable state for this strategy instance *)
  (* Add other metadata: parameters, indicators/predictions required *)
}

(** The main strategy engine type. *)
type t = {
  data_manager: Strategy_data.t;
  grpc_clients: Strategy_grpc.t;
  config: Strategy_config.t;
  mutable loaded_strategies: loaded_strategy list; (* List of strategies *)
  mutable stop_requested: bool; (* Flag for graceful shutdown *)
  stop_promise: unit Lwt.t; (* Promise that resolves when stop is requested *)
  resolve_stop: unit Lwt.u; (* Resolver for stop_promise *)
  engine_mutex: Lwt_mutex.t; (* Mutex for engine state if needed, e.g. loaded_strategies *)
}

(** Create a new strategy engine instance. *)
let create data_manager grpc_clients config =
   let stop_promise, resolve_stop = Lwt.wait () in
  {
    data_manager = data_manager;
    grpc_clients = grpc_clients;
    config = config;
    loaded_strategies = [];
    stop_requested = false;
    stop_promise = stop_promise;
    resolve_stop = resolve_stop;
    engine_mutex = Lwt_mutex.create ();
  }

(* Helper to parse, type check, and compile a single strategy file *)
let process_strategy_file data_manager filepath =
  info (fun m -> m "Processing strategy file: %s" filepath);
  let channel = open_in filepath in
  let lexbuf = Lexing.from_channel channel in
  lexbuf.Lexing.lex_curr_p <- { lexbuf.Lexing.lex_curr_p with Lexing.pos_fname = filepath }; (* Set filename for error messages *)
  let strategy_ast =
    try
      Strategy_parser.strategy Strategy_lexer.token lexbuf (* Parse the file *)
    with
    | Strategy_parser.Error ->
        let pos = Lexing.lexeme_start_p lexbuf in
        let file = pos.Lexing.pos_fname in
        let line = pos.Lexing.pos_lnum in
        let col = pos.Lexing.pos_cnum - pos.Lexing.pos_bol in
        let msg = Printf.sprintf "Syntax error in %s at line %d, column %d" file line col in
        error (fun m -> m "%s" msg);
        close_in channel;
        raise (Strategy_parser.Error msg) (* Re-raise with context *)
    | e ->
        error (fun m -> m "Unexpected error during parsing %s: %s" filepath (Printexc.to_string e));
        close_in channel;
        raise e (* Re-raise unexpected exceptions *)
  in
  close_in channel;
  info (fun m -> m "Strategy '%s' parsed successfully" strategy_ast.name);

  (* Type check the strategy *)
  Lwt.catch (fun () ->
    Strategy_typecheck.typecheck_strategy data_manager strategy_ast >>= fun type_env ->
    info (fun m -> m "Strategy '%s' type checked successfully" strategy_ast.name);

    (* Create the initial execution environment (state) *)
    let initial_exec_env = Hashtbl.create (Hashtbl.length type_env) in
    strategy_ast.initial_state |> List.iter (fun (name, initial_value) ->
        Hashtbl.add initial_exec_env name initial_value (* Add initial variable values *)
    );
    info (fun m -> m "Initial state created for '%s'" strategy_ast.name);

    (* Compile/Interpret the strategy *)
    let compiled = Strategy_compiler.compile_strategy strategy_ast in
    info (fun m -> m "Strategy '%s' compiled successfully" strategy_ast.name);

    Lwt.return (Some { name = strategy_ast.name; compiled_logic = compiled; state = initial_exec_env }) (* Return loaded strategy *)

  ) (fun e ->
    error (fun m -> m "Skipping strategy file %s due to processing error: %s" filepath (Printexc.to_string e));
    Lwt.return None (* Return None on any error during type check or compile *)
  )


(** Load strategies from files based on configuration. *)
let load_strategies engine =
  info (fun m -> m "Loading strategies...");
  let strategies_path = engine.config.strategy_engine.strategies_path in
  let active_strategy_files = engine.config.strategy_engine.active_strategies in

  (* Process each active strategy file *)
  let loading_tasks = List.map (fun filename ->
    let filepath = Filename.concat strategies_path filename in
    if Sys.file_exists filepath then
      process_strategy_file engine.data_manager filepath (* Lwt task for processing *)
    else (
      warning (fun m -> m "Strategy file not found: %s" filepath);
      Lwt.return None (* File not found, return None *)
    )
  ) active_strategy_files in

  (* Wait for all loading tasks to complete *)
  Lwt.all loading_tasks >>= fun results ->
  let successfully_loaded = List.filter_map (fun x -> x) results in (* Filter out None *)

  Lwt_mutex.with_lock engine.engine_mutex (fun () ->
     engine.loaded_strategies <- successfully_loaded; (* Update the list of loaded strategies *)
     info (fun m -> m "Finished loading strategies. %d loaded successfully." (List.length engine.loaded_strategies));
     Lwt.return_unit
  )


(* Main execution loop task *)
let rec execution_loop engine =
  if engine.stop_requested then (
    info (fun m -> m "Execution loop stopping.");
    Lwt.return_unit (* Stop if shutdown requested *)
  ) else (
    (* Wait for new data notification *)
    Strategy_data.wait_for_new_data engine.data_manager >>= fun symbols_with_new_data ->
    debug (fun m -> m "Received new data notification for symbols: %s" (String.concat ", " symbols_with_new_data));

    (* Iterate through symbols with new data *)
    Lwt_list.iter_s (fun symbol ->
      (* Iterate through loaded strategies and execute those relevant to the symbol *)
      Lwt_mutex.with_lock engine.engine_mutex (fun () -> (* Lock access to loaded_strategies *)
        Hashtbl.copy engine.loaded_strategies (* Make a copy to avoid issues if list changes *)
        |> Hashtbl.to_list (* Convert to list of (name, strategy) *)
      ) >>= fun strategies_to_run ->

      Lwt_list.iter_s (fun (strat_name, loaded_strat) ->
        (* Check if the strategy is relevant to this symbol.
           Currently, the DSL doesn't specify symbol relevance explicitly.
           Assuming all strategies are relevant to any symbol for simplicity.
           A real system would need mapping from strategy to symbol. *)

        Lwt.catch (fun () ->
          debug (fun m -> m "Executing strategy '%s' for symbol '%s'" loaded_strat.name symbol);
          Strategy_compiler.execute_compiled_strategy loaded_strat.compiled_logic engine.data_manager engine.grpc_clients symbol loaded_strat.state >>= fun (updated_state, actions) ->
          debug (fun m -> m "Strategy '%s' generated %d actions. State updated." loaded_strat.name (List.length actions));
          (* Update the strategy's state *)
          loaded_strat.state <- updated_state;
          (* Actions are executed *within* execute_compiled_strategy by calling grpc_clients *)
          Lwt.return_unit (* Move to next strategy *)
        ) (fun e ->
          error (fun m -> m "Error executing strategy '%s' for symbol '%s': %s" loaded_strat.name symbol (Printexc.to_string e));
          Lwt.return_unit (* Continue with next strategy even if one fails *)
        )
      ) strategies_to_run (* Await sequential execution of strategies for this symbol *)
    ) symbols_with_new_data >>= fun () -> (* Await sequential processing of symbols *)
    execution_loop engine (* Continue the loop *)
  )


(** Start the main execution loop. *)
let start_execution_loop engine =
  info (fun m -> m "Starting execution loop.");
  execution_loop engine (* Start the recursive loop *)

(** Get a list of names of currently loaded strategies. Returns string list Lwt.t. *)
let get_loaded_strategy_names engine =
   Lwt_mutex.with_lock engine.engine_mutex (fun () ->
      Hashtbl.keys engine.loaded_strategies |> List.of_seq |> Lwt.return
   )


(** Stop the execution loop gracefully. *)
let stop_execution_loop engine =
   info (fun m -> m "Requesting execution loop stop.");
   engine.stop_requested <- true; (* Set the stop flag *)
   Lwt_condition.signal engine.data_manager.new_data_condition; (* Signal the condition variable to unblock wait *)
   Lwt.return_unit (* Return immediately *)