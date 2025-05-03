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
open Batteries.Hashtbl (* For Hashtbl functions *)
open Prometheus (* For metrics *)

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
  loaded_strategies: (string, loaded_strategy) Hashtbl.t; (* Hashtbl for strategies by name *)
  mutable stop_requested: bool; (* Flag for graceful shutdown *)
  stop_promise: unit Lwt.t; (* Promise that resolves when stop is requested *)
  resolve_stop: unit Lwt.u; (* Resolver for stop_promise *)
  engine_mutex: Lwt_mutex.t; (* Mutex for engine state if needed *)
}

(* Prometheus Metrics *)
let executed_strategies_total =
  Prometheus.Counter.v
    ~help:"Total number of strategy execution runs triggered by data updates."
    "strategy_engine_executed_strategies_total"

let strategy_execution_errors_total =
  Prometheus.Counter.v
    ~help:"Total number of errors encountered during strategy execution."
    "strategy_engine_execution_errors_total"

(** Create a new strategy engine instance. *)
let create data_manager grpc_clients config =
   let stop_promise, resolve_stop = Lwt.wait () in
  {
    data_manager = data_manager;
    grpc_clients = grpc_clients;
    config = config;
    loaded_strategies = Hashtbl.create 10; (* Initial size for strategies map *)
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
    | Strategy_parser.Error msg ->
        let pos = Lexing.lexeme_start_p lexbuf in
        let file = pos.Lexing.pos_fname in
        let line = pos.Lexing.pos_lnum in
        let col = pos.Lexing.pos_cnum - pos.Lexing.pos_bol in
        let full_msg = Printf.sprintf "Syntax error in %s at line %d, column %d: %s" file line col msg in
        error (fun m -> m "%s" full_msg);
        close_in channel;
        Result.Error (`ParseError full_msg)
    | e ->
        error (fun m -> m "Unexpected error during parsing %s: %s" filepath (Printexc.to_string e));
        close_in channel;
        Result.Error (`ParseError (Printf.sprintf "Unexpected parsing error: %s" (Printexc.to_string e)))
  in
  close_in channel;

  strategy_ast |> Result.map_error (fun msg -> `ParseError msg) (* Ensure parse errors are tagged *)
  |> Result.bind_lwt (fun ast ->
      info (fun m -> m "Strategy '%s' parsed successfully" ast.name);
      (* Type check the strategy *)
      Lwt.catch (fun () ->
         Strategy_typecheck.typecheck_strategy data_manager ast >>= fun type_env ->
         info (fun m -> m "Strategy '%s' type checked successfully" ast.name);

         (* Create the initial execution environment (state) *)
         let initial_exec_env = Hashtbl.create (Hashtbl.length type_env) in
         ast.initial_state |> List.iter (fun (name, initial_value) ->
             Hashtbl.add initial_exec_env name initial_value (* Add initial variable values *)
         );
         info (fun m -> m "Initial state created for '%s'" ast.name);

         (* Compile/Interpret the strategy *)
         let compiled = Strategy_compiler.compile_strategy ast in
         info (fun m -> m "Strategy '%s' compiled successfully" ast.name);

         Lwt.return (Result.Ok { name = ast.name; compiled_logic = compiled; state = initial_exec_env }) (* Return loaded strategy *)

      ) (fun e ->
         let msg = Printf.sprintf "Processing error for '%s': %s" ast.name (Printexc.to_string e) in
         error (fun m -> m "%s" msg);
         Lwt.return (Result.Error (`ProcessError msg)) (* Wrap other errors *)
      )
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
      >>= fun result ->
      (match result with
       | Result.Ok loaded_strat -> Lwt.return (Some loaded_strat)
       | Result.Error (`ParseError msg) ->
           error (fun m -> m "Failed to load strategy '%s' due to parse error: %s" filename msg);
           Lwt.return None
       | Result.Error (`ProcessError msg) ->
           error (fun m -> m "Failed to load strategy '%s' due to processing error: %s" filename msg);
           Lwt.return None
       (* Add other error types here *)
      )
    else (
      warning (fun m -> m "Strategy file not found: %s" filepath);
      Lwt.return None (* File not found, return None *)
    )
  ) active_strategy_files in

  (* Wait for all loading tasks to complete *)
  Lwt.all loading_tasks >>= fun results ->
  let successfully_loaded = List.filter_map (fun x -> x) results in (* Filter out None *)

  Lwt_mutex.with_lock engine.engine_mutex (fun () ->
     (* Clear existing loaded strategies and add the new ones *)
     Hashtbl.clear engine.loaded_strategies;
     List.iter (fun s -> Hashtbl.add engine.loaded_strategies s.name s) successfully_loaded;
     info (fun m -> m "Finished loading strategies. %d loaded successfully." (Hashtbl.length engine.loaded_strategies));
     Lwt.return_unit
  )


(* Main execution loop task *)
let rec execution_loop engine =
  if engine.stop_requested then (
    info (fun m -> m "Execution loop stopping.");
    Lwt.return_unit (* Stop if shutdown requested *)
  ) else (
    (* Wait for new data notification *)
    let* symbols_with_new_data = Strategy_data.wait_for_new_data engine.data_manager in
    debug (fun m -> m "Received new data notification for symbols: %s" (String.concat ", " symbols_with_new_data));

    (* Increment metric for triggered executions *)
    Prometheus.Counter.inc executed_strategies_total;

    (* Iterate through symbols with new data *)
    let* () = Lwt_list.iter_s (fun symbol ->
      (* Iterate through loaded strategies and execute those relevant to the symbol *)
      (* Note: We iterate over a copy of the keys to avoid issues if the Hashtbl is modified *)
      let strategy_names_to_run = Lwt_main.run (Lwt_mutex.with_lock engine.engine_mutex (fun () ->
         Hashtbl.keys engine.loaded_strategies |> List.of_seq |> Lwt.return
      )) (* Use Lwt_main.run for a quick sync access, or refactor *)
      (* Correction: Accessing mutable shared state (Hashtbl) needs Lwt_mutex.
                     Let's get the list of strategies while holding the mutex. *)
       let strategies_to_run = Lwt_main.run (Lwt_mutex.with_lock engine.engine_mutex (fun () ->
          Hashtbl.to_list engine.loaded_strategies |> Lwt.return (* Get list of (name, strategy) *)
       ))
      in


      let* () = Lwt_list.iter_s (fun (strat_name, loaded_strat) ->
        (* Check if the strategy is relevant to this symbol.
           Currently, the DSL doesn't specify symbol relevance explicitly.
           Assuming all strategies are relevant to any symbol for simplicity.
           A real system would need mapping from strategy to symbol, e.g., based on indicator/prediction names used. *)

        debug (fun m -> m "Executing strategy '%s' for symbol '%s'" loaded_strat.name symbol);
        Strategy_compiler.execute_compiled_strategy loaded_strat.compiled_logic engine.data_manager engine.grpc_clients symbol loaded_strat.state >>= fun result ->

        match result with
        | Result.Ok (updated_state, actions) ->
            debug (fun m -> m "Strategy '%s' execution succeeded. Generated %d actions. State updated." loaded_strat.name (List.length actions));
            (* Update the strategy's state *)
            loaded_strat.state <- updated_state;
            (* Actions are executed *within* execute_compiled_strategy by calling grpc_clients *)
            Lwt.return_unit (* Move to next strategy *)
        | Result.Error (`ExecutionError msg) ->
            error (fun m -> m "Strategy '%s' execution failed for symbol '%s': %s" loaded_strat.name symbol msg);
            Prometheus.Counter.inc strategy_execution_errors_total; (* Increment error metric *)
            Lwt.return_unit (* Continue with next strategy even if one fails *)
        (* Add other specific error handling here *)
      ) strategies_to_run (* Await sequential execution of strategies for this symbol *)
    ) symbols_with_new_data (* Await sequential processing of symbols *)
    in

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