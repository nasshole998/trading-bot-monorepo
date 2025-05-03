(** strategy_compiler.ml - Compiler (Interpreter) for the Strategy DSL AST *)

open Strategy_ast
open Strategy_compiler_intf (* Include interface *)
open Strategy_data (* Need data types *)
open Strategy_grpc (* Need gRPC client for order execution *)
open Lwt.Infix (* For Lwt binds *)
open Logs (* For logging *)
open Batteries.Float (* For nan, isnan *)

exception ExecutionError of string

(* Helper to evaluate an expression *)
let rec eval_expr env data_mgr symbol = function
  | Const v -> Lwt.return v
  | Get_indicator name ->
      let* latest_opt = Strategy_data.get_latest_indicator data_mgr symbol name in
      (match latest_opt with
       | Some (_, value_str, _) ->
           (match float_of_string_opt value_str with
            | Some f -> Lwt.return (Numeric f)
            | None ->
                error (fun m -> m "Execution error: Could not convert indicator '%s' value '%s' to float" name value_str);
                Lwt.fail (ExecutionError (Printf.sprintf "Invalid numeric value for indicator '%s': %s" name value_str)))
       | None ->
           error (fun m -> m "Execution error: No data found for indicator '%s'" name);
           Lwt.fail (ExecutionError (Printf.sprintf "No data found for indicator '%s'" name)))
  | Get_prediction name ->
      let* latest_opt = Strategy_data.get_latest_prediction data_mgr symbol name in
      (match latest_opt with
       | Some (_, value_str, _) ->
           (* Attempt float first, then boolean *)
           (match float_of_string_opt value_str with
            | Some f -> Lwt.return (Numeric f)
            | None ->
                (match bool_of_string_opt value_str with
                 | Some b -> Lwt.return (Boolean b)
                 | None ->
                     error (fun m -> m "Execution error: Could not convert prediction '%s' value '%s' to float or boolean" name value_str);
                     Lwt.fail (ExecutionError (Printf.sprintf "Invalid value for prediction '%s': %s" name value_str)))))
       | None ->
           error (fun m -> m "Execution error: No data found for prediction '%s'" name);
           Lwt.fail (ExecutionError (Printf.sprintf "No data found for prediction '%s'" name)))
  | Get_prev_indicator name ->
      let* prev_opt = Strategy_data.get_previous_indicator data_mgr symbol name in
      (match prev_opt with
       | Some (_, value_str, _) ->
           (match float_of_string_opt value_str with
            | Some f -> Lwt.return (Numeric f)
            | None ->
                error (fun m -> m "Execution error: Could not convert previous indicator '%s' value '%s' to float" name value_str);
                Lwt.fail (ExecutionError (Printf.sprintf "Invalid numeric value for previous indicator '%s': %s" name value_str)))
       | None ->
           debug (fun m -> m "Execution: No previous data found for indicator '%s'. Using 0.0" name);
           Lwt.return (Numeric 0.0) (* Handle missing previous value - use 0.0 or raise error? Let's use 0.0 *)
       )
  | Get_prev_prediction name ->
      let* prev_opt = Strategy_data.get_previous_prediction data_mgr symbol name in
      (match prev_opt with
       | Some (_, value_str, _) ->
           (* Attempt float first, then boolean *)
           (match float_of_string_opt value_str with
            | Some f -> Lwt.return (Numeric f)
            | None ->
                (match bool_of_string_opt value_str with
                 | Some b -> Lwt.return (Boolean b)
                 | None ->
                     error (fun m -> m "Execution error: Could not convert previous prediction '%s' value '%s' to float or boolean" name value_str);
                     Lwt.fail (ExecutionError (Printf.sprintf "Invalid value for previous prediction '%s': %s" name value_str)))))
       | None ->
           debug (fun m -> m "Execution: No previous data found for prediction '%s'. Using default (Numeric 0.0 or Boolean false)" name);
            (* Needs type context to return correct default *)
            (* For simplicity, let's assume Numeric 0.0 default for missing prev predictions *)
           Lwt.return (Numeric 0.0) (* Or Lwt.return (Boolean false) depending on expected type *)
       )
  | Get_var name ->
       (match Hashtbl.find_opt env name with
       | Some v -> Lwt.return v
       | None ->
           error (fun m -> m "Execution error: Accessing undeclared variable '%s'" name);
           Lwt.fail (ExecutionError (Printf.sprintf "Accessing undeclared variable '%s'" name)))

  | Bin_op (op, e1, e2) ->
      let* v1 = eval_expr env data_mgr symbol e1 in
      let* v2 = eval_expr env data_mgr symbol e2 in
      Lwt.return (
          match op with
          | Add | Sub | Mul | Div ->
              (match v1, v2 with
               | Numeric f1, Numeric f2 -> Numeric (
                   match op with
                   | Add -> f1 +. f2
                   | Sub -> f1 -. f2
                   | Mul -> f1 *. f2
                   | Div -> if f2 <> 0.0 then f1 /. f2 else (error (fun m -> m "Execution error: Division by zero"); nan)) (* Handle division by zero, return NaN *)
               | _ ->
                   error (fun m -> m "Execution error: Arithmetic operation on non-numeric types");
                   raise (ExecutionError "Arithmetic operation on non-numeric types"))
          | Eq -> Boolean (v1 = v2) (* OCaml's structural equality *)
          | Neq -> Boolean (v1 <> v2)
          | Lt | Gt | Leq | Geq ->
              (match v1, v2 with
               | Numeric f1, Numeric f2 -> Boolean (
                   match op with
                   | Lt -> f1 < f2
                   | Gt -> f1 > f2
                   | Leq -> f1 <= f2
                   | Geq -> f1 >= f2
                   | _ -> assert false (* Should not happen *)
               )
               | _ ->
                   error (fun m -> m "Execution error: Comparison operation on non-numeric types");
                   raise (ExecutionError "Comparison operation on non-numeric types for numeric comparison"))
          | And | Or ->
              (match v1, v2 with
               | Boolean b1, Boolean b2 -> Boolean (
                   match op with
                   | And -> b1 && b2
                   | Or -> b1 || b2
                   | _ -> assert false (* Should not happen *)
               )
               | _ ->
                   error (fun m -> m "Execution error: Logical operation on non-boolean types");
                   raise (ExecutionError "Logical operation on non-boolean types"))
      )

(* Helper to execute an action *)
let execute_action action_env data_mgr grpc_client_env symbol = function
  | Buy qty_expr ->
      let* qty_value = eval_expr action_env data_mgr symbol qty_expr in
      (match qty_value with
       | Numeric qty when qty > 0.0 && not (isnan qty) && not (isinf qty) ->
           info (fun m -> m "Executing BUY %.8f on %s" qty symbol); (* Log with more precision *)
           (* In a real system, you might get exchange from strategy config or symbol mapping *)
           let exchange = "binance" (* Placeholder exchange *) in
           Strategy_grpc.send_order grpc_client_env exchange symbol Market_data_pb.ORDER_SIDE_BUY Market_data_pb.ORDER_TYPE_MARKET (string_of_float qty) "0.0" (* Price 0.0 for Market order *)
       | _ ->
           error (fun m -> m "BUY quantity expression evaluated to non-positive or invalid numeric value");
           Lwt.return (Error (`ExecutionError "BUY quantity must be a positive numeric value")))
  | Sell qty_expr ->
      let* qty_value = eval_expr action_env data_mgr symbol qty_expr in
      (match qty_value with
       | Numeric qty when qty > 0.0 && not (isnan qty) && not (isinf qty) ->
           info (fun m -> m "Executing SELL %.8f on %s" qty symbol); (* Log with more precision *)
           let exchange = "binance" (* Placeholder exchange *) in
           Strategy_grpc.send_order grpc_client_env exchange symbol Market_data_pb.ORDER_SIDE_SELL Market_data_pb.ORDER_TYPE_MARKET (string_of_float qty) "0.0"
       | _ ->
           error (fun m -> m "SELL quantity expression evaluated to non-positive or invalid numeric value");
           Lwt.return (Error (`ExecutionError "SELL quantity must be a positive numeric value")))
  | Hold ->
      debug (fun m -> m "Executing HOLD");
      Lwt.return (Ok ()) (* Hold is a no-op, but needs to be in Lwt *)
  | Log expr ->
      let* value = eval_expr action_env data_mgr symbol expr in
      info (fun m -> m "Strategy Log: %s" (Sexplib.Sexp.to_string (sexp_of_value value))); (* Log value as sexp *)
      Lwt.return (Ok ()) (* Log is async in terms of execution flow *)

(* Helper to execute a list of statements *)
(* Now takes and returns the execution environment (state) *)
let rec execute_statements statement_env data_mgr grpc_client_env symbol = function
  | [] -> Lwt.return (statement_env, []) (* Return updated env and empty list of executed actions *)
  | stmt :: rest ->
      let* updated_env, actions1 = execute_statement statement_env data_mgr grpc_client_env symbol stmt in
      let* final_env, actions2 = execute_statements updated_env data_mgr grpc_client_env symbol rest in
      Lwt.return (final_env, actions1 @ actions2) (* Pass updated env and concatenate actions *)


(* Helper to execute a single statement *)
(* Now takes and returns the execution environment (state) *)
and execute_statement statement_env data_mgr grpc_client_env symbol = function
  | If (cond_expr, then_stmts, else_stmts) ->
      let* cond_value = eval_expr statement_env data_mgr symbol cond_expr in
      (match cond_value with
       | Boolean true ->
           debug (fun m -> m "IF condition true. Executing THEN block.");
           execute_statements statement_env data_mgr grpc_client_env symbol then_stmts (* Execute THEN block *)
       | Boolean false ->
           debug (fun m -> m "IF condition false. Executing ELSE block.");
           execute_statements statement_env data_mgr grpc_client_env symbol else_stmts (* Execute ELSE block *)
       | _ ->
           error (fun m -> m "Execution error: IF condition must be boolean");
           Lwt.fail (ExecutionError "IF condition must be boolean"))
  | Action action ->
      execute_action statement_env data_mgr grpc_client_env symbol action >>= fun result ->
      (match result with
       | Ok () -> Lwt.return (statement_env, [action]) (* Return current env and the action if execution was attempted *)
       | Error (`GrpcError msg) ->
           error (fun m -> m "gRPC error executing action: %s" msg);
           Lwt.return (statement_env, []) (* Do not return action on gRPC error? Or return error state? *)
       | Error (`ExecutionError msg) ->
           error (fun m -> m "Execution error executing action: %s" msg);
           Lwt.return (statement_env, []) (* Do not return action on execution error? *))
  | Var_decl (name, _) ->
      (* Var_decl statements are handled during strategy loading to set up initial state.
         This branch should not be hit during main statement execution. *)
       debug (fun m -> m "Encountered Var_decl '%s' during execution. Skipping." name);
       Lwt.return (statement_env, [])
  | Set_var (name, expr) ->
      let* value_to_set = eval_expr statement_env data_mgr symbol expr in
      (* Check if variable exists in env (type checker should handle this) *)
      if Hashtbl.mem statement_env name then (
          debug (fun m -> m "Setting variable '%s' to %s" name (Sexplib.Sexp.to_string (sexp_of_value value_to_set)));
          Hashtbl.replace statement_env name value_to_set; (* Update the value in the environment *)
          Lwt.return (statement_env, []) (* Return updated env and no action *)
      ) else (
          error (fun m -> m "Execution error: Attempted to set undeclared variable '%s'" name);
          Lwt.fail (ExecutionError (Printf.sprintf "Attempted to set undeclared variable '%s'" name))
      )


(* Compile (Interpret) a strategy AST into an executable function. *)
(* The compiled strategy now takes/returns the exec_env *)
let compile_strategy strategy_ast : compiled_strategy =
  info (fun m -> m "Compiling (Interpreting) strategy '%s'" strategy_ast.name);
  (* The "compiled" strategy is simply an OCaml function that closes over the AST *)
  let execute_func exec_env data_mgr grpc_client_env symbol : (exec_env * action list) Lwt.t =
      (* execute_statements now manages state updates *)
      execute_statements exec_env data_mgr grpc_client_env symbol strategy_ast.logic
  in
  info (fun m -> m "Strategy '%s' compiled successfully" strategy_ast.name);
  execute_func


(* Execute a compiled strategy using the latest data from the data manager. *)
(* Now takes the strategy's current execution environment (state) as input and returns the updated state *)
let execute_compiled_strategy (compiled_strat : compiled_strategy) (data_mgr : Strategy_data.t) (grpc_client_env : Strategy_grpc.t) (symbol : string) (current_exec_env : exec_env) : (exec_env * action list) Lwt.t =
    info (fun m -> m "Executing compiled strategy for symbol '%s'" symbol);

    (* The execution environment (state) is passed in and updated by the compiled_strat function.
       It already contains the variables.
       We need to ensure it also contains the *current* indicator and prediction values for *this execution cycle*.
       We will merge the data values into a temporary environment or pass them separately.
       Let's pass the data manager and symbol to eval_expr so it can fetch values on demand.
       This avoids populating a massive env upfront and allows fetching previous values easily.
       The VARs live in the `current_exec_env`. *)

    (* The compiled_strat function already has the correct signature.
       We just need to call it with the current_exec_env.
       The `eval_expr` helper inside the compiled function
       needs access to the DataManager to fetch indicator/prediction values.
       Let's pass data_mgr and symbol into `eval_expr`.
       The `execute_statement` and `execute_statements` helpers also need these. *)

    (* The compiler function `compile_strategy` returns a function that expects `exec_env`, `data_mgr`, `grpc_client_env`, `symbol`.
       So `compiled_strat` already has this signature.
       We just call it with the current state and resources. *)
    Lwt.catch (fun () ->
       compiled_strat current_exec_env data_mgr grpc_client_env symbol
    ) (fun e ->
       error (fun m -> m "Unhandled exception during strategy execution for symbol '%s': %s" symbol (Printexc.to_string e));
       Lwt.return (current_exec_env, []) (* Return current state and no actions on error *)
    )