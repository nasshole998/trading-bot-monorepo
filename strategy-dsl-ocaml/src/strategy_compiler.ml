(** strategy_compiler.ml - Compiler (Interpreter) for the Strategy DSL AST *)

open Strategy_ast
open Strategy_compiler_intf (* Include interface *)
open Strategy_data (* Need data types *)
open Strategy_grpc (* Need gRPC client for order execution *)
open Lwt.Infix (* For Lwt binds *)
open Result (* Need Result type *)
open Result.Infix (* For Result binds *)
open Logs (* For logging *)
open Batteries.Float (* For nan, isnan *)
open Batteries.Int (* For Batteries.Int.neg *)


exception ExecutionError of string

(* Helper to convert Result to Lwt Result *)
let (let*) x f = Lwt.bind x f
let (and*) x y =
  let* a = x in
  let* b = y in
  Lwt.return (a, b)

(* Helper to evaluate an expression, returning Result Lwt.t *)
let rec eval_expr env data_mgr symbol = function
  | Const v -> Lwt.return (Ok v)
  | Get_indicator name ->
      let* latest_opt = Strategy_data.get_latest_indicator data_mgr symbol name in
      (match latest_opt with
       | Some (_, value_str, _) ->
           (match float_of_string_opt value_str with
            | Some f -> Lwt.return (Ok (Numeric f))
            | None ->
                error (fun m -> m "Execution error: Could not convert indicator '%s' value '%s' to float" name value_str);
                Lwt.return (Error (`ExecutionError (Printf.sprintf "Invalid numeric value for indicator '%s': %s" name value_str)))))
       | None ->
           debug (fun m -> m "Execution: No data found for indicator '%s'. Using 0.0" name);
           Lwt.return (Ok (Numeric 0.0)) (* Handle missing data - use 0.0 or error? Using 0.0 *)
       )
  | Get_prediction name ->
      let* latest_opt = Strategy_data.get_latest_prediction data_mgr symbol name in
      (match latest_opt with
       | Some (_, value_str, _) ->
           (* Attempt float first, then boolean *)
           (match float_of_string_opt value_str with
            | Some f -> Lwt.return (Ok (Numeric f))
            | None ->
                (match bool_of_string_opt value_str with
                 | Some b -> Lwt.return (Ok (Boolean b))
                 | None ->
                     error (fun m -> m "Execution error: Could not convert prediction '%s' value '%s' to float or boolean" name value_str);
                     Lwt.return (Error (`ExecutionError (Printf.sprintf "Invalid value for prediction '%s': %s" name value_str))))))
       | None ->
            debug (fun m -> m "Execution: No data found for prediction '%s'. Using default (Numeric 0.0)" name);
            Lwt.return (Ok (Numeric 0.0)) (* Handle missing data *)
       )
  | Get_prev_indicator name ->
      let* prev_opt = Strategy_data.get_previous_indicator data_mgr symbol name in
      (match prev_opt with
       | Some (_, value_str, _) ->
           (match float_of_string_opt value_str with
            | Some f -> Lwt.return (Ok (Numeric f))
            | None ->
                error (fun m -> m "Execution error: Could not convert previous indicator '%s' value '%s' to float" name value_str);
                Lwt.return (Error (`ExecutionError (Printf.sprintf "Invalid numeric value for previous indicator '%s': %s" name value_str)))))
       | None ->
           debug (fun m -> m "Execution: No previous data found for indicator '%s'. Using 0.0" name);
           Lwt.return (Ok (Numeric 0.0)) (* Handle missing previous value - use 0.0 *)
       )
  | Get_prev_prediction name ->
      let* prev_opt = Strategy_data.get_previous_prediction data_mgr symbol name in
      (match prev_opt with
       | Some (_, value_str, _) ->
           (* Attempt float first, then boolean *)
           (match float_of_string_opt value_str with
            | Some f -> Lwt.return (Ok (Numeric f))
            | None ->
                (match bool_of_string_opt value_str with
                 | Some b -> Lwt.return (Ok (Boolean b))
                 | None ->
                     error (fun m -> m "Execution error: Could not convert previous prediction '%s' value '%s' to float or boolean" name value_str);
                     Lwt.return (Error (`ExecutionError (Printf.sprintf "Invalid value for previous prediction '%s': %s" name value_str))))))
       | None ->
            debug (fun m -> m "Execution: No previous data found for prediction '%s'. Using default (Numeric 0.0)" name);
            Lwt.return (Ok (Numeric 0.0)) (* Handle missing previous data *)
       )
  | Get_var name ->
       (match Hashtbl.find_opt env name with
       | Some v -> Lwt.return (Ok v)
       | None ->
           (* This should ideally be caught by type checker, but defensive *)
           error (fun m -> m "Execution error: Accessing undeclared variable '%s'" name);
           Lwt.return (Error (`ExecutionError (Printf.sprintf "Accessing undeclared variable '%s'" name))))

  | Bin_op (op, e1, e2) ->
      let* v1_res = eval_expr env data_mgr symbol e1 in
      let* v2_res = eval_expr env data_mgr symbol e2 in
      (match v1_res, v2_res with
       | Ok v1, Ok v2 ->
           Lwt.return (Ok (
               match op with
               | Add | Sub | Mul | Div ->
                   (match v1, v2 with
                    | Numeric f1, Numeric f2 ->
                        let result_f =
                            match op with
                            | Add -> f1 +. f2
                            | Sub -> f1 -. f2
                            | Mul -> f1 *. f2
                            | Div -> if f2 <> 0.0 then f1 /. f2 else nan (* Return NaN on division by zero *)
                            | _ -> assert false (* Should not happen *)
                        in
                        if isnan result_f || isinf result_f then (
                            error (fun m -> m "Execution error: Arithmetic operation resulted in NaN or Inf");
                            (* Indicate error within the result, but return a value *)
                            Numeric result_f (* Return NaN/Inf value *)
                        ) else Numeric result_f
                    | _ ->
                        error (fun m -> m "Execution error: Arithmetic operation on non-numeric types");
                        raise (ExecutionError "Arithmetic operation on non-numeric types")) (* Type checker should prevent *)
               | Eq | Neq -> Boolean (v1 = v2) (* OCaml's structural equality *)
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
                        raise (ExecutionError "Comparison operation on non-numeric types for numeric comparison")) (* Type checker should prevent *)
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
                        raise (ExecutionError "Logical operation on non-boolean types")) (* Type checker should prevent *)
           ))
       | Error msg, _ | _, Error msg -> Lwt.return (Error msg) (* Propagate the first error found *)
      )

  | Builtin_func (func, args) ->
      let* arg_values_res = Lwt_list.map_p (eval_expr env data_mgr symbol) args |> Result.all in (* Evaluate all arguments, collect errors *)
      match arg_values_res with
      | Error msg -> Lwt.return (Error msg) (* Propagate error from argument evaluation *)
      | Ok arg_values ->
          Lwt.return (Ok (
              match func, arg_values with
              | Abs, [Numeric f] -> Numeric (abs_float f)
              | Min, [Numeric f1; Numeric f2] -> Numeric (min f1 f2)
              | Max, [Numeric f1; Numeric f2] -> Numeric (max f1 f2)
              (* Add evaluation for other built-in functions *)
              | _ -> (* This case should be caught by type checker *)
                  error (fun m -> m "Execution error: Invalid arguments for built-in function");
                  raise (ExecutionError "Invalid arguments for built-in function")
          ))

(* Helper to execute an action, returning Result Lwt.t *)
let execute_action action_env data_mgr grpc_client_env symbol = function
  | Buy qty_expr ->
      let* qty_value_res = eval_expr action_env data_mgr symbol qty_expr in
      (match qty_value_res with
       | Ok (Numeric qty) when qty > 0.0 && not (isnan qty) && not (isinf qty) ->
           info (fun m -> m "Executing BUY %.8f on %s" qty symbol); (* Log with more precision *)
           let exchange = "binance" (* Placeholder exchange *) in
           Strategy_grpc.send_order grpc_client_env exchange symbol Market_data_pb.ORDER_SIDE_BUY Market_data_pb.ORDER_TYPE_MARKET (string_of_float qty) "0.0" (* Price 0.0 for Market order *)
       | Ok (Numeric qty) ->
           error (fun m -> m "BUY quantity expression evaluated to non-positive or invalid numeric value (%.8f)" qty);
           Lwt.return (Error (`ExecutionError "BUY quantity must be a positive finite numeric value")))
       | Ok _ ->
           error (fun m -> m "BUY quantity expression evaluated to non-numeric type");
           Lwt.return (Error (`ExecutionError "BUY quantity expression must be numeric")))
       | Error msg -> Lwt.return (Error msg) (* Propagate error from evaluation *)
      )
  | Sell qty_expr ->
      let* qty_value_res = eval_expr action_env data_mgr symbol qty_expr in
      (match qty_value_res with
       | Ok (Numeric qty) when qty > 0.0 && not (isnan qty) && not (isinf qty) ->
           info (fun m -> m "Executing SELL %.8f on %s" qty symbol); (* Log with more precision *)
           let exchange = "binance" (* Placeholder exchange *) in
           Strategy_grpc.send_order grpc_client_env exchange symbol Market_data_pb.ORDER_SIDE_SELL Market_data_pb.ORDER_TYPE_MARKET (string_of_float qty) "0.0"
       | Ok (Numeric qty) ->
           error (fun m -> m "SELL quantity expression evaluated to non-positive or invalid numeric value (%.8f)" qty);
           Lwt.return (Error (`ExecutionError "SELL quantity must be a positive finite numeric value")))
       | Ok _ ->
           error (fun m -> m "SELL quantity expression evaluated to non-numeric type");
           Lwt.return (Error (`ExecutionError "SELL quantity expression must be numeric")))
       | Error msg -> Lwt.return (Error msg) (* Propagate error from evaluation *)
      )
  | Hold ->
      debug (fun m -> m "Executing HOLD");
      Lwt.return (Ok ()) (* Hold is a no-op, but needs to be in Lwt *)
  | Log expr ->
      let* value_res = eval_expr action_env data_mgr symbol expr in
      (match value_res with
       | Ok value ->
           info (fun m -> m "Strategy Log: %s" (Sexplib.Sexp.to_string (sexp_of_value value))); (* Log value as sexp *)
           Lwt.return (Ok ()) (* Log is async in terms of execution flow *)
       | Error msg -> Lwt.return (Error msg) (* Propagate error from evaluation *)
      )


(* Helper to execute a single statement, returning Result Lwt.t *)
(* Now takes and returns the execution environment (state) *)
let rec execute_statement statement_env data_mgr grpc_client_env symbol = function
  | If (cond_expr, then_stmts, else_stmts) ->
      let* cond_value_res = eval_expr statement_env data_mgr symbol cond_expr in
      (match cond_value_res with
       | Ok (Boolean true) ->
           debug (fun m -> m "IF condition true. Executing THEN block.");
           execute_statements statement_env data_mgr grpc_client_env symbol then_stmts (* Execute THEN block *)
       | Ok (Boolean false) ->
           debug (fun m -> m "IF condition false. Executing ELSE block.");
           execute_statements statement_env data_mgr grpc_client_env symbol else_stmts (* Execute ELSE block *)
       | Ok _ ->
           error (fun m -> m "Execution error: IF condition evaluated to non-boolean type");
           Lwt.return (Error (`ExecutionError "IF condition must be boolean")))
       | Error msg -> Lwt.return (Error msg) (* Propagate error from condition evaluation *)
      )
  | Action action ->
      let* action_result = execute_action statement_env data_mgr grpc_client_env symbol action in
      (* We just log action execution errors, they don't stop the strategy *)
      Lwt.return (Ok (statement_env, [])) (* Return current env and no action list from here *)
  | Var_decl (name, _) ->
      (* Var_decl statements are handled during strategy loading to set up initial state.
         This branch should not be hit during main statement execution. *)
       debug (fun m -> m "Encountered Var_decl '%s' during execution. Skipping." name);
       Lwt.return (Ok (statement_env, []))
  | Set_var (name, expr) ->
      let* value_to_set_res = eval_expr statement_env data_mgr symbol expr in
      (match value_to_set_res with
       | Ok value_to_set ->
           (* Check if variable exists in env (type checker should handle this) *)
           if Hashtbl.mem statement_env name then (
               debug (fun m -> m "Setting variable '%s' to %s" name (Sexplib.Sexp.to_string (sexp_of_value value_to_set)));
               Hashtbl.replace statement_env name value_to_set; (* Update the value in the environment *)
               Lwt.return (Ok (statement_env, [])) (* Return updated env and no action *)
           ) else (
               error (fun m -> m "Execution error: Attempted to set undeclared variable '%s'" name);
               Lwt.return (Error (`ExecutionError (Printf.sprintf "Attempted to set undeclared variable '%s'" name)))
           )
       | Error msg -> Lwt.return (Error msg) (* Propagate error from evaluation *)
      )


(* Helper to execute a list of statements, returning Result Lwt.t *)
(* Now takes and returns the execution environment (state) *)
and execute_statements statement_env data_mgr grpc_client_env symbol = function
  | [] -> Lwt.return (Ok (statement_env, [])) (* Return updated env and empty list of executed actions *)
  | stmt :: rest ->
      let* stmt_result = execute_statement statement_env data_mgr grpc_client_env symbol stmt in
      (match stmt_result with
       | Ok (updated_env, actions1) -> (* Successfully executed statement *)
           let* rest_result = execute_statements updated_env data_mgr grpc_client_env symbol rest in
           (match rest_result with
            | Ok (final_env, actions2) -> Lwt.return (Ok (final_env, actions1 @ actions2)) (* Propagate success and combine actions *)
            | Error msg -> Lwt.return (Error msg) (* Propagate error from rest *)
           )
       | Error msg -> Lwt.return (Error msg) (* Propagate error from current statement *)
      )


(* Compile (Interpret) a strategy AST into an executable function. *)
(* The compiled strategy now takes/returns the exec_env and returns Result Lwt.t *)
let compile_strategy strategy_ast : compiled_strategy =
  info (fun m -> m "Compiling (Interpreting) strategy '%s'" strategy_ast.name);
  (* The "compiled" strategy is simply an OCaml function that closes over the AST *)
  let execute_func exec_env data_mgr grpc_client_env symbol : (exec_env * action list, [> `ExecutionError of string]) result Lwt.t =
      (* execute_statements now manages state updates and returns Result *)
      execute_statements exec_env data_mgr grpc_client_env symbol strategy_ast.logic
  in
  info (fun m -> m "Strategy '%s' compiled successfully" strategy_ast.name);
  execute_func


(* Execute a compiled strategy using the latest data from the data manager. *)
(* Now takes the strategy's current execution environment (state) as input and returns the updated state, returning Result Lwt.t *)
let execute_compiled_strategy (compiled_strat : compiled_strategy) (data_mgr : Strategy_data.t) (grpc_client_env : Strategy_grpc.t) (symbol : string) (current_exec_env : exec_env) : (exec_env * action list, [> `ExecutionError of string]) result Lwt.t =
    debug (fun m -> m "Attempting execution of strategy for symbol '%s'" symbol);

    (* The compiled_strat function already has the correct signature.
       It takes the current_exec_env and returns the updated env and actions, wrapped in Result Lwt.t.
       We just call it with the current state and resources. *)
    Lwt.catch (fun () ->
       compiled_strat current_exec_env data_mgr grpc_client_env symbol
    ) (fun e ->
       error (fun m -> m "Unhandled exception during strategy execution for symbol '%s': %s" symbol (Printexc.to_string e));
       (* Return current state and no actions, indicate execution error *)
       Lwt.return (Error (`ExecutionError (Printf.sprintf "Unhandled exception: %s" (Printexc.to_string e))))
    )