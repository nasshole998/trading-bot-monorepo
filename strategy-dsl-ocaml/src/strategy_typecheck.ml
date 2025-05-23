(** strategy_typecheck.ml - Type Checker for the Strategy DSL AST *)

open Strategy_ast
open Strategy_typecheck_intf (* Include interface file *)
open Strategy_data (* Need data types for env population *)
open Lwt.Infix (* For Lwt binds *)
open Logs (* For logging *)
open Result (* For Result type *)
open Batteries.Float (* For nan, isnan *)

exception TypeError of string

(* Helper to get the type of a value constant *)
let type_of_value = function
  | Numeric _ -> `Numeric
  | Boolean _ -> `Boolean

(* Helper to check if a type matches expected *)
let check_type env expr expected_type =
  let inferred_type = typecheck_expr env expr in
  if inferred_type <> expected_type then
    raise (TypeError (Printf.sprintf "Expected type %s, but found %s"
                        (match expected_type with `Numeric -> "Numeric" | `Boolean -> "Boolean")
                        (match inferred_type with `Numeric -> "Numeric" | `Boolean -> "Boolean")))

(* Helper to evaluate an expression that only contains constants *)
let rec eval_constant_expr = function
  | Const v -> Result.Ok v
  | Bin_op (op, e1, e2) ->
      let open Result.Infix in
      eval_constant_expr e1 >>= fun v1 ->
      eval_constant_expr e2 >>= fun v2 ->
      Result.Ok (
          match op with
          | Add | Sub | Mul | Div ->
              (match v1, v2 with
               | Numeric f1, Numeric f2 -> Numeric (
                   match op with
                   | Add -> f1 +. f2
                   | Sub -> f1 -. f2
                   | Mul -> f1 *. f2
                   | Div -> if f2 <> 0.0 then f1 /. f2 else nan)
               | _ -> Result.Error "Arithmetic operation on non-numeric constants")
          | Eq | Neq -> Boolean (v1 = v2)
          | Lt | Gt | Leq | Geq ->
              (match v1, v2 with
               | Numeric f1, Numeric f2 -> Boolean (
                   match op with
                   | Lt -> f1 < f2
                   | Gt -> f1 > f2
                   | Leq -> f1 <= f2
                   | Geq -> f1 >= f2)
               | _ -> Result.Error "Comparison operation on non-numeric constants")
          | And | Or ->
              (match v1, v2 with
               | Boolean b1, Boolean b2 -> Boolean (
                   match op with
                   | And -> b1 && b2
                   | Or -> b1 || b2)
               | _ -> Result.Error "Logical operation on non-boolean constants")
      )
  | Builtin_func (_, _) -> Result.Error "Cannot evaluate built-in function call at compile time"
  | Get_indicator name | Get_prediction name | Get_prev_indicator name | Get_prev_prediction name | Get_var name ->
      (* Cannot evaluate non-constant expressions at compile time *)
      Result.Error (Printf.sprintf "Cannot evaluate non-constant expression '%s' at compile time" name)


(* Type check an expression *)
let rec typecheck_expr env = function
  | Const v -> type_of_value v
  | Get_indicator name | Get_prev_indicator name ->
      (match Hashtbl.find_opt env name with
       | Some `Numeric -> `Numeric (* Indicators are expected to be numeric *)
       | Some _ -> raise (TypeError (Printf.sprintf "Indicator '%s' found in environment but is not Numeric" name))
       | None -> raise (TypeError (Printf.sprintf "Undefined indicator '%s'" name)))
  | Get_prediction name | Get_prev_prediction name ->
      (match Hashtbl.find_opt env name with
       | Some (`Numeric | `Boolean as t) -> t (* Predictions can be numeric or boolean *)
       | None -> raise (TypeError (Printf.sprintf "Undefined prediction '%s'" name)))
  | Get_var name ->
       (match Hashtbl.find_opt env name with
       | Some t -> t (* Variable type is known from declaration *)
       | None -> raise (TypeError (Printf.sprintf "Undeclared variable '%s'" name)))
  | Bin_op (op, e1, e2) ->
      let t1 = typecheck_expr env e1 in
      let t2 = typecheck_expr env e2 in
      match op with
      | Add | Sub | Mul | Div ->
          if t1 = `Numeric && t2 = `Numeric then `Numeric
          else raise (TypeError "Arithmetic operations require Numeric operands")
      | Eq | Neq | Lt | Gt | Leq | Geq ->
          if t1 = t2 then `Boolean (* Comparison ops require operands of the same type *)
          else raise (TypeError "Comparison operations require operands of the same type")
      | And | Or ->
          if t1 = `Boolean && t2 = `Boolean then `Boolean
          else raise (TypeError "Logical operations require Boolean operands")
  | Builtin_func (func, args) ->
      match func, args with
      | Abs, [e] ->
          check_type env e `Numeric; `Numeric (* ABS requires Numeric, returns Numeric *)
      | Min, [e1; e2] | Max, [e1; e2] ->
          check_type env e1 `Numeric;
          check_type env e2 `Numeric;
          `Numeric (* MIN/MAX require two Numeric, return Numeric *)
      | Abs, _ -> raise (TypeError "ABS function requires exactly 1 argument")
      | (Min | Max), _ -> raise (TypeError "MIN/MAX functions require exactly 2 arguments")
      (* Add type checks for other built-in functions *)


(* Type check a single action *)
let typecheck_action env = function
  | Buy expr | Sell expr ->
      check_type env expr `Numeric (* Buy/Sell quantity expression must be numeric *)
  | Hold -> () (* Hold requires no type check *)
  | Log expr ->
      let _ = typecheck_expr env expr in () (* Log can take any type *)

(* Type check a list of statements *)
let rec typecheck_statements env = function
  | [] -> ()
  | stmt :: rest ->
      typecheck_statement env stmt;
      typecheck_statements env rest

(* Type check a single statement *)
and typecheck_statement env = function
  | If (cond_expr, then_stmts, else_stmts) ->
      check_type env cond_expr `Boolean; (* IF condition must be boolean *)
      typecheck_statements env then_stmts; (* Type check THEN block *)
      typecheck_statements env else_stmts (* Type check ELSE block *)
  | Action action ->
      typecheck_action env action (* Type check the action *)
  | Var_decl (name, initial_expr) ->
      (* Var_decl statements are processed *before* typechecking statements. *)
      () (* Do nothing here, handled during AST processing *)
  | Set_var (name, expr) ->
      (* Check if variable is declared and check type consistency *)
       (match Hashtbl.find_opt env name with
       | Some expected_type -> check_type env expr expected_type
       | None -> raise (TypeError (Printf.sprintf "Assignment to undeclared variable '%s'" name)))


(* Type check a complete strategy AST. *)
let typecheck_strategy data_mgr strategy_ast =
  let open Lwt.Infix in
  info (fun m -> m "Starting type checking for strategy '%s'" strategy_ast.name);

  (* Create the initial type environment *)
  let type_env = Hashtbl.create 50 in (* Generous initial size *)

  (* Add known indicators and predictions from DataManager *)
  let* known_indicators = Strategy_data.get_known_indicator_names data_mgr in
  known_indicators |> List.iter (fun name -> Hashtbl.add type_env name `Numeric);
  info (fun m -> m "Added %d known indicators to type environment." (List.length known_indicators));

  let* known_predictions = Strategy_data.get_known_prediction_names data_mgr in
  known_predictions |> List.iter (fun name ->
      (* Infer type based on name pattern (simplified) *)
      let ptype = if String.contains name '_' then
                    match String.split_on_char '_' name |> List.hd with
                    | "buy" | "sell" | "signal" | "bool" | "flag" -> `Boolean (* Add more boolean patterns *)
                    | _ -> `Numeric
                  else `Numeric (* Default to numeric if no clear pattern *)
      in
      Hashtbl.add type_env name ptype
  );
  info (fun m -> m "Added %d known predictions to type environment." (List.length known_predictions));


  (* Add declared state variables to the environment with their inferred initial types *)
  strategy_ast.initial_state |> List.iter (fun (name, initial_value) ->
      (* Check if name conflicts with indicator/prediction *)
      if Hashtbl.mem type_env name then
         raise (TypeError (Printf.sprintf "Variable '%s' conflicts with existing indicator/prediction name" name));
      let initial_type = type_of_value initial_value in
      Hashtbl.add type_env name initial_type;
      info (fun m -> m "Added variable '%s' (%s) to type environment." name (match initial_type with `Numeric -> "Numeric" | `Boolean -> "Boolean"));
  );

  (* Add "current_price" if needed by the DSL and not already added *)
  if not (Hashtbl.mem type_env "current_price") then (
    Hashtbl.add type_env "current_price" `Numeric;
    debug (fun m -> m "Added default 'current_price' (Numeric) to type environment.");
  );


  try
    typecheck_statements type_env strategy_ast.logic; (* Type check the main logic statements *)
    info (fun m -> m "Strategy '%s' type checked successfully" strategy_ast.name);
    Lwt.return type_env (* Return the final type environment (includes variables) *)
  with
  | TypeError msg ->
      error (fun m -> m "Type error in strategy '%s': %s" strategy_ast.name msg);
      raise (TypeError msg)
  | e ->
      error (fun m -> m "Unknown error during type checking for strategy '%s': %s" strategy_ast.name (Printexc.to_string e));
      raise e