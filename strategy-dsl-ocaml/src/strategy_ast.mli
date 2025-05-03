(** strategy_ast.mli - Abstract Syntax Tree for the Strategy DSL *)

(** Types for values within the DSL. Simple: Numeric and Boolean. *)
type value =
  | Numeric of float
  | Boolean of bool
[@@deriving sexp] (* Use ppx_sexp_conv for easy S-expression conversion *)

(** Types for expressions (things that evaluate to values). *)
type expr =
  | Const of value                                     (* Constant value *)
  | Get_indicator of string                            (* Get latest indicator value by name *)
  | Get_prediction of string                           (* Get latest latest prediction value by name *)
  | Get_prev_indicator of string                       (* Get previous indicator value by name *)
  | Get_prev_prediction of string                      (* Get previous prediction value by name *)
  | Get_var of string                                  (* Get strategy state variable value *)
  | Bin_op of bin_op * expr * expr                     (* Binary operation *)
  | Builtin_func of builtin_func * expr list           (* Built-in function call *)

(** Types for binary operators. *)
and bin_op =
  (* Arithmetic *)
  | Add | Sub | Mul | Div
  (* Comparison *)
  | Eq | Neq | Lt | Gt | Leq | Geq
  (* Logical *)
  | And | Or
[@@deriving sexp]

(** Types for built-in functions. *)
and builtin_func =
  | Abs | Min | Max (* Basic math functions *)
  (* Add more built-in functions here *)
[@@deriving sexp]


(** Types for actions (things the strategy can do). *)
type action =
  | Buy of expr (* Quantity expression to buy *)
  | Sell of expr (* Quantity expression to sell *)
  | Hold           (* Explicitly do nothing (optional) *)
  | Log of expr    (* Log the value of an expression (for debugging) *)
  (* Add other actions: Set_parameter, Cancel_order, etc. *)
[@@deriving sexp]

(** Types for statements (executable units). *)
type statement =
  | If of expr * statement list * statement list (* IF condition THEN statements ELSE statements *)
  | Action of action                             (* Execute a single action *)
  | Var_decl of string * expr                    (* VAR name = initial_value; *)
  | Set_var of string * expr                     (* SET name = expression; *)
  (* Add other statement types: Sequential_block, Loop, etc. *)
[@@deriving sexp]

(** Top-level structure for a parsed strategy. *)
type strategy = {
  name: string;
  logic: statement list; (* The main sequence of statements to execute *)
  (* Define initial state variables here, collected from Var_decl statements *)
  initial_state: (string * value) list;
}
[@@deriving sexp]

(* Helper function to extract Var_decl statements and other statements *)
val extract_var_decls : statement list -> (string * expr) list * statement list