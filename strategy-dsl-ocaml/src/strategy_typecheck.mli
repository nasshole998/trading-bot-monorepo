(** strategy_typecheck.mli - Type Checker for the Strategy DSL AST *)

open Strategy_ast (* Need AST types *)
open Result (* Need Result type *)

(** The type-checking environment. Maps names (for indicators, predictions, variables) to their expected types. *)
type type_env = (string, [`Numeric | `Boolean]) Hashtbl.t

(** Type check an expression. Returns the inferred type or raises an error. *)
val typecheck_expr : type_env -> expr -> [`Numeric | `Boolean]

(** Type check a list of statements. Raises an error if any statement is ill-typed. *)
val typecheck_statements : type_env -> statement list -> unit

(** Type check a complete strategy AST. Initializes the type environment and checks logic. *)
val typecheck_strategy : Strategy_data.t -> strategy -> type_env Lwt.t

(** Helper to evaluate an expression that only contains constants (used for initial VAR values). Returns Result.t. *)
val eval_constant_expr : expr -> (Strategy_ast.value, string) Result.t

(** Exception raised on type errors. *)
exception TypeError of string