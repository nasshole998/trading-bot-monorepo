(** strategy_ast.ml - Abstract Syntax Tree for the Strategy DSL *)

(* Just includes the mli file for type definitions and generated code.
   The [@@deriving sexp] annotations generate the conversion functions. *)
include Strategy_ast_intf

(* Helper function to extract Var_decl statements and other statements *)
let rec extract_var_decls stmts =
  match stmts with
  | [] -> [], []
  | (Var_decl (name, initial_value)) :: rest ->
      let decls, other = extract_var_decls rest in
      (name, initial_value) :: decls, other
  | other_stmt :: rest ->
      let decls, other = extract_var_decls rest in
      decls, other_stmt :: other