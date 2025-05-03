(** strategy_lexer.mll - Lexer for the Strategy DSL *)

{
open Strategy_parser (* The parser defines the token types *)
open Lexing (* For position information *)

(* Helper to get position information *)
let pos lexbuf =
  let p = Lexing.lexeme_start_p lexbuf in
  let file = p.pos_fname in
  let line = p.pos_lnum in
  let col = p.pos_cnum - p.pos_bol in
  (file, line, col)

(* Helper for error reporting *)
let error lexbuf msg =
  let (file, line, col) = pos lexbuf in
  failwith (Printf.sprintf "Lexing error in %s at line %d, column %d: %s" file line col msg)
}

rule token = parse
  [' ' '\t' '\n'] { token lexbuf } (* Skip whitespace *)
| ['0'-'9']+ '.' ['0'-'9']*
| '.' ['0'-'9']+ { FLOAT (float_of_string (Lexing.lexeme lexbuf)) } (* Floating point numbers *)
| ['0'-'9']+     { INT (int_of_string (Lexing.lexeme lexbuf)) }    (* Integers (will be treated as floats in evaluation) *)

| '+' { PLUS }
| '-' { MINUS }
| '*' { TIMES }
| '/' { DIV }
| '=' { ASSIGN } (* Use ASSIGN for variable assignment *)
| "==" { EQ } (* Allow == for equality comparison *)
| "!=" { NEQ }
| '<' { LT }
| '>' { GT }
| "<=" { LEQ }
| ">=" { GEQ }
| "&&" { AND }
| "||" { OR }

| '(' { LPAREN }
| ')' { RPAREN }
| '{' { LBRACE }
| '}' { RBRACE }
| '; ' { SEMICOLON } (* Add space after semicolon for cleaner lexing? *)
| ';' { SEMICOLON }
| ',' { COMMA }
| '.' { DOT } (* For accessing .prev *)


| "STRATEGY" { K_STRATEGY }
| "PARAMS"   { K_PARAMS }   (* Not used in simplified AST, but keep for syntax *)
| "INDICATORS" { K_INDICATORS } (* Not used in simplified AST, but keep for syntax *)
| "PREDICTIONS" { K_PREDICTIONS } (* Not used in simplified AST, but keep for syntax *)
| "ON"       { K_ON }       (* Not used in simplified AST, assume execute on data *)
| "DataUpdate" { K_DataUpdate } (* Not used in simplified AST *)
| "IF"       { K_IF }
| "THEN"     { K_THEN }
| "ELSE"     { K_ELSE }
| "BUY"      { K_BUY }
| "SELL"     { K_SELL }
| "HOLD"     { K_HOLD }
| "Log"      { K_Log }      (* Log action *)
| "VAR"      { K_VAR }      (* Variable declaration *)
| "SET"      { K_SET }      (* Variable assignment *)
| "prev"     { K_prev }     (* Keyword for accessing previous value *)

| "ABS"      { K_ABS }      (* Built-in function *)
| "MIN"      { K_MIN }      (* Built-in function *)
| "MAX"      { K_MAX }      (* Built-in function *)

| "Indicator" { K_Indicator } (* Keyword to access current indicator values *)
| "Prediction" { K_Prediction } (* Keyword to access current prediction values *)


| ['a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z' '0'-'9' '_']* { ID (Lexing.lexeme lexbuf) } (* Identifiers *)

| '"' ([^'"'] as c)* '"' { STRING (Lexing.lexeme lexbuf |> String.sub 1 (String.length (Lexing.lexeme lexbuf) - 2)) } (* Basic strings *)

| eof { EOF } (* End of file *)

| _ { error lexbuf (Printf.sprintf "Unexpected character: '%s'" (Lexing.lexeme lexbuf)) } (* Catch anything else *)