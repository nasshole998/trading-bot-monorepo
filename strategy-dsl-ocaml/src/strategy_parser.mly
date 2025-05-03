(** strategy_parser.mly - Parser for the Strategy DSL *)

%{
open Strategy_ast (* The AST definition *)
open Strategy_typecheck (* Need eval_constant_expr *)

(* Helper for error reporting *)
let error lexbuf msg =
  let pos = Lexing.lexeme_start_p lexbuf in
  let file = pos.Lexing.pos_fname in
  let line = pos.Lexing.pos_lnum in
  let col = pos.Lexing.pos_cnum - pos.Lexing.pos_bol in
  failwith (Printf.sprintf "Parsing error in %s at line %d, column %d: %s" file line col msg)
%}

(* Declare tokens from the lexer *)
%token <int> INT
%token <float> FLOAT
%token <string> ID
%token <string> STRING
%token PLUS MINUS TIMES DIV
%token EQ NEQ LT GT LEQ GEQ
%token AND OR
%token LPAREN RPAREN LBRACE RBRACE SEMICOLON COMMA DOT
%token K_STRATEGY K_PARAMS K_INDICATORS K_PREDICTIONS K_ON K_DataUpdate K_IF K_THEN K_ELSE K_BUY K_SELL K_HOLD K_Log K_Indicator K_Prediction K_VAR K_SET K_prev
%token ASSIGN (* New token for assignment (=) *)
%token K_ABS K_MIN K_MAX (* Built-in function tokens *)
%token EOF

(* Define precedence and associativity for binary operators *)
%left OR
%left AND
%left EQ NEQ LT GT LEQ GEQ
%left PLUS MINUS
%left TIMES DIV

(* Define the start symbol and its type *)
%start <Strategy_ast.strategy> strategy

%%

(* Grammar rules *)

(* Top-level strategy structure *)
strategy:
  K_STRATEGY ID SEMICOLON LBRACE statements RBRACE EOF
    {
      let var_decls, other_stmts = extract_var_decls $5 in
      let initial_state_evaluated = List.map (fun (name, expr) ->
          match eval_constant_expr expr with (* Evaluate initial VAR expressions *)
          | Result.Ok value -> (name, value)
          | Result.Error msg -> error lexbuf (Printf.sprintf "Error evaluating initial value for variable '%s': %s" name msg)
      ) var_decls
      in
      { name = $2; logic = other_stmts; initial_state = initial_state_evaluated }
    }

(* List of statements *)
statements:
  statement SEMICOLON statements
    { $1 :: $3 }
| (* empty *)
    { [] }
| statement (* Allow last statement without semicolon *)
    { [$1] }

(* Single statement *)
statement:
  K_IF expr K_THEN LBRACE statements RBRACE K_ELSE LBRACE statements RBRACE
    { If ($2, $5, $9) } (* IF condition THEN statements ELSE statements *)
| K_IF expr K_THEN LBRACE statements RBRACE (* IF condition THEN statements (no ELSE) *)
    { If ($2, $5, []) } (* Treat no ELSE as empty ELSE block *)
| action
    { Action $1 } (* An action as a statement *)
| K_VAR ID ASSIGN expr (* VAR name = initial_value; *)
    { Var_decl ($2, $4) }
| K_SET ID ASSIGN expr (* SET name = expression; *)
    { Set_var ($2, $4) }


(* Action *)
action:
  K_BUY expr
    { Buy $2 } (* BUY quantity is now an expression *)
| K_SELL expr
    { Sell $2 } (* SELL quantity is now an expression *)
| K_HOLD
    { Hold } (* HOLD action *)
| K_Log LPAREN expr RPAREN
    { Log $3 } (* Log(expression) action *)


(* Expression - defined with precedence and associativity *)
expr:
  bool_expr { $1 }

bool_expr:
  bool_expr OR and_expr { Bin_op (OR, $1, $3) }
| and_expr { $1 }

and_expr:
  and_expr AND comp_expr { Bin_op (AND, $1, $3) }
| comp_expr { $1 }

comp_expr:
  arith_expr EQ arith_expr { Bin_op (Eq, $1, $3) }
| arith_expr NEQ arith_expr { Bin_op (Neq, $1, $3) }
| arith_expr LT arith_expr { Bin_op (Lt, $1, $3) }
| arith_expr GT arith_expr { Bin_op (Gt, $1, $3) }
| arith_expr LEQ arith_expr { Bin_op (Leq, $1, $3) }
| arith_expr GEQ arith_expr { Bin_op (Geq, $1, $3) }
| arith_expr { $1 }

arith_expr:
  arith_expr PLUS term { Bin_op (Add, $1, $3) }
| arith_expr MINUS term { Bin_op (Sub, $1, $3) }
| term { $1 }

term:
  term TIMES factor { Bin_op (Mul, $1, $3) }
| term DIV factor { Bin_op (Div, $1, $3) }
| factor { $1 }

factor:
  FLOAT { Const (Numeric $1) } (* Floating point constant *)
| INT   { Const (Numeric (float_of_int $1)) } (* Integer constant, convert to float *)
| STRING { Const (Boolean (bool_of_string $1)) } (* Allow "true", "false" as boolean constants - simplify *)
| K_Indicator LPAREN STRING RPAREN { Get_indicator $3 } (* Access current indicator value by string name *)
| K_Prediction LPAREN STRING RPAREN { Get_prediction $3 } (* Access current prediction value by string name *)
| K_Indicator LPAREN STRING RPAREN DOT K_prev { Get_prev_indicator $3 } (* Access previous indicator value *)
| K_Prediction LPAREN STRING RPAREN DOT K_prev { Get_prev_prediction $3 } (* Access previous prediction value *)
| ID { Get_var $1 } (* Access state variable by ID *)
| builtin_func_call { $1 } (* Built-in function calls *)
| LPAREN expr RPAREN { $2 } (* Parenthesized expression *)
(* Add other factors: unary minus, etc. *)

(* Built-in function calls *)
builtin_func_call:
  K_ABS LPAREN expr RPAREN { Builtin_func (Abs, [$3]) } (* ABS(expr) *)
| K_MIN LPAREN expr COMMA expr RPAREN { Builtin_func (Min, [$3; $5]) } (* MIN(expr, expr) *)
| K_MAX LPAREN expr COMMA expr RPAREN { Builtin_func (Max, [$3; $5]) } (* MAX(expr, expr) *)
(* Add more built-in functions here following the pattern *)


(* Error handling - Catch unexpected tokens *)
%%