// src/Dsl/Ast.fs
module BacktesterFsharp.Dsl.Ast

open System.Collections.Generic

// Represents a DSL value (Numeric or Boolean)
type Value =
    | Numeric of decimal
    | Boolean of bool

// Represents a binary operator
type BinOp =
    | Add | Sub | Mul | Div
    | Eq | Neq | Lt | Gt | Leq | Geq
    | And | Or

// Represents a built-in function
type BuiltinFunc =
    | Abs | Min | Max
    // Add other built-in functions here

// Represents a DSL expression
type Expr =
    | Const of Value
    | GetIndicator of string // Get latest indicator value by name
    | GetPrediction of string // Get latest prediction value by name
    | GetPrevIndicator of string // Get previous indicator value by name
    | GetPrevPrediction of string // Get previous prediction value by name
    | GetVar of string // Get strategy state variable value
    | BinOp of BinOp * Expr * Expr // Binary operation
    | BuiltinFunc of BuiltinFunc * Expr list // Built-in function call

// Represents a DSL action
type Action =
    | Buy of Expr // Quantity expression
    | Sell of Expr // Quantity expression
    | Hold
    | Log of Expr // Expression to log

// Represents a DSL statement
type Statement =
    | If of Expr * Statement list * Statement list // IF condition THEN statements ELSE statements
    | Action of Action
    | VarDecl of string * Expr // VAR name = initial_value
    | SetVar of string * Expr // SET name = expression

// Represents a complete parsed strategy
type Strategy = {
    Name: string
    InitialState: Dictionary<string, Value> // Variable name -> initial value
    Logic: Statement list // The main sequence of statements to execute
    // Add other metadata if needed
}