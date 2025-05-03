// src/Dsl/Interpreter.fs
module BacktesterFsharp.Dsl.Interpreter

open System
open System.Collections.Generic
open BacktesterFsharp.Dsl.Ast
open BacktesterFsharp.Simulation.SimulationTypes // Needs simulation types
open System.Globalization // For InvariantCulture
open MlPredictionPb // For PredictionValue
open System.Linq // For LINQ extensions

// Define a Result type for interpreter operations that can fail
type InterpretationResult<'T> = Result<'T, string>

// Represents the runtime execution environment (variable state and data context)
type ExecutionEnvironment = {
    Vars: Dictionary<string, Value> // Strategy state variables (mutable)
    IndicatorsHistory: Dictionary<string, DataPoint list> // Indicator history (name -> list of points)
    PredictionsHistory: Dictionary<string, DataPoint list> // Prediction history (name -> list of points)
    MlPredictions: Dictionary<string, Dictionary<DateTimeOffset, PredictionValue>> // ML predictions (type -> (Timestamp -> PredictionValue))
    // Add Order Update history here if needed by DSL - not integrated into interpreter env yet
    Symbol: string // Symbol the strategy is running on
    CurrentTime: DateTimeOffset // Current simulation time
    CurrentPrice: decimal // Current market price
}

// Helper to find the latest data point in a history list whose timestamp is <= the given timestamp
let findLatestDataPointBeforeOrAt (history: DataPoint list) (timestamp: DateTimeOffset) : DataPoint option =
    history
    |> List.filter (fun dp -> dp.Timestamp <= timestamp)
    |> List.sortByDescending (fun dp -> dp.Timestamp)
    |> List.tryHead

// Helper to find the latest prediction value in a prediction history dictionary
// whose timestamp is <= the given timestamp. Prioritizes ML predictions.
let findLatestPredictionValueBeforeOrAt (env: ExecutionEnvironment) (name: string) (timestamp: DateTimeOffset) : InterpretationResult<Value> =
    // 1. Check ML Predictions first
    match env.MlPredictions.TryGetValue(name) with
    | true, timestampDict ->
        // Find the latest ML prediction in the timestamp dictionary <= the requested timestamp
        let latestMlPredictionOpt =
            timestampDict
            |> Seq.filter (fun kvp -> kvp.Key <= timestamp)
            |> Seq.sortByDescending (fun kvp -> kvp.Key)
            |> Seq.tryHead
            |> Option.map (fun kvp -> kvp.Value)

        match latestMlPredictionOpt with
        | Some predictionValue ->
            // Found a relevant ML prediction
            let valueStr = predictionValue.Value
            match valueStr |> decimal.TryParse(CultureInfo.InvariantCulture) with
            | true, d -> Ok (Numeric d)
            | false, _ ->
                match valueStr |> bool.TryParse with
                | true, b -> Ok (Boolean b)
                | false, _ -> Error (sprintf "Interpreter Error: Could not parse ML prediction '%s' value '%s' as decimal or boolean <= %A" name valueStr timestamp)
        | None ->
            // No relevant ML prediction found <= timestamp, fallback to historical data
            printfn "[Interpreter] Info: No ML prediction '%s' found <= %A. Falling back to historical data." name timestamp
            // Fallback to historical prediction data if available
            match env.PredictionsHistory.TryGetValue(name) with
            | true, dataPoints ->
                match findLatestDataPointBeforeOrAt dataPoints timestamp with
                | Some dp ->
                     match dp.Value |> decimal.TryParse(CultureInfo.InvariantCulture) with
                     | true, d -> Ok (Numeric d)
                     | false, _ ->
                         match dp.Value |> bool.TryParse with
                         | true, b -> Ok (Boolean b)
                         | false, _ -> Error (sprintf "Interpreter Error: Could not parse historical prediction '%s' value '%s' as decimal or boolean <= %A" name dp.Value timestamp)
                | None ->
                    printfn "[Interpreter] Warning: Historical prediction '%s' not found <= %A. Using 0.0" name timestamp
                    Ok (Numeric 0.0m) // Use 0.0 if not found in historical data either
            | false, _ ->
                 printfn "[Interpreter] Warning: Prediction history '%s' not found. Using 0.0" name
                 Ok (Numeric 0.0m) // Use 0.0 if prediction history not found

    | false, _ ->
        // ML predictions not available for this name at all, fallback to historical data
        printfn "[Interpreter] Info: ML prediction history for '%s' not found. Falling back to historical data." name
        match env.PredictionsHistory.TryGetValue(name) with
        | true, dataPoints ->
             match findLatestDataPointBeforeOrAt dataPoints timestamp with
             | Some dp ->
                  match dp.Value |> decimal.TryParse(CultureInfo.InvariantCulture) with
                  | true, d -> Ok (Numeric d)
                  | false, _ ->
                      match dp.Value |> bool.TryParse with
                      | true, b -> Ok (Boolean b)
                      | false, _ -> Error (sprintf "Interpreter Error: Could not parse historical prediction '%s' value '%s' as decimal or boolean <= %A" name dp.Value timestamp)
             | None ->
                 printfn "[Interpreter] Warning: Historical prediction '%s' not found <= %A. Using 0.0" name timestamp
                 Ok (Numeric 0.0m) // Use 0.0 if not found in historical data
        | false, _ ->
             printfn "[Interpreter] Warning: Prediction history '%s' not found. Using 0.0" name
             Ok (Numeric 0.0m) // Use 0.0 if prediction history not found


// Helper to evaluate an expression, returning InterpretationResult
let rec evalExpr (env: ExecutionEnvironment) (expr: Expr) : InterpretationResult<Value> =
    match expr with
    | Const v -> Ok v
    | GetIndicator name ->
        // Find the latest indicator value corresponding to the current timestamp <= env.CurrentTime
        match env.IndicatorsHistory.TryGetValue(name) with
        | true, dataPoints ->
             match findLatestDataPointBeforeOrAt dataPoints env.CurrentTime with
             | Some dp ->
                  match dp.Value |> decimal.TryParse(CultureInfo.InvariantCulture) with
                  | true, d -> Ok (Numeric d)
                  | false, _ -> Error (sprintf "Interpreter Error: Could not parse indicator '%s' value '%s' as decimal <= %A" name dp.Value env.CurrentTime)
             | None ->
                 printfn "[Interpreter] Warning: Indicator '%s' not found <= current timestamp %A. Using 0.0" name env.CurrentTime
                 Ok (Numeric 0.0m)
        | false, _ ->
            printfn "[Interpreter] Warning: Indicator history '%s' not found. Using 0.0" name
            Ok (Numeric 0.0m)

    | GetPrediction name ->
        // Find the latest prediction value corresponding to the current timestamp <= env.CurrentTime
        findLatestPredictionValueBeforeOrAt env name env.CurrentTime

    | GetPrevIndicator name ->
        // Find the latest indicator value corresponding to a timestamp *before* the current one.
        // Find the timestamp of the latest point strictly before CurrentTime.
        let prevTimestampOpt =
            env.IndicatorsHistory.Values // Look across all indicator histories to find a relevant previous timestamp
            |> Seq.collect id // Flatten all indicator data points
            |> Seq.filter (fun dp -> dp.Timestamp < env.CurrentTime) // Find points strictly before current
            |> Seq.sortByDescending (fun dp -> dp.Timestamp) // Sort descending by timestamp
            |> Seq.tryHead // Get the latest timestamp strictly before CurrentTime
            |> Option.map (fun dp -> dp.Timestamp)

        match prevTimestampOpt with
        | Some prevTs ->
            // Now find the indicator value at this previous timestamp
            match env.IndicatorsHistory.TryGetValue(name) with
            | true, dataPoints ->
                 match findLatestDataPointBeforeOrAt dataPoints prevTs with // Find the latest point at or before prevTs
                 | Some dp ->
                      match dp.Value |> decimal.TryParse(CultureInfo.CultureInfo.InvariantCulture) with
                      | true, d -> Ok (Numeric d)
                      | false, _ -> Error (sprintf "Interpreter Error: Could not parse previous indicator '%s' value '%s' as decimal <= %A" name dp.Value prevTs)
                 | None ->
                     printfn "[Interpreter] Warning: Previous indicator '%s' not found <= previous timestamp %A. Using 0.0" name prevTs
                     Ok (Numeric 0.0m)
            | false, _ ->
                 printfn "[Interpreter] Warning: Previous indicator history '%s' not found. Using 0.0" name
                 Ok (Numeric 0.0m)

        | None ->
            printfn "[Interpreter] Warning: No previous timestamp found before %A for GetPrevIndicator '%s'. Using 0.0" env.CurrentTime name
            Ok (Numeric 0.0m)


    | GetPrevPrediction name ->
        // Find the latest prediction value corresponding to a timestamp *before* the current one.
        let prevTimestampOpt =
             env.IndicatorsHistory.Values // Use indicator timestamps as reference points
             |> Seq.collect id
             |> Seq.filter (fun dp -> dp.Timestamp < env.CurrentTime)
             |> Seq.sortByDescending (fun dp -> dp.Timestamp)
             |> Seq.tryHead
             |> Option.map (fun dp -> dp.Timestamp)

        match prevTimestampOpt with
        | Some prevTs ->
            // Now find the prediction value at this previous timestamp (prioritizing ML)
            findLatestPredictionValueBeforeOrAt env name prevTs
        | None ->
             printfn "[Interpreter] Warning: No previous timestamp found before %A for GetPrevPrediction '%s'. Using 0.0" env.CurrentTime name
             Ok (Numeric 0.0m)


    | GetVar name ->
        match env.Vars.TryGetValue(name) with
        | true, v -> Ok v
        | false, _ -> Error (sprintf "Interpreter Error: Variable '%s' not found at %A" name env.CurrentTime)

    | BinOp (op, e1, e2) ->
        evalExpr env e1 >>= fun v1 ->
        evalExpr env e2 >>= fun v2 ->
        match op, v1, v2 with
        | Add, Numeric f1, Numeric f2 -> Ok (Numeric (f1 + f2))
        | Sub, Numeric f1, Numeric f2 -> Ok (Numeric (f1 - f2))
        | Mul, Numeric f1, Numeric f2 -> Ok (Numeric (f1 * f2))
        | Div, Numeric f1, Numeric f2 -> Ok (Numeric (if f2 <> 0m then f1 / f2 else System.Decimal.Zero / System.Decimal.Zero))
        | Eq, _, _ -> Ok (Boolean (v1 = v2))
        | Neq, _, _ -> Ok (Boolean (v1 <> v2))
        | Lt, Numeric f1, Numeric f2 -> Ok (Boolean (f1 < f2))
        | Gt, Numeric f1, Numeric f2 -> Ok (Boolean (f1 > f2))
        | Leq, Numeric f1, Numeric f2 -> Ok (Boolean (f1 <= f2))
        | Geq, Numeric f1, Numeric f2 -> Ok (Boolean (f1 >= f2))
        | And, Boolean b1, Boolean b2 -> Ok (Boolean (b1 && b2))
        | Or, Boolean b1, Boolean b2 -> Ok (Boolean (b1 || b2))
        | _, _, _ -> Error (sprintf "Interpreter Error: Type mismatch for binary operation %A: %A vs %A at %A" op v1 v2 env.CurrentTime)

    | BuiltinFunc (func, args) ->
        Result.sequence (List.map (evalExpr env) args) >>= fun argValues ->
        match func, argValues with
        | Abs, [Numeric f] -> Ok (Numeric (System.Decimal.Abs(f)))
        | Min, [Numeric f1; Numeric f2] -> Ok (Numeric (min f1 f2))
        | Max, [Numeric f1; Numeric f2] -> Ok (Numeric (max f1 f2))
        | _, _ -> Error (sprintf "Interpreter Error: Invalid arguments or types for built-in function %A: %A at %A" func argValues env.CurrentTime)


// Helper to execute a list of statements, returning collected actions or an error
let rec executeStatements (env: ExecutionEnvironment) (statements: Statement list) : InterpretationResult<Action list> =
    match statements with
    | [] -> Ok []
    | stmt :: rest ->
        executeStatement env stmt >>= fun actions -> // Execute current stmt, propagate error
        executeStatements env rest >>= fun restActions -> // Execute rest, propagate error
        Ok (actions @ restActions)


// Helper to execute a single statement, returning collected actions or an error
and executeStatement (env: ExecutionEnvironment) (statement: Statement) : InterpretationResult<Action list> =
    match statement with
    | If (condExpr, thenStmts, elseStmts) ->
        evalExpr env condExpr >>= fun condValue -> // Evaluate condition, propagate error
        match condValue with
        | Boolean true -> executeStatements env thenStmts // Execute THEN block
        | Boolean false -> executeStatements env elseStmts // Execute ELSE block
        | _ -> Error (sprintf "Interpreter Error: IF condition must be boolean, got %A at %A" condValue env.CurrentTime)
    | Action action -> Ok [action] // Return the action to be processed by the simulation engine
    | VarDecl (name, initialExpr) ->
        printfn "[Interpreter] Warning: Encountered VarDecl '%s' during execution walk at %A. Skipping." name env.CurrentTime
        Ok []
    | SetVar (name, expr) ->
        match env.Vars.TryGetValue(name) with
        | true, declaredValue ->
            evalExpr env expr >>= fun valueToSet -> // Evaluate expression to set, propagate error
            env.Vars.[name] <- valueToSet
            Ok [] // SetVar does not produce actions
        | false, _ -> Error (sprintf "Interpreter Error: Attempted to set undeclared variable '%s' at %A" name env.CurrentTime)


// Main function to run the strategy logic for a single data point
// Returns collected actions or an error
let runStrategyLogic (strategy: Strategy) (env: ExecutionEnvironment) : InterpretationResult<Action list> =
    executeStatements env strategy.Logic