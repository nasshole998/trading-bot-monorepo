// src/Simulation/SimulationEngine.fs
module BacktesterFsharp.Simulation.SimulationEngine

open System
open System.Collections.Generic
open BacktesterFsharp.Dsl.Ast
open BacktesterFsharp.Dsl.Interpreter
open BacktesterFsharp.Data.HistoricalDataLoader
open BacktesterFsharp.Simulation.SimulationTypes
open BacktesterFsharp.Metrics.MetricsCalculator
open BacktesterFsharp.Config // For fees, slippage
open MarketDataPb // For OrderSide, OrderType, OrderStatus
open MlTestingPb // For HistoricalPredictionsRequest
open MlPredictionPb // For PredictionValue
open BacktesterFsharp.MlTesting.MlTestingClient // Needs ML testing client
open Google.Protobuf.WellKnownTypes // For Timestamp conversions
open System.Globalization // For InvariantCulture
open System.Linq // For LINQ extensions like ToDictionary

// Helper to get history for a specific indicator name
let getIndicatorHistory (historyDict: Dictionary<string, DataPoint list>) (name: string) : DataPoint list =
    match historyDict.TryGetValue(name) with
    | true, history -> history
    | false, _ -> [] // Return empty list if name not found

// Helper to get historical predictions for a specific type, keyed by timestamp
let getPredictionHistory (historyDict: Dictionary<string, DataPoint list>) (name: string) : DataPoint list =
    match historyDict.TryGetValue(name) with
    | true, history -> history
    | false, _ -> [] // Return empty list if name not found

// Helper to get ML predictions for a specific type, keyed by timestamp
let getMlPredictionHistory (mlPredictionsDict: Dictionary<string, Dictionary<DateTimeOffset, PredictionValue>>) (name: string) : Dictionary<DateTimeOffset, PredictionValue> =
    match mlPredictionsDict.TryGetValue(name) with
    | true, history -> history
    | false, _ -> Dictionary<DateTimeOffset, PredictionValue>() // Return empty dictionary if name not found


// Simulate placing a trade order (improved execution simulation)
// Returns a Result to indicate success or failure of order placement (e.g., insufficient funds/position)
let simulateOrder (state: SimulationState) (order: SimulatedOrder) : Result<unit, string> =
    printfn "[Simulation] Strategy '%s' placing order: %A at %A" order.StrategyName order.ClientOrderId state.HistoricalData.[state.CurrentDataIndex].Timestamp

    // Add order to the tracking list
    if state.ActiveOrders.ContainsKey(order.ClientOrderId) then
         Error (sprintf "Duplicate client order ID: %s at %A" order.ClientOrderId state.HistoricalData.[state.CurrentDataIndex].Timestamp)
    else
        state.ActiveOrders.Add(order.ClientOrderId, order)
        order.Status <- OrderStatus.ACKNOWLEDGED // Simulate immediate acknowledgement

        // Process based on order type and current market data
        let currentDataPoint = state.HistoricalData.[state.CurrentDataIndex]
        let currentPrice = currentDataPoint.Price

        match order.OrderType with
        | OrderType.MARKET ->
            // Simulate immediate fill at current price with slippage
            let fillPrice =
                match order.Side with
                | OrderSide.BUY -> currentPrice * (1.0m + slippagePercent) // Buy adds slippage
                | OrderSide.SELL -> currentPrice * (1.0m - slippagePercent) // Sell subtracts slippage
                | _ -> currentPrice // Should not happen

            let quantityToFill = order.Quantity // Market orders try to fill full quantity immediately
            let feeRate = if state.Account.Position.CompareTo(0m) = 0 then takerFee else makerFee // Simplified fee logic
            let fee = quantityToFill * fillPrice * feeRate // Fee based on filled quantity

            // Check if executable (basic checks)
            match order.Side with
            | OrderSide.BUY ->
                let cost = quantityToFill * fillPrice
                if state.Account.Capital < cost + fee then
                    order.Status <- OrderStatus.REJECTED // Not enough capital
                    printfn "[Simulation] Order %s Rejected (BUY %.8m) - Insufficient capital (%.2m needed, have %.2m) at %A" order.ClientOrderId quantityToFill (cost + fee) state.Account.Capital currentDataPoint.Timestamp
                    Ok () // Placement failed, but function succeeded
                else
                    // Execute the fill
                    let totalCostBeforeFee = state.Account.Position * state.Account.AvgEntryPrice + cost
                    let totalQuantity = state.Account.Position + quantityToFill
                    state.Account.AvgEntryPrice <- if totalQuantity.CompareTo(0m) > 0 then totalCostBeforeFee / totalQuantity else 0m // Handle potential 0 division
                    state.Account.Capital <- state.Account.Capital - cost - fee
                    state.Account.Position <- state.Account.Position + quantityToFill
                    state.Account.TotalFeesPaid <- state.Account.TotalFeesPaid + fee
                    order.Status <- OrderStatus.FILLED // Mark as filled
                    order.FilledQuantity <- quantityToFill
                    order.AvgFillPrice <- fillPrice
                    order.CumulativeCommission <- order.CumulativeCommission + fee

                    printfn "[Simulation] Order %s Filled (BUY %.8m @ %.8m). Fee %.8m. New Pos: %.8m @ %.8m, Cap: %.2m at %A" order.ClientOrderId quantityToFill fillPrice fee state.Account.Position state.Account.AvgEntryPrice state.Account.Capital currentDataPoint.Timestamp

                    // Add trade record
                    let trade = {
                        Timestamp = currentDataPoint.Timestamp
                        Symbol = order.Symbol
                        Side = order.Side
                        Price = fillPrice
                        Quantity = quantityToFill
                        Fee = fee
                        RelatedOrderId = order.ClientOrderId
                        RealizedPnl = None // P/L calculated on exit
                    }
                    state.TradeHistory <- trade :: state.TradeHistory
                    Ok ()

            | OrderSide.SELL ->
                if state.Account.Position < quantityToFill then
                    order.Status <- OrderStatus.REJECTED // Not enough position
                    printfn "[Simulation] Order %s Rejected (SELL %.8m) - Insufficient position (%.8m needed, have %.8m) at %A" order.ClientOrderId quantityToFill quantityToFill state.Account.Position currentDataPoint.Timestamp
                    Ok () // Placement failed, but function succeeded
                else
                    // Execute the fill
                    let revenue = quantityToFill * fillPrice
                    let pnl = (fillPrice - state.Account.AvgEntryPrice) * quantityToFill // P/L for the part being sold
                    state.Account.Capital <- state.Account.Capital + revenue - fee
                    state.Account.Position <- state.Account.Position - quantityToFill
                    state.Account.RealizedProfitLoss <- state.Account.RealizedProfitLoss + pnl // Accumulate realized P/L
                    state.Account.TotalFeesPaid <- state.Account.TotalFeesPaid + fee
                    order.Status <- OrderStatus.FILLED // Mark as filled
                    order.FilledQuantity <- quantityToFill
                    order.AvgFillPrice <- fillPrice
                    order.CumulativeCommission <- order.CumulativeCommission + fee

                    printfn "[Simulation] Order %s Filled (SELL %.8m @ %.8m). Fee %.8m. P/L %.2m. New Pos: %.8m, Cap: %.2m at %A" order.ClientOrderId quantityToFill fillPrice fee pnl state.Account.Position state.Account.Capital currentDataPoint.Timestamp

                    // Add trade record
                    let trade = {
                        Timestamp = currentDataPoint.Timestamp
                        Symbol = order.Symbol
                        Side = order.Side
                        Price = fillPrice
                        Quantity = quantityToFill
                        Fee = fee
                        RelatedOrderId = order.ClientOrderId
                        RealizedPnl = Some pnl
                    }
                    state.TradeHistory <- trade :: state.TradeHistory

                    // If position is now zero, reset AvgEntryPrice
                    if state.Account.Position.CompareTo(0m) = 0 then // Use CompareTo for exact zero
                        state.Account.AvgEntryPrice <- 0m

                    Ok ()
            | _ -> Error (sprintf "Unsupported order side for market order: %A at %A" order.Side currentDataPoint.Timestamp)


        | OrderType.LIMIT ->
            // Check if a Limit order is triggered by the current data point's price range (Open, High, Low, Close if available)
            // Using just the current Price for simplicity.
            match order.Price with
            | None -> Error (sprintf "Limit order %s is missing price at %A" order.ClientOrderId currentDataPoint.Timestamp)
            | Some limitPrice ->
                let isTriggered =
                    match order.Side with
                    | OrderSide.BUY -> currentPrice <= limitPrice // Buy Limit fills if current price touches or goes below limit
                    | OrderSide.SELL -> currentPrice >= limitPrice // Sell Limit fills if current price touches or goes above limit
                    | _ -> false

                if isTriggered then
                     // Simulate immediate fill at the limit price or potentially better (currentPrice)
                    let actualFillPrice =
                        match order.Side with
                        | OrderSide.BUY when currentPrice < limitPrice -> currentPrice // Fill at better price if available
                        | OrderSide.SELL when currentPrice > limitPrice -> currentPrice // Fill at better price if available
                        | _ -> limitPrice // Fill at the limit price

                    let quantityToFill = order.Quantity - order.FilledQuantity // Remaining quantity
                    let feeRate = makerFee // Limit orders typically pay maker fee
                    let fee = quantityToFill * actualFillPrice * feeRate

                    // Check if executable (basic checks)
                    match order.Side with
                    | OrderSide.BUY ->
                        let cost = quantityToFill * actualFillPrice
                        if state.Account.Capital < cost + fee then
                            order.Status <- OrderStatus.REJECTED // Not enough capital
                             printfn "[Simulation] Order %s Rejected (BUY LIMIT %.8m @ %.8m) - Insufficient capital at %A" order.ClientOrderId quantityToFill limitPrice currentDataPoint.Timestamp
                            Ok ()
                        else
                             // Execute the fill
                            let totalCostBeforeFee = state.Account.Position * state.Account.AvgEntryPrice + cost
                            let totalQuantity = state.Account.Position + quantityToFill
                            state.Account.AvgEntryPrice <- if totalQuantity.CompareTo(0m) > 0 then totalCostBeforeFee / totalQuantity else 0m
                            state.Account.Capital <- state.Account.Capital - cost - fee
                            state.Account.Position <- state.Account.Position + quantityToFill
                            state.Account.TotalFeesPaid <- state.Account.TotalFeesPaid + fee
                            order.Status <- OrderStatus.FILLED // Mark as filled (assuming full fill)
                            order.FilledQuantity <- order.Quantity // Fill the whole quantity
                            order.AvgFillPrice <- actualFillPrice // Simplified average fill price
                            order.CumulativeCommission <- order.CumulativeCommission + fee

                            printfn "[Simulation] Order %s Filled (BUY LIMIT %.8m @ %.8m). Fee %.8m. New Pos: %.8m @ %.8m, Cap: %.2m at %A" order.ClientOrderId quantityToFill actualFillPrice fee state.Account.Position state.Account.AvgEntryPrice state.Account.Capital currentDataPoint.Timestamp

                            // Add trade record
                            let trade = {
                                Timestamp = currentDataPoint.Timestamp
                                Symbol = order.Symbol
                                Side = order.Side
                                Price = actualFillPrice
                                Quantity = quantityToFill
                                Fee = fee
                                RelatedOrderId = order.ClientOrderId
                                RealizedPnl = None
                            }
                            state.TradeHistory <- trade :: state.TradeHistory
                            Ok ()

                    | OrderSide.SELL ->
                         if state.Account.Position < quantityToFill then
                            order.Status <- OrderStatus.REJECTED // Not enough position
                            printfn "[Simulation] Order %s Rejected (SELL LIMIT %.8m @ %.8m) - Insufficient position at %A" order.ClientOrderId quantityToFill limitPrice currentDataPoint.Timestamp
                            Ok ()
                        else
                             // Execute the fill
                            let revenue = quantityToFill * actualFillPrice
                            let pnl = (actualFillPrice - state.Account.AvgEntryPrice) * quantityToFill // P/L for the part being sold
                            state.Account.Capital <- state.Account.Capital + revenue - fee
                            state.Account.Position <- state.Account.Position - quantityToFill
                            state.Account.RealizedProfitLoss <- state.Account.RealizedProfitLoss + pnl // Accumulate realized P/L
                            state.Account.TotalFeesPaid <- state.Account.TotalFeesPaid + fee
                            order.Status <- OrderStatus.FILLED // Mark as filled
                            order.FilledQuantity <- order.Quantity // Fill the whole quantity
                            order.AvgFillPrice <- actualFillPrice
                            order.CumulativeCommission <- order.CumulativeCommission + fee

                            printfn "[Simulation] Order %s Filled (SELL LIMIT %.8m @ %.8m). Fee %.8m. P/L %.2m. New Pos: %.8m, Cap: %.2m at %A" order.ClientOrderId quantityToFill actualFillPrice fee pnl state.Account.Position state.Account.Capital currentDataPoint.Timestamp

                            // Add trade record
                            let trade = {
                                Timestamp = currentDataPoint.Timestamp
                                Symbol = order.Symbol
                                Side = order.Side
                                Price = actualFillPrice
                                Quantity = quantityToFill
                                Fee = fee
                                RelatedOrderId = order.ClientOrderId
                                RealizedPnl = Some pnl
                            }
                            state.TradeHistory <- trade :: state.TradeHistory

                            // If position is now zero, reset AvgEntryPrice
                            if state.Account.Position.CompareTo(0m) = 0 then
                                state.Account.AvgEntryPrice <- 0m
                            Ok ()
                    | _ -> Error (sprintf "Unsupported order side for limit order: %A at %A" order.Side currentDataPoint.Timestamp)

                else
                     // Limit order not triggered by this data point, status remains ACKNOWLEDGED
                     printfn "[Simulation] Order %s (LIMIT %.8m @ %.8m) not triggered by current price %.8m at %A" order.ClientOrderId order.Quantity (limitPrice) currentPrice currentDataPoint.Timestamp
                     Ok () // Placement succeeded, but not filled yet


        | _ ->
            // Unsupported order type
            order.Status <- OrderStatus.REJECTED
            printfn "[Simulation] Unsupported order type %A for order %s at %A" order.OrderType order.ClientOrderId currentDataPoint.Timestamp
            Error (sprintf "Unsupported order type: %A at %A" order.OrderType currentDataPoint.Timestamp)


// Process actions returned by the strategy interpreter
let processActions (state: SimulationState) (actions: Action list) : Result<unit, string list> = // Return list of errors
    let mutable errors = []
    for action in actions do
        match action with
        | Buy qtyExpr ->
            let interpreterEnv = {
                 Vars = state.Account.Vars
                 IndicatorsHistory = state.IndicatorsData
                 PredictionsHistory = state.PredictionsData
                 MlPredictions = state.MlPredictions
                 Symbol = state.HistoricalData.[state.CurrentDataIndex].Symbol
                 CurrentTime = state.HistoricalData.[state.CurrentDataIndex].Timestamp
                 CurrentPrice = state.HistoricalData.[state.CurrentDataIndex].Price
             }
            match Interpreter.evalExpr interpreterEnv qtyExpr with
            | Ok (Numeric qty) when qty.CompareTo(0m) > 0 ->
                let clientOrderId = sprintf "SIMORDER_%s_%s" state.Strategy.Name (Guid.NewGuid().ToString("N")) // Unique ID
                 let order = {
                     ClientOrderId = clientOrderId
                     StrategyName = state.Strategy.Name
                     Symbol = interpreterEnv.Symbol
                     Side = OrderSide.BUY
                     OrderType = OrderType.MARKET // Simulate market order for now
                     Quantity = qty
                     Price = Some interpreterEnv.CurrentPrice // Use current price for market order simulation
                     PlacedTime = interpreterEnv.CurrentTime
                     Status = OrderStatus.NEW
                     FilledQuantity = 0.0m
                     AvgFillPrice = 0.0m
                     CumulativeCommission = 0.0m
                 }
                 match simulateOrder state order with
                 | Ok () -> ()
                 | Error msg -> errors <- msg :: errors
            | Ok (Numeric qty) ->
                let err = sprintf "BUY quantity expression evaluated to non-positive: %.8m at %A. Ignoring action." qty interpreterEnv.CurrentTime
                printfn "[Simulation] Warning: %s" err
                errors <- err :: errors
            | Ok v ->
                 let err = sprintf "BUY quantity expression evaluated to non-numeric type, got %A at %A. Ignoring action." v interpreterEnv.CurrentTime
                 printfn "[Simulation] Warning: %s" err
                 errors <- err :: errors
            | Error msg ->
                 let err = sprintf "Error evaluating BUY quantity expression: %s at %A. Ignoring action." msg interpreterEnv.CurrentTime
                 printfn "[Simulation] Warning: %s" err
                 errors <- err :: errors

        | Sell qtyExpr ->
            let interpreterEnv = {
                 Vars = state.Account.Vars
                 IndicatorsHistory = state.IndicatorsData
                 PredictionsHistory = state.PredictionsData
                 MlPredictions = state.MlPredictions
                 Symbol = state.HistoricalData.[state.CurrentDataIndex].Symbol
                 CurrentTime = state.HistoricalData.[state.CurrentDataIndex].Timestamp
                 CurrentPrice = state.HistoricalData.[state.CurrentDataIndex].Price
             }
             match Interpreter.evalExpr interpreterEnv qtyExpr with
            | Ok (Numeric qty) when qty.CompareTo(0m) > 0 ->
                 let clientOrderId = sprintf "SIMORDER_%s_%s" state.Strategy.Name (Guid.NewGuid().ToString("N")) // Unique ID
                 let order = {
                     ClientOrderId = clientOrderId
                     StrategyName = state.Strategy.Name
                     Symbol = interpreterEnv.Symbol
                     Side = OrderSide.SELL
                     OrderType = OrderType.MARKET // Simulate market order for now
                     Quantity = qty
                     Price = Some interpreterEnv.CurrentPrice // Use current price for market order simulation
                     PlacedTime = interpreterEnv.CurrentTime
                     Status = OrderStatus.NEW
                     FilledQuantity = 0.0m
                     AvgFillPrice = 0.0m
                     CumulativeCommission = 0.0m
                 }
                 match simulateOrder state order with
                 | Ok () -> ()
                 | Error msg -> errors <- msg :: errors
            | Ok (Numeric qty) ->
                 let err = sprintf "SELL quantity expression evaluated to non-positive: %.8m at %A. Ignoring action." qty interpreterEnv.CurrentTime
                 printfn "[Simulation] Warning: %s" err
                 errors <- err :: errors
            | Ok v ->
                 let err = sprintf "SELL quantity expression evaluated to non-numeric type, got %A at %A. Ignoring action." v interpreterEnv.CurrentTime
                 printfn "[Simulation] Warning: %s" err
                 errors <- err :: errors
            | Error msg ->
                 let err = sprintf "Error evaluating SELL quantity expression: %s at %A. Ignoring action." msg interpreterEnv.CurrentTime
                 printfn "[Simulation] Warning: %s" err
                 errors <- err :: errors

        | Hold -> ()
        | Log expr ->
             let interpreterEnv = {
                 Vars = state.Account.Vars
                 IndicatorsHistory = state.IndicatorsData
                 PredictionsHistory = state.PredictionsData
                 MlPredictions = state.MlPredictions
                 Symbol = state.HistoricalData.[state.CurrentDataIndex].Symbol
                 CurrentTime = state.HistoricalData.[state.CurrentDataIndex].Timestamp
                 CurrentPrice = state.HistoricalData.[state.CurrentDataIndex].Price
             }
             match Interpreter.evalExpr interpreterEnv expr with
             | Ok value ->
                 printfn "[Simulation] Strategy Log @ %A: %A" interpreterEnv.CurrentTime value
             | Error msg ->
                 let err = sprintf "Error evaluating Log expression: %s at %A" msg interpreterEnv.CurrentTime
                 printfn "[Simulation] Warning: %s" err
                 errors <- err :: errors

        | _ ->
            let err = sprintf "Unsupported action type: %A at %A" action state.HistoricalData.[state.CurrentDataIndex].Timestamp
            printfn "[Simulation] Warning: %s" err
            errors <- err :: errors

    if List.isEmpty errors then Ok () else Error errors


// Main simulation engine execution
let runSimulation (request: BacktesterPb.BacktestRequest) (strategy: Strategy) (history: DataPoint list) (initialCapital: decimal) (mlPredictions: Dictionary<string, Dictionary<DateTimeOffset, PredictionValue>>) : Result<AccountState * SimulatedTrade list, string list> = // Return list of errors

    if List.isEmpty history then
        Error ["Cannot run simulation on empty historical data."]
    else

        // --- Data Preprocessing for Interpreter History ---
        // Load historical indicators/predictions (if not part of raw data)
        // Assume 'current_price' is available in DataPoint and used as a default indicator/prediction name.

        let indicatorsData = Dictionary<string, DataPoint list>() // name -> history
        let predictionsData = Dictionary<string, DataPoint list>() // name -> history

        // Add 'current_price' from raw history
        indicatorsData.Add("current_price", history)
        predictionsData.Add("current_price", history)
        // Add logic here to load or compute other indicator/prediction histories based on names used in the strategy
        // For example, if strategy uses SMA_20, load SMA_20 history here or compute it from price history


        let initialAccountState = {
            Capital = initialCapital
            Position = 0.0m
            HoldingValue = 0.0m
            TotalFeesPaid = 0.0m
            RealizedProfitLoss = 0.0m
            UnrealizedProfitLoss = 0.0m
            AvgEntryPrice = 0.0m
            PeakEquity = initialCapital
            MaxDrawdown = 0.0m
            EquityHistory = [(history.Head.Timestamp, initialCapital)] // Initial equity point
        }

        let simulationState = {
            Account = initialAccountState
            TradeHistory = []
            ActiveOrders = Dictionary<string, SimulatedOrder>()
            HistoricalData = history
            CurrentDataIndex = 0
            InitialCapital = initialCapital
            Strategy = strategy
            IndicatorsData = indicatorsData
            PredictionsData = predictionsData
            MlPredictions = mlPredictions // Pass the loaded ML predictions
        }

        // Initialize strategy variables in a mutable dictionary
        let strategyVars = new Dictionary<string, Dsl.Ast.Value>(strategy.InitialState)

        printfn "Starting backtest simulation..."

        let mutable simulationErrors = []

        // --- Simulation Loop ---
        for i = 0 to history.Length - 1 do
            simulationState.CurrentDataIndex <- i
            let currentDataPoint = history.[i]

            // Update Holding Value and Unrealized P/L based on current price
            simulationState.Account.HoldingValue <- simulationState.Account.Position * currentDataPoint.Price
            simulationState.Account.UnrealizedProfitLoss <-
                if simulationState.Account.Position.CompareTo(0m) <> 0 then
                    (currentDataPoint.Price - simulationState.Account.AvgEntryPrice) * simulationState.Account.Position
                else
                    0m

            // Update peak equity and calculate drawdown *before* processing orders/actions
            let currentEquity = simulationState.Account.Capital + simulationState.Account.HoldingValue
            simulationState.Account.PeakEquity <- max simulationState.Account.PeakEquity currentEquity
            let drawdown = simulationState.Account.PeakEquity - currentEquity
            simulationState.Account.MaxDrawdown <- max simulationState.Account.MaxDrawdown drawdown

            // --- Process Active Orders (Check for Limit Order Fills) ---
            // Create a list of orders to process to avoid modifying dictionary while iterating
            let activeOrderIds = simulationState.ActiveOrders.Keys |> List.ofSeq
            for orderId in activeOrderIds do
                match simulationState.ActiveOrders.TryGetValue(orderId) with
                | true, order when order.Status = OrderStatus.ACKNOWLEDGED || order.Status = OrderStatus.NEW ->
                     match simulateOrder simulationState order with // simulateOrder handles the fill logic and state updates
                     | Ok () -> ()
                     | Error msg -> simulationErrors <- msg :: simulationErrors // Collect order simulation errors

                | true, _ -> () // Order is in a final state (FILLED, REJECTED, CANCELED, etc.), do nothing
                | false, _ -> () // Order not found (shouldn't happen if iterating ActiveOrders.Keys)


            // --- Prepare Environment for Interpreter ---
            let interpreterEnv = {
                 Vars = strategyVars
                 IndicatorsHistory = indicatorsData
                 PredictionsHistory = predictionsData
                 MlPredictions = mlPredictions
                 Symbol = currentDataPoint.Symbol
                 CurrentTime = currentDataPoint.Timestamp
                 CurrentPrice = currentDataPoint.Price
             }

            // --- Run Strategy Logic ---
            try
                match runStrategyLogic simulationState.Strategy interpreterEnv with
                | Ok actions ->
                    match processActions simulationState actions with
                    | Ok () -> ()
                    | Error msgs -> simulationErrors <- msgs @ simulationErrors // Collect action processing errors
                | Error msg ->
                    simulationErrors <- msg :: simulationErrors // Collect strategy logic execution error
            with
            | ex ->
                let err = sprintf "Unhandled exception during strategy logic at %A: %s" currentDataPoint.Timestamp ex.Message
                printfn "[Simulation] Error: %s" err
                simulationErrors <- err :: simulationErrors

            // Record equity at this step for accurate metrics *after* potential trades/equity changes
            let currentEquityAfter = simulationState.Account.Capital + simulationState.Account.HoldingValue
            simulationState.Account.EquityHistory <- (currentDataPoint.Timestamp, currentEquityAfter) :: simulationState.Account.EquityHistory
            // Note: Drawdown is calculated based on EquityHistory during metrics calculation as well,
            // but tracking PeakEquity here gives a running max drawdown during the simulation.


        printfn "Backtest simulation finished."
        printfn "Total simulation errors/warnings: %d" simulationErrors.Length
        if List.length simulationErrors > 0 then
            printfn "First few errors: %A" (List.take 5 simulationErrors)

        // Final Account State and Trades (reverse trades and equity history for chronological order)
        let finalAccountState = simulationState.Account
        finalAccountState.EquityHistory <- List.rev finalAccountState.EquityHistory
        Ok (finalAccountState, List.rev simulationState.TradeHistory)


// Implementation of the gRPC service - Calls the main task-based function
type BacktesterServiceImpl() =
    inherit BacktesterService.BacktesterServiceBase()

    override this.BacktestStrategy(request: BacktesterPb.BacktestRequest, context: ServerCallContext) : Task<BacktesterPb.BacktestResponse> =
        task {
            printfn "Handling BacktestStrategy request for '%s' on '%s' (%A to %A)"
                    request.StrategyName request.Symbol request.StartTime request.EndTime

            let strategyName = request.StrategyName
            let dslCode = request.StrategyDslCode
            let symbol = request.Symbol
            let startTime = request.StartTime.ToDateTimeOffset() // Convert Protobuf Timestamp
            let endTime = request.EndTime.ToDateTimeOffset()

            // 1. Parse the DSL
            let strategyResult =
                try
                    use reader = new StringReader(dslCode)
                    let lexbuf = LexBuffer<char>.New(reader, fileName = sprintf "%s.strat" strategyName) // Set filename for error messages
                    let parsedStrategy = Parser.strategy Lexer.token lexbuf
                    Ok parsedStrategy
                with
                | ex ->
                    Error (sprintf "DSL Parsing failed: %s" ex.Message)

            match strategyResult with
            | Error msg ->
                printfn "Backtest failed due to parsing error: %s" msg
                return! Task.FromResult(BacktesterPb.BacktestResponse(Success = false, ErrorMessage = msg))
            | Ok strategy ->
                printfn "DSL parsed successfully."

                // 2. Load Historical Market Data
                match HistoricalDataLoader.loadHistory symbol startTime endTime Config.historyDataPath with
                | Error msg ->
                     printfn "Backtest failed: %s" msg
                     return! Task.FromResult(BacktesterPb.BacktestResponse(Success = false, ErrorMessage = msg))
                | Ok history ->

                    if List.isEmpty history then
                         printfn "Backtest failed: No historical data loaded for '%s' between %A and %A." symbol startTime endTime
                         return! Task.FromResult(BacktesterPb.BacktestResponse(Success = false, Error