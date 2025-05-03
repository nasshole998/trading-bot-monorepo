// src/BacktesterService.fs
module BacktesterFsharp.BacktesterService

open System.Threading.Tasks
open Grpc.Core
open BacktesterPb // Generated Backtester protobuf types
open MarketDataPb // Generated MarketData protobuf types (for enums)
open BacktesterFsharp.Simulation.SimulationEngine // Needs simulation engine
open BacktesterFsharp.Dsl.Ast // Needs DSL AST types
open System.Globalization // For InvariantCulture

// Implement the BacktesterServiceBase abstract class
type BacktesterServiceImpl() =
    inherit BacktesterService.BacktesterServiceBase()

    override this.BacktestStrategy(request: BacktesterPb.BacktestRequest, context: ServerCallContext) : Task<BacktesterPb.BacktestResponse> =
        printfn "Handling BacktestStrategy request for '%s' on '%s' (%A to %A)"
                request.StrategyName request.Symbol request.StartTime request.EndTime

        // Run the backtest simulation
        let response = SimulationEngine.runBacktest request // This now returns BacktesterPb.BacktestResponse

        Task.FromResult(response)