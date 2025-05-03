// src/Metrics/MetricsCalculator.fs
module BacktesterFsharp.Metrics.MetricsCalculator

open System
open BacktesterFsharp.Simulation.SimulationTypes
open BacktesterFsharp.Config // For risk-free rate
open MathNet.Numerics.Statistics // For statistical functions
open System.Linq // For LINQ methods

// Represents the calculated performance metrics
type BacktestMetrics = {
    FinalEquity: decimal
    TotalProfitLoss: decimal
    TotalFeesPaid: decimal
    MaxDrawdown: decimal
    TotalTrades: int // Count of execution fills (each buy/sell segment)
    WinRate: double // Percentage of winning trades (trades with positive realized P/L)
    ProfitFactor: double // Gross Profit / Gross Loss (|Total positive P/L| / |Total negative P/L|)
    SharpeRatio: double // Using double for Sharpe ratio calculation
    SortinoRatio: double // Using double for Sortino ratio calculation
    // Add other metrics here (e.g., CAGR, Calmar Ratio, Average Trade, Max Consecutive Wins/Losses)
}

// Helper to calculate period returns from equity history
// For daily returns, sample equity once per day
let calculatePeriodReturns (equityHistory: (DateTimeOffset * decimal) list) (period: TimeSpan) : double list =
    if List.length equityHistory < 2 then
        [] // Need at least two points to calculate a return

    let sortedHistory = equityHistory |> List.sortBy (fun (ts, _) -> ts)

    let mutable lastTimestamp = sortedHistory.Head |> fst
    let mutable lastEquity = sortedHistory.Head |> snd
    let mutable periodEndEquity = lastEquity // Track equity at the end of the current period
    let mutable returns = []
    let mutable currentPeriodStart = lastTimestamp.Date // Start of the first period

    for (currentTimestamp, currentEquity) in sortedHistory.Tail do
         // Check if we crossed into a new period
         let elapsed = currentTimestamp - currentPeriodStart
         if elapsed >= period then
             // Calculate return for the completed period
             if lastEquity <> 0m then // Avoid division by zero
                 let periodReturn = (float (currentEquity - periodEndEquity)) / (float periodEndEquity)
                 returns <- periodReturn :: returns

             // Start the next period
             currentPeriodStart <- currentTimestamp.Date // For daily, this is simple
             // Adjust for other periods (weekly, monthly) if needed
             // Find the true start of the next period based on currentTimestamp and 'period'
             periodEndEquity <- currentEquity // New starting equity for the next period

         lastTimestamp <- currentTimestamp
         lastEquity <- currentEquity // Keep track of last equity value for next step's calculation


    // Handle the last period if not explicitly captured
    if lastEquity <> periodEndEquity && periodEndEquity <> 0m then
         let periodReturn = (float (lastEquity - periodEndEquity)) / (float periodEndEquity)
         returns <- periodReturn :: returns

    returns |> List.rev // Return returns in chronological order

// Helper to calculate Downside Deviation
let calculateDownsideDeviation (returns: double list) (minimumAcceptableReturn: double) : double =
    if List.isEmpty returns then 0.0
    else
        let downsideReturns = returns |> List.filter (fun r -> r < minimumAcceptableReturn)
        if List.isEmpty downsideReturns then 0.0 // No downside deviation if no returns are below MAR
        else
            let squaredDeviations = downsideReturns |> List.map (fun r -> (minimumAcceptableReturn - r) ** 2.0)
            let meanSquaredDeviation = squaredDeviations |> Seq.average
            System.Math.Sqrt(meanSquaredDeviation) // Sample or population standard deviation? Library default might be sample.

// Calculate metrics from the final simulation state and trade history
let calculateMetrics (finalAccountState: AccountState) (tradeHistory: SimulatedTrade list) (startTime: DateTimeOffset) (endTime: DateTimeOffset) : BacktestMetrics =
    let finalEquity = finalAccountState.Capital + finalAccountState.HoldingValue
    let totalRealizedProfitLoss = finalAccountState.RealizedProfitLoss // Accumulated P/L from closed trades
    let totalUnrealizedProfitLoss = finalAccountState.UnrealizedProfitLoss // P/L on current open position
    let totalProfitLoss = totalRealizedProfitLoss + totalUnrealizedProfitLoss // Total P/L including open position

    let totalFeesPaid = finalAccountState.TotalFeesPaid
    let maxDrawdown = finalAccountState.MaxDrawdown // Already tracked in AccountState

    let totalTrades = tradeHistory.Length

    // Win Rate, Profit Factor
    let realizedPnls = tradeHistory |> List.choose (fun trade -> trade.RealizedPnl) // Get P/L for exit trades
    let winningPnls = realizedPnls |> List.filter (fun pnl -> pnl > 0m)
    let losingPnls = realizedPnls |> List.filter (fun pnl -> pnl < 0m)
    let totalWinningPnl = winningPnls |> List.sum
    let totalLosingPnl = losingPnls |> List.sum

    let winningTradesCount = winningPnls.Length
    let losingTradesCount = losingPnls.Length
    let totalClosedTrades = winningTradesCount + losingTradesCount

    let winRate = if totalClosedTrades > 0 then (float winningTradesCount) / (float totalClosedTrades) else 0.0
    let profitFactor = if totalLosingPnl <> 0m then (float totalWinningPnl) / (float (System.Decimal.Abs(totalLosingPnl))) else if totalWinningPnl > 0m then System.Double.PositiveInfinity else 0.0 // Handle division by zero

    // Sharpe Ratio and Sortino Ratio using Equity History
    // Use daily returns (period = 1 day)
    let dailyReturns = calculatePeriodReturns finalAccountState.EquityHistory TimeSpan.FromDays(1.0)

    let annualizationFactor = System.Math.Sqrt(252.0) // Approx. 252 trading days per year

    let sharpeRatio =
        if List.length dailyReturns > 1 then // Need at least two daily returns
            let meanDailyReturn = dailyReturns |> Seq.average
            let stdDevDailyReturn = dailyReturns |> Seq.stdDev
            if stdDevDailyReturn > 0.0 then
                let annualizedMeanReturn = meanDailyReturn * 252.0 // Annualize mean daily return
                let annualizedRiskFreeRate = Config.riskFreeRate // Assume risk-free rate is already annualized or per-period matching daily
                let annualizedStdDev = stdDevDailyReturn * annualizationFactor // Annualize volatility
                (annualizedMeanReturn - annualizedRiskFreeRate) / annualizedStdDev
            else
                0.0 // Cannot calculate if no variance in returns
        else
            0.0 // Cannot calculate with insufficient daily returns

    let sortinoRatio =
        if List.length dailyReturns > 1 then
             let meanDailyReturn = dailyReturns |> Seq.average
             let minimumAcceptableReturn = Config.riskFreeRate / 252.0 // Convert annualized risk-free rate to daily
             let downsideDeviation = calculateDownsideDeviation dailyReturns minimumAcceptableReturn
             if downsideDeviation > 0.0 then
                 let annualizedMeanReturn = meanDailyReturn * 252.0
                 let annualizedRiskFreeRate = Config.riskFreeRate
                 (annualizedMeanReturn - annualizedRiskFreeRate) / (downsideDeviation * annualizationFactor) // Annualize downside deviation
             else
                 0.0
        else
            0.0

    {
        FinalEquity = finalEquity
        TotalProfitLoss = totalProfitLoss
        TotalFeesPaid = totalFeesPaid
        MaxDrawdown = maxDrawdown
        TotalTrades = totalTrades
        WinRate = winRate
        ProfitFactor = profitFactor
        SharpeRatio = sharpeRatio
        SortinoRatio = sortinoRatio
    }