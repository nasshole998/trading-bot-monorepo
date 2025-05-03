// src/Config.fs
module BacktesterFsharp.Config

open FSharp.Configuration
open System.IO

// Define the structure of the configuration YAML file
type AppSettings = YamlConfig<"settings.yml">

// Load configuration from the file
let settings =
    let configFilePath = Path.Combine(__SOURCE_DIRECTORY__, "..", "..", "config", "settings.yml")
    // Ensure the file path is correct relative to the compiled assembly or source file location
    // In a built application, settings.yml might be copied to the output directory
    // For simplicity here, we assume it's relative to the source file during development
    // or copied correctly during deployment.
    if File.Exists(configFilePath) then
        AppSettings(configFilePath)
    else
        failwithf "Configuration file not found at %s" configFilePath

// Provide access to configuration values
let grpcListenAddress = settings.Grpc.ListenAddress
let historyDataPath = settings.Data.HistoryDataPath
let defaultInitialCapital = settings.Simulation.DefaultInitialCapital
let makerFee = settings.Simulation.MakerFee
let takerFee = settings.Simulation.TakerFee
let slippagePercent = settings.Simulation.SlippagePercent
let riskFreeRate = settings.Metrics.RiskFreeRate