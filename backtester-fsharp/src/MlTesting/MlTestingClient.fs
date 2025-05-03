// src/MlTesting/MlTestingClient.fs
module BacktesterFsharp.MlTesting.MlTestingClient

open System
open System.Threading
open System.Threading.Tasks
open Grpc.Core
open Grpc.Net.Client
open Google.Protobuf.WellKnownTypes // For Timestamp
open MlTestingPb // Generated ML Testing protobuf types
open MlPredictionPb // Generated ML Prediction protobuf types (for PredictionValue)
open System.Collections.Generic // For Dictionary

// Helper function to create a gRPC channel with retry logic
let rec createChannelWithRetry (address: string) (attempt: int) : Async<GrpcChannel> =
    let delay = min (TimeSpan.FromSeconds(5.0 * (pown 2.0 attempt))) (TimeSpan.FromMinutes 1.0) // Exponential backoff up to 1 min
    printfn "Attempt %d: Creating gRPC channel to %s (retrying in %A)" attempt address delay
    async {
        try
            // Assume insecure for now
            use cts = new CancellationTokenSource(TimeSpan.FromSeconds 10.0) // Timeout for channel creation
            let channel = GrpcChannel.ForAddress(address)
            // Optional: Add a quick check if the channel is connected if possible
            // (Grpc.Net.Client doesn't expose connection state easily)
            printfn "Successfully created gRPC channel to %s" address
            return channel
        with
        | ex ->
            printfn "Failed to create gRPC channel to %s: %s. Retrying..." address ex.Message
            do! Async.Sleep (int delay.TotalMilliseconds)
            return! createChannelWithRetry address (attempt + 1)
    }

// Create a channel instance (asynchronously)
let mlTestingChannelAsync =
    // Get address from config (needs to be added to settings.yml and Config.fs)
    // For now, hardcode or get from a hypothetical config value
    // Let's assume a config value `mlTestingAddress` exists
    let mlTestingAddress = "localhost:50055" // Placeholder address
    createChannelWithRetry mlTestingAddress 1
    |> Async.StartAsTask // Start the async task immediately

// Get historical predictions for a specific model, symbol, and time range
let getHistoricalPredictions (mlModelId: string) (mlModelVersion: string) (symbol: string) (startTime: Timestamp) (endTime: Timestamp) : Async<list<PredictionValue>> =
    async {
        printfn "Requesting historical predictions for model '%s' version '%s' on '%s' (%A to %A)"
                mlModelId mlModelVersion symbol startTime endTime

        let! channel = mlTestingChannelAsync // Wait for the channel task to complete

        let client = new MlTestingService.MlTestingServiceClient(channel)

        let request = new HistoricalPredictionsRequest(
            MlModelId = mlModelId,
            MlModelVersion = mlModelVersion,
            Symbol = symbol,
            StartTime = startTime,
            EndTime = endTime
            // Add other required fields like RequiredIndicatorNames, RequiredPredictionNames if known
        )

        try
            use call = client.GetHistoricalPredictions(request)
            let! predictions = call.ResponseStream.ReadAllAsync() |> Async.AwaitTask // Read all streaming results
            printfn "Received %d historical predictions." predictions.Length
            return predictions |> List.ofSeq // Convert to F# list
        with
        | ex ->
            printfn "Error getting historical predictions: %s" ex.Message
            // Handle specific gRPC errors if needed
            return [] // Return empty list on error
    }