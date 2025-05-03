// src/Data/HistoricalDataLoader.fs
module BacktesterFsharp.Data.HistoricalDataLoader

open System
open System.IO
open System.Globalization
open CsvHelper
open CsvHelper.Configuration
open BacktesterFsharp.Simulation.SimulationTypes // Needs DataPoint
open System.Text.Json // For potential future JSON data loading

// Define a record type to map CSV columns to
// Use DateTime instead of DateTimeOffset if CSV stores local times, convert to UTC later
type CsvDataPoint = {
    timestamp: DateTime
    price: string // Read as string first for robust parsing
    volume: string option // Read as string option
}

// Configuration for CsvHelper - use InvariantCulture for parsing numbers
let csvConfig = new CsvConfiguration(CultureInfo.InvariantCulture)

let loadHistory (symbol: string) (startTime: DateTimeOffset) (endTime: DateTimeOffset) (dataDir: string) : Result<DataPoint list, string> =
    let filePath = Path.Combine(dataDir, sprintf "%s.csv" symbol)
    printfn "Attempting to load historical data from %s for %s (%A to %A)" filePath symbol startTime endTime

    if not (File.Exists(filePath)) then
        Error (sprintf "Historical data file not found for %s at %s" symbol filePath)
    else
        try
            use reader = new StreamReader(filePath)
            use csv = new CsvReader(reader, csvConfig)

            csv.GetRecords<CsvDataPoint>()
            |> Seq.filter (fun dp ->
                // Convert DateTime to DateTimeOffset (assuming UTC or specify source timezone if known)
                let timestamp = DateTimeOffset.SpecifyUtc(dp.timestamp, TimeSpan.Zero) // Assume timestamps in CSV are UTC
                timestamp >= startTime && timestamp <= endTime)
            |> Seq.map (fun dp -> // Parse price and volume strings into decimals
                match decimal.TryParse(dp.price, CultureInfo.InvariantCulture) with
                | true, priceDecimal ->
                    let volumeDecimal = dp.volume
                                        |> Option.bind (fun v -> decimal.TryParse(v, CultureInfo.InvariantCulture) |> Option.ofTuple)
                                        |> Option.defaultValue 0.0m // Use 0 if volume is missing or unparseable
                    Ok { Timestamp = DateTimeOffset.SpecifyUtc(dp.timestamp, TimeSpan.Zero)
                         Price = priceDecimal
                         Volume = volumeDecimal
                         Symbol = symbol }
                | false, _ -> Error (sprintf "Failed to parse price '%s' for symbol '%s' at %A" dp.price symbol dp.timestamp)
            )
            |> Seq.collect (function Ok dp -> [dp] | Error msg -> printfn "[DataLoader] Warning: %s" msg; []) // Filter out errors, log warnings
            |> Seq.sortBy (fun dp -> dp.Timestamp) // Ensure data is sorted by timestamp
            |> List.ofSeq // Convert to F# list
            |> fun data ->
                if List.isEmpty data then
                    Error (sprintf "No valid historical data points loaded for '%s' in the specified time range." symbol)
                else
                     printfn "Successfully loaded and parsed %d historical data points for %s." data.Length symbol
                     Ok data

        with
        | ex ->
            Error (sprintf "Error reading historical data from %s: %s" filePath ex.Message)