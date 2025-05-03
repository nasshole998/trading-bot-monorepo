// src/Program.fs
module BacktesterFsharp.Program

open Grpc.Net.Server
open Microsoft.Extensions.Hosting
open BacktesterFsharp.Config // Needs config for address
open BacktesterFsharp.BacktesterService // Needs the service implementation

[<EntryPoint>]
let main argv =
    printfn "Starting F# Backtester service..."

    let listenAddress = Config.grpcListenAddress
    printfn "gRPC server listening on %s" listenAddress

    Host.CreateDefaultBuilder(argv)
        .ConfigureServices(fun services ->
            services.AddGrpc() |> ignore // Add gRPC services
        )
        .ConfigureWebHostDefaults(fun webBuilder ->
            webBuilder.ConfigureKestrel(fun kestrelOptions ->
                // Configure Kestrel to listen for gRPC on the specified address
                kestrelOptions.ListenLocalhost(System.Int32.Parse(listenAddress.Split(':')[1]), fun listenOptions ->
                    listenOptions.Protocols = Microsoft.AspNetCore.Server.Kestrel.Core.HttpProtocols.Http2 // gRPC requires HTTP/2
                )
            )
            .Configure(fun app ->
                app.UseRouting() // Use routing middleware
                app.UseEndpoints(fun endpoints ->
                    // Map the BacktesterService endpoint
                    endpoints.MapGrpcService<BacktesterServiceImpl>() |> ignore
                )
            ) |> ignore // Configure the application pipeline
        )
        .Build() // Build the host
        .Run() // Run the host (starts the Kestrel server)

    printfn "F# Backtester service stopped."
    0 // Exit code