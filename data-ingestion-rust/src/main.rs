// data-ingestion-rust/src/main.rs
mod config;
mod error;
mod types;
mod market_stream;
mod grpc_client;
mod order_exec;

use config::Settings;
use error::{DataIngestionError, Result};
use log::{info, error, LevelFilter};
use std::path::PathBuf;
use tokio::sync::mpsc;

#[tokio::main]
async fn main() -> Result<()> {
    // --- Configuration ---
    // Determine config directory relative to executable or use environment variable
    let config_dir = std::env::var("CONFIG_DIR")
        .map(PathBuf::from)
        .unwrap_or_else(|_| {
            // Default to ./config relative to executable if CARGO_MANIFEST_DIR is available (dev)
            // or relative to current dir otherwise.
            let base = std::env::var("CARGO_MANIFEST_DIR").map(PathBuf::from);
            base.map(|p| p.join("config")).unwrap_or_else(|_| PathBuf::from("./config"))
        });

    // Determine run mode (e.g., "development", "production")
    let run_mode = std::env::var("RUN_MODE").unwrap_or_else(|_| "development".into());

    let settings = Settings::new(&config_dir, &run_mode)
        .map_err(|e| {
            // Provide more context on config loading failure
            eprintln!("FATAL: Failed to load configuration from directory '{:?}' for mode '{}': {}", config_dir, run_mode, e);
            eprintln!("Ensure configuration files (e.g., default.toml, local.toml) exist and are correctly formatted.");
            eprintln!("Check environment variables (APP_...) for overrides.");
            e
        })?;


    // --- Logging ---
    let log_level = settings.log_level.parse::<LevelFilter>().unwrap_or(LevelFilter::Info);
    env_logger::Builder::new()
        .filter_level(log_level) // Set log level from config
        .filter_module("tungstenite", LevelFilter::Info) // Reduce verbosity from deps
        .filter_module("tokio_tungstenite", LevelFilter::Info)
        .filter_module("reqwest", LevelFilter::Info)
        .filter_module("hyper", LevelFilter::Info)
        .filter_module("tonic", LevelFilter::Info)
        .init();

    info!("--------------------------------------------------");
    info!(" Starting Data Ingestion Service");
    info!(" Run Mode: {}", run_mode);
    info!(" Log Level: {}", settings.log_level);
    info!(" Alpaca Paper Mode: {}", settings.alpaca.paper);
    info!(" Alpaca Data Stream URL: {}", settings.alpaca.data_stream_url);
    info!(" Indicator Engine gRPC URL: {}", settings.grpc.indicator_engine_url);
    info!(" Order Receiver gRPC Bind Address: {}", settings.grpc.order_receiver_bind_address);
    info!(" Subscribing to Symbols: {:?}", settings.symbols);
    info!("--------------------------------------------------");


    // --- Channels ---
    // Channel for market data (from WebSocket task to gRPC sender task)
    let (market_data_tx, market_data_rx) = mpsc::channel(settings.channel_buffer_size);


    // --- Spawn Tasks ---
    let market_stream_handle = tokio::spawn(
        market_stream::run_market_stream_task(settings.clone(), market_data_tx)
    );

    let grpc_sender_handle = tokio::spawn(
        grpc_client::run_grpc_sender_task(settings.clone(), market_data_rx)
    );

    let grpc_server_handle = tokio::spawn(
        order_exec::run_grpc_server_task(settings.clone())
    );


    // --- Monitor Tasks ---
    // Use select! to wait for any task to exit (potentially due to error)
    tokio::select! {
        res = market_stream_handle => {
            match res {
                Ok(Ok(())) => error!("Market stream task exited unexpectedly without error."),
                Ok(Err(e)) => error!("Market stream task failed: {}", e),
                Err(e) => error!("Failed to join market stream task: {}", e),
            }
        },
        res = grpc_sender_handle => {
             match res {
                Ok(Ok(())) => error!("gRPC sender task exited unexpectedly without error."),
                Ok(Err(e)) => error!("gRPC sender task failed: {}", e),
                Err(e) => error!("Failed to join gRPC sender task: {}", e),
            }
        },
         res = grpc_server_handle => {
             match res {
                Ok(Ok(())) => error!("gRPC server task exited unexpectedly without error."),
                Ok(Err(e)) => error!("gRPC server task failed: {}", e),
                Err(e) => error!("Failed to join gRPC server task: {}", e),
            }
        },
    }

    // If any task finishes (especially with an error), the select! macro completes.
    // You might want more sophisticated shutdown logic here (e.g., signaling other tasks to stop).
    error!("A critical task has exited. Shutting down service.");

    // Explicitly abort other tasks for a quicker shutdown (optional)
    // market_stream_handle.abort();
    // grpc_sender_handle.abort();
    // grpc_server_handle.abort();


    // Return an error to indicate abnormal termination
    Err(DataIngestionError::Unknown("A critical task terminated.".to_string()))
}

