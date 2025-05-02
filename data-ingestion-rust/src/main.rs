use tracing::{info, error, Level};
use tracing_subscriber::FmtSubscriber;
use crate::Result; // Use the custom Result type
use tokio::signal::ctrl_c;
use tokio::sync::mpsc;
use tokio_util::sync::CancellationToken; // For graceful shutdown
use futures::FutureExt; // For .fuse() and .join_all
use std::future::Future; // For Future trait
use futures::stream::FuturesUnordered; // To manage spawned tasks


mod config;
mod connector;
mod market_stream;
mod order_exec;
mod user_data_stream;
mod utils;
mod health_check; // New module

// Include auto-generated gRPC code via lib.rs re-exports
use data_ingestion_rust::{market_data, execution};


const MARKET_DATA_CHANNEL_SIZE: usize = 4096; // Increased buffer for market data volume
const ORDER_UPDATE_CHANNEL_SIZE: usize = 1024; // Buffer size for order updates

#[tokio::main]
async fn main() -> Result<()> {
    // --- Setup Logging ---
    let subscriber = FmtSubscriber::builder()
        .with_max_level(Level::INFO) // Use INFO for standard operation, DEBUG/TRACE for development
        .finish();
    tracing::subscriber::set_global_default(subscriber)
        .map_err(|e| anyhow!("Failed to set logging subscriber: {}", e).into())?;


    info!("Starting Data Ingestion & Execution Service...");

    // --- Load Configuration ---
    let config = match config::AppConfig::load() {
        Ok(cfg) => {
            info!("Configuration loaded successfully.");
            debug!("Config: {:?}", cfg);
            cfg
        },
        Err(e) => {
            error!("Failed to load configuration: {:?}", e);
            return Err(e);
        }
    };

    // --- Initialize Connector Factory ---
    let connector_factory = match connector::ConnectorFactory::new(&config.exchanges) {
        Ok(factory) => {
            info!("Connector factory initialized.");
            factory
        },
        Err(e) => {
             error!("Failed to initialize connector factory: {:?}", e);
             return Err(e);
        }
    };


    // --- Setup Internal Channels ---
    // Channel to send MARKET DATA from ingestion task TO the gRPC sender task
    let (market_data_tx, market_data_rx) = mpsc::channel::<market_data::MarketDataEvent>(MARKET_DATA_CHANNEL_SIZE);

    // Channel to send ORDER UPDATES from execution server/user data stream tasks TO the gRPC sender task
    let (order_update_tx, order_update_rx) = mpsc::channel::<execution::OrderUpdate>(ORDER_UPDATE_CHANNEL_SIZE);


    // --- Setup Graceful Shutdown ---
    let cancel_token = CancellationToken::new();
    let mut shutdown_signal = ctrl_c().fuse(); // Create a future for Ctrl+C that can be polled


    // --- Spawn Tasks ---
    // Use FuturesUnordered to manage spawned tasks and join them later
    let mut tasks = FuturesUnordered::new();

    // Task 1: Market Data Ingestion Loop (Barter-Data -> MPSC)
    let ingestion_config = config.clone();
    let ingestion_cancel_token = cancel_token.clone();
    tasks.push(tokio::spawn(async move {
        let res = market_stream::start_market_data_ingestion_loop(ingestion_config, market_data_tx, ingestion_cancel_token).await;
        if let Err(e) = res {
            error!("Market data ingestion loop task failed: {:?}", e);
        }
        info!("Market data ingestion loop task stopped.");
    }).fuse());

    // Task 2: Market Data gRPC Sender Client (MPSC -> gRPC Stream TO Indicator/ML)
    let grpc_md_sender_config = config.clone();
     let grpc_md_sender_cancel_token = cancel_token.clone();
     tasks.push(tokio::spawn(async move {
        let res = market_stream::grpc_market_data_sender_task(grpc_md_sender_config.grpc.market_data_server_addr.clone(), market_data_rx, grpc_md_sender_cancel_token).await;
        if let Err(e) = res {
            error!("gRPC market data sender task failed: {:?}", e);
        }
        info!("gRPC market data sender task stopped.");
     }).fuse());


    // Task 3: Execution Request gRPC Server (Receives FROM Infra/Risk)
    // Uses the connector factory and order_update_tx channel.
    let execution_server_config = config.clone();
    let execution_server_connector_factory = connector_factory.clone(); // Clone Arc
    let execution_server_order_update_tx = order_update_tx.clone(); // Clone sender for server impl
     let execution_server_cancel_token = cancel_token.clone();
     tasks.push(tokio::spawn(async move {
        // The server uses serve_with_shutdown and listens on the cancel_token directly
        let res = order_exec::start_execution_grpc_server(execution_server_config, execution_server_connector_factory, execution_server_order_update_tx, execution_server_cancel_token).await;
        if let Err(e) = res {
            error!("Execution gRPC server task failed: {:?}", e);
        }
        info!("Execution gRPC server task stopped.");
    }).fuse());


    // Task 4: Order Update Streaming Client (MPSC -> gRPC Stream TO Infrastructure)
    let order_update_stream_config = config.clone();
     let order_update_stream_cancel_token = cancel_token.clone();
     tasks.push(tokio::spawn(async move {
         let res = order_exec::stream_order_updates_to_infrastructure_task(order_update_stream_config.grpc.execution_server_addr.clone(), order_update_rx, order_update_stream_cancel_token).await;
         if let Err(e) = res {
             error!("Order update streaming task failed: {:?}", e);
         }
          info!("Order update streaming task stopped.");
     }).fuse());


    // Task 5: Binance User Data Stream (WebSocket -> MPSC order_update_tx)
    let uds_config = config.clone();
    let uds_connector_factory = connector_factory.clone(); // Clone for UDS task
    let uds_order_update_tx = order_update_tx.clone(); // Clone sender for UDS task
    let uds_cancel_token = cancel_token.clone();
    tasks.push(tokio::spawn(async move {
        let res = user_data_stream::start_binance_user_data_stream_task(uds_config, uds_connector_factory, uds_order_update_tx, uds_cancel_token).await;
        if let Err(e) = res {
            error!("Binance User Data Stream task failed: {:?}", e);
        }
        info!("Binance User Data Stream task stopped.");
    }).fuse());

    // Task 6: Health Check Server (HTTP)
    let health_check_config = config.clone();
    let health_check_cancel_token = cancel_token.clone();
    tasks.push(tokio::spawn(async move {
        let res = health_check::start_health_check_server_task(health_check_config.health_check_listen_addr, health_check_cancel_token).await;
         if let Err(e) = res {
             error!("Health Check server task failed: {:?}", e);
         }
         info!("Health Check server task stopped.");
    }).fuse());


    // --- Wait for Shutdown Signal or Task Failure ---
    info!("Service is running. Press Ctrl+C to shut down.");

    tokio::select! {
        _ = shutdown_signal => {
            info!("Ctrl+C received. Initiating graceful shutdown...");
        }
        // Wait for any spawned task to complete (which might indicate a crash)
        // If a task finishes, it means something went wrong or it completed its work.
        // In a long-running service, unexpected completion usually means an error.
        Some(task_result) = tasks.next() => {
            match task_result {
                Ok(_) => info!("One of the service tasks completed unexpectedly. Initiating shutdown."),
                Err(e) => error!("One of the service tasks failed: {}. Initiating shutdown.", e),
            }
        }
    }

    // --- Perform Graceful Shutdown ---
    // Signal all tasks to cancel
    cancel_token.cancel();
    info!("Shutdown signal sent to tasks.");

    // Explicitly drop the sender halves of the channels.
    // This is important for the gRPC client streaming tasks (Task 2 and Task 4)
    // which are consuming from the receiver halves. When all senders are dropped,
    // the receivers will yield `None`, signaling the stream is finished.
    drop(market_data_tx); // Dropped the original tx from main
    drop(order_update_tx); // Dropped the original tx from main
    // Note: Cloned senders held by tasks (like ExecutionServiceImpl, UDS task)
    // will also need to be dropped when those tasks receive the cancel signal and exit their loops.
    // The `serve_with_shutdown` in the gRPC server and the `cancel_token.cancelled()` checks
    // in the other loops handle this implicitly as the tasks unwind and drop their local variables.

    // Wait for all spawned tasks to complete.
    // The tasks should now be shutting down because of the cancel_token or dropped channels.
    info!("Waiting for all tasks to finish...");
    while let Some(result) = tasks.next().await {
        match result {
            Ok(_) => trace!("A task finished."),
            Err(e) => error!("A task failed during shutdown: {}", e),
        }
    }


    info!("All tasks finished. Data Ingestion & Execution Service shut down completely.");

    Ok(())
}