// src/main.rs
mod config;
mod risk_state;
mod risk_rules;
mod risk_manager;
mod execution_client;
mod market_data_client;
mod account_state_client;
mod data_stream_handlers;
mod strategy_engine_client; // **NEW:** Strategy Engine client module


// Include the generated protobuf code
#[allow(clippy::all)] // Ignore clippy warnings in generated code
pub mod proto {
    include!(concat!(env!("OUT_DIR"), "/proto/risk_management.rs"));
    include!(concat!(env!("OUT_DIR"), "/proto/market_data.rs"));
    include!(concat!(env!("OUT_DIR"), "/proto/account_state.rs"));
    include!(concat!(env!("OUT_DIR"), "/proto/strategy_engine.rs")); // **NEW:** Include strategy_engine proto
    pub mod google {
        pub mod protobuf {
             include!(concat!(env!("OUT_DIR"), "/proto/google.protobuf.rs"));
        }
    }
}


use tonic::transport::{Server, Channel, Endpoint};
use risk_manager::RiskManager;
use risk_state::RiskState;
use std::sync::Arc;
use parking_lot::RwLock;
use rust_decimal_macros::dec;
use execution_client::ExecutionClient;
use market_data_client::MarketDataClient;
use account_state_client::AccountStateClient;
use strategy_engine_client::StrategyEngineClient; // **NEW:** Strategy Engine Client struct
use tracing_subscriber::fmt;
use tokio::task;


#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    // Initialize logging
    fmt::init();
    tracing::info!("Risk Management service starting...");

    // Load configuration
    let config_arc = config::GLOBAL_CONFIG.clone();
    let config_path = std::path::Path::new("config").join("settings.yaml");
     if let Err(e) = config_arc.load_from_file(config_path.to_str().unwrap_or("config/settings.yaml")) {
         tracing::error!("Failed to load configuration: {}", e);
         panic!("Failed to load configuration: {}", e);
     }

    let config_guard = config_arc.get_config().expect("Config should be loaded by now").expect("Config should be Some");
    let grpc_listen_addr = config_guard.grpc.listen_address.parse()?;
    let data_service_addr = config_guard.grpc.data_service_address.clone();
    let strategy_engine_addr = config_guard.grpc.strategy_engine_address.clone(); // **NEW:** Get SE address
    let default_account_id = config_guard.default_account_id.clone();
    let initial_equity = dec!(100_000.0); // Placeholder initial equity (should ideally come from account state on startup)

    // Initialize shared risk state
    let risk_state: risk_state::SharedRiskState = Arc::new(RwLock::new(RiskState::new(default_account_id.clone(), initial_equity)));
    tracing::info!("Risk state initialized for account '{}' with placeholder equity: {}", default_account_id, initial_equity);


    // --- Connect to External Services ---
    // Connect to the Data Service (Execution, AccountState, MarketData)
    let data_channel = Endpoint::from_shared(data_service_addr)?
        .connect()
        .await?;
    tracing::info!("Connected to Data Service.");

    // Connect to the Strategy Engine Service (for reporting decisions)
     let strategy_engine_channel = Endpoint::from_shared(strategy_engine_addr)?
        .connect()
        .await?;
     tracing::info!("Connected to Strategy Engine Service.");


    // --- Create Clients ---
    let execution_client = ExecutionClient::connect(config_guard.grpc.data_service_address.clone()).await?;
    let account_state_client = AccountStateClient::connect(config_guard.grpc.data_service_address.clone()).await?; // AccountState and Execution Reports streams
    let market_data_client = MarketDataClient::connect(config_guard.grpc.data_service_address.clone()).await?;
    let strategy_engine_client = StrategyEngineClient::connect(config_guard.grpc.strategy_engine_address.clone()).await?; // **NEW:** Strategy Engine Client


    // --- Clone shared state and clients for background tasks ---
    let risk_state_clone_acc = risk_state.clone();
    let account_state_client_clone_acc = account_state_client.clone();
    let config_clone_acc = config_guard.clone();

    let risk_state_clone_exec = risk_state.clone();
    let account_state_client_clone_exec = account_state_client.clone(); // Use the same client for both streams
    let config_clone_exec = config_guard.clone();

    let risk_state_clone_md = risk_state.clone();
    let market_data_client_clone_md = market_data_client.clone();
    let config_clone_md = config_guard.clone();


    // --- Spawn Background Tasks to Handle Data Streams ---
    let account_updates_task = task::spawn(async move {
        data_stream_handlers::handle_account_updates_stream(config_clone_acc, account_state_client_clone_acc, risk_state_clone_acc).await;
    });

    let execution_reports_task = task::spawn(async move { // **NEW:** Spawn Execution Reports task
         data_stream_handlers::handle_execution_reports_stream(config_clone_exec, account_state_client_clone_exec, risk_state_clone_exec).await;
    });

     let market_data_task = task::spawn(async move {
         data_stream_handlers::handle_market_data_stream(config_clone_md, market_data_client_clone_md, risk_state_clone_md).await;
     });


    // Create the RiskManager service instance
    // Pass clones of shared state, config, and clients needed by the service logic (Evaluation RPC)
    let risk_manager_service = RiskManager::new(risk_state.clone(), config_guard.clone(), execution_client.clone(), strategy_engine_client.clone()); // **NEW:** Pass StrategyEngineClient
    let risk_manager_server = proto::risk_management::risk_management_service_server::RiskManagementServiceServer::new(risk_manager_service);
    tracing::info!("Risk Management service initialized.");


    tracing::info!("Risk Management service listening on {}", grpc_listen_addr);

    // Start the gRPC server (main task)
    tracing::info!("Starting gRPC server...");
    let server_task = Server::builder()
        .add_service(risk_manager_server)
        .serve(grpc_listen_addr);

     // Await the server task to keep the service running
    server_task.await?;


    // Optional: Join background tasks for graceful shutdown (more advanced)
    // let _ = tokio::join!(account_updates_task, execution_reports_task, market_data_task, execution_reports_task);


    tracing::info!("Risk Management service shut down.");

    Ok(())
}