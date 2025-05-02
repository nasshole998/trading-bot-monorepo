use crate::Result; // Use the custom Result type
use serde::Deserialize;
use std::collections::HashMap;
use std::time::Duration;

#[derive(Debug, Deserialize, Clone)]
pub struct ExchangeConfig {
    pub api_key: String,
    pub secret_key: String,
    pub rest_api_url: String,
    pub websocket_market_data_url: String, // URL for barter-data WebSocket streams
    pub websocket_user_data_url: String, // Base URL for user data streams
    pub symbols: Vec<String>, // Symbols to subscribe to (e.g., "btc_usdt" for barter-data)
    pub data_types: Vec<String>, // "trade", "quote", "order_book"
    pub recv_window_ms: u64, // Binance specific: time in ms for signed requests validity
}

#[derive(Debug, Deserialize, Clone)]
pub struct GrpcConfig {
    pub market_data_server_addr: String, // Address of the Indicator/ML engine gRPC server (for streaming market data TO)
    pub execution_server_addr: String, // Address of the Infrastructure gRPC server (for streaming order updates TO)
    pub listen_addr: String, // Address for this service's own gRPC server (for receiving execution requests FROM)
}

#[derive(Debug, Deserialize, Clone)]
pub struct AppConfig {
    pub exchanges: HashMap<String, ExchangeConfig>, // Expecting only "binance" here for now
    pub grpc: GrpcConfig,
     pub health_check_listen_addr: String, // Address for the HTTP health check server
    // Add other global settings like logging level, monitoring endpoints, etc.
}

impl AppConfig {
    /// Loads configuration from a file (config.yaml, config.json, etc.)
    /// and environment variables.
    pub fn load() -> Result<Self> {
        let settings = config::Config::builder()
            // Add configuration sources in order of decreasing priority
            .add_source(config::File::with_name("config/settings").required(false))
            .add_source(config::Environment::with_prefix("TRADING_BOT")) // Load from TRADING_BOT_EXCHANGES_BINANCE_API_KEY etc.
            .build()?;

        settings.try_deserialize().map_err(Into::into) // Convert config::ConfigError to DataIngestionError
    }
}

// Example of a default settings file structure (config/settings.yaml)
/*
exchanges:
  binance:
    api_key: "YOUR_BINANCE_API_KEY"
    secret_key: "YOUR_BINANCE_SECRET_KEY"
    rest_api_url: "https://api.binance.com/api/v3"
    websocket_market_data_url: "wss://stream.binance.com:9443/ws" # Barter-data default
    websocket_user_data_url: "wss://stream.binance.com:9443/ws/" # Base path for user data stream
    symbols: ["btc_usdt", "eth_btc"] # barter-data format (lowercase)
    data_types: ["trade", "quote", "order_book"] # Include order_book
    recv_window_ms: 5000 # Recommended value

grpc:
  market_data_server_addr: "http://indicator_engine:50051"
  execution_server_addr: "http://infrastructure:50052" # Address where Infra listens for order updates stream
  listen_addr: "[::1]:50053" # Address where this service listens for execution requests from Infra/Risk

health_check_listen_addr: "[::1]:8080" # Address for the HTTP health check server
*/