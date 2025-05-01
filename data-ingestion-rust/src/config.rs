// data-ingestion-rust/src/config.rs
use serde::Deserialize;
use std::path::Path;
use config::{ConfigError, File, Environment};

#[derive(Debug, Deserialize, Clone)]
pub struct AlpacaConfig {
    pub api_key: String,
    pub secret_key: String,
    pub paper: bool, // Use paper trading endpoint?
    pub data_stream_url: String, // e.g., "wss://stream.data.alpaca.markets/v2/sip" or crypto url
    pub rest_api_url: String, // Auto-detected based on paper flag usually
}

#[derive(Debug, Deserialize, Clone)]
pub struct GrpcConfig {
    pub indicator_engine_url: String, // e.g., "http://indicator-engine-cpp:50051"
    pub order_receiver_bind_address: String, // e.g., "0.0.0.0:50052"
}

// Example for NATS, uncomment and adapt if using instead of gRPC for some parts
// #[derive(Debug, Deserialize, Clone)]
// pub struct NatsConfig {
//     pub url: String, // e.g., "nats://nats-server:4222"
//     pub market_data_subject_prefix: String, // e.g., "market_data"
//     pub order_request_subject: String, // e.g., "orders.requests"
//     pub order_update_subject: String, // e.g., "orders.updates"
// }

#[derive(Debug, Deserialize, Clone)]
pub struct Settings {
    pub alpaca: AlpacaConfig,
    pub grpc: GrpcConfig,
    // pub nats: Option<NatsConfig>, // Optional NATS config
    pub symbols: Vec<String>, // List of symbols to subscribe to
    pub log_level: String,
    #[serde(default = "default_reconnect_delay_ms")]
    pub reconnect_delay_ms: u64,
    #[serde(default = "default_buffer_size")]
    pub channel_buffer_size: usize,
}

fn default_reconnect_delay_ms() -> u64 { 5000 } // 5 seconds
fn default_buffer_size() -> usize { 1024 }

impl Settings {
    /// Loads configuration from files and environment variables.
    /// Looks for `config/default.toml`, `config/{run_mode}.toml`, and `config/local.toml`.
    /// Environment variables override file settings (e.g., `APP_ALPACA__API_KEY=...`).
    pub fn new(config_dir: &Path, run_mode: &str) -> Result<Self, ConfigError> {
        let default_path = config_dir.join("default.toml");
        let mode_path = config_dir.join(format!("{}.toml", run_mode));
        let local_path = config_dir.join("local.toml");

        let settings = config::Config::builder()
            // Start with default configuration file
            .add_source(File::from(default_path).required(true))
            // Add run mode specific configuration (e.g., development, production)
            .add_source(File::from(mode_path).required(false))
            // Add local configuration file (gitignored) for secrets/overrides
            .add_source(File::from(local_path).required(false))
            // Add settings from environment variables (with a prefix `APP_` and `__` separators)
            .add_source(Environment::with_prefix("APP").separator("__").try_parsing(true))
            .build()?;

        // Deserialize the configuration
        settings.try_deserialize()
    }

    /// Gets the correct Alpaca REST API base URL based on the paper trading flag.
    pub fn get_alpaca_rest_base_url(&self) -> String {
        if self.alpaca.paper {
            "https://paper-api.alpaca.markets".to_string()
        } else {
            "https://api.alpaca.markets".to_string()
        }
    }
}

// --- Example Configuration Files (Place in `data-ingestion-rust/config/`) ---

// File: config/default.toml
/*
[alpaca]
# api_key = "YOUR_DEFAULT_KEY_ID" # Better to use env vars or local.toml for secrets
# secret_key = "YOUR_DEFAULT_SECRET"
paper = true # Default to paper trading
data_stream_url = "wss://stream.data.alpaca.markets/v2/sip" # SIP for US stocks
# For Crypto: "wss://stream.data.alpaca.markets/v1beta3/crypto/us"

[grpc]
indicator_engine_url = "http://indicator-engine-cpp:50051" # Service name in Docker Compose
order_receiver_bind_address = "0.0.0.0:50052" # Listen on all interfaces inside container

# [nats] # Example NATS config
# url = "nats://nats:4222"
# market_data_subject_prefix = "market_data"
# order_request_subject = "orders.requests"
# order_update_subject = "orders.updates"


symbols = ["AAPL", "MSFT", "GOOG", "BTC/USD", "ETH/USD"] # Example symbols (mix of stock/crypto)
log_level = "info"
reconnect_delay_ms = 5000
channel_buffer_size = 1024
*/

// File: config/production.toml (Example override for production)
/*
[alpaca]
paper = false # Use live account in production
data_stream_url = "wss://stream.data.alpaca.markets/v2/sip" # Or crypto prod URL

[grpc]
indicator_engine_url = "http://prod-indicator-engine.internal:50051" # Example prod URL
order_receiver_bind_address = "0.0.0.0:50052"

log_level = "warn"
*/

// File: config/local.toml (Gitignored - for local secrets/overrides)
/*
[alpaca]
api_key = "YOUR_LOCAL_API_KEY_ID"
secret_key = "YOUR_LOCAL_SECRET_KEY"

# Override symbols for local testing
# symbols = ["SPY"]
*/
```
**Important:** Create the `config` directory inside `data-ingestion-rust` and add the example `.toml` files (especially `default.toml` and `local.toml`). Remember to add `config/local.toml` to your `.gitignore` file if not already covered by a broader ru