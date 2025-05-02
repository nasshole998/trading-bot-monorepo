// This file can be used for common modules or re-exporting
// components if this crate were a library. For this example,
// we'll keep main logic in main.rs and other specific files.

pub mod config;
pub mod connector;
pub mod market_stream;
pub mod order_exec;
pub mod user_data_stream; // New module for user data stream
pub mod utils;

// Define custom error type
#[derive(thiserror::Error, Debug)]
pub enum DataIngestionError {
    #[error("Configuration error: {0}")]
    ConfigError(#[from] config::ConfigError),
    #[error("Exchange connector error: {0}")]
    ConnectorError(#[from] anyhow::Error), // Use anyhow for errors originating from connector implementations
    #[error("Market data stream error: {0}")]
    MarketStreamError(#[from] anyhow::Error),
    #[error("User data stream error: {0}")]
    UserDataStreamError(#[from] anyhow::Error),
    #[error("gRPC transport error: {0}")]
    TransportError(#[from] tonic::transport::Error),
     #[error("gRPC request error: {0}")]
    GrpcStatus(#[from] tonic::Status),
    #[error("Serialization/Deserialization error: {0}")]
    SerdeError(#[from] serde_json::Error),
    #[error("WebSocket error: {0}")]
    WebSocketError(#[from] tokio_tungstenite::tungstenite::Error),
    #[error("URL parse error: {0}")]
    UrlParseError(#[from] url::ParseError),
     #[error("Decimal parse error: {0}")]
    DecimalParseError(#[from] rust_decimal::Error),
    #[error("Join error: {0}")]
    JoinError(#[from] tokio::task::JoinError),
    #[error("HTTP server error: {0}")]
    HttpServerError(#[from] warp::Error),
    #[error("Channel send error (market data): {0}")]
    MarketDataChannelSendError(#[from] tokio::sync::mpsc::SendError<market_data::MarketDataEvent>),
     #[error("Channel send error (order update): {0}")]
    OrderUpdateChannelSendError(#[from] tokio::sync::mpsc::SendError<execution::OrderUpdate>),
     #[error("Channel try_send error (order update): {0}")]
    OrderUpdateChannelTrySendError(#[from] tokio::sync::mpsc::TrySendError<execution::OrderUpdate>),


    #[error("Other error: {0}")]
    Other(#[from] Box<dyn std::error::Error + Send + Sync>),
}

// Use the custom error type as the Result alias within this crate
pub type Result<T> = std::result::Result<T, DataIngestionError>;


// Include auto-generated gRPC code
// The build.rs puts these directly in src/
#[allow(clippy::all)]
#[allow(unused)]
pub mod market_data {
    tonic::include_proto!("market_data");
}

#[allow(clippy::all)]
#[allow(unused)]
pub mod execution {
    tonic::include_proto!("execution");
}