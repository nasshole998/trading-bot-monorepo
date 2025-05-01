// data-ingestion-rust/src/error.rs
use thiserror::Error;
use tokio_tungstenite::tungstenite;

#[derive(Error, Debug)]
pub enum DataIngestionError {
    #[error("Configuration error: {0}")]
    Config(#[from] config::ConfigError),

    #[error("I/O error: {0}")]
    Io(#[from] std::io::Error),

    #[error("WebSocket connection error: {0}")]
    WebSocket(#[from] tungstenite::Error),

    #[error("WebSocket message processing error: {0}")]
    WebSocketMessage(String),

    #[error("URL parsing error: {0}")]
    UrlParse(#[from] url::ParseError),

    #[error("JSON serialization/deserialization error: {0}")]
    Json(#[from] serde_json::Error),

    #[error("Authentication failed: {0}")]
    Authentication(String),

    #[error("Subscription failed: {0}")]
    Subscription(String),

    #[error("Channel send error: {0}")]
    ChannelSend(String), // Wrap specific channel errors

    #[error("gRPC transport error: {0}")]
    GrpcTransport(#[from] tonic::transport::Error),

    #[error("gRPC service error: {0}")]
    GrpcStatus(#[from] tonic::Status),

    #[error("HTTP request error: {0}")]
    HttpRequest(#[from] reqwest::Error),

    #[error("Alpaca API error: {status} - {message}")]
    AlpacaApi { status: u16, message: String },

    #[error("Order execution failed: {0}")]
    OrderExecution(String),

    #[error("Initialization error: {0}")]
    Initialization(String),

    #[error("Task join error: {0}")]
    TaskJoin(#[from] tokio::task::JoinError),

    #[error("Missing configuration value: {0}")]
    MissingConfig(String),

    #[error("Invalid configuration value: {0}")]
    InvalidConfig(String),

    #[error("Protobuf encoding/decoding error: {0}")]
    Prost(String), // Wrap prost errors if they occur outside gRPC context

    // Add other specific error types as needed
    #[error("Unknown error: {0}")]
    Unknown(String),
}

// Implement conversion from prost encode/decode errors if needed directly
impl From<prost::EncodeError> for DataIngestionError {
    fn from(err: prost::EncodeError) -> Self {
        DataIngestionError::Prost(format!("Protobuf encode error: {}", err))
    }
}

impl From<prost::DecodeError> for DataIngestionError {
    fn from(err: prost::DecodeError) -> Self {
        DataIngestionError::Prost(format!("Protobuf decode error: {}", err))
    }
}

// Helper to convert generic channel send errors
impl<T> From<tokio::sync::mpsc::error::SendError<T>> for DataIngestionError {
    fn from(err: tokio::sync::mpsc::error::SendError<T>) -> Self {
        DataIngestionError::ChannelSend(format!("MPSC channel send error: {}", err))
    }
}

// Add similar From implementations for other channel types if used (e.g., crossbeam)

pub type Result<T> = std::result::Result<T, DataIngestionError>;

