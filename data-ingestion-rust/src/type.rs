// data-ingestion-rust/src/types.rs
use serde::{Deserialize, Serialize};
use chrono::{DateTime, Utc};
use uuid::Uuid;

// --- Internal Representations ---
// These structs represent the data after parsing from Alpaca
// and before sending via gRPC/messaging.

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TickData {
    pub symbol: String,
    pub timestamp: DateTime<Utc>,
    pub price: f64,
    pub size: i64,
    pub exchange: String,
    #[serde(default)]
    pub conditions: Vec<String>,
    pub tape: String,
    #[serde(default)] // Use default if missing
    pub trade_id: Option<i64>,
    // Add a unique ID for internal tracking/correlation if needed
    #[serde(default = "Uuid::new_v4")]
    pub internal_id: Uuid,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct QuoteData {
    pub symbol: String,
    pub timestamp: DateTime<Utc>,
    pub bid_price: f64,
    pub bid_size: i64,
    pub bid_exchange: String,
    pub ask_price: f64,
    pub ask_size: i64,
    pub ask_exchange: String,
    #[serde(default)]
    pub conditions: Vec<String>,
    pub tape: String,
    #[serde(default = "Uuid::new_v4")]
    pub internal_id: Uuid,
}

// Enum to represent different types of market data received
#[derive(Debug, Clone)]
pub enum MarketData {
    Tick(TickData),
    Quote(QuoteData),
    // Add other types like Bars, OrderBookUpdates if needed
}


// --- Alpaca Specific Deserialization Structs ---
// These structs match the JSON structure received from Alpaca WebSocket streams.
// Naming convention uses camelCase matching the API JSON fields.

#[derive(Debug, Deserialize, Clone)]
#[serde(tag = "T", rename_all = "camelCase")] // "T" is the type discriminator in Alpaca messages
pub enum AlpacaStreamMessage {
    #[serde(rename = "t")]
    Trade(AlpacaTrade),
    #[serde(rename = "q")]
    Quote(AlpacaQuote),
    #[serde(rename = "b")]
    Bar(AlpacaBar), // Example if subscribing to bars
    #[serde(rename = "success", alias = "error")] // Handle auth/sub responses
    Control(AlpacaControlMessage),
    // Add other message types: dailyBar, status, luld, etc.
    #[serde(other)] // Catchall for unhandled message types
    Unknown,
}

#[derive(Debug, Deserialize, Clone)]
pub struct AlpacaTrade {
    #[serde(rename = "sym")]
    pub symbol: String,
    #[serde(rename = "p")]
    pub price: f64,
    #[serde(rename = "s")]
    pub size: i64,
    #[serde(rename = "t")]
    pub timestamp: DateTime<Utc>,
    #[serde(rename = "x")]
    pub exchange: String,
    #[serde(rename = "c", default)]
    pub conditions: Vec<String>,
    #[serde(rename = "z")]
    pub tape: String,
    #[serde(rename = "i", default)]
    pub trade_id: Option<i64>, // Trade ID might not always be present
}

#[derive(Debug, Deserialize, Clone)]
pub struct AlpacaQuote {
    #[serde(rename = "sym")]
    pub symbol: String,
    #[serde(rename = "bp")]
    pub bid_price: f64,
    #[serde(rename = "bs")]
    pub bid_size: i64,
    #[serde(rename = "bx")]
    pub bid_exchange: String,
    #[serde(rename = "ap")]
    pub ask_price: f64,
    #[serde(rename = "as")]
    pub ask_size: i64,
    #[serde(rename = "ax")]
    pub ask_exchange: String,
    #[serde(rename = "t")]
    pub timestamp: DateTime<Utc>,
    #[serde(rename = "c", default)]
    pub conditions: Vec<String>,
    #[serde(rename = "z")]
    pub tape: String,
}

#[derive(Debug, Deserialize, Clone)]
pub struct AlpacaBar {
    #[serde(rename = "sym")]
    pub symbol: String,
    #[serde(rename = "o")]
    pub open: f64,
    #[serde(rename = "h")]
    pub high: f64,
    #[serde(rename = "l")]
    pub low: f64,
    #[serde(rename = "c")]
    pub close: f64,
    #[serde(rename = "v")]
    pub volume: i64,
    #[serde(rename = "t")]
    pub timestamp: DateTime<Utc>,
    #[serde(rename = "n", default)]
    pub trade_count: Option<i64>,
    #[serde(rename = "vw", default)]
    pub vwap: Option<f64>,
}


#[derive(Debug, Deserialize, Clone)]
pub struct AlpacaControlMessage {
    pub msg: String,
    #[serde(default)]
    pub code: Option<i64>, // Present in error messages
}

// --- Order Execution Related Structs ---
// These might mirror the gRPC definitions or be used with reqwest

#[derive(Debug, Serialize, Deserialize, Clone)]
pub struct AlpacaOrderRequest {
    pub symbol: String,
    pub qty: Option<String>, // Use string for fractional quantities
    pub notional: Option<String>, // Alternative to qty for dollar amount orders
    pub side: String, // "buy" or "sell"
    #[serde(rename = "type")]
    pub order_type: String, // "market", "limit", "stop", "stop_limit", "trailing_stop"
    pub time_in_force: String, // "day", "gtc", "opg", "cls", "ioc", "fok"
    pub limit_price: Option<String>,
    pub stop_price: Option<String>,
    pub trail_price: Option<String>, // For trailing stop price offset
    pub trail_percent: Option<String>, // For trailing stop percent offset
    pub extended_hours: Option<bool>,
    pub client_order_id: Option<String>, // Unique ID from client (max 48 chars)
    pub order_class: Option<String>, // e.g., "simple", "bracket", "oco", "oto"
    // Add fields for take_profit, stop_loss if using bracket orders etc.
}

#[derive(Debug, Serialize, Deserialize, Clone)]
pub struct AlpacaOrderResponse {
    pub id: Uuid, // Alpaca's order ID
    pub client_order_id: String,
    pub created_at: DateTime<Utc>,
    pub updated_at: Option<DateTime<Utc>>,
    pub submitted_at: Option<DateTime<Utc>>,
    pub filled_at: Option<DateTime<Utc>>,
    pub expired_at: Option<DateTime<Utc>>,
    pub canceled_at: Option<DateTime<Utc>>,
    pub failed_at: Option<DateTime<Utc>>,
    pub replaced_at: Option<DateTime<Utc>>,
    pub replaces: Option<Uuid>,
    pub asset_id: Uuid,
    pub symbol: String,
    pub asset_class: String,
    pub qty: Option<String>,
    pub filled_qty: Option<String>,
    pub notional: Option<String>, // Filled notional value might be returned
    #[serde(rename = "type")]
    pub order_type: String,
    pub side: String,
    pub time_in_force: String,
    pub limit_price: Option<String>,
    pub stop_price: Option<String>,
    pub filled_avg_price: Option<String>,
    pub status: String, // e.g. "new", "filled", "canceled", "rejected"
    pub extended_hours: bool,
    pub legs: Option<Vec<AlpacaOrderResponse>>, // For complex orders (OCO, bracket)
    pub trail_percent: Option<String>,
    pub trail_price: Option<String>,
    pub hwm: Option<String>, // High water mark for trailing stops
    // Add commission, etc. if needed
}

#[derive(Debug, Serialize, Deserialize, Clone)]
pub struct AlpacaApiError {
    pub code: i32,
    pub message: String,
}

