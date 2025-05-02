use async_trait::async_trait;
use reqwest::{Client, Method, StatusCode}; // Import Method and StatusCode
use rust_decimal::Decimal;
use crate::execution::{
    PlaceOrderRequest, PlaceOrderResponse, CancelOrderRequest, CancelOrderResponse,
    OrderStatusRequest, OrderStatusResponse, OrderStatus, OrderType, OrderSide,
};
use crate::Result; // Use the custom Result type
use anyhow::anyhow; // Still use anyhow for connector-specific errors before converting
use tracing::{info, error, debug, trace};
use std::sync::Arc;
use std::time::{SystemTime, Duration};
use hmac::{Hmac, Mac};
use sha2::Sha256;
use base64::Engine;
use governor::{RateLimiter, Identity, state::keyed::DefaultKeyedStateStore};
use governor::prelude::*;
use std::num::NonZeroU32;
use std::str::FromStr;
use crate::utils::parse_decimal; // Use our helper
use std::any::Any; // Required for AsAny trait


// Define the HMAC-SHA256 type
type HmacSha256 = Hmac<Sha256>;

// --- Binance Specific Structures (Mapping from Binance JSON) ---
// Define structures that match the expected JSON responses from Binance REST API.
// Include necessary fields for mapping to our Protobuf types.
#[derive(Debug, serde::Deserialize)]
#[serde(rename_all = "camelCase")] // Automatically convert camelCase JSON to snake_case Rust fields
struct BinancePlaceOrderResponse {
    symbol: String,
    order_id: i64,
    client_order_id: String,
    transact_time: u64,
    price: String,
    orig_qty: String,
    executed_qty: String,
    cum_quote_qty: String,
    status: String,
    time_in_force: String,
    #[serde(rename = "type")] // Handle keyword field name
    type_str: String,
    side: String,
    // Add other fields if needed from https://binance-docs.github.io/apidocs/spot/en/#place-new-order-trade
    // fills: Option<Vec<BinanceFill>>, // For MARKET orders
}

// #[derive(Debug, serde::Deserialize)]
// #[serde(rename_all = "camelCase")]
// struct BinanceFill {
//     price: String,
//     qty: String,
//     commission: String,
//     commission_asset: String,
// }


#[derive(Debug, serde::Deserialize)]
#[serde(rename_all = "camelCase")]
struct BinanceCancelOrderResponse {
    symbol: String,
    order_id: i64,
    client_order_id: String,
    // Binance cancel response also includes status, price, origQty, executedQty etc.
    status: String,
    price: String,
    orig_qty: String,
    executed_qty: String,
    cum_quote_qty: String,
    #[serde(rename = "type")]
    type_str: String,
    side: String,
}

#[derive(Debug, serde::Deserialize)]
#[serde(rename_all = "camelCase")] // Automatically convert camelCase JSON to snake_case Rust fields
struct BinanceOrderStatusResponse {
    symbol: String,
    order_id: i64, // Use snake_case field name to match auto-rename
    client_order_id: String,
    price: String,
    orig_qty: String, // Original quantity
    executed_qty: String, // Filled quantity
    cum_quote_qty: String, // Cumulative filled quote quantity
    status: String, // e.g., "NEW", "FILLED"
    time_in_force: String,
    #[serde(rename = "type")] // Handle keyword field name
    type_str: String,
    #[serde(rename = "side")] // Handle keyword field name
    side_str: String,
    stop_price: String, // Stop price (optional)
    iceberg_qty: String, // Iceberg quantity (optional)
    time: u64, // Order creation time
    update_time: u64, // Last update time
    is_working: bool, // True if the order is currently active
    orig_quote_order_qty: String, // Original quote order quantity
}

#[derive(Debug, serde::Deserialize)]
#[serde(rename_all = "camelCase")]
struct BinanceListenKeyResponse {
    listen_key: String,
}


// --- Exchange Connector Trait ---
// Add `Any` to the trait bound for downcasting
#[async_trait]
pub trait ExchangeConnector: Send + Sync + Any {
    /// Places a new order on the exchange.
    async fn place_order(&self, request: PlaceOrderRequest) -> Result<PlaceOrderResponse>;

    /// Cancels an existing order on the exchange.
    async fn cancel_order(&self, request: CancelOrderRequest) -> Result<CancelOrderResponse>;

    /// Gets the current status of an order.
    async fn get_order_status(&self, request: OrderStatusRequest) -> Result<OrderStatusResponse>;

    /// Gets the listen key for the user data stream (Binance specific).
    async fn get_listen_key(&self) -> Result<String>;

    /// Renews the listen key for the user data stream (Binance specific).
    async fn renew_listen_key(&self, listen_key: &str) -> Result<()>;

    /// Deletes the listen key for the user data stream (Binance specific).
     async fn delete_listen_key(&self, listen_key: &str) -> Result<()>;

    // TODO: Add methods for other execution types:
    // async fn get_balance(&self) -> Result<Balance>; // Balance needs Protobuf definition
    // async fn get_open_orders(&self, symbol: Option<&str>) -> Result<Vec<OrderStatusResponse>>; // Return Protobuf status list
}

// Helper trait to enable downcasting Arc<dyn Trait>
// https://stackoverflow.com/questions/26189350/is-it-possible-to-downcast-a-trait-object-to-a-concrete-type
impl AsAny for dyn ExchangeConnector {
    fn as_any(&self) -> &dyn Any {
        self
    }
}
// Implement for any type T that is Any and implements ExchangeConnector
impl<T: Any + ExchangeConnector> AsAny for T {
    fn as_any(&self) -> &dyn Any {
        self
    }
}
trait AsAny {
    fn as_any(&self) -> &dyn Any;
}


// --- Implementations for Specific Exchanges ---

#[cfg(feature = "binance_rest")]
pub struct BinanceSpotConnector {
    rest_client: Client,
    api_key: String,
    secret_key: HmacSha256, // Store the HMAC key directly
    base_url: String,
    recv_window: u64,
    // Simple rate limiter for demonstration. Binance has complex weight-based limits.
    // A real implementation might need multiple limiters or a dedicated service.
    rate_limiter: RateLimiter<Identity, DefaultKeyedStateStore<Identity>>,
}

#[cfg(feature = "binance_rest")]
impl BinanceSpotConnector {
    pub fn new(base_url: String, api_key: String, secret_key: String, recv_window_ms: u64) -> Result<Self> {
        let secret_key_mac = HmacSha256::new_from_slice(secret_key.as_bytes())
            .map_err(|e| anyhow!("Invalid Binance secret key: {}", e))?; // Use anyhow for internal connector errors

         // Basic rate limiter: e.g., 100 requests per minute (adjust based on Binance docs and endpoint weights)
         // Binance weights are per endpoint category and accumulate over a minute.
         // This limiter uses a simple rate-per-minute for the *total* request count as a basic safeguard.
         // A weight-based limiter would require tracking weights returned in response headers (X-MBX-USED-WEIGHT).
         let rate_limiter = RateLimiter::keyed(
             Quota::per_minute(NonZeroU32::new(1200).expect("1200 > 0")), // Binance has 1200 weight/min limit. Most simple requests are weight 1-10. This is a rough limit.
             NonZeroU32::new(100).expect("100 > 0") // Burst capacity
         );


        Ok(Self {
            rest_client: Client::builder()
                .timeout(Duration::from_secs(15)) // Set a reasonable timeout, slightly longer than recvWindow
                .build()?,
            api_key,
            secret_key: secret_key_mac,
            base_url,
            recv_window: recv_window_ms,
            rate_limiter,
        })
    }

    // Helper to generate Binance signature
    fn generate_signature(&self, query_string: &str) -> String {
        let mut mac = self.secret_key.clone();
        mac.update(query_string.as_bytes());
        let result = mac.finalize();
        let signature_bytes = result.into_bytes();
        base64::engine::general_purpose::STANDARD.encode(&signature_bytes)
    }

    // Helper to add necessary parameters for signed requests
    fn signed_params(&self) -> String {
        let timestamp = SystemTime::now()
            .duration_since(SystemTime::UNIX_EPOCH)
            .expect("Time went backwards")
            .as_millis();
        format!("timestamp={}&recvWindow={}", timestamp, self.recv_window)
    }

     // Helper to send signed requests and parse response
    async fn send_signed_request<T: serde::de::DeserializeOwned>(
        &self,
        method: Method,
        path: &str,
        mut query_params: Vec<(&str, &str)>,
        body: Option<&serde_json::Value>, // Binance signed requests typically use query, but keep option
    ) -> Result<T> {
        // Add timestamp and recvWindow to query params for signing
        let signed_part = self.signed_params();
        let mut initial_query_pairs: Vec<(String, String)> = query_params.iter().map(|(k, v)| (k.to_string(), v.to_string())).collect();
        let signed_part_pairs: Vec<(String, String)> = signed_part.split('&').filter_map(|pair| {
            pair.split_once('=')
                .map(|(key, val)| (key.to_string(), val.to_string()))
        }).collect();
        initial_query_pairs.extend(signed_part_pairs);

        let query_to_sign = crate::utils::url_encoding::form_urlencoded(initial_query_pairs.clone().into_iter());

        let signature = self.generate_signature(&query_to_sign);

        // Rebuild query params with signature for the final URL
        let mut final_query_params = initial_query_pairs;
        final_query_params.push(("signature".to_string(), signature));

        let final_query_string = crate::utils::url_encoding::form_urlencoded(final_query_params.into_iter());
        let url = format!("{}{}?{}", self.base_url, path, final_query_string);


        // Apply rate limiting before sending the request
        // Use a static key like "binance_rest" for a global limit
        self.rate_limiter.until_key_ready(&Identity {}).await.map_err(|e| anyhow!("Rate limiter error: {}", e))?; // Convert governor error


        let request_builder = match method {
            Method::GET => self.rest_client.get(&url),
            Method::POST => {
                let mut builder = self.rest_client.post(&url);
                if let Some(b) = body {
                    builder = builder.json(b);
                }
                builder
            }
             Method::DELETE => self.rest_client.delete(&url),
            _ => return Err(anyhow!("Unsupported HTTP method for Binance: {:?}", method).into()), // Convert to DataIngestionError
        };

        let request = request_builder
            .header("X-MBX-APIKEY", &self.api_key)
            .build().map_err(|e| anyhow!("Failed to build request: {}", e).into())?; // Convert errors

        trace!("Sending Binance API request: {:?} {}", request.method(), request.url());

        let response = self.rest_client.execute(request).await.map_err(|e| anyhow!("Request execution failed: {}", e).into())?; // Convert errors

        let status = response.status();
        let text = response.text().await.map_err(|e| anyhow!("Failed to get response text: {}", e).into())?; // Convert errors
        trace!("Received Binance response (Status: {}): {}", status, text);


        if status.is_success() {
             serde_json::from_str(&text).map_err(|e| anyhow!("Failed to parse Binance response: {} - {}", e, text).into()) // Convert errors
        } else {
            // Attempt to parse Binance error response format { "code": -XXXX, "msg": "..." }
            let binance_error: serde_json::Value = serde_json::from_str(&text)
                .unwrap_or_else(|_| serde_json::json!({ "code": status.as_u16(), "msg": text })); // Fallback if not JSON

            let error_code = binance_error["code"].as_i64().unwrap_or(-1);
            let error_msg = binance_error["msg"].as_str().unwrap_or("Unknown error").to_string();

            error!("Binance API request failed: Status={}, Code={}, Msg={}", status, error_code, error_msg);

            // Specific error handling could go here based on error_code
            // e.g., if error_code is for insufficient funds, return a specific error variant

            Err(anyhow!("Binance API error {}: {} (Code: {})", status, error_msg, error_code).into()) // Convert errors
        }
    }

     // Helper to send public (unsigned) requests
     async fn send_public_request<T: serde::de::DeserializeOwned>(
        &self,
        method: Method,
        path: &str,
        query_params: Vec<(&str, &str)>,
    ) -> Result<T> {
        let query_string = crate::utils::url_encoding::form_urlencoded(query_params.into_iter().map(|(k, v)| (k.to_string(), v.to_string())));
        let url = format!("{}{}?{}", self.base_url, path, query_string);

        // Apply rate limiting
        self.rate_limiter.until_key_ready(&Identity {}).await.map_err(|e| anyhow!("Rate limiter error: {}", e))?; // Convert governor error


        let request_builder = match method {
            Method::GET => self.rest_client.get(&url),
            Method::POST => self.rest_client.post(&url), // Public POSTs might not need body
            _ => return Err(anyhow!("Unsupported HTTP method for Binance public endpoint: {:?}", method).into()), // Convert errors
        };

        let request = request_builder.build().map_err(|e| anyhow!("Failed to build public request: {}", e).into())?;

         trace!("Sending Binance public API request: {:?} {}", request.method(), request.url());

        let response = request_builder.send().await.map_err(|e| anyhow!("Public request execution failed: {}", e).into())?; // Convert errors

        let status = response.status();
        let text = response.text().await.map_err(|e| anyhow!("Failed to get public response text: {}", e).into())?;
        trace!("Received Binance public response (Status: {}): {}", status, text);

        if status.is_success() {
            serde_json::from_str(&text).map_err(|e| anyhow!("Failed to parse Binance public response: {} - {}", e, text).into())
        } else {
             let binance_error: serde_json::Value = serde_json::from_str(&text)
                .unwrap_or_else(|_| serde_json::json!({ "code": status.as_u16(), "msg": text }));

            let error_code = binance_error["code"].as_i64().unwrap_or(-1);
            let error_msg = binance_error["msg"].as_str().unwrap_or("Unknown error").to_string();

            error!("Binance Public API request failed: Status={}, Code={}, Msg={}", status, error_code, error_msg);
            Err(anyhow!("Binance Public API error {}: {} (Code: {})", status, error_msg, error_code).into())
        }
     }

    // Helper to map Binance order status string to Protobuf enum
    fn map_binance_status_to_proto(&self, status_str: &str) -> OrderStatus {
        match status_str.to_uppercase().as_str() {
            "NEW" => OrderStatus::NEW,
            "PARTIALLY_FILLED" => OrderStatus::PARTIALLY_FILLED,
            "FILLED" => OrderStatus::FILLED,
            "CANCELLING" => OrderStatus::CANCELLING,
            "CANCELLED" => OrderStatus::CANCELLED,
            "REJECTED" => OrderStatus::REJECTED,
            "EXPIRED" => OrderStatus::EXPIRED,
            _ => {
                error!("Unknown Binance order status: {}", status_str);
                OrderStatus::STATUS_UNKNOWN
            }
        }
    }

    // Helper to map Binance order type string to Protobuf enum
    fn map_binance_type_to_proto(&self, type_str: &str) -> OrderType {
         match type_str.to_uppercase().as_str() {
            "LIMIT" => OrderType::LIMIT,
            "MARKET" => OrderType::MARKET,
            "STOP_LOSS" => OrderType::STOP_LOSS,
            "STOP_LOSS_LIMIT" => OrderType::STOP_LOSS_LIMIT,
            "TAKE_PROFIT" => OrderType::TAKE_PROFIT,
            "TAKE_PROFIT_LIMIT" => OrderType::TAKE_PROFIT_LIMIT,
            "LIMIT_MAKER" => OrderType::LIMIT_MAKER,
            _ => {
                error!("Unknown Binance order type: {}", type_str);
                OrderType::ORDER_TYPE_UNKNOWN
            }
        }
    }

    // Helper to map Binance order side string to Protobuf enum
     fn map_binance_side_to_proto(&self, side_str: &str) -> OrderSide {
         match side_str.to_uppercase().as_str() {
            "BUY" => OrderSide::BUY,
            "SELL" => OrderSide::SELL,
            _ => {
                error!("Unknown Binance order side: {}", side_str);
                OrderSide::SIDE_UNKNOWN
            }
        }
    }
}


#[cfg(feature = "binance_rest")]
#[async_trait]
impl ExchangeConnector for BinanceSpotConnector {
    async fn place_order(&self, request: PlaceOrderRequest) -> Result<PlaceOrderResponse> {
        info!("Placing Binance order: {:?}", request);
        let path = "/api/v3/order";
        let method = Method::POST;

        // Binance requires symbol in uppercase for REST API calls
        let symbol_upper = request.symbol.to_uppercase();

        let mut query_params = vec![
            ("symbol", symbol_upper.as_str()),
            ("side", match request.side() { OrderSide::BUY => "BUY", OrderSide::SELL => "SELL", OrderSide::SIDE_UNKNOWN => return Err(anyhow!("Unsupported order side").into()) }),
            ("type", match request.order_type() {
                OrderType::LIMIT => "LIMIT",
                OrderType::MARKET => "MARKET",
                // TODO: Add other types and their required parameters
                _ => return Err(anyhow!("Unsupported order type for placement").into()),
            }),
            ("quantity", request.quantity.as_str()),
            ("newClientOrderId", request.client_order_id.as_str()), // Use newClientOrderId for custom ID
        ];

        if request.order_type() == OrderType::LIMIT {
             if request.price.is_empty() {
                 return Err(anyhow!("Price is required for LIMIT orders").into());
             }
             query_params.push(("price", request.price.as_str()));
             // Add timeInForce for LIMIT orders (e.g., GTC by default)
             // If not provided, Binance defaults to GTC for limit orders
             // query_params.push(("timeInForce", "GTC"));
        }
        // Add recvWindow if specified in the request (from config)
        if request.recv_window > 0 {
             query_params.push(("recvWindow", request.recv_window.to_string().as_str())); // Needs &str lifetime
        }
        // The signing process will add timestamp and recvWindow if not already added

        let body = None; // Binance POST order uses query parameters

        let binance_response: BinancePlaceOrderResponse = self.send_signed_request(method, path, query_params, body).await?;

        // Map Binance response to Protobuf
        Ok(PlaceOrderResponse {
            exchange: "binance".to_string(),
            symbol: binance_response.symbol.to_string(),
            order_id: binance_response.order_id.to_string(),
            client_order_id: binance_response.client_order_id.to_string(),
            status: self.map_binance_status_to_proto(&binance_response.status) as i32,
            message: "Order placed successfully".to_string(), // Binance success response usually doesn't have a message field, hardcode or derive.
            transac_time: Some(prost_types::Timestamp::from(std::time::UNIX_EPOCH + Duration::from_millis(binance_response.transact_time))),
        })
    }

    async fn cancel_order(&self, request: CancelOrderRequest) -> Result<CancelOrderResponse> {
        info!("Cancelling Binance order: {:?}", request);
        let path = "/api/v3/order";
        let method = Method::DELETE;

        let symbol_upper = request.symbol.to_uppercase();
        let mut query_params = vec![
            ("symbol", symbol_upper.as_str()),
        ];

        // Need either orderId or origClientOrderId
        if !request.order_id.is_empty() {
            query_params.push(("orderId", request.order_id.as_str()));
        } else if !request.client_order_id.is_empty() {
            query_params.push(("origClientOrderId", request.client_order_id.as_str()));
        } else {
             return Err(anyhow!("Either order_id or client_order_id is required for cancellation").into());
        }
        // Add recvWindow if specified
        if request.recv_window > 0 {
             query_params.push(("recvWindow", request.recv_window.to_string().as_str()));
        }


        let body = None; // DELETE request usually doesn't have a body

         let binance_response: BinanceCancelOrderResponse = self.send_signed_request(method, path, query_params, body).await?;

        // Map Binance response to Protobuf
        Ok(CancelOrderResponse {
             exchange: "binance".to_string(),
             symbol: binance_response.symbol.to_string(),
             order_id: binance_response.order_id.to_string(),
             client_order_id: binance_response.client_order_id.to_string(),
             success: true, // If we got a response, the request was accepted. Actual cancellation might be async.
             message: "Cancellation request accepted".to_string(),
             // Fields from Binance cancel response like status, price, qty could be added to Protobuf
             // If added, map them here:
             // status: self.map_binance_status_to_proto(&binance_response.status) as i32,
             // price: binance_response.price,
             // quantity: binance_response.orig_qty,
        })
    }

    async fn get_order_status(&self, request: OrderStatusRequest) -> Result<OrderStatusResponse> {
         info!("Getting Binance order status: {:?}", request);
         let path = "/api/v3/order";
         let method = Method::GET;

         let symbol_upper = request.symbol.to_uppercase();
         let mut query_params = vec![
            ("symbol", symbol_upper.as_str()),
        ];

        // Need either orderId or origClientOrderId
        if !request.order_id.is_empty() {
            query_params.push(("orderId", request.order_id.as_str()));
        } else if !request.client_order_id.is_empty() {
            query_params.push(("origClientOrderId", request.client_order_id.as_str()));
        } else {
             return Err(anyhow!("Either order_id or client_order_id is required for status check").into());
        }
         // Add recvWindow if specified
        if request.recv_window > 0 {
             query_params.push(("recvWindow", request.recv_window.to_string().as_str()));
        }


        let binance_response: BinanceOrderStatusResponse = self.send_signed_request(method, path, query_params, None).await?;

        // Calculate avg executed price (handle division by zero if executed_qty is zero)
        let executed_qty = parse_decimal(&binance_response.executed_qty)?;
        let cumulative_quote_qty = parse_decimal(&binance_response.cum_quote_qty)?;
        let executed_price = if executed_qty.is_zero() {
            Decimal::ZERO
        } else {
             cumulative_quote_qty / executed_qty
        };


        // Map Binance response to Protobuf
         Ok(OrderStatusResponse {
            exchange: "binance".to_string(),
            symbol: binance_response.symbol.to_string(),
            order_id: binance_response.order_id.to_string(),
            client_order_id: binance_response.client_order_id.to_string(),
            status: self.map_binance_status_to_proto(&binance_response.status) as i32,
            price: binance_response.price.to_string(),
            quantity: binance_response.orig_qty.to_string(),
            executed_quantity: binance_response.executed_qty.to_string(),
            cumulative_quote_quantity: binance_response.cum_quote_qty.to_string(),
            executed_price: executed_price.to_string(),
            message: "Status retrieved".to_string(), // Derive message
            update_time: Some(prost_types::Timestamp::from(std::time::UNIX_EPOCH + Duration::from_millis(binance_response.update_time))),
            // Populate other fields from Binance response if added to Protobuf
        })
    }

    // Method to get listen key for User Data Stream
    async fn get_listen_key(&self) -> Result<String> {
        info!("Getting Binance listen key...");
        let path = "/api/v3/userDataStream";
        let method = Method::POST;

        // Binance GET/PUT/DELETE user data stream endpoints are also signed, POST is not (only needs API key header)
        let body = None; // POST requires empty body

        let response: BinanceListenKeyResponse = self.send_public_request(method, path, vec![]).await?;

        info!("Received Binance listen key: {}", response.listen_key);
        Ok(response.listen_key)
    }

     // Method to renew listen key for User Data Stream
    async fn renew_listen_key(&self, listen_key: &str) -> Result<()> {
        info!("Renewing Binance listen key: {}", listen_key);
        let path = "/api/v3/userDataStream";
        let method = Method::PUT;

        let query_params = vec![
            ("listenKey", listen_key),
        ];

        self.send_signed_request::<serde_json::Value>(method, path, query_params, None).await?;
        info!("Binance listen key renewed successfully.");
        Ok(())
    }

    // Method to delete listen key for User Data Stream
     async fn delete_listen_key(&self, listen_key: &str) -> Result<()> {
        info!("Deleting Binance listen key: {}", listen_key);
        let path = "/api/v3/userDataStream";
        let method = Method::DELETE;

         let query_params = vec![
            ("listenKey", listen_key),
        ];

        self.send_signed_request::<serde_json::Value>(method, path, query_params, None).await?;
        info!("Binance listen key deleted successfully.");
        Ok(())
     }
}


// --- Connector Factory ---
pub struct ConnectorFactory {
    connectors: HashMap<String, Arc<dyn ExchangeConnector>>,
}

impl ConnectorFactory {
    pub fn new(config: &HashMap<String, super::config::ExchangeConfig>) -> Result<Self> {
        let mut connectors: HashMap<String, Arc<dyn ExchangeConnector>> = HashMap::new();

        for (exchange_name, exchange_config) in config {
            match exchange_name.to_lowercase().as_str() {
                #[cfg(feature = "binance_rest")]
                "binance" => {
                     let connector = BinanceSpotConnector::new(
                         exchange_config.rest_api_url.clone(),
                         exchange_config.api_key.clone(),
                         exchange_config.secret_key.clone(),
                         exchange_config.recv_window_ms,
                     )?;
                     connectors.insert(
                         exchange_name.clone(),
                         Arc::new(connector)
                     );
                     info!("Binance connector created.");
                }
                _ => {
                    error!("Unsupported exchange configured: {}", exchange_name);
                    return Err(anyhow!("Unsupported exchange configured: {}", exchange_name).into());
                }
            }
        }

        if connectors.is_empty() {
             error!("No supported exchanges configured.");
             return Err(anyhow!("No supported exchanges configured.").into());
        }

        Ok(Self { connectors })
    }

    /// Gets the connector for a specific exchange.
    pub fn get_connector(&self, exchange: &str) -> Result<&Arc<dyn ExchangeConnector>> {
        self.connectors.get(exchange)
            .ok_or_else(|| anyhow!("Connector not found for exchange: {}", exchange).into())
    }

    /// Gets the Binance connector specifically (since we only have Binance for now)
    #[cfg(feature = "binance_rest")]
    pub fn get_binance_connector(&self) -> Result<&Arc<BinanceSpotConnector>> {
         self.connectors.get("binance")
            .ok_or_else(|| anyhow!("Binance connector not found").into()) // Check if key exists first
            .and_then(|arc_connector| {
                // Downcast the Arc<dyn ExchangeConnector> to Arc<BinanceSpotConnector>
                arc_connector.clone().into_any().downcast::<BinanceSpotConnector>()
                     .map_err(|_| anyhow!("Failed to downcast connector to BinanceSpotConnector").into())
            })
    }
}
// Need Arc::into_any() which requires `Any` trait on the type, not the trait object
// Let's adjust get_binance_connector to borrow instead of consuming the Arc, simpler.
#[cfg(feature = "binance_rest")]
impl ConnectorFactory {
     pub fn get_binance_connector_ref(&self) -> Result<&BinanceSpotConnector> {
         self.connectors.get("binance")
            .ok_or_else(|| anyhow!("Binance connector not found").into())
            .and_then(|arc_connector| {
                 arc_connector.as_any().downcast_ref::<BinanceSpotConnector>()
                     .ok_or_else(|| anyhow!("Failed to downcast connector reference to BinanceSpotConnector").into())
            })
     }
}