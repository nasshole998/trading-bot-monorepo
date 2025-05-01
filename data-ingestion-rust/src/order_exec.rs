// data-ingestion-rust/src/order_exec.rs
use crate::config::Settings;
use crate::error::{DataIngestionError, Result};
use crate::types::{AlpacaOrderRequest, AlpacaOrderResponse, AlpacaApiError};
use log::{info, error, warn, debug};
use reqwest::{Client, StatusCode};
use std::time::Duration;
use tonic::{Request, Response, Status};

// Import generated gRPC server code
pub mod orders_proto {
    tonic::include_proto!("orders"); // Use the package name defined in orders.proto
}
use orders_proto::order_receiver_server::{OrderReceiver, OrderReceiverServer};
use orders_proto::{OrderRequest as ProtoOrderRequest, OrderResponse as ProtoOrderResponse, CancelRequest as ProtoCancelRequest, CancelResponse as ProtoCancelResponse, OrderSide as ProtoOrderSide, OrderType as ProtoOrderType, TimeInForce as ProtoTimeInForce, OrderStatus as ProtoOrderStatus};

// --- gRPC Service Implementation ---

#[derive(Clone)] // Clone is needed for the Tonic server
pub struct OrderReceiverService {
    settings: Settings,
    http_client: Client,
}

impl OrderReceiverService {
    fn new(settings: Settings) -> Result<Self> {
        // Build Reqwest client with timeouts and headers
        let http_client = Client::builder()
            .timeout(Duration::from_secs(15)) // General request timeout
            .connect_timeout(Duration::from_secs(5)) // TCP connect timeout
            .user_agent(format!("TradingBot/{} Rust Reqwest", env!("CARGO_PKG_VERSION")))
            .build()
            .map_err(DataIngestionError::HttpRequest)?;

        Ok(OrderReceiverService { settings, http_client })
    }

    /// Helper to make authenticated requests to Alpaca REST API
    async fn post_alpaca<T: serde::Serialize>(
        &self,
        endpoint: &str,
        payload: &T,
    ) -> Result<reqwest::Response> {
        let base_url = self.settings.get_alpaca_rest_base_url();
        let url = format!("{}/v2/{}", base_url, endpoint);
        let api_key = &self.settings.alpaca.api_key;
        let secret_key = &self.settings.alpaca.secret_key;

        debug!("POSTing to Alpaca: URL={}, Payload={:?}", url, serde_json::to_string(payload).unwrap_or_default());

        let response = self.http_client
            .post(&url)
            .header("APCA-API-KEY-ID", api_key)
            .header("APCA-API-SECRET-KEY", secret_key)
            .json(payload)
            .send()
            .await?;

        debug!("Alpaca Response Status: {}", response.status());
        Ok(response)
    }

     /// Helper to delete orders via Alpaca REST API
    async fn delete_alpaca(
        &self,
        endpoint: &str, // e.g., "orders/{order_id}"
    ) -> Result<reqwest::Response> {
        let base_url = self.settings.get_alpaca_rest_base_url();
        let url = format!("{}/v2/{}", base_url, endpoint);
        let api_key = &self.settings.alpaca.api_key;
        let secret_key = &self.settings.alpaca.secret_key;

        debug!("DELETEing from Alpaca: URL={}", url);

        let response = self.http_client
            .delete(&url)
            .header("APCA-API-KEY-ID", api_key)
            .header("APCA-API-SECRET-KEY", secret_key)
            .send()
            .await?;

        debug!("Alpaca Response Status: {}", response.status());
        Ok(response)
    }

    /// Helper to handle Alpaca API response, parsing success or error
    async fn handle_alpaca_response<R: serde::de::DeserializeOwned>(
        &self,
        response: reqwest::Response,
    ) -> Result<R> {
        let status = response.status();
        let raw_body = response.text().await?; // Read body once
        debug!("Raw Alpaca Response Body: {}", raw_body);

        if status.is_success() {
            serde_json::from_str::<R>(&raw_body).map_err(DataIngestionError::Json)
        } else {
            // Try parsing Alpaca's error format
            let api_error: AlpacaApiError = serde_json::from_str(&raw_body)
                .unwrap_or_else(|e| AlpacaApiError {
                    code: status.as_u16() as i32, // Use HTTP status code if parsing fails
                    message: format!("Failed to parse error body: {}. Raw: {}", e, raw_body),
                });
            error!("Alpaca API Error: Status={}, Code={}, Message={}", status, api_error.code, api_error.message);
            Err(DataIngestionError::AlpacaApi {
                status: status.as_u16(),
                message: api_error.message,
            })
        }
    }

    // --- Conversion Helpers ---

    fn proto_to_alpaca_side(side: ProtoOrderSide) -> String {
        match side {
            ProtoOrderSide::Buy => "buy".to_string(),
            ProtoOrderSide::Sell => "sell".to_string(),
            ProtoOrderSide::OrderSideUnspecified => "buy".to_string(), // Default or error?
        }
    }

    fn proto_to_alpaca_type(order_type: ProtoOrderType) -> String {
        match order_type {
            ProtoOrderType::Market => "market".to_string(),
            ProtoOrderType::Limit => "limit".to_string(),
            ProtoOrderType::Stop => "stop".to_string(),
            ProtoOrderType::StopLimit => "stop_limit".to_string(),
            ProtoOrderType::TrailingStop => "trailing_stop".to_string(),
            ProtoOrderType::OrderTypeUnspecified => "market".to_string(), // Default or error?
        }
    }

     fn proto_to_alpaca_tif(tif: ProtoTimeInForce) -> String {
        match tif {
            ProtoTimeInForce::Day => "day".to_string(),
            ProtoTimeInForce::Gtc => "gtc".to_string(),
            ProtoTimeInForce::Opg => "opg".to_string(),
            ProtoTimeInForce::Cls => "cls".to_string(),
            ProtoTimeInForce::Ioc => "ioc".to_string(),
            ProtoTimeInForce::Fok => "fok".to_string(),
            ProtoTimeInForce::TimeInForceUnspecified => "day".to_string(), // Default or error?
        }
    }

    fn alpaca_to_proto_status(status: &str) -> ProtoOrderStatus {
        match status {
            "new" | "pending_new" | "accepted" | "pending_replace" | "accepted_for_bidding" => ProtoOrderStatus::PendingNew,
            "calculated" => ProtoOrderStatus::PendingNew, // Often pre-submission state
            "partially_filled" => ProtoOrderStatus::PartiallyFilled,
            "filled" => ProtoOrderStatus::Filled,
            "done_for_day" => ProtoOrderStatus::Filled, // Or map based on filled_qty?
            "canceled" | "pending_cancel" => ProtoOrderStatus::Canceled,
            "stopped" => ProtoOrderStatus::Canceled, // Stop triggered
            "rejected" => ProtoOrderStatus::Rejected,
            "expired" => ProtoOrderStatus::Expired,
            "replaced" => ProtoOrderStatus::Accepted, // Status after successful replace
            _ => {
                warn!("Unmapped Alpaca order status: {}", status);
                ProtoOrderStatus::OrderStatusUnspecified
            }
        }
    }

}

#[async_trait::async_trait]
impl OrderReceiver for OrderReceiverService {
    /// Handles incoming gRPC request to place an order
    async fn place_order(
        &self,
        request: Request<ProtoOrderRequest>,
    ) -> std::result::Result<Response<ProtoOrderResponse>, Status> {
        let proto_req = request.into_inner();
        info!("Received PlaceOrder request: {:?}", proto_req);

        // Validate and convert proto request to Alpaca REST request format
        let alpaca_req = AlpacaOrderRequest {
            symbol: proto_req.symbol,
            // Use String::from for quantity to handle potential float inaccuracies if needed
            qty: Some(proto_req.quantity.to_string()),
            notional: None, // Assuming quantity-based orders for now
            side: Self::proto_to_alpaca_side(ProtoOrderSide::try_from(proto_req.side).unwrap_or_default()),
            order_type: Self::proto_to_alpaca_type(ProtoOrderType::try_from(proto_req.r#type).unwrap_or_default()),
            time_in_force: Self::proto_to_alpaca_tif(ProtoTimeInForce::try_from(proto_req.time_in_force).unwrap_or_default()),
            limit_price: proto_req.limit_price.map(|p| p.to_string()),
            stop_price: proto_req.stop_price.map(|p| p.to_string()),
            trail_price: proto_req.trail_price.map(|p| p.to_string()),
            trail_percent: proto_req.trail_percent.map(|p| p.to_string()),
            extended_hours: proto_req.extended_hours,
            client_order_id: Some(proto_req.client_order_id.clone()), // Ensure it's unique and within limits
            order_class: None, // Add logic for complex orders if needed
        };

        // Send request to Alpaca
        match self.post_alpaca("orders", &alpaca_req).await {
            Ok(response) => {
                // Handle Alpaca response
                match self.handle_alpaca_response::<AlpacaOrderResponse>(response).await {
                    Ok(alpaca_resp) => {
                        info!("Alpaca order placement successful: {:?}", alpaca_resp);
                        // Convert Alpaca response back to proto response
                        let proto_resp = ProtoOrderResponse {
                            client_order_id: alpaca_resp.client_order_id,
                            server_order_id: alpaca_resp.id.to_string(),
                            status: Self::alpaca_to_proto_status(&alpaca_resp.status).into(),
                            created_at: to_proto_timestamp(alpaca_resp.created_at),
                            message: None,
                        };
                        Ok(Response::new(proto_resp))
                    }
                    Err(e) => {
                        error!("Failed to handle Alpaca order response: {}", e);
                        // Return gRPC error status
                        Err(Status::internal(format!("Alpaca API interaction failed: {}", e)))
                    }
                }
            }
            Err(e) => {
                error!("Failed to send order request to Alpaca: {}", e);
                Err(Status::internal(format!("HTTP request to Alpaca failed: {}", e)))
            }
        }
    }

    /// Handles incoming gRPC request to cancel an order
    async fn cancel_order(
        &self,
        request: Request<ProtoCancelRequest>,
    ) -> std::result::Result<Response<ProtoCancelResponse>, Status> {
        let proto_req = request.into_inner();
        info!("Received CancelOrder request: {:?}", proto_req);

        // Prefer server_order_id if available, otherwise use client_order_id
        let order_id_to_cancel = if !proto_req.server_order_id.is_empty() {
            proto_req.server_order_id
        } else if !proto_req.client_order_id.is_empty() {
            // Note: Canceling by client_order_id might not be directly supported or reliable via DELETE.
            // Alpaca's DELETE /v2/orders/{order_id} uses their ID.
            // If only client_order_id is provided, we might need to first GET the order by client_order_id
            // to find the server_order_id, which adds latency and complexity.
            // For simplicity here, we assume server_order_id is preferred.
             warn!("Cancellation by client_order_id requested - prefer using server_order_id from Alpaca.");
             return Err(Status::invalid_argument("Cancellation by client_order_id is less reliable; use server_order_id if possible."));
            // proto_req.client_order_id
        } else {
            return Err(Status::invalid_argument("Either server_order_id or client_order_id must be provided for cancellation."));
        };

        let endpoint = format!("orders/{}", order_id_to_cancel);

        // Send DELETE request to Alpaca
        match self.delete_alpaca(&endpoint).await {
            Ok(response) => {
                let status_code = response.status();
                 // Alpaca DELETE /v2/orders/{order_id} might return:
                 // - 204 No Content on successful cancellation request (order might still be pending_cancel)
                 // - 404 Not Found if order doesn't exist or already done
                 // - 422 Unprocessable Entity if order cannot be canceled (e.g., already filled)
                 // - 207 Multi-Status (less common for single cancel)
                if status_code == StatusCode::NO_CONTENT {
                     info!("Alpaca order cancellation request successful for {}", order_id_to_cancel);
                     // We don't get the full order details back on DELETE 204.
                     // We might need a subsequent GET if we need the final status immediately.
                     // For now, return a success indicating the request was accepted.
                     let proto_resp = ProtoCancelResponse {
                         // Try to echo back IDs if possible
                         client_order_id: if proto_req.server_order_id.is_empty() { proto_req.client_order_id } else { "".to_string() },
                         server_order_id: if !proto_req.server_order_id.is_empty() { proto_req.server_order_id } else { order_id_to_cancel }, // Best guess
                         status: ProtoOrderStatus::PendingCancel.into(), // Assume pending cancel
                         message: Some("Cancellation request accepted by Alpaca.".to_string()),
                     };
                     Ok(Response::new(proto_resp))
                } else if status_code == StatusCode::NOT_FOUND {
                    warn!("Order {} not found or already processed, cannot cancel.", order_id_to_cancel);
                    Err(Status::not_found(format!("Order {} not found or already processed.", order_id_to_cancel)))
                } else if status_code == StatusCode::UNPROCESSABLE_ENTITY {
                     error!("Order {} cannot be canceled (likely already filled/rejected/canceled).", order_id_to_cancel);
                     // Try to parse error message if available
                     let body = response.text().await.unwrap_or_default();
                     let api_error: AlpacaApiError = serde_json::from_str(&body).unwrap_or_else(|_| AlpacaApiError { code: 422, message: body });
                     Err(Status::failed_precondition(format!("Order {} cannot be canceled: {}", order_id_to_cancel, api_error.message)))
                }
                 else {
                    // Handle other unexpected statuses
                    let body = response.text().await.unwrap_or_default();
                    error!("Unexpected status {} during Alpaca order cancellation for {}: {}", status_code, order_id_to_cancel, body);
                    Err(Status::internal(format!("Alpaca cancellation failed with status {}: {}", status_code, body)))
                }
            }
            Err(e) => {
                error!("Failed to send cancel request to Alpaca for {}: {}", order_id_to_cancel, e);
                Err(Status::internal(format!("HTTP request to Alpaca failed: {}", e)))
            }
        }
    }
}


/// Task responsible for running the gRPC server to receive order requests.
pub async fn run_grpc_server_task(settings: Settings) -> Result<()> {
    let bind_address = settings.grpc.order_receiver_bind_address.parse()?;
    info!("Starting gRPC Order Receiver server on {}...", bind_address);

    let order_service = OrderReceiverService::new(settings.clone())?;

    tonic::transport::Server::builder()
        .add_service(OrderReceiverServer::new(order_service))
        .serve(bind_address)
        .await?; // This runs indefinitely until shutdown or error

    warn!("gRPC Order Receiver server has shut down.");
    Ok(())
}

// Helper to convert chrono DateTime to prost Timestamp (duplicate from grpc_client, consider moving to utils)
fn to_proto_timestamp(dt: chrono::DateTime<chrono::Utc>) -> Option<prost_types::Timestamp> {
    let seconds = dt.timestamp();
    let nanos = dt.timestamp_subsec_nanos();
    if nanos >= 1_000_000_000 {
        error!("Timestamp nanoseconds out of range: {}", nanos);
        return None;
    }
    Some(prost_types::Timestamp {
        seconds,
        nanos: nanos as i32,
    })
}
