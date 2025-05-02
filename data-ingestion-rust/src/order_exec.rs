use tonic::{Request, Response, Status};
use tracing::{info, error, debug, trace};
use crate::Result; // Use the custom Result type
use futures::Stream;
use std::pin::Pin;
use std::time::{SystemTime, Duration};

use crate::execution::{
    execution_service_server::{ExecutionService, ExecutionServiceServer},
    PlaceOrderRequest, PlaceOrderResponse, CancelOrderRequest, CancelOrderResponse,
    OrderStatusRequest, OrderStatusResponse, OrderUpdate, StreamOrderUpdatesResponse,
    OrderStatus, OrderType, OrderSide,
    // Include other relevant enums if needed from proto
};
use crate::connector::ConnectorFactory;
use crate::config::AppConfig;
use tonic::transport::Channel;
use crate::execution::execution_service_client::ExecutionServiceClient;
use tokio::sync::mpsc;
use tokio_stream::wrappers::ReceiverStream;
use tokio_util::sync::CancellationToken; // For graceful shutdown
use prost_types::Timestamp;
use crate::utils::parse_decimal;


// --- gRPC Server Implementation ---

/// Implementation of the ExecutionService gRPC server.
/// Receives Place/Cancel/Status requests FROM Infrastructure/Risk.
pub struct ExecutionServiceImpl {
    connector_factory: ConnectorFactory,
    // Channel to send order updates *from* this service *to* the Infrastructure service.
    // This channel is fed by both the REST responses (provisional) and the user data stream (authoritative).
    order_update_tx: mpsc::Sender<OrderUpdate>,
    // Config access might be needed for recv_window defaults
    config: AppConfig,
}

impl ExecutionServiceImpl {
    pub fn new(connector_factory: ConnectorFactory, order_update_tx: mpsc::Sender<OrderUpdate>, config: AppConfig) -> Self {
        Self {
            connector_factory,
            order_update_tx,
            config,
        }
    }

    // Helper to get the connector and handle errors
    fn get_connector(&self, exchange: &str) -> Result<&dyn crate::connector::ExchangeConnector, Status> {
        self.connector_factory.get_connector(exchange)
            .map(|arc_connector| arc_connector.as_ref()) // Get reference to the trait object
            .map_err(|e| {
                error!("Failed to get connector for exchange {}: {:?}", exchange, e); // Log underlying error
                Status::invalid_argument(format!("Unsupported or misconfigured exchange: {}", exchange))
            })
    }

    // Helper to get the Binance specific connector reference
    #[cfg(feature = "binance_rest")]
    fn get_binance_connector_ref(&self) -> Result<&crate::connector::BinanceSpotConnector, Status> {
        self.connector_factory.get_binance_connector_ref()
             .map_err(|e| {
                error!("Failed to get Binance connector: {:?}", e);
                Status::internal("Internal error getting Binance connector")
            })
    }

    // Helper to send an OrderUpdate message via the internal channel (non-blocking)
    fn send_order_update(&self, update: OrderUpdate) {
         trace!("Attempting to send OrderUpdate: {:?}", update);
         // Use try_send to avoid blocking the gRPC server handler
         if let Err(e) = self.order_update_tx.try_send(update) {
             warn!("Failed to send order update to channel: {}", e);
             // Channel likely full or receiver dropped. Monitor this.
         }
    }
}

#[tonic::async_trait]
impl ExecutionService for ExecutionServiceImpl {
    async fn place_order(&self, request: Request<PlaceOrderRequest>) -> Result<Response<PlaceOrderResponse>, Status> {
        let mut req = request.into_inner();
        info!("Received PlaceOrder request: {:?}", req);

        // Basic validation
        if req.exchange.is_empty() || req.symbol.is_empty() || req.quantity.is_empty() || req.client_order_id.is_empty() {
             return Err(Status::invalid_argument("Missing required fields: exchange, symbol, quantity, client_order_id"));
        }
        if req.order_type == OrderType::ORDER_TYPE_UNKNOWN as i32 || req.side == OrderSide::SIDE_UNKNOWN as i32 {
            return Err(Status::invalid_argument("Invalid order_type or side"));
        }
         if req.order_type == OrderType::LIMIT as i32 && req.price.is_empty() {
             return Err(Status::invalid_argument("Price is required for LIMIT orders"));
         }
         // TODO: More sophisticated validation (e.g., quantity/price format, symbol format)
         if crate::utils::parse_decimal(&req.quantity).is_err() { return Err(Status::invalid_argument("Invalid quantity format")); }
         if req.order_type == OrderType::LIMIT as i32 && crate::utils::parse_decimal(&req.price).is_err() { return Err(Status::invalid_argument("Invalid price format")); }


        let connector = self.get_connector(&req.exchange)?; // Gets the BinanceConnector

        // Add default recvWindow from config if not provided in request
        if req.recv_window == 0 {
            if let Some(exchange_cfg) = self.config.exchanges.get(&req.exchange) {
                 req.recv_window = exchange_cfg.recv_window_ms as i64;
            } else {
                 // Should not happen if get_connector succeeded, but defensive
                 warn!("No config found for exchange {}", req.exchange);
            }
        }


        match connector.place_order(req.clone()).await {
            Ok(response) => {
                info!("PlaceOrder successful for client_order_id {}: {:?}", req.client_order_id, response);

                // Immediately send a provisional order update back to Infrastructure via the channel
                // This uses data from the REST response which might be incomplete but is faster
                // than waiting for the User Data Stream.
                let provisional_update = OrderUpdate {
                    exchange: req.exchange.clone(),
                    symbol: req.symbol.clone(), // Use symbol from request (exchange format expected by Binance API)
                    order_id: response.order_id.clone(),
                    client_order_id: req.client_order_id.clone(),
                    status: response.status, // Status from Binance response (e.g., NEW)
                    executed_quantity: "0".to_string(), // Provisional
                    cumulative_quote_quantity: "0".to_string(), // Provisional
                    executed_price: "0".to_string(), // Provisional
                    timestamp: response.transac_time.or_else(|| Some(Timestamp::from(SystemTime::now()))), // Use exchange time or local
                    message: response.message.clone(),
                    order_type: req.order_type,
                    side: req.side,
                    price: req.price.clone(), // Original price
                    quantity: req.quantity.clone(), // Original quantity
                };
                self.send_order_update(provisional_update);

                Ok(Response::new(response))
            },
            Err(e) => {
                error!("PlaceOrder failed for client_order_id {}: {:?}", req.client_order_id, e); // Log underlying error
                // Attempt to derive a meaningful status/message from the error
                let status = match e {
                     // Map specific DataIngestionError variants to gRPC Status codes
                     DataIngestionError::ConnectorError(ref anyhow_err) => {
                         // Could inspect anyhow_err details if connector provided specific info
                         Status::internal(format!("Exchange API error: {}", anyhow_err))
                     },
                     DataIngestionError::GrpcStatus(s) => s, // If connector returned a gRPC status directly
                     _ => Status::internal(format!("Internal error: {}", e)),
                 };

                 let failed_response = PlaceOrderResponse {
                     exchange: req.exchange,
                     symbol: req.symbol,
                     order_id: None, // No exchange ID on failure
                     client_order_id: req.client_order_id.clone(),
                     status: OrderStatus::REJECTED as i32, // Assume rejected on API error
                     message: status.message().to_string(),
                     transac_time: Some(Timestamp::from(SystemTime::now())), // Use local time for failure
                 };
                 // Send a rejected update
                  let rejected_update = OrderUpdate {
                     exchange: failed_response.exchange.clone(),
                     symbol: failed_response.symbol.clone(),
                     order_id: failed_response.order_id.clone().unwrap_or_default(),
                     client_order_id: failed_response.client_order_id.clone(),
                     status: failed_response.status,
                     executed_quantity: "0".to_string(),
                     cumulative_quote_quantity: "0".to_string(),
                     executed_price: "0".to_string(),
                     timestamp: Some(Timestamp::from(SystemTime::now())),
                     message: failed_response.message.clone(),
                     order_type: req.order_type,
                     side: req.side,
                     price: req.price.clone(),
                     quantity: req.quantity.clone(),
                  };
                 self.send_order_update(rejected_update);

                Err(status) // Return the gRPC status error
            }
        }
    }

     async fn cancel_order(&self, request: Request<CancelOrderRequest>) -> Result<Response<CancelOrderResponse>, Status> {
        let mut req = request.into_inner();
        info!("Received CancelOrder request: {:?}", req);

        // Basic validation
         if req.exchange.is_empty() || req.symbol.is_empty() || (req.order_id.is_empty() && req.client_order_id.is_empty()) {
             return Err(Status::invalid_argument("Missing exchange, symbol, and either order_id or client_order_id"));
         }
         // TODO: Validate symbol format

        let connector = self.get_connector(&req.exchange)?; // Gets the BinanceConnector

         // Add default recvWindow from config if not provided in request
        if req.recv_window == 0 {
            if let Some(exchange_cfg) = self.config.exchanges.get(&req.exchange) {
                 req.recv_window = exchange_cfg.recv_window_ms as i64;
            }
        }


        match connector.cancel_order(req.clone()).await {
            Ok(response) => {
                info!("CancelOrder successful for order {}: {:?}", req.order_id, response);

                // Send an order update reflecting the cancellation attempt status (might be CANCELLING initially)
                // The authoritative status (CANCELLED or PARTIALLY_FILLED/FILLED if it executed before cancel)
                // will come from the user data stream.
                let cancellation_update = OrderUpdate {
                     exchange: req.exchange.clone(),
                     symbol: req.symbol.clone(),
                     order_id: response.order_id.clone(),
                     client_order_id: response.client_order_id.clone(),
                     status: if response.success { OrderStatus::CANCELLING as i32 } else { OrderStatus::STATUS_UNKNOWN as i32 /* Could be REJECTED depending on API response */ }, // Best guess status based on success flag
                     executed_quantity: "0".to_string(), // Don't know filled qty from cancel response typically
                     cumulative_quote_quantity: "0".to_string(),
                     executed_price: "0".to_string(),
                     timestamp: Some(Timestamp::from(SystemTime::now())), // Use local time for provisional
                     message: response.message.clone(),
                     order_type: 0, // Unknown from cancel response fields provided in proto
                     side: 0,       // Unknown from cancel response fields provided in proto
                     price: "0".to_string(), // Unknown
                     quantity: "0".to_string(), // Unknown
                };
                self.send_order_update(cancellation_update);

                Ok(Response::new(response))
            },
            Err(e) => {
                error!("CancelOrder failed for order {}: {:?}", req.order_id, e);
                 let status = match e {
                     DataIngestionError::ConnectorError(ref anyhow_err) => {
                         Status::internal(format!("Exchange API error: {}", anyhow_err))
                     },
                     DataIngestionError::GrpcStatus(s) => s,
                     _ => Status::internal(format!("Internal error: {}", e)),
                 };
                 // Could potentially send an update with status REJECTED if the error indicates that
                Err(status)
            }
        }
     }

    async fn get_order_status(&self, request: Request<OrderStatusRequest>) -> Result<Response<OrderStatusResponse>, Status> {
         let mut req = request.into_inner();
         info!("Received GetOrderStatus request: {:?}", req);

          if req.exchange.is_empty() || req.symbol.is_empty() || (req.order_id.is_empty() && req.client_order_id.is_empty()) {
             return Err(Status::invalid_argument("Missing exchange, symbol, and either order_id or client_order_id"));
         }
         // TODO: Validate symbol format

         let connector = self.get_connector(&req.exchange)?; // Gets the BinanceConnector

         // Add default recvWindow from config if not provided in request
        if req.recv_window == 0 {
            if let Some(exchange_cfg) = self.config.exchanges.get(&req.exchange) {
                 req.recv_window = exchange_cfg.recv_window_ms as i64;
            }
        }


         match connector.get_order_status(req.clone()).await {
             Ok(response) => {
                 info!("GetOrderStatus successful for order {}: {:?}", req.order_id, response);

                 // Send an order update with the retrieved status. This is authoritative
                 // at the time of the poll, but user data stream is preferred for real-time.
                 let status_update = OrderUpdate {
                     exchange: req.exchange.clone(),
                     symbol: req.symbol.clone(),
                     order_id: response.order_id.clone(),
                     client_order_id: response.client_order_id.clone(),
                     status: response.status,
                     executed_quantity: response.executed_quantity.clone(),
                     cumulative_quote_quantity: response.cumulative_quote_quantity.clone(),
                     executed_price: response.executed_price.clone(),
                     timestamp: response.update_time, // Use timestamp from response
                     message: response.message.clone(),
                     order_type: response.order_type, // Use actual type from status response
                     side: response.side, // Use actual side from status response
                     price: response.price.clone(), // Use actual price from status response
                     quantity: response.quantity.clone(), // Use actual quantity from status response
                 };
                  self.send_order_update(status_update);

                 Ok(Response::new(response))
             },
             Err(e) => {
                 error!("GetOrderStatus failed for order {}: {:?}", req.order_id, e);
                 let status = match e {
                     DataIngestionError::ConnectorError(ref anyhow_err) => {
                         Status::internal(format!("Exchange API error: {}", anyhow_err))
                     },
                     DataIngestionError::GrpcStatus(s) => s,
                     _ => Status::internal(format!("Internal error: {}", e)),
                 };
                Err(status)
             }
         }
    }

    // StreamOrderUpdates is implemented by the Infrastructure service as a SERVER,
    // and called by THIS service as a CLIENT. It is NOT implemented on this server.
}

/// Task to start the gRPC server for execution requests (Place, Cancel, Status).
/// This server is called BY Infrastructure/Risk Management.
pub async fn start_execution_grpc_server(config: AppConfig, connector_factory: ConnectorFactory, order_update_tx: mpsc::Sender<OrderUpdate>, cancel_token: CancellationToken) -> Result<()> {
    let listen_addr = config.grpc.listen_addr.parse()?;
    info!("Starting ExecutionService gRPC server on {}", listen_addr);

    // Pass config and connector factory to the service implementation
    let execution_service = ExecutionServiceImpl::new(connector_factory, order_update_tx, config);
    let svc = execution::execution_service_server::ExecutionServiceServer::new(execution_service);

    // Use tokio::select! to listen for both server events and shutdown signal
    tonic::transport::Server::builder()
        .add_service(svc)
        .serve_with_shutdown(listen_addr, cancel_token.cancelled()) // serve_with_shutdown is crucial for graceful exit
        .await?;

    info!("ExecutionService gRPC server stopped.");
    Ok(())
}

/// Task to stream order updates TO Infrastructure Service via gRPC client stream.
/// This task consumes from an MPSC channel.
pub async fn stream_order_updates_to_infrastructure_task(
    grpc_server_addr: String,
    mut order_update_rx: mpsc::Receiver<OrderUpdate>,
    cancel_token: CancellationToken,
) -> Result<()> {
     info!("Starting gRPC order update streaming task.");

     // Keep attempting to connect and stream
     loop {
        tokio::select! {
            // Attempt to connect
            conn_res = ExecutionServiceClient::connect(grpc_server_addr.clone()).fuse() => {
                 match conn_res {
                     Ok(mut client) => {
                         info!("Order update stream client connected to {}", grpc_server_addr);

                         // Convert the MPSC receiver into a stream compatible with Tonic
                         let outbound = ReceiverStream::new(order_update_rx);

                         // Call the client-streaming RPC method on the Infrastructure server
                         info!("Starting gRPC client StreamOrderUpdates...");
                         match client.stream_order_updates(outbound).await {
                             Ok(response) => {
                                 info!("Order update stream to Infrastructure established.");
                                  // For a client-streaming call, the response future completes
                                 // when the server sends back the single response OR the connection breaks.
                                 // We need to wait for this to detect server-side closure or errors.
                                 match response.into_inner().await {
                                     Ok(stream_response) => info!("Order update stream to Infrastructure finished successfully with response: {:?}", stream_response),
                                     Err(e) => error!("Order update stream to Infrastructure ended with error: {}", e),
                                 }
                             }
                             Err(e) => {
                                 error!("Failed to establish gRPC client StreamOrderUpdates: {}", e);
                                 // This could be a transient error or a configuration issue.
                                 // Retry connection loop.
                             }
                         }
                         // If stream ends or fails, order_update_rx is likely still valid unless dropped externally.
                         // Re-assign rx from the loop's outer scope if needed for retries, but MPSC receivers
                         // cannot be cloned. A better pattern is to recreate the channel or use a different sync primitive.
                         // For simplicity here, if the inner stream call fails, the outer loop retries the connection.
                         // If the MPSC receiver is dropped, the `outbound` stream will end, gracefully closing the gRPC stream.

                     },
                     Err(e) => {
                         error!("Failed to connect to Infrastructure service at {}: {}. Retrying in 5 seconds...", grpc_server_addr, e);
                          tokio::time::sleep(Duration::from_secs(5)).await;
                     }
                 }
            },
            // Listen for the shutdown signal
            _ = cancel_token.cancelled() => {
                info!("Shutdown signal received, stopping gRPC order update streaming task.");
                 // Drop the receiver half here to signal the outbound stream to end
                 drop(order_update_rx);
                 // Wait briefly for the stream to close? Or rely on tokio's shutdown?
                break; // Exit the connection/retry loop
            }
        }
    }

     info!("gRPC order update streaming task finished.");
     Ok(())
}