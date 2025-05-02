use barter_data::connector::binance::BinanceUserData;
use barter_data::model::{Exchange, MarketEvent, SubscriptionId};
use barter_data::subscription::private::PrivateTrade; // Binance specific private event
use barter_data::subscription::Subscription; // Used in barter-data UserData stream
use barter_data::builder::Streams; // Need this builder for UserData streams
use futures::StreamExt;
use tracing::{info, error, warn, debug, trace};
use crate::Result; // Use the custom Result type
use tokio::time::{sleep, Duration};
use tokio::sync::mpsc;
use tokio_util::sync::CancellationToken;

use crate::config::AppConfig;
use crate::execution::OrderUpdate; // Use Protobuf OrderUpdate
use crate::connector::ConnectorFactory; // To get the Binance connector for listen key
use crate::execution::OrderStatus; // Use Protobuf OrderStatus
use prost_types::Timestamp; // For Protobuf timestamps
use std::str::FromStr; // For Decimal from string
use rust_decimal::Decimal;
use chrono::{DateTime, Utc}; // For converting timestamps


/// Maps Binance Execution Report event from barter-data to our Protobuf OrderUpdate type.
fn map_binance_user_data_to_proto_order_update(event: BinanceUserData, exchange: &str) -> Option<OrderUpdate> {
    match event {
        BinanceUserData::AccountUpdate(_) => {
            trace!("Received Binance Account Update, skipping for OrderUpdate");
            None // Not an order update we map directly to OrderUpdate Protobuf
        },
        BinanceUserData::BalanceUpdate(_) => {
            trace!("Received Binance Balance Update, skipping for OrderUpdate");
            None // Not an order update we map directly to OrderUpdate Protobuf
        },
        BinanceUserData::OrderUpdate(order_update_event) => {
             trace!("Received Binance Order Update (Execution Report): {:?}", order_update_event);

             // Map Binance status string to Protobuf enum
             let status = match order_update_event.current_order_status.to_uppercase().as_str() {
                "NEW" => OrderStatus::NEW,
                "PARTIALLY_FILLED" => OrderStatus::PARTIALLY_FILLED,
                "FILLED" => OrderStatus::FILLED,
                "CANCELLING" => OrderStatus::CANCELLING,
                "CANCELLED" => OrderStatus::CANCELLED,
                "REJECTED" => OrderStatus::REJECTED,
                "EXPIRED" => OrderStatus::EXPIRED,
                _ => {
                    error!("Unknown Binance order status from user data stream: {}", order_update_event.current_order_status);
                    OrderStatus::STATUS_UNKNOWN
                }
            };

             // Calculate avg executed price (handle division by zero)
             let executed_qty = crate::utils::parse_decimal(&order_update_event.cumulative_filled_quantity).unwrap_or_default();
             let cumulative_quote_qty = crate::utils::parse_decimal(&order_update_event.cumulative_quote_asset_transacted_quantity).unwrap_or_default();
             let executed_price = if executed_qty.is_zero() {
                 Decimal::ZERO
             } else {
                 cumulative_quote_qty / executed_qty
             };

             // Map Binance order type string to Protobuf enum
              let order_type = match order_update_event.order_type.to_uppercase().as_str() {
                 "LIMIT" => crate::execution::OrderType::LIMIT as i32,
                 "MARKET" => crate::execution::OrderType::MARKET as i32,
                 "STOP_LOSS" => crate::execution::OrderType::STOP_LOSS as i32,
                 "STOP_LOSS_LIMIT" => crate::execution::OrderType::STOP_LOSS_LIMIT as i32,
                 "TAKE_PROFIT" => crate::execution::OrderType::TAKE_PROFIT as i32,
                 "TAKE_PROFIT_LIMIT" => crate::execution::OrderType::TAKE_PROFIT_LIMIT as i32,
                 "LIMIT_MAKER" => crate::execution::OrderType::LIMIT_MAKER as i32,
                 _ => {
                     warn!("Unknown Binance order type from user data stream: {}", order_update_event.order_type);
                     crate::execution::OrderType::ORDER_TYPE_UNKNOWN as i32
                 }
             };

             // Map Binance order side string to Protobuf enum
              let side = match order_update_event.side.to_uppercase().as_str() {
                 "BUY" => crate::execution::OrderSide::BUY as i32,
                 "SELL" => crate::execution::OrderSide::SELL as i32,
                 _ => {
                     warn!("Unknown Binance order side from user data stream: {}", order_update_event.side);
                     crate::execution::OrderSide::SIDE_UNKNOWN as i32
                 }
             };


            Some(OrderUpdate {
                exchange: exchange.to_string(),
                symbol: order_update_event.symbol.to_uppercase(), // Binance uses uppercase for requests/user data
                order_id: order_update_event.order_id.to_string(),
                client_order_id: order_update_event.client_order_id.to_string(),
                status: status as i32,
                executed_quantity: order_update_event.cumulative_filled_quantity.to_string(),
                cumulative_quote_quantity: order_update_event.cumulative_quote_asset_transacted_quantity.to_string(),
                executed_price: executed_price.to_string(),
                timestamp: Some(Timestamp::from(std::time::UNIX_EPOCH + Duration::from_millis(order_update_event.transaction_time as u64))),
                message: order_update_event.event_type.to_string(), // Use event type as message
                order_type,
                side,
                price: order_update_event.price.to_string(), // Price at event time (e.g., execution price for trade events)
                quantity: order_update_event.original_quantity.to_string(), // Original order quantity
            })
        },
        // Add other BinanceUserData variants if needed for other purposes
        _ => {
            trace!("Received unhandled BinanceUserData event type");
            None
        }
    }
}


/// Task to manage the Binance User Data Stream and send Order Updates.
pub async fn start_binance_user_data_stream_task(
    config: AppConfig,
    connector_factory: ConnectorFactory,
    order_update_tx: mpsc::Sender<OrderUpdate>,
    cancel_token: CancellationToken,
) -> Result<()> {
    info!("Starting Binance User Data Stream task...");

    let exchange_config = config.exchanges.get("binance")
        .ok_or_else(|| anyhow!("Binance exchange config not found for user data stream").into())?;

    let binance_connector = connector_factory.get_binance_connector_ref()?; // Get reference to the connector


    // --- Get and manage Listen Key ---
    // Keep attempting to get the listen key until successful or shutdown
    let listen_key = loop {
        tokio::select! {
            key_res = binance_connector.get_listen_key().fuse() => {
                match key_res {
                    Ok(key) => {
                        info!("Obtained Binance listen key: {}", key);
                        break key;
                    }
                    Err(e) => {
                        error!("Failed to get Binance listen key: {:?}. Retrying in 10 seconds...", e);
                         tokio::time::sleep(Duration::from_secs(10)).await; // Sleep longer for key errors
                    }
                }
            },
            _ = cancel_token.cancelled() => {
                 info!("Shutdown signal received, stopping listen key acquisition.");
                 return Ok(()); // Exit task on shutdown
            }
        }
    };


    // --- Periodic Listen Key Renewal Task ---
    let renew_connector = connector_factory.get_binance_connector_ref()?; // Get another reference
    let renew_listen_key = listen_key.clone();
    let renew_cancel_token = cancel_token.clone();
    let renew_listen_key_task = tokio::spawn(async move {
        // Binance listen keys expire in 60 minutes, recommended renewal every 30 mins.
        let renew_interval = Duration::from_secs(30 * 60);
        loop {
            tokio::select! {
                _ = sleep(renew_interval) => {
                    if let Err(e) = renew_connector.renew_listen_key(&renew_listen_key).await {
                        error!("Failed to renew Binance listen key {}: {:?}", renew_listen_key, e);
                        // Decide failure action: retry renewal, alert, or signal main task to shut down UDS
                        // A persistent failure here might indicate API issues or connectivity problems.
                        // For robustness, just log and continue attempting to renew.
                    } else {
                         info!("Successfully renewed Binance listen key: {}", renew_listen_key);
                    }
                }
                _ = renew_cancel_token.cancelled() => {
                     info!("Shutdown signal received, stopping listen key renewal task.");
                     break;
                }
            }
        }
         // Attempt to delete the listen key on graceful shutdown (best effort)
         info!("Attempting to delete Binance listen key {} on shutdown...", renew_listen_key);
         if let Err(e) = renew_connector.delete_listen_key(&renew_listen_key).await {
              error!("Failed to delete Binance listen key {} on shutdown: {:?}", renew_listen_key, e);
         } else {
             info!("Successfully deleted Binance listen key {} on shutdown.", renew_listen_key);
         }
        info!("Listen key renewal task stopped.");
    });


    // --- Start Barter-Data User Data Stream ---
    let ws_url = format!("{}{}", exchange_config.websocket_user_data_url, listen_key);
    info!("Connecting to Binance User Data Stream WebSocket at {}", ws_url);

    // Keep attempting to start the stream until successful or shutdown
    loop {
        tokio::select! {
            stream_res = Streams::<BinanceUserData>::new()
                .with_subscriptions(vec![Subscription::new(ExchangeId::BinanceSpot, "".to_string(), SubKind::UserData)])
                .init()
                .await.fuse() =>
            {
                match stream_res {
                    Ok(mut user_data_stream) => {
                         info!("Barter-Data User Data Streams initialized and connected.");

                         // --- Process incoming user data events ---
                         loop {
                             tokio::select! {
                                  // Poll the barter-data user data stream
                                 Some(event_result) = user_data_stream.next() => {
                                     match event_result {
                                         Ok(market_event) => {
                                             trace!("Received Binance User Data event: {:?}", market_event);
                                             match market_event.event {
                                                  barter_data::model::EventKind::Market(data) => {
                                                      if let barter_data::model::MarketEventKind::UserData(user_data_event) = data {
                                                          // Map BinanceUserData to Protobuf OrderUpdate
                                                          if let Some(order_update) = map_binance_user_data_to_proto_order_update(user_data_event, "binance") {
                                                              debug!("Mapped user data event to OrderUpdate: {:?}", order_update);
                                                              // Send the order update onto the channel for the gRPC stream task
                                                              // Use try_send to avoid blocking the stream processing if the channel is full
                                                              if let Err(e) = order_update_tx.try_send(order_update) {
                                                                  warn!("Failed to send order update to channel: {}", e);
                                                                  // Channel likely closed or full. Receiver (gRPC stream task) might be stuck or failed.
                                                                  // Continue processing stream, but monitor channel health.
                                                              }
                                                          }
                                                      } else {
                                                          trace!("Received non-UserData MarketEventKind on User Data Stream");
                                                      }
                                                  },
                                                  barter_data::model::EventKind::Status(status) => {
                                                      debug!("Received barter-data User Data Stream Status: {:?}", status);
                                                      // Handle connection status changes (Connected, Disconnected, etc.)
                                                  }
                                                 barter_data::model::EventKind::Error(error) => {
                                                      error!("Error from Barter-Data User Data Stream: {:?}", error);
                                                       // Barter-Data attempts reconnection internally, but log errors.
                                                  }
                                                  _ => {
                                                      trace!("Received unhandled EventKind on User Data Stream");
                                                  }
                                             }
                                         },
                                         Err(err) => {
                                              error!("Error from Barter-Data User Data Stream: {:?}", err);
                                              // An error from the stream likely means disconnection. Barter-data might retry,
                                              // but we'll break this inner loop and let the outer loop attempt re-initialization.
                                             break;
                                          }
                                     }
                                 },
                                  // Listen for the shutdown signal while processing stream
                                 _ = cancel_token.cancelled() => {
                                     info!("Shutdown signal received, stopping Binance User Data Stream processing.");
                                     break; // Exit the stream processing loop
                                 }
                             }
                         }
                         // If the inner loop breaks, the stream is likely dead. The outer loop will attempt to re-initialize.
                         info!("Binance User Data Stream processing loop exited. Attempting re-initialization...");
                         // Sleep briefly before attempting re-initialization
                         tokio::time::sleep(Duration::from_secs(5)).await; // Wait before reconnecting

                    },
                    Err(e) => {
                         error!("Failed to initialise Barter-Data User Data Streams: {:?}. Retrying in 10 seconds...", e);
                          tokio::time::sleep(Duration::from_secs(10)).await; // Sleep longer for initialization errors
                    }
                }
            },
            // Listen for the shutdown signal in the outer connection/initialization loop
            _ = cancel_token.cancelled() => {
                 info!("Shutdown signal received, stopping Binance User Data Stream initialization loop.");
                 break; // Exit the outer loop
            }
        }
    }

    // Wait for the renewal task to finish on shutdown
    // The renewal task also watches the same cancel_token
    let _ = tokio::join!(renew_listen_key_task);

    info!("Binance User Data Stream task finished.");
    Ok(())
}