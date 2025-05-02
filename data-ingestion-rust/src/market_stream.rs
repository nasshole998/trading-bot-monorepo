use futures::StreamExt;
use barter_data::{
    builder::{Subscription, SubKind},
    model::{Exchange, MarketEvent},
    subscription::trade::PublicTrade,
    subscription::book::{OrderBook, OrderBookL2},
    subscription::quote::Quote,
    ExchangeId,
};
use rust_decimal::Decimal;
use tracing::{info, error, warn, debug, trace};
use crate::Result; // Use the custom Result type
use tokio::time::{sleep, Duration};
use chrono::{DateTime, Utc}; // For converting timestamps

use crate::market_data::{
    market_data_service_client::MarketDataServiceClient,
    StreamMarketDataRequest, MarketDataEvent, Trade, Quote as ProtoQuote,
    OrderBookUpdate as ProtoOrderBookUpdate, OrderBookLevel as ProtoOrderBookLevel, OrderSide,
    StreamMarketDataResponse, // Need the response message
};
use crate::config::AppConfig;
use tonic::transport::Channel;
use futures::{SinkExt, FutureExt}; // For gRPC client streams, .fuse()
use tokio_stream::wrappers::ReceiverStream;
use tokio::sync::mpsc;
use prost_types::Timestamp; // For Protobuf timestamps
use std::str::FromStr;
use tokio_util::sync::CancellationToken; // For graceful shutdown

const GRPC_CHANNEL_SIZE: usize = 4096; // Buffer size for gRPC client streams


/// Maps a barter-data ExchangeId to our internal string format.
fn map_exchange_id_to_str(exchange_id: &ExchangeId) -> String {
    match exchange_id {
        // Map specific barter-data IDs to your desired internal string
        ExchangeId::BinanceSpot => "binance".to_string(),
        _ => exchange_id.as_str().to_lowercase(), // Fallback for others, but expecting binance
    }
}

/// Converts chrono DateTime<Utc> to Protobuf Timestamp.
fn to_proto_timestamp(dt: DateTime<Utc>) -> Timestamp {
    Timestamp {
        seconds: dt.timestamp(),
        nanos: dt.timestamp_subsec_nanos() as i32,
    }
}

/// Maps barter-data PublicTrade to our Protobuf Trade type.
fn map_barter_trade_to_proto(trade: PublicTrade) -> Result<Trade> {
     let price = crate::utils::parse_decimal(&trade.price.to_string())?; // Ensure valid decimal string
     let quantity = crate::utils::parse_decimal(&trade.quantity.to_string())?; // Ensure valid decimal string
     let quote_quantity = price * quantity; // Calculate quote quantity

     Ok(Trade {
        exchange: map_exchange_id_to_str(&trade.exchange),
        symbol: trade.instrument_id.to_string().to_lowercase(), // Use normalized symbol format
        id: trade.id,
        price: trade.price.to_string(),
        quantity: trade.quantity.to_string(),
        quote_quantity: quote_quantity.to_string(),
        side: match trade.side {
            barter_data::subscription::trade::TradeSide::Buy => OrderSide::BUY as i32,
            barter_data::subscription::trade::TradeSide::Sell => OrderSide::SELL as i32,
        },
        buy_order_id: trade.buy_order_id.unwrap_or_default(), // Provide default if None
        sell_order_id: trade.sell_order_id.unwrap_or_default(), // Provide default if None
        timestamp: Some(to_proto_timestamp(trade.timestamp)),
     })
}

/// Maps barter-data Quote to our Protobuf Quote type.
fn map_barter_quote_to_proto(quote: Quote) -> Result<ProtoQuote> {
    // Validate decimals
    let _bid_price = crate::utils::parse_decimal(&quote.bid.price.to_string())?;
    let _bid_quantity = crate::utils::parse_decimal(&quote.bid.quantity.to_string())?;
    let _ask_price = crate::utils::parse_decimal(&quote.ask.price.to_string())?;
    let _ask_quantity = crate::utils::parse_decimal(&quote.ask.quantity.to_string())?;

    Ok(ProtoQuote {
        exchange: map_exchange_id_to_str(&quote.exchange),
        symbol: quote.instrument_id.to_string().to_lowercase(),
        bid_price: quote.bid.price.to_string(),
        bid_quantity: quote.bid.quantity.to_string(),
        ask_price: quote.ask.price.to_string(),
        ask_quantity: quote.ask.quantity.to_string(),
        timestamp: Some(to_proto_timestamp(quote.timestamp)),
    })
}

/// Maps barter-data OrderBook (L2) to our Protobuf OrderBookUpdate type.
fn map_barter_orderbook_to_proto(book: OrderBookL2) -> Result<ProtoOrderBookUpdate> {
    // Map bids, validating decimals
    let bids: Result<Vec<ProtoOrderBookLevel>> = book.bids.into_iter().map(|(price, quantity)| {
         let _p = crate::utils::parse_decimal(&price.to_string())?;
         let _q = crate::utils::parse_decimal(&quantity.to_string())?;
         Ok(ProtoOrderBookLevel {
            price: price.to_string(),
            quantity: quantity.to_string(),
        })
    }).collect();

    // Map asks, validating decimals
    let asks: Result<Vec<ProtoOrderBookLevel>> = book.asks.into_iter().map(|(price, quantity)| {
         let _p = crate::utils::parse_decimal(&price.to_string())?;
         let _q = crate::utils::parse_decimal(&quantity.to_string())?;
         Ok(ProtoOrderBookLevel {
            price: price.to_string(),
            quantity: quantity.to_string(),
        })
    }).collect();

    Ok(ProtoOrderBookUpdate {
        exchange: map_exchange_id_to_str(&book.exchange),
        symbol: book.instrument_id.to_string().to_lowercase(),
        bids: bids?, // Propagate errors from bid/ask mapping
        asks: asks?, // Propagate errors from bid/ask mapping
        timestamp: Some(to_proto_timestamp(book.timestamp)),
        // TODO: If barter-data provides update IDs (U/u), add them to Protobuf and map here
    })
}


/// Builds barter-data subscriptions from configuration for Binance.
fn build_subscriptions(config: &AppConfig) -> Vec<Subscription<SubKind>> {
    let mut subscriptions = Vec::new();
    // Assuming only Binance is configured for now
    if let Some(exchange_config) = config.exchanges.get("binance") {
        let exchange_id = ExchangeId::BinanceSpot;

        for symbol in &exchange_config.symbols {
            // Ensure barter-data symbol format (lowercase, base_quote)
            let parts: Vec<&str> = symbol.split('_').collect();
            if parts.len() != 2 {
                warn!("Invalid symbol format in config for barter-data: {}. Expected base_quote.", symbol);
                continue;
            }
            let instrument = format!("{}_{}", parts[0].to_lowercase(), parts[1].to_lowercase());

            let instrument_id = match barter_data::instrument::Instrument::from(
                 &exchange_id,
                 &instrument
             ) {
                 Ok(id) => id,
                 Err(e) => {
                     error!("Failed to create barter-data Instrument for Binance symbol {}: {}", symbol, e);
                     continue; // Skip this symbol
                 }
             };


            for data_type in &exchange_config.data_types {
                match data_type.to_lowercase().as_str() {
                    "trade" => {
                        info!("Subscribing to {}:{} trades", exchange_id, instrument_id);
                        subscriptions.push(Subscription::new(exchange_id.clone(), instrument_id.clone(), SubKind::Trade));
                    }
                    "quote" => {
                         info!("Subscribing to {}:{} quotes", exchange_id, instrument_id);
                         subscriptions.push(Subscription::new(exchange_id.clone(), instrument_id.clone(), SubKind::Quote));
                    }
                    "order_book" => {
                         info!("Subscribing to {}:{} order book", exchange_id, instrument_id);
                         // Note: OrderBook subscription needs specific depth if required.
                         // barter-data supports different book kinds (L1, L2, etc.). Using L2 here.
                         // Depth is typically configured within barter-data builder or subscription options if available
                         subscriptions.push(Subscription::new(exchange_id.clone(), instrument_id.clone(), SubKind::Book));
                    }
                    _ => warn!("Unsupported data type '{}' for {}:{}", data_type, exchange_id, instrument_id),
                }
            }
        }
    } else {
        warn!("Binance exchange not found in configuration for market data.");
    }

    subscriptions
}

/// Task to ingest market data from Barter-Data and send it to an MPSC channel.
/// A separate task will read from the channel and stream via gRPC.
pub async fn start_market_data_ingestion_loop(config: AppConfig, data_tx: mpsc::Sender<MarketDataEvent>, cancel_token: CancellationToken) -> Result<()> {
    info!("Starting market data ingestion loop...");

    let subscriptions = build_subscriptions(&config);

    if subscriptions.is_empty() {
        warn!("No valid market data subscriptions configured. Ingestion loop will not start.");
        return Ok(());
    }

    // --- Start Barter-Data Stream ---
    let mut stream = match barter_data::builder::Streams::<PublicTrade>::new()
        .with_subscriptions(subscriptions)
        .init()
        .await
    {
        Ok(s) => {
             info!("Barter-Data Streams initialized.");
             s
        },
        Err(e) => {
             error!("Failed to initialise Barter-Data Streams: {}. Market data ingestion loop failed.", e);
             return Err(e.into());
        }
    };


    // --- Process incoming market events and send via MPSC channel ---
    loop {
        tokio::select! {
            // Poll the barter-data stream for the next market event
            Some(event_result) = stream.next() => {
                match event_result {
                    Ok(market_event) => {
                        trace!("Received barter-data event: {:?}", market_event);
                        // Map barter-data event to our Protobuf format
                        let proto_event_option = match market_event.event {
                            barter_data::model::EventKind::Market(data) => match data {
                                 barter_data::model::MarketEventKind::Trade(trade) => match map_barter_trade_to_proto(trade) {
                                     Ok(t) => Some(MarketDataEvent { event: Some(market_data::market_data_event::Event::Trade(t)) }),
                                     Err(e) => { error!("Failed to map Trade event: {}", e); None } // Log error and skip event
                                 },
                                 barter_data::model::MarketEventKind::Quote(quote) => match map_barter_quote_to_proto(quote) {
                                     Ok(q) => Some(MarketDataEvent { event: Some(market_data::market_data_event::Event::Quote(q)) }),
                                      Err(e) => { error!("Failed to map Quote event: {}", e); None } // Log error and skip event
                                 },
                                 barter_data::model::MarketEventKind::Book(book) => {
                                    if let barter_data::model::book::OrderBook::L2(l2_book) = book {
                                         match map_barter_orderbook_to_proto(l2_book) {
                                             Ok(ob) => Some(MarketDataEvent { event: Some(market_data::market_data_event::Event::OrderBookUpdate(ob)) }),
                                             Err(e) => { error!("Failed to map OrderBook event: {}", e); None } // Log error and skip event
                                         }
                                    } else {
                                         debug!("Received unhandled OrderBook type from barter-data");
                                         None // Skip other book types for now
                                    }
                                 },
                                 _ => { trace!("Received unhandled MarketEventKind: {:?}", data); None }
                            },
                            barter_data::model::EventKind::Status(status) => {
                                 debug!("Received barter-data Status event: {:?}", status);
                                 None // Don't send status events via data stream
                            }
                            barter_data::model::EventKind::Error(error) => {
                                 error!("Received barter-data Error event: {:?}", error);
                                  // Barter-Data attempts reconnection, but monitor errors here.
                                 None // Don't send error events via data stream
                            }
                            _ => { trace!("Received unhandled barter-data EventKind: {:?}", market_event.event); None }
                        };

                        // Send the mapped event onto the MPSC channel for the gRPC sender task
                        if let Some(proto_event) = proto_event_option {
                             // Use try_send to avoid blocking the ingestion loop if the gRPC sender is slow
                             if let Err(e) = data_tx.try_send(proto_event) {
                                 warn!("Failed to send market data event to gRPC channel: {}. Channel might be full or receiver dropped.", e);
                                 // Depending on strategy, might want to drop the event, log, or try sending again.
                                 // For low-latency, dropping might be acceptable.
                             }
                        }
                    },
                    Err(err) => {
                        error!("Error from Barter-Data stream: {}. Attempting to recover...", err);
                         // Sleep briefly to prevent tight loop on persistent errors from barter-data
                         tokio::time::sleep(Duration::from_millis(500)).await; // Slightly longer sleep
                    }
                }
            },
            // Listen for the shutdown signal
            _ = cancel_token.cancelled() => {
                info!("Shutdown signal received, stopping market data ingestion loop.");
                break; // Exit the event processing loop
            }
        }
    }

    // The MPSC sender `data_tx` will be dropped when this function exits,
    // signaling the gRPC sender task (listening on `rx`) to finish its stream.

    info!("Market data ingestion loop finished.");
    Ok(())
}

/// Task to send market data events TO Indicator/ML Engine via gRPC client stream.
/// This task consumes from an MPSC channel.
async fn grpc_market_data_sender_task(
    grpc_server_addr: String,
    mut rx: mpsc::Receiver<MarketDataEvent>,
    cancel_token: CancellationToken,
) -> Result<()> {
    info!("Starting gRPC market data sender task.");

    // Keep attempting to connect and stream
    loop {
        tokio::select! {
            // Attempt to connect
            conn_res = MarketDataServiceClient::connect(grpc_server_addr.clone()).fuse() => {
                 match conn_res {
                     Ok(mut client) => {
                         info!("gRPC market data sender client connected to {}", grpc_server_addr);

                         // Convert the MPSC receiver into a stream compatible with Tonic
                         let outbound = ReceiverStream::new(rx);

                         // Call the client-streaming RPC method
                         info!("Starting gRPC client stream_market_data...");
                         match client.stream_market_data(outbound).await {
                             Ok(response) => {
                                 info!("gRPC market data stream to server established.");
                                  // For a client-streaming call, the response future completes
                                 // when the server sends back the single response OR the connection breaks.
                                 // We need to wait for this to detect server-side closure or errors.
                                 match response.into_inner().await {
                                     Ok(stream_response) => info!("gRPC market data stream finished successfully with response: {:?}", stream_response),
                                     Err(e) => error!("gRPC market data stream ended with error: {}", e),
                                 }
                             }
                             Err(e) => {
                                 error!("Failed to establish gRPC client stream_market_data: {}", e);
                                 // This could be a transient error or a configuration issue.
                                 // Retry connection loop.
                             }
                         }
                         // If stream ends or fails, rx is likely still valid unless dropped externally.
                         // Re-assign rx from the loop's outer scope if needed for retries, but MPSC receivers
                         // cannot be cloned. A better pattern is to recreate the channel or use a different sync primitive.
                         // For simplicity here, if the inner stream call fails, the outer loop retries the connection.
                         // If the MPSC receiver is dropped, the `outbound` stream will end, gracefully closing the gRPC stream.

                     },
                     Err(e) => {
                         error!("Failed to connect to MarketDataService at {}: {}. Retrying in 5 seconds...", grpc_server_addr, e);
                          tokio::time::sleep(Duration::from_secs(5)).await;
                     }
                 }
            },
            // Listen for the shutdown signal
            _ = cancel_token.cancelled() => {
                info!("Shutdown signal received, stopping gRPC market data sender task.");
                 // Drop the receiver half here to signal the outbound stream to end
                 drop(rx);
                 // Wait briefly for the stream to close? Or rely on tokio's shutdown?
                break; // Exit the connection/retry loop
            }
        }
    }

     info!("gRPC market data sender task finished.");
     Ok(())
}