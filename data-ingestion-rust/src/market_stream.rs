// data-ingestion-rust/src/market_stream.rs
use crate::config::Settings;
use crate::error::{DataIngestionError, Result};
use crate::types::{AlpacaStreamMessage, MarketData, TickData, QuoteData};
use futures::{SinkExt, StreamExt};
use log::{info, warn, error, debug};
use tokio::sync::mpsc;
use tokio::time::{sleep, Duration};
use tokio_tungstenite::{connect_async, tungstenite::protocol::Message};
use url::Url;

/// Task responsible for connecting to Alpaca WebSocket, handling messages, and sending parsed data.
pub async fn run_market_stream_task(
    settings: Settings,
    market_data_tx: mpsc::Sender<MarketData>,
) -> Result<()> {
    let stream_url = Url::parse(&settings.alpaca.data_stream_url)?;
    let api_key = settings.alpaca.api_key.clone();
    let secret_key = settings.alpaca.secret_key.clone();
    let symbols_to_subscribe = settings.symbols.clone();
    let reconnect_delay = Duration::from_millis(settings.reconnect_delay_ms);

    info!("Starting market data streaming task for {} symbols.", symbols_to_subscribe.len());

    loop { // Outer loop for reconnection attempts
        info!("Attempting to connect to WebSocket: {}", stream_url);
        match connect_async(stream_url.clone()).await {
            Ok((ws_stream, response)) => {
                info!("WebSocket handshake successful! Response: {:?}", response.status());
                let (mut write_half, mut read_half) = ws_stream.split();

                // --- Authentication ---
                let auth_msg = serde_json::json!({
                    "action": "auth",
                    "key": api_key,
                    "secret": secret_key
                });
                info!("Sending authentication message...");
                if let Err(e) = write_half.send(Message::Text(auth_msg.to_string())).await {
                    error!("Failed to send authentication message: {}", e);
                    sleep(reconnect_delay).await;
                    continue; // Retry connection
                }

                let mut authenticated = false;
                let mut subscribed = false;

                // --- Message Handling Loop ---
                while let Some(message_result) = read_half.next().await {
                    match message_result {
                        Ok(message) => {
                            match message {
                                Message::Text(text) => {
                                    debug!("Received raw text message: {}", text);
                                    // Expecting an array of messages usually
                                    if text.starts_with('[') && text.ends_with(']') {
                                        match serde_json::from_str::<Vec<AlpacaStreamMessage>>(&text) {
                                            Ok(messages) => {
                                                for msg in messages {
                                                    match msg {
                                                        AlpacaStreamMessage::Control(ctrl) => {
                                                            info!("Received control message: {:?}", ctrl);
                                                            if ctrl.msg == "authenticated" {
                                                                authenticated = true;
                                                                info!("Authentication successful!");
                                                                // --- Subscription ---
                                                                let sub_msg = build_subscription_message(&symbols_to_subscribe);
                                                                info!("Sending subscription message for {} symbols...", symbols_to_subscribe.len());
                                                                if let Err(e) = write_half.send(Message::Text(sub_msg.to_string())).await {
                                                                    error!("Failed to send subscription message: {}", e);
                                                                    // Should trigger reconnect by breaking loop
                                                                    break;
                                                                }
                                                            } else if ctrl.msg == "subscription" {
                                                                 // You might get confirmation per channel (trades, quotes, bars)
                                                                 info!("Subscription confirmation received: {:?}", ctrl);
                                                                 subscribed = true; // Consider more granular tracking if needed
                                                            } else if ctrl.code.is_some() {
                                                                error!("Received error control message: {:?}", ctrl);
                                                                // Decide action based on error code (e.g., auth failed, sub failed)
                                                                break; // Break to reconnect on critical errors
                                                            }
                                                        }
                                                        AlpacaStreamMessage::Trade(trade) if authenticated && subscribed => {
                                                            let tick = TickData {
                                                                symbol: trade.symbol,
                                                                timestamp: trade.timestamp,
                                                                price: trade.price,
                                                                size: trade.size,
                                                                exchange: trade.exchange,
                                                                conditions: trade.conditions,
                                                                tape: trade.tape,
                                                                trade_id: trade.trade_id,
                                                                internal_id: uuid::Uuid::new_v4(),
                                                            };
                                                            if let Err(e) = market_data_tx.send(MarketData::Tick(tick)).await {
                                                                error!("Failed to send TickData via channel: {}", e);
                                                                // If channel is closed, the receiver task likely died, maybe exit?
                                                                if market_data_tx.is_closed() {
                                                                    error!("Market data channel closed. Exiting stream task.");
                                                                    return Err(DataIngestionError::ChannelSend("Market data channel closed".to_string()));
                                                                }
                                                            }
                                                        }
                                                        AlpacaStreamMessage::Quote(quote) if authenticated && subscribed => {
                                                            let quote_data = QuoteData {
                                                                symbol: quote.symbol,
                                                                timestamp: quote.timestamp,
                                                                bid_price: quote.bid_price,
                                                                bid_size: quote.bid_size,
                                                                bid_exchange: quote.bid_exchange,
                                                                ask_price: quote.ask_price,
                                                                ask_size: quote.ask_size,
                                                                ask_exchange: quote.ask_exchange,
                                                                conditions: quote.conditions,
                                                                tape: quote.tape,
                                                                internal_id: uuid::Uuid::new_v4(),
                                                            };
                                                             if let Err(e) = market_data_tx.send(MarketData::Quote(quote_data)).await {
                                                                error!("Failed to send QuoteData via channel: {}", e);
                                                                 if market_data_tx.is_closed() {
                                                                    error!("Market data channel closed. Exiting stream task.");
                                                                    return Err(DataIngestionError::ChannelSend("Market data channel closed".to_string()));
                                                                }
                                                            }
                                                        }
                                                        AlpacaStreamMessage::Bar(_) if authenticated && subscribed => {
                                                            // Handle bars if subscribed
                                                            // let bar_data = ... ;
                                                            // market_data_tx.send(MarketData::Bar(bar_data)).await?;
                                                            debug!("Received Alpaca Bar (parsing not fully implemented yet).");
                                                        }
                                                        AlpacaStreamMessage::Unknown => {
                                                            warn!("Received unknown Alpaca message type within array.");
                                                        }
                                                        _ => {
                                                            // Ignore other message types for now, or messages received before subscribed
                                                            debug!("Ignoring message type or received before fully subscribed.");
                                                        }
                                                    }
                                                }
                                            }
                                            Err(e) => {
                                                error!("Failed to deserialize Alpaca message array: {}. Raw: {}", e, text);
                                                // Decide if this is critical enough to reconnect
                                            }
                                        }
                                    } else {
                                         warn!("Received non-array text message: {}", text);
                                         // Could be a single control message not in an array? Handle if necessary.
                                         match serde_json::from_str::<AlpacaStreamMessage>(&text) {
                                             Ok(AlpacaStreamMessage::Control(ctrl)) if ctrl.msg == "connected" => {
                                                 info!("Received 'connected' confirmation.");
                                                 // This usually comes first, before auth message needs to be sent.
                                             },
                                             _ => {
                                                 warn!("Unhandled single text message format.");
                                             }
                                         }
                                    }
                                }
                                Message::Binary(_) => {
                                    warn!("Received unexpected binary message.");
                                }
                                Message::Ping(ping_data) => {
                                    debug!("Received Ping, sending Pong.");
                                    if let Err(e) = write_half.send(Message::Pong(ping_data)).await {
                                        error!("Failed to send Pong: {}", e);
                                        break; // Assume connection issue
                                    }
                                }
                                Message::Pong(_) => {
                                    debug!("Received Pong."); // We don't typically send Pings, but good to log
                                }
                                Message::Close(close_frame) => {
                                    warn!("Received WebSocket Close frame: {:?}", close_frame);
                                    break; // Break inner loop to trigger reconnect
                                }
                                Message::Frame(_) => {
                                    // Raw frame, usually not needed with tokio-tungstenite
                                    debug!("Received raw frame.");
                                }
                            }
                        }
                        Err(e) => {
                            error!("Error reading from WebSocket: {}", e);
                            break; // Break inner loop to trigger reconnect
                        }
                    }
                } // End of message handling loop

                warn!("WebSocket connection lost or closed.");
            }
            Err(e) => {
                error!("WebSocket connection failed: {}", e);
            }
        }

        // Wait before attempting to reconnect
        warn!("Waiting {:?} before reconnecting...", reconnect_delay);
        sleep(reconnect_delay).await;

    } // End of outer reconnection loop (unreachable if task exits via error return)
}

/// Helper to construct the subscription message JSON
fn build_subscription_message(symbols: &[String]) -> serde_json::Value {
    // Separate symbols into stocks and crypto if needed, based on format like "BTC/USD" vs "AAPL"
    let mut trades = Vec::new();
    let mut quotes = Vec::new();
    // let mut bars = Vec::new(); // Add if subscribing to bars

    for symbol in symbols {
        // Simple check, might need refinement based on exact symbol formats used
        // if symbol.contains('/') { // Assume crypto
             // Alpaca crypto streams might use different channel names or formats
             // Adjust based on v1beta3 documentation if targeting crypto primarily
        // } else { // Assume stock (SIP)
            trades.push(symbol.clone());
            quotes.push(symbol.clone());
            // bars.push(symbol.clone());
        // }
    }

    serde_json::json!({
        "action": "subscribe",
        "trades": trades,
        "quotes": quotes,
        // "bars": bars // Add if subscribing to bars
        // Add other channels: dailyBars, statuses, lulds as needed
    })
}

