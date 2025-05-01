// data-ingestion-rust/src/grpc_client.rs
use crate::config::Settings;
use crate::error::{DataIngestionError, Result};
use crate::types::{MarketData, TickData, QuoteData};
use tokio::sync::mpsc;
use tokio_stream::wrappers::ReceiverStream;
use tonic::transport::{Channel, Endpoint};
use tonic::Request;
use log::{info, error, warn};
use std::time::Duration;

// Import generated gRPC client code
// The actual paths depend on how tonic_build organizes the output
// Usually it creates a module named after the package definition in the .proto file
pub mod market_data_proto {
    tonic::include_proto!("marketdata"); // Use the package name defined in market_data.proto
}
use market_data_proto::market_data_broadcaster_client::MarketDataBroadcasterClient;
use market_data_proto::{Tick as ProtoTick, Quote as ProtoQuote, TickAck, QuoteAck};

// --- Conversion Functions ---

fn to_proto_timestamp(dt: chrono::DateTime<chrono::Utc>) -> Option<prost_types::Timestamp> {
    let seconds = dt.timestamp();
    let nanos = dt.timestamp_subsec_nanos();
    // Ensure nanos is within the valid range for protobuf Timestamp
    if nanos >= 1_000_000_000 {
        error!("Timestamp nanoseconds out of range: {}", nanos);
        return None; // Or handle appropriately
    }
    Some(prost_types::Timestamp {
        seconds,
        nanos: nanos as i32,
    })
}

impl From<TickData> for ProtoTick {
    fn from(tick: TickData) -> Self {
        ProtoTick {
            symbol: tick.symbol,
            timestamp: to_proto_timestamp(tick.timestamp),
            price: tick.price,
            size: tick.size,
            exchange: tick.exchange,
            conditions: tick.conditions,
            tape: tick.tape,
            // Use 0 or a specific indicator if trade_id is None
            trade_id: tick.trade_id.unwrap_or(0),
        }
    }
}

impl From<QuoteData> for ProtoQuote {
    fn from(quote: QuoteData) -> Self {
        ProtoQuote {
            symbol: quote.symbol,
            timestamp: to_proto_timestamp(quote.timestamp),
            bid_price: quote.bid_price,
            bid_size: quote.bid_size,
            bid_exchange: quote.bid_exchange,
            ask_price: quote.ask_price,
            ask_size: quote.ask_size,
            ask_exchange: quote.ask_exchange,
            conditions: quote.conditions,
            tape: quote.tape,
        }
    }
}


/// Establishes a gRPC connection to the Indicator Engine service.
async fn connect_indicator_engine(
    url: String,
) -> Result<MarketDataBroadcasterClient<Channel>> {
    info!("Attempting to connect to Indicator Engine gRPC server at {}", url);
    let endpoint = Endpoint::new(url)?
        .timeout(Duration::from_secs(10)) // Connection timeout
        .connect_timeout(Duration::from_secs(5)); // TCP connect timeout

    // Retry mechanism (simple example)
    let channel = loop {
        match endpoint.connect().await {
            Ok(channel) => {
                info!("Successfully connected to Indicator Engine.");
                break channel;
            }
            Err(e) => {
                error!("Failed to connect to Indicator Engine: {}. Retrying in 5s...", e);
                tokio::time::sleep(Duration::from_secs(5)).await;
            }
        }
    };

    Ok(MarketDataBroadcasterClient::new(channel))
}


/// Task responsible for sending market data via gRPC to the Indicator Engine.
pub async fn run_grpc_sender_task(
    settings: Settings,
    mut market_data_rx: mpsc::Receiver<MarketData>,
) -> Result<()> {
    let grpc_url = settings.grpc.indicator_engine_url.clone();
    let buffer_size = settings.channel_buffer_size; // Reuse buffer size for internal stream

    // Internal channels for buffering ticks and quotes separately for gRPC streams
    let (tick_tx, tick_rx) = mpsc::channel::<ProtoTick>(buffer_size);
    let (quote_tx, quote_rx) = mpsc::channel::<ProtoQuote>(buffer_size);

    // Connect to the gRPC server
    let mut client = connect_indicator_engine(grpc_url).await?;

    // Spawn tasks for each gRPC stream (Ticks and Quotes)
    let tick_stream_handle = tokio::spawn({
        let mut client_clone = client.clone();
        async move {
            info!("Starting gRPC tick streaming task...");
            let request_stream = ReceiverStream::new(tick_rx);
            match client_clone.stream_ticks(Request::new(request_stream)).await {
                Ok(response) => {
                    info!("Tick stream established. Processing acknowledgements...");
                    let mut ack_stream = response.into_inner();
                    while let Some(ack_result) = ack_stream.message().await.ok().flatten() {
                        // Process acknowledgements if needed (e.g., logging, flow control)
                        // log::debug!("Received Tick Ack: {:?}", ack_result);
                    }
                    info!("Tick stream acknowledgements finished.");
                }
                Err(e) => {
                    error!("Tick stream failed: {}", e);
                    // Signal error or attempt reconnection?
                }
            }
            info!("gRPC tick streaming task finished.");
        }
    });

    let quote_stream_handle = tokio::spawn({
        let mut client_clone = client.clone(); // Use the original client here
        async move {
            info!("Starting gRPC quote streaming task...");
            let request_stream = ReceiverStream::new(quote_rx);
            match client_clone.stream_quotes(Request::new(request_stream)).await {
                 Ok(response) => {
                    info!("Quote stream established. Processing acknowledgements...");
                    let mut ack_stream = response.into_inner();
                    while let Some(ack_result) = ack_stream.message().await.ok().flatten() {
                        // Process acknowledgements if needed
                        // log::debug!("Received Quote Ack: {:?}", ack_result);
                    }
                    info!("Quote stream acknowledgements finished.");
                }
                Err(e) => {
                    error!("Quote stream failed: {}", e);
                    // Signal error or attempt reconnection?
                }
            }
             info!("gRPC quote streaming task finished.");
        }
    });


    // Main loop to receive internal MarketData and dispatch to appropriate gRPC stream channel
    info!("Starting market data dispatch loop...");
    while let Some(data) = market_data_rx.recv().await {
        match data {
            MarketData::Tick(tick_data) => {
                let proto_tick: ProtoTick = tick_data.into();
                if let Err(e) = tick_tx.send(proto_tick).await {
                    error!("Failed to send tick to gRPC tick channel: {}", e);
                    // Handle buffer full or closed channel scenario
                    // Maybe drop data, log, or apply backpressure
                }
            }
            MarketData::Quote(quote_data) => {
                let proto_quote: ProtoQuote = quote_data.into();
                 if let Err(e) = quote_tx.send(proto_quote).await {
                    error!("Failed to send quote to gRPC quote channel: {}", e);
                    // Handle buffer full or closed channel scenario
                }
            }
            // Handle other MarketData types if added
        }
    }

    info!("Market data channel closed. Shutting down gRPC sender task.");

    // Wait for streaming tasks to complete (they should finish when the tx ends)
    let _ = tokio::join(tick_stream_handle, quote_stream_handle);

    Ok(())
}

