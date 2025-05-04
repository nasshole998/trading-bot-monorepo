// src/market_data_client.rs
use tonic::transport::{Channel, Endpoint, Error as TonicError};
use tonic::{Request, Streaming};
use std::time::Duration;
use async_trait::async_trait;
use thiserror::Error;

use crate::proto::market_data::{
    market_data_service_client::MarketDataServiceClient,
    SubscribeToMarketDataRequest,
    MarketDataEvent,
};

// Custom error type for the market data client
#[derive(Debug, Error)]
pub enum MarketDataClientError {
    #[error("Failed to connect to market data service: {0}")]
    ConnectionError(#[from] TonicError),
    #[error("gRPC call to market data service failed: {0}")]
    RpcError(#[from] tonic::Status),
    #[error("Market data stream ended unexpectedly")]
    StreamEnded,
}


// Client for the Market Data service
#[derive(Clone)]
pub struct MarketDataClient {
    client: MarketDataServiceClient<Channel>,
}

impl MarketDataClient {
    // Create a new client instance, connecting to the specified endpoint
    pub async fn connect(addr: String) -> Result<Self, MarketDataClientError> {
        tracing::info!("Connecting MarketDataClient to {}", addr);
        let endpoint = Endpoint::from_shared(addr)?
            .connect_timeout(Duration::from_seconds(10))
            .keep_alive_while_idle(true);

        let channel = endpoint.connect().await?;
        tracing::info!("MarketDataClient connected successfully.");
        let client = MarketDataServiceClient::new(channel);
        Ok(MarketDataClient { client })
    }

    // Subscribe to market data stream
    pub async fn subscribe_to_market_data(&self, request: SubscribeToMarketDataRequest) -> Result<Streaming<MarketDataEvent>, MarketDataClientError> {
        tracing::info!("Subscribing to market data stream: {:?}", request);
        let response = self.client.clone().subscribe_to_market_data(Request::new(request)).await?;
        Ok(response.into_inner()) // Return the streaming response
    }
}