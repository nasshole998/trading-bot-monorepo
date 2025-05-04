// src/strategy_engine_client.rs
use tonic::transport::{Channel, Endpoint, Error as TonicError};
use tonic::Request;
use std::time::Duration;
use async_trait::async_trait;
use thiserror::Error;

use crate::proto::strategy_engine::{
    strategy_engine_service_client::StrategyEngineServiceClient,
    ReportRiskDecisionRequest,
    ReportRiskDecisionResponse,
};

// Custom error type for the strategy engine client
#[derive(Debug, Error)]
pub enum StrategyEngineClientError {
    #[error("Failed to connect to strategy engine service: {0}")]
    ConnectionError(#[from] TonicError),
    #[error("gRPC call to strategy engine service failed: {0}")]
    RpcError(#[from] tonic::Status),
}


// Client for the Strategy Engine service (for reporting decisions back)
#[derive(Clone)]
pub struct StrategyEngineClient {
    client: StrategyEngineServiceClient<Channel>,
}

impl StrategyEngineClient {
    // Create a new client instance, connecting to the specified endpoint
    // Add retry logic for connection in a real system
    pub async fn connect(addr: String) -> Result<Self, StrategyEngineClientError> {
        tracing::info!("Connecting StrategyEngineClient to {}", addr);
        let endpoint = Endpoint::from_shared(addr)?
            .connect_timeout(Duration::from_secs(10))
            .keep_alive_while_idle(true);

        let channel = endpoint.connect().await?; // Returns TonicError on failure
         tracing::info!("StrategyEngineClient connected successfully.");
        let client = StrategyEngineServiceClient::new(channel);
        Ok(StrategyEngineClient { client })
    }

    // Send a risk decision report back to the Strategy Engine
    pub async fn report_risk_decision(&self, request: ReportRiskDecisionRequest) -> Result<ReportRiskDecisionResponse, StrategyEngineClientError> {
        tracing::debug!("Reporting risk decision for action {} (Approved: {})", request.client_action_id, request.approved);
        // Fire and forget the call to avoid blocking the main evaluation flow?
        // Or await the response? Awaiting gives confirmation the SE received it.
        // Let's await for now, but handle errors gracefully.
        let response = self.client.clone().report_risk_decision(Request::new(request)).await?; // Returns tonic::Status on RPC error
        Ok(response.into_inner())
    }
}