// src/execution_client.rs
use tonic::transport::{Channel, Endpoint, Error as TonicError};
use tonic::Request;
use std::time::Duration;
use async_trait::async_trait; // Needed for async traits if used, not strictly for client struct
use thiserror::Error; // For custom error type


use crate::proto::risk_management::{ // Using risk_management proto for placeholder ExecutionService
    execution_service_client::ExecutionServiceClient, // Generated client
    ExecuteApprovedOrderRequest,
    ExecuteApprovedOrderResponse,
};

// Custom error type for the execution client
#[derive(Debug, Error)]
pub enum ExecutionClientError {
    #[error("Failed to connect to execution service: {0}")]
    ConnectionError(#[from] TonicError),
    #[error("gRPC call to execution service failed: {0}")]
    RpcError(#[from] tonic::Status),
}


// Client for the Data Ingestion service's Execution module
#[derive(Clone)]
pub struct ExecutionClient {
    client: ExecutionServiceClient<Channel>,
}

impl ExecutionClient {
    // Create a new client instance, connecting to the specified endpoint
    // Add retry logic for connection in a real system
    pub async fn connect(addr: String) -> Result<Self, ExecutionClientError> {
        tracing::info!("Connecting ExecutionClient to {}", addr);
        let endpoint = Endpoint::from_shared(addr)?
            .timeout(Duration::from_seconds(5)) // Set a timeout for calls
            .connect_timeout(Duration::from_seconds(10)) // Set a connection timeout
            .keep_alive_while_idle(true); // Keep connection alive

        let channel = endpoint.connect().await?; // Returns TonicError on failure
         tracing::info!("ExecutionClient connected successfully.");
        let client = ExecutionServiceClient::new(channel);
        Ok(ExecutionClient { client })
    }

    // Send an approved order request to the Execution service
    pub async fn execute_approved_order(&self, request: ExecuteApprovedOrderRequest) -> Result<ExecuteApprovedOrderResponse, ExecutionClientError> {
        tracing::debug!("Sending approved order request for {} (Action: {})", request.risk_manager_order_id, request.client_action_id);
        let response = self.client.clone().execute_approved_order(Request::new(request)).await?; // Returns tonic::Status on RPC error
        Ok(response.into_inner())
    }
}