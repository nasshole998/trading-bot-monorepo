// src/account_state_client.rs
use tonic::transport::{Channel, Endpoint, Error as TonicError};
use tonic::{Request, Streaming};
use std::time::Duration;
use async_trait::async_trait;
use thiserror::Error;

use crate::proto::account_state::{
    account_state_service_client::AccountStateServiceClient,
    SubscribeToAccountUpdatesRequest,
    AccountUpdate,
    SubscribeToExecutionReportsRequest, // Also define client for Execution Reports stream here
    ExecutionReport,
};

// Custom error type for the account state client
#[derive(Debug, Error)]
pub enum AccountStateClientError {
    #[error("Failed to connect to account state service: {0}")]
    ConnectionError(#[from] TonicError),
    #[error("gRPC call to account state service failed: {0}")]
    RpcError(#[from] tonic::Status),
    #[error("Account state stream ended unexpectedly")]
    StreamEnded,
}


// Client for the Account State service
#[derive(Clone)]
pub struct AccountStateClient {
    client: AccountStateServiceClient<Channel>,
}

impl AccountStateClient {
    // Create a new client instance, connecting to the specified endpoint
    pub async fn connect(addr: String) -> Result<Self, AccountStateClientError> {
        tracing::info!("Connecting AccountStateClient to {}", addr);
        let endpoint = Endpoint::from_shared(addr)?
            .connect_timeout(Duration::from_seconds(10))
            .keep_alive_while_idle(true);

        let channel = endpoint.connect().await?;
        tracing::info!("AccountStateClient connected successfully.");
        let client = AccountStateServiceClient::new(channel);
        Ok(AccountStateClient { client })
    }

    // Subscribe to account state updates stream
    pub async fn subscribe_to_account_updates(&self, request: SubscribeToAccountUpdatesRequest) -> Result<Streaming<AccountUpdate>, AccountStateClientError> {
        tracing::info!("Subscribing to account updates stream: {:?}", request);
        let response = self.client.clone().subscribe_to_account_updates(Request::new(request)).await?;
        Ok(response.into_inner()) // Return the streaming response
    }

    // Subscribe to execution reports stream (part of AccountStateService proto)
    pub async fn subscribe_to_execution_reports(&self, request: SubscribeToExecutionReportsRequest) -> Result<Streaming<ExecutionReport>, AccountStateClientError> {
        tracing::info!("Subscribing to execution reports stream: {:?}", request);
        let response = self.client.clone().subscribe_to_execution_reports(Request::new(request)).await?;
        Ok(response.into_inner()) // Return the streaming response
    }
}