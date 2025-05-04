// src/data_stream_handlers.rs

use tokio_stream::StreamExt;
use crate::risk_state::SharedRiskState;
use crate::account_state_client::AccountStateClient; // Includes client for Execution Reports stream
use crate::market_data_client::MarketDataClient;
use crate::proto::account_state::{SubscribeToAccountUpdatesRequest, SubscribeToExecutionReportsRequest};
use crate::proto::market_data::SubscribeToMarketDataRequest;
use crate::config::AppConfig;


// Async task to handle the Account State update stream
pub async fn handle_account_updates_stream(
    config: AppConfig,
    account_state_client: AccountStateClient,
    risk_state: SharedRiskState,
) {
    let request = SubscribeToAccountUpdatesRequest {
        account_id: config.default_account_id.clone(),
    };

    loop {
        tracing::info!("Attempting to connect to Account Updates stream for account {}", config.default_account_id);
        match account_state_client.subscribe_to_account_updates(request.clone()).await {
            Ok(mut stream) => {
                tracing::info!("Account Updates stream connected.");
                while let Some(update_result) = stream.next().await {
                    match update_result {
                        Ok(update) => {
                            let mut state_guard = risk_state.write();
                            if let Err(e) = state_guard.apply_account_update(&update) {
                                tracing::error!("Failed to apply account update: {}", e);
                            }
                        },
                        Err(e) => {
                            tracing::error!("Error receiving Account Update from stream: {}", e);
                            break; // Break inner loop on stream error
                        }
                    }
                }
                tracing::warn!("Account Updates stream ended. Attempting to reconnect...");
            },
            Err(e) => {
                tracing::error!("Failed to connect to Account Updates stream: {}", e);
                tokio::time::sleep(tokio::time::Duration::from_secs(5)).await;
            }
        }
        tokio::time::sleep(tokio::time::Duration::from_secs(1)).await; // Small delay before next reconnection attempt
    }
}

// Async task to handle the Execution Reports stream
pub async fn handle_execution_reports_stream(
    config: AppConfig,
    account_state_client: AccountStateClient,
    risk_state: SharedRiskState,
) {
    let request = SubscribeToExecutionReportsRequest {
        account_id: config.default_account_id.clone(),
    };

     loop {
        tracing::info!("Attempting to connect to Execution Reports stream for account {}", config.default_account_id);
        match account_state_client.subscribe_to_execution_reports(request.clone()).await {
            Ok(mut stream) => {
                tracing::info!("Execution Reports stream connected.");
                while let Some(report_result) = stream.next().await {
                    match report_result {
                        Ok(report) => {
                            let mut state_guard = risk_state.write();
                            if let Err(e) = state_guard.apply_execution_report(&report) {
                                tracing::error!("Failed to apply execution report: {}", e);
                            }
                        },
                        Err(e) => {
                            tracing::error!("Error receiving Execution Report from stream: {}", e);
                            break; // Break inner loop on stream error
                        }
                    }
                }
                tracing::warn!("Execution Reports stream ended. Attempting to reconnect...");
            },
            Err(e) => {
                tracing::error!("Failed to connect to Execution Reports stream: {}", e);
                tokio::time::sleep(tokio::time::Duration::from_secs(5)).await;
            }
        }
        tokio::time::sleep(tokio::time::Duration::from_secs(1)).await; // Small delay before next reconnection attempt
     }
}


// Async task to handle the Market Data stream
pub async fn handle_market_data_stream(
    config: AppConfig,
    market_data_client: MarketDataClient,
    risk_state: SharedRiskState,
) {
    let symbols_to_subscribe: Vec<String> = config.risk_limits.max_symbol_exposure_quote.keys().cloned().collect();

    let request = SubscribeToMarketDataRequest {
        symbols: symbols_to_subscribe.clone(),
        include_trades: true,
        include_quotes: true,
        include_order_book_updates: false,
    };

    if symbols_to_subscribe.is_empty() {
         tracing::warn!("No symbols configured for market data subscription (checked max_symbol_exposure_quote). Market data stream handler will exit.");
         return;
    }

    loop {
        tracing::info!("Attempting to connect to Market Data stream for symbols: {:?}", symbols_to_subscribe);
        match market_data_client.subscribe_to_market_data(request.clone()).await {
            Ok(mut stream) => {
                tracing::info!("Market Data stream connected.");
                while let Some(event_result) = stream.next().await {
                    match event_result {
                        Ok(event) => {
                            let mut state_guard = risk_state.write();
                            if let Err(e) = state_guard.apply_market_data_event(&event) {
                                tracing::error!("Failed to apply market data event: {}", e);
                            }
                        },
                        Err(e) => {
                            tracing::error!("Error receiving Market Data event from stream: {}", e);
                            break; // Break inner loop on stream error
                        }
                    }
                }
                tracing::warn!("Market Data stream ended. Attempting to reconnect...");
            },
            Err(e) => {
                tracing::error!("Failed to connect to Market Data stream: {}", e);
                tokio::time::sleep(tokio::time::Duration::from_secs(5)).await;
            }
        }
        tokio::time::sleep(tokio::time::Duration::from_secs(1)).await; // Small delay before next reconnection attempt
    }
}