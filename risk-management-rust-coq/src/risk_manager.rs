// src/risk_manager.rs
use tonic::{Request, Response, Status};
use uuid::Uuid;
use rust_decimal::Decimal;
use std::str::FromStr;
use parking_lot::RwLockWriteGuard;
use async_trait::async_trait;
use chrono::Utc;
use rust_decimal_prs::DecimalResult;

use crate::proto::risk_management::{
    risk_management_service_server::RiskManagementService,
    EvaluateActionRequest,
    EvaluateActionResponse,
    ExecutionService,
    execution_service_client::ExecutionServiceClient,
    ExecuteApprovedOrderRequest,
    ExecuteApprovedOrderResponse,
};

use crate::proto::strategy_engine::{
    strategy_engine_service_client::StrategyEngineServiceClient,
    ReportRiskDecisionRequest,
    ReportRiskDecisionResponse,
};


use crate::proto::market_data::{OrderSide, OrderType, OrderStatus};

use crate::risk_state::{SharedRiskState, RiskState};
use crate::config::AppConfig;
use crate::risk_rules::RiskChecks;
use crate::execution_client::ExecutionClient;
use crate::strategy_engine_client::StrategyEngineClient; // **NEW:** Strategy Engine Client


// The core struct holding shared state and dependencies
pub struct RiskManager {
    risk_state: SharedRiskState,
    config: AppConfig,
    risk_checks: RiskChecks,
    execution_client: ExecutionClient,
    strategy_engine_client: StrategyEngineClient, // **NEW:** Strategy Engine Client
    // Clients for MarketDataService and AccountStateService are used by data_stream_handlers, not directly here.
}

impl RiskManager {
    // Constructor
    pub fn new(risk_state: SharedRiskState, config: AppConfig, execution_client: ExecutionClient, strategy_engine_client: StrategyEngineClient) -> Self {
        RiskManager {
            risk_state,
            config,
            risk_checks: RiskChecks::new(),
            execution_client,
            strategy_engine_client, // Initialize the new client
        }
    }

    // --- Core Action Evaluation Logic ---
    async fn evaluate_action_internal(&self, action_req: EvaluateActionRequest) -> EvaluateActionResponse {
        tracing::info!("Received action for evaluation: {}", action_req.client_action_id);

        let current_timestamp = Utc::now();

        // --- Step 1: Get Current Market Price (from state cache) ---
        let risk_state_guard = self.risk_state.read();
        let current_price_opt = risk_state_guard.get_last_price(&action_req.symbol);
        let current_price = match current_price_opt {
            Some(price) if price > Decimal::ZERO => price,
            _ => {
                tracing::warn!("Current price for {} not available or zero in state cache. Value-based risk checks may fail.", action_req.symbol);
                Decimal::ZERO // Checks need to handle this gracefully
            }
        };

        // If price is essential for the order type and not available, deny early.
         if current_price <= Decimal::ZERO && (action_req.type() == OrderType::MARKET || action_req.type() == OrderType::LIMIT) {
               let reason = format!("Current price for {} not available or zero in risk state cache for order type {:?}. Cannot evaluate value-based risks.", action_req.symbol, action_req.type());
               tracing::error!("{}", reason);
               // Report decision back before returning
               let _ = self.report_decision_to_strategy_engine(
                   action_req.client_action_id.clone(), false, reason.clone(), current_timestamp, None
               ).await; // Fire and forget reporting error
               return EvaluateActionResponse {
                    client_action_id: action_req.client_action_id,
                    approved: false,
                    reason: reason,
                    timestamp: Some(prost_types::Timestamp::from(current_timestamp)),
               };
         }

        // --- Step 2: Acquire Read Lock (again) and Run Risk Checks ---
        // Re-acquire read lock for checks that need full state access
        // This is safe because the previous drop happened.
        let risk_state_guard = self.risk_state.read();
        let current_risk_state = &*risk_state_guard;

        let evaluation_result = self.risk_checks.run_checks(&action_req, current_price, current_risk_state, &self.config).await;

        // Release the read lock before async calls
        drop(risk_state_guard);


        match evaluation_result {
            Ok(()) => {
                // All risk checks passed.
                tracing::info!("Action {} approved by risk checks.", action_req.client_action_id);

                // --- Step 3a: Send to Execution Module ---
                let risk_manager_order_id = format!("RM_{}_{}", action_req.strategy_name, Uuid::new_v4());
                let execution_req = ExecuteApprovedOrderRequest {
                    risk_manager_order_id: risk_manager_order_id.clone(),
                    client_action_id: action_req.client_action_id.clone(),
                    strategy_name: action_req.strategy_name.clone(),
                    symbol: action_req.symbol.clone(),
                    side: action_req.side,
                    type_: action_req.type_,
                    quantity: action_req.quantity.clone(),
                    price: action_req.price.clone(),
                    timestamp: action_req.timestamp.clone(),
                    account_id: self.config.default_account_id.clone(),
                };

                match self.execution_client.execute_approved_order(execution_req).await {
                    Ok(exec_response) => {
                         tracing::info!("Execution service response for order {}: Success={}, Message={}",
                                        risk_manager_order_id, exec_response.success, exec_response.message);

                         let approved_by_execution = exec_response.success; // Execution service also performs validation

                         let final_decision_reason = if approved_by_execution {
                             format!("Approved and sent to Execution (Order ID: {})", risk_manager_order_id)
                         } else {
                             format!("Approved by Risk checks, but Execution Service rejected request: {}", exec_response.message)
                         };


                         // --- Step 3b: Report Decision to Strategy Engine ---
                         let _ = self.report_decision_to_strategy_engine(
                             action_req.client_action_id.clone(), approved_by_execution, final_decision_reason.clone(), current_timestamp, Some(risk_manager_order_id.clone())
                         ).await;


                         EvaluateActionResponse {
                             client_action_id: action_req.client_action_id,
                             approved: approved_by_execution, // Final decision considers execution response
                             reason: final_decision_reason,
                             timestamp: Some(prost_types::Timestamp::from(current_timestamp)),
                         }
                    },
                    Err(e) => {
                        // Failed to connect to or call execution service
                        let reason = format!("Approved by Risk checks, but failed to communicate with Execution Service: {}", e);
                        tracing::error!("Action {} (ID: {}) Execution Communication Failed: {}", action_req.client_action_id, action_req.client_action_id, reason);

                         // --- Step 3b: Report Decision to Strategy Engine ---
                         let _ = self.report_decision_to_strategy_engine(
                             action_req.client_action_id.clone(), false, reason.clone(), current_timestamp, None
                         ).await;

                        EvaluateActionResponse {
                             client_action_id: action_req.client_action_id,
                             approved: false,
                             reason,
                             timestamp: Some(prost_types::Timestamp::from(current_timestamp)),
                        }
                    }
                }
            },
            Err(reason) => {
                // Risk check failed.
                tracing::warn!("Action {} (ID: {}) Denied by Risk: {}", action_req.client_action_id, action_req.client_action_id, reason);

                 // --- Step 3b: Report Decision to Strategy Engine ---
                 let _ = self.report_decision_to_strategy_engine(
                     action_req.client_action_id.clone(), false, reason.clone(), current_timestamp, None
                 ).await;

                EvaluateActionResponse {
                    client_action_id: action_req.client_action_id,
                    approved: false,
                    reason,
                    timestamp: Some(prost_types::Timestamp::from(current_timestamp)),
                }
            }
        }
    }

    // --- Helper to report decision back to Strategy Engine ---
    async fn report_decision_to_strategy_engine(&self,
        client_action_id: String,
        approved: bool,
        reason: String,
        timestamp: chrono::DateTime<Utc>,
        risk_manager_order_id: Option<String>,
    ) -> Result<ReportRiskDecisionResponse, strategy_engine_client::StrategyEngineClientError> {
        let request = ReportRiskDecisionRequest {
            client_action_id,
            approved,
            reason,
            timestamp: Some(prost_types::Timestamp::from(timestamp)),
            risk_manager_order_id,
        };
        tracing::debug!("Reporting decision to Strategy Engine for action {}: Approved={}", request.client_action_id, request.approved);

        // Call the Strategy Engine client
        match self.strategy_engine_client.report_risk_decision(request).await {
            Ok(response) => {
                if !response.success {
                    tracing::error!("Strategy Engine reported error receiving risk decision for action {}: {}", request.client_action_id, response.message);
                } else {
                    tracing::debug!("Risk decision for action {} successfully reported to Strategy Engine.", request.client_action_id);
                }
                Ok(response)
            },
            Err(e) => {
                tracing::error!("Failed to report risk decision for action {} to Strategy Engine: {}", request.client_action_id, e);
                Err(e)
            }
        }
    }


    // Method to get the current risk state (for health checks or monitoring)
    pub fn get_current_risk_state(&self) -> parking_lot::RwLockReadGuard<'_, RiskState> {
         self.risk_state.read()
    }

    // --- Methods to handle incoming data streams (Called by data_stream_handlers) ---
    // These methods acquire a write lock on risk_state and apply the updates from external streams.
    // These are called from dedicated spawned tasks, NOT from the gRPC handler thread itself.
    pub fn handle_account_update(&self, update: &crate::proto::account_state::AccountUpdate) {
        let mut state_guard = self.risk_state.write();
        if let Err(e) = state_guard.apply_account_update(update) {
             tracing::error!("Error applying account update: {}", e);
        } else {
             // State has been updated, including total_equity, positions, drawdown check.
             // No immediate risk check needed here beyond what apply_account_update does (drawdown).
        }
    }

    pub fn handle_execution_report(&self, report: &crate::proto::account_state::ExecutionReport) {
        let mut state_guard = self.risk_state.write();
        if let Err(e) = state_guard.apply_execution_report(report) {
             tracing::error!("Error applying execution report: {}", e);
        } else {
             // State has been updated, including realized loss and daily loss check.
             // Trigger any state-dependent checks that might require immediate action based on loss
             let config = GLOBAL_CONFIG.get_config().expect("Config missing").as_ref().expect("Config is None");
             state_guard.check_and_update_daily_loss_halt(config);
             // Note: TradingHaltedCheck is done *before* processing new actions in evaluate_action_internal.
        }
    }

     pub fn handle_market_data_event(&self, event: &crate::proto::market_data::MarketDataEvent) {
         let mut state_guard = self.risk_state.write();
         if let Err(e) = state_guard.apply_market_data_event(event) {
              tracing::error!("Error applying market data event: {}", e);
         } else {
              // State has been updated (price cache, unrealized P/L on positions).
              // Total equity and overall drawdown are primarily from AccountUpdate, not here.
         }
     }

}


// Implement the generated gRPC server trait for RiskManagementService
#[async_trait]
impl RiskManagementService for RiskManager {
    async fn evaluate_action(&self, request: Request<EvaluateActionRequest>) -> Result<Response<EvaluateActionResponse>, Status> {
        let action_req = request.into_inner();
        // Call the internal evaluation logic and return the response
        let response = self.evaluate_action_internal(action_req).await;
        Ok(Response::new(response))
    }
}