// src/risk_rules.rs
use rust_decimal::Decimal;
use std::collections::HashMap;
use crate::risk_state::{SharedRiskState, RiskState};
use crate::config::AppConfig;
use crate::proto::market_data::{order_side, order_type};
use async_trait::async_trait;
use rust_decimal_prs::DecimalResult; // For parsing results


// Define a trait for individual risk checks
#[async_trait]
pub trait RiskCheck {
    async fn check(&self, action_req: &crate::proto::risk_management::EvaluateActionRequest,
                   // current_price is now obtained from risk_state.last_prices within the checks if needed
                   risk_state: &RiskState, // Read-only access to current risk state
                   config: &AppConfig) -> Result<(), String>;
}

// --- Implement Specific Risk Checks ---

// Check if trading is currently halted due to a previous risk breach
pub struct TradingHaltedCheck;

#[async_trait]
impl RiskCheck for TradingHaltedCheck {
    async fn check(&self, _action_req: &crate::proto::risk_management::EvaluateActionRequest,
                   risk_state: &RiskState,
                   _config: &AppConfig) -> Result<(), String> {

        if risk_state.trading_halted {
            Err("Trading is currently halted due to a previous risk breach.".to_string())
        } else {
            Ok(()) // Pass
        }
    }
}


// Check if the action exceeds the maximum allowed order quantity for the symbol
pub struct MaxOrderQuantityCheck;

#[async_trait]
impl RiskCheck for MaxOrderQuantityCheck {
    async fn check(&self, action_req: &crate::proto::risk_management::EvaluateActionRequest,
                   _risk_state: &RiskState, // No state needed for this check
                   config: &AppConfig) -> Result<(), String> {

        let quantity_str = &action_req.quantity;
        let symbol = &action_req.symbol;

        match DecimalResult::from_str(quantity_str) {
            Ok(quantity) => {
                 if quantity <= Decimal::ZERO {
                     return Err(format!("Order quantity must be positive, got {}", quantity));
                 }
                match config.risk_limits.max_order_quantity.get(symbol) {
                    Some(max_qty) => {
                        if quantity > *max_qty {
                            Err(format!("Order quantity {} exceeds max allowed quantity {} for {}", quantity, max_qty, symbol))
                        } else {
                            Ok(()) // Pass
                        }
                    },
                    None => {
                        tracing::debug!("No specific max order quantity limit configured for {}", symbol);
                        Ok(()) // Pass if no specific limit
                    }
                }
            },
            Err(e) => Err(format!("Failed to parse order quantity '{}': {}", quantity_str, e)),
        }
    }
}

// Check if the action exceeds the maximum allowed order value (using current price from state)
pub struct MaxOrderValueCheck;

#[async_trait]
impl RiskCheck for MaxOrderValueCheck {
    async fn check(&self, action_req: &crate::proto::risk_management::EvaluateActionRequest,
                   risk_state: &RiskState,
                   config: &AppConfig) -> Result<(), String> {

         let quantity_str = &action_req.quantity;
         let symbol = &action_req.symbol;

         // Get current price from risk state cache
         let current_price = risk_state.get_last_price(symbol)
             .ok_or(format!("Current price for {} not available in risk state cache for Order Value check.", symbol))?;

         if current_price <= Decimal::ZERO {
             return Err(format!("Current price is zero or negative ({}) for order value check for {}.", current_price, symbol));
         }


         match DecimalResult::from_str(quantity_str) {
             Ok(quantity) => {
                 if quantity <= Decimal::ZERO {
                     return Err(format!("Order quantity must be positive, got {}", quantity));
                 }
                 let order_value = quantity * current_price;
                 let max_value = config.risk_limits.max_order_value_quote;

                 if order_value > max_value {
                     Err(format!("Order value {} exceeds max allowed value {} for {}", order_value, max_value, symbol))
                 } else {
                     Ok(()) // Pass
                 }
             },
             Err(e) => Err(format!("Failed to parse order quantity '{}': {}", quantity_str, e)),
         }
    }
}

// Check if the action will exceed the maximum allowed exposure for the symbol (using current price from state)
pub struct MaxSymbolExposureCheck;

#[async_trait]
impl RiskCheck for MaxSymbolExposureCheck {
    async fn check(&self, action_req: &crate::proto::risk_management::EvaluateActionRequest,
                   risk_state: &RiskState,
                   config: &AppConfig) -> Result<(), String> {

        let symbol = &action_req.symbol;
        let quantity_str = &action_req.quantity;
        let side = action_req.side();

         let quantity = DecimalResult::from_str(quantity_str).map_err(|e| format!("Failed to parse order quantity '{}': {}", quantity_str, e))?;
         if quantity <= Decimal::ZERO {
             return Err(format!("Order quantity must be positive, got {}", quantity));
         }

         // Get current price from risk state cache
         let current_price = risk_state.get_last_price(symbol)
             .ok_or(format!("Current price for {} not available in risk state cache for Symbol Exposure check.", symbol))?;

         if current_price <= Decimal::ZERO {
              return Err(format!("Current price is zero or negative ({}) for symbol exposure check for {}.", current_price, symbol));
         }

         // Get current position for the symbol
         let current_position_opt = risk_state.positions.get(symbol);

         // Calculate the potential *net* exposure if this order is executed
         let current_signed_quantity = current_position_opt.map(|p| p.quantity).unwrap_or(Decimal::ZERO);
         let order_signed_quantity = match side {
             OrderSide::BUY => quantity,
             OrderSide::SELL => -quantity, // Use negative for sell quantity
             _ => return Err(format!("Unsupported order side {:?} for exposure check", side)),
         };

         // Calculate the potential new position quantity and its value
         let potential_new_signed_quantity = current_signed_quantity + order_signed_quantity;
         let potential_new_exposure_quote = potential_new_signed_quantity.abs() * current_price; // Exposure is value

         let max_symbol_exposure = config.risk_limits.max_symbol_exposure_quote.get(symbol).copied().unwrap_or(Decimal::MAX);

         if potential_new_exposure_quote > max_symbol_exposure {
             Err(format!("Potential new exposure {:.2} exceeds max allowed symbol exposure {:.2} for {}", potential_new_exposure_quote, max_symbol_exposure, symbol))
         } else {
             Ok(()) // Pass
         }
    }
}


// Check if the action will exceed the maximum allowed total net exposure across all symbols (using current prices from state)
pub struct MaxTotalNetExposureCheck;

#[async_trait]
impl RiskCheck for MaxTotalNetExposureCheck {
     async fn check(&self, action_req: &crate::proto::risk_management::EvaluateActionRequest,
                   risk_state: &RiskState,
                   config: &AppConfig) -> Result<(), String> {

        let symbol = &action_req.symbol;
        let quantity_str = &action_req.quantity;
        let side = action_req.side();

        let quantity = DecimalResult::from_str(quantity_str).map_err(|e| format!("Failed to parse order quantity '{}': {}", quantity_str, e))?;
         if quantity <= Decimal::ZERO {
             return Err(format!("Order quantity must be positive, got {}", quantity));
         }

         // Get current price for the order symbol from risk state cache
         let current_price = risk_state.get_last_price(symbol)
             .ok_or(format!("Current price for {} not available in risk state cache for Total Net Exposure check.", symbol))?;

         if current_price <= Decimal::ZERO {
              return Err(format!("Current price is zero or negative ({}) for total net exposure check for {}.", current_price, symbol));
         }

         // Calculate current total net exposure (sum of absolute values of positions * their *current* price)
         let current_total_net_exposure = risk_state.positions.values()
            .filter_map(|pos| {
                 // Get the latest price for THIS position's symbol from the cache
                 risk_state.get_last_price(&pos.symbol)
                     .map(|price| pos.quantity.abs() * price) // Calculate value for each position
                     .ok_or_else(|| tracing::warn!("Price for position symbol {} not in cache for total exposure check.", pos.symbol)) // Log warning if price missing
                     .ok() // Discard error, continue with other symbols
            })
            .sum::<Decimal>();


         // Calculate the potential *new* total net exposure by calculating the new position's value
         // for the order symbol and re-summing with other symbols.
         let current_signed_quantity = risk_state.positions.get(symbol).map(|p| p.quantity).unwrap_or(Decimal::ZERO);
         let order_signed_quantity = match side {
             OrderSide::BUY => quantity,
             OrderSide::SELL => -quantity,
             _ => return Err(format!("Unsupported order side {:?} for total net exposure check", side)),
         };
         let potential_new_signed_quantity = current_signed_quantity + order_signed_quantity;

         // Calculate the exposure of the potential new position in this symbol
         let potential_new_symbol_exposure = potential_new_signed_quantity.abs() * current_price; // Use current price for valuation

         // Calculate the exposure of all *other* symbols (values already calculated in current_total_net_exposure logic)
         let other_symbols_exposure = risk_state.positions.iter()
            .filter(|(s, _)| *s != symbol)
            .filter_map(|(_, pos)| {
                 risk_state.get_last_price(&pos.symbol)
                      .map(|price| pos.quantity.abs() * price)
                     .ok_or_else(|| tracing::warn!("Price for other position symbol {} not in cache for total exposure check.", pos.symbol))
                      .ok()
            })
            .sum::<Decimal>();

         // Potential new total net exposure is the sum of the potential new symbol exposure and others
         let potential_new_total_net_exposure = potential_new_symbol_exposure + other_symbols_exposure;


         let max_total_net_exposure = config.risk_limits.max_total_net_exposure_quote;

         if potential_new_total_net_exposure > max_total_net_exposure {
             Err(format!("Potential new total net exposure {:.2} exceeds max allowed {:.2}", potential_new_total_net_exposure, max_total_net_exposure))
         } else {
             Ok(()) // Pass
         }
     }
}


// Check if the daily realized loss limit has been breached
pub struct MaxDailyRealizedLossCheck;

#[async_trait]
impl RiskCheck for MaxDailyRealizedLossCheck {
    async fn check(&self, _action_req: &crate::proto::risk_management::EvaluateActionRequest,
                   risk_state: &RiskState,
                   config: &AppConfig) -> Result<(), String> {

        let config_daily_loss_limit = config.risk_limits.max_daily_realized_loss.abs();

        if risk_state.daily_realized_loss.abs() > config_daily_loss_limit && config_daily_loss_limit > Decimal::ZERO {
            Err(format!("Daily realized loss {} exceeds limit {}", risk_state.daily_realized_loss, config_daily_loss_limit))
        } else {
            Ok(()) // Pass
        }
    }
}


// Add other risk checks here following the same pattern


// A collection of risk checks to be performed
pub struct RiskChecks {
    checks: Vec<Box<dyn RiskCheck + Send + Sync>>,
}

impl RiskChecks {
    pub fn new() -> Self {
        let mut checks: Vec<Box<dyn RiskCheck + Send + Sync>> = Vec::new();
        checks.push(Box::new(TradingHaltedCheck));
        checks.push(Box::new(MaxDailyRealizedLossCheck));
        checks.push(Box::new(MaxOrderQuantityCheck));
        checks.push(Box::new(MaxOrderValueCheck));
        checks.push(Box::new(MaxSymbolExposureCheck)); // Now uses state
        checks.push(Box::new(MaxTotalNetExposureCheck)); // Now uses state
        // Add other checks

        RiskChecks { checks }
    }

    // Run all configured risk checks against an action
    pub async fn run_checks(&self, action_req: &crate::proto::risk_management::EvaluateActionRequest,
                            // current_price is obtained within value/exposure checks from state cache
                            risk_state: &RiskState, // Pass the read guard's inner state
                            config: &AppConfig) -> Result<(), String> {

        for check in &self.checks {
            match check.check(action_req, risk_state, config).await { // Pass risk_state directly
                Ok(()) => (),
                Err(reason) => {
                    tracing::warn!("Risk check failed for action {}: {}", action_req.client_action_id, reason);
                    return Err(reason);
                }
            }
        }
        Ok(()) // All checks passed
    }
}