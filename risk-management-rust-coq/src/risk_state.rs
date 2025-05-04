// src/risk_state.rs
use rust_decimal::Decimal;
use parking_lot::{RwLock, RwLockWriteGuard};
use std::collections::HashMap;
use std::sync::Arc;
use chrono::{DateTime, Utc, Duration};
use rust_decimal_prs::DecimalResult;
use rust_decimal_macros::dec;

// Represents the position details for a single symbol
#[derive(Debug, Clone, Default)]
pub struct SymbolPosition {
    pub symbol: String,
    pub quantity: Decimal, // Positive for long, negative for short
    pub avg_entry_price: Decimal,
    pub unrealized_pnl: Decimal, // Unrealized P/L based on current price
    // Add other position metrics (e.g., margin used)
}

// Represents the overall risk state managed by the component
#[derive(Debug, Default)]
pub struct RiskState {
    // Account-level state (updated by AccountStateService::SubscribeToAccountUpdates)
    pub account_id: String,
    pub total_equity: Decimal,
    pub total_capital: Decimal, // Available cash
    pub positions: HashMap<String, SymbolPosition>, // Position details per symbol
    pub open_orders: HashMap<String, crate::proto::account_state::OpenOrder>, // Open orders by ClientOrderId

    // Risk metrics derived from state or execution reports
    pub total_realized_loss: Decimal, // Total loss from closed trades
    pub daily_realized_loss: Decimal, // Realized loss within the current day
    pub last_daily_loss_reset: DateTime<Utc>, // Timestamp of the last daily loss reset

    // Metrics for drawdown tracking
    pub peak_equity: Decimal, // Highest equity reached

    // Cache of last known prices per symbol (updated by MarketDataService::SubscribeToMarketData)
    pub last_prices: HashMap<String, Decimal>,

    // Overall control flag
    pub trading_halted: bool,

    // Add other state variables needed for risk checks (e.g., market volatility, correlation)
}

// Thread-safe container for the RiskState
pub type SharedRiskState = Arc<RwLock<RiskState>>;

impl RiskState {
    pub fn new(account_id: String, initial_equity: Decimal) -> Self {
        let now = Utc::now();
        RiskState {
            account_id,
            total_equity: initial_equity,
            total_capital: initial_equity,
            positions: HashMap::new(),
            open_orders: HashMap::new(),
            total_realized_loss: Decimal::ZERO,
            daily_realized_loss: Decimal::ZERO,
            last_daily_loss_reset: now.date().and_hms(0, 0, 0).expect("Failed to create start of day Utc DateTime"), // Initialize to start of current day UTC
            peak_equity: initial_equity,
            last_prices: HashMap::new(),
            trading_halted: false,
            // Initialize other fields
        }
    }

    pub fn write_guard(&self) -> RwLockWriteGuard<'_, RiskState> {
        self.write()
    }

     pub fn read_guard(&self) -> parking_lot::RwLockReadGuard<'_, RiskState> {
        self.read()
     }

    pub fn get_last_price(&self, symbol: &str) -> Option<Decimal> {
        self.last_prices.get(symbol).copied()
    }


    // --- State Update Methods (Called by external data handlers) ---

    pub fn apply_account_update(&mut self, update: &crate::proto::account_state::AccountUpdate) -> Result<(), String> {
        let timestamp = update.timestamp.as_ref().ok_or("AccountUpdate missing timestamp")?.to_chrono().map_err(|e| format!("Invalid timestamp in AccountUpdate: {}", e))?;

        let total_equity = DecimalResult::from_str(&update.total_equity).map_err(|e| format!("Invalid total_equity in AccountUpdate: {}", e))?;
        let total_capital = DecimalResult::from_str(&update.total_capital).map_err(|e| format!("Invalid total_capital in AccountUpdate: {}", e))?;

        // Apply core account values
        self.account_id = update.account_id.clone();
        self.total_equity = total_equity;
        self.total_capital = total_capital;

        // Update positions based on the snapshot
        self.positions.clear();
        for pos_proto in &update.positions {
             let quantity = DecimalResult::from_str(&pos_proto.quantity).map_err(|e| format!("Invalid position quantity for {}: {}", pos_proto.symbol, e))?;
             let avg_entry_price = DecimalResult::from_str(&pos_proto.avg_entry_price).map_err(|e| format!("Invalid position avg_entry_price for {}: {}", pos_proto.symbol, e))?;
             let unrealized_pnl = DecimalResult::from_str(&pos_proto.unrealized_pnl).map_err(|e| format!("Invalid position unrealized_pnl for {}: {}", pos_proto.symbol, e))?;

             self.positions.insert(pos_proto.symbol.clone(), SymbolPosition {
                 symbol: pos_proto.symbol.clone(),
                 quantity: quantity,
                 avg_entry_price: avg_entry_price,
                 unrealized_pnl: unrealized_pnl,
             });
        }

        // Update open orders map based on the snapshot
        self.open_orders.clear();
        for order_proto in &update.open_orders {
            if let Some(client_id) = &order_proto.client_order_id {
                 self.open_orders.insert(client_id.clone(), order_proto.clone());
            } else {
                 tracing::warn!("Received OpenOrder update with no client_order_id: {:?}", order_proto);
            }
        }

        // Update peak equity if needed after applying updates
        if self.total_equity > self.peak_equity {
            self.peak_equity = self.total_equity;
        }

        // Re-check drawdown and potentially halt trading after total equity update
        let config = GLOBAL_CONFIG.get_config().expect("Config missing").as_ref().expect("Config is None");
        self.check_and_update_drawdown_halt(config);

        tracing::debug!("RiskState updated from AccountUpdate at {}", timestamp.to_rfc3339());
        Ok(())
    }

    // Update state based on a received ExecutionReport
    pub fn apply_execution_report(&mut self, report: &crate::proto::account_state::ExecutionReport) -> Result<(), String> {
        let timestamp = report.timestamp.as_ref().ok_or("ExecutionReport missing timestamp")?.to_chrono().map_err(|e| format!("Invalid timestamp in ExecutionReport: {}", e))?;

        // Check daily realized loss limit reset based on report timestamp
        if timestamp.date() > self.last_daily_loss_reset.date() {
            self.daily_realized_loss = Decimal::ZERO;
            self.last_daily_loss_reset = timestamp.date().and_hms(0, 0, 0).expect("Failed to create start of day Utc DateTime");
            tracing::info!("Daily realized loss reset for {}.", timestamp.date());
        }

        // --- Update realized loss ---
        // Use the realized_pnl field from the report if present
        if let Some(pnl_str) = &report.realized_pnl {
             match DecimalResult::from_str(pnl_str) {
                 Ok(realized_pnl) => {
                     self.total_realized_loss += realized_pnl;
                     self.daily_realized_loss += realized_pnl;
                     tracing::debug!("Applied realized P/L {} from ExecutionReport. Total RL: {}, Daily RL: {}", realized_pnl, self.total_realized_loss, self.daily_realized_loss);
                 },
                 Err(e) => {
                      tracing::error!("Invalid realized_pnl in ExecutionReport for order {}: {}", report.exchange_order_id, e);
                 }
             }
        } else {
            // If realized_pnl is not provided, we can only account for commission as loss
            let commission = DecimalResult::from_str(&report.commission).map_err(|e| format!("Invalid commission in ExecutionReport: {}", e))?;
            if commission > Decimal::ZERO { // Commissions are typically positive values representing cost
                 self.total_realized_loss -= commission; // Subtract commission as a loss
                 self.daily_realized_loss -= commission;
                 tracing::debug!("Applied commission {} from ExecutionReport as loss. Total RL: {}, Daily RL: {}", commission, self.total_realized_loss, self.daily_realized_loss);
            }
        }


        // --- Update open orders map based on report status ---
        // This should be done based on the order's client_order_id from the report matching an entry in self.open_orders
        // Relying on AccountUpdate stream for the *definitive* state of open orders is safer.
        // However, we can remove orders from our map here if the report indicates a final state.
         if let Some(client_id) = &report.client_order_id {
             match report.status {
                 crate::proto::market_data::OrderStatus::FILLED |
                 crate::proto::market_data::OrderStatus::CANCELED |
                 crate::proto::market_data::OrderStatus::REJECTED |
                 crate::proto::market_data::OrderStatus::EXPIRED => {
                     if self.open_orders.remove(client_id).is_some() {
                         tracing::debug!("Removed order {} (Status: {:?}) from open orders map based on ExecutionReport.", client_id, report.status);
                     } // else: already removed or never in map
                 },
                 crate::proto::market_data::OrderStatus::PARTIALLY_FILLED => {
                     // Update filled quantity or avg price if needed, or rely on AccountUpdate
                     // Relying on AccountUpdate stream for simplicity in Message 3.
                     tracing::debug!("Execution report for order {} (Status: {:?}). Relying on AccountUpdate stream for open orders map state.", client_id, report.status);
                 }
                 _ => {
                      // Other statuses might need updates (e.g., ACKNOWLEDGED, PENDING_CANCEL/REPLACE)
                      // Relying on AccountUpdate stream for simplicity.
                      tracing::debug!("Execution report for order {}. Status: {:?}. No open order map change.", client_id, report.status);
                 }
             }
         } else {
             tracing::warn!("Received ExecutionReport with no client_order_id: {:?}", report);
         }


        // Re-check limits that might be breached by the change (daily loss, drawdown)
        let config = GLOBAL_CONFIG.get_config().expect("Config missing").as_ref().expect("Config is None");
        self.check_and_update_daily_loss_halt(config);
        // Redundant drawdown check here if AccountUpdate is the primary source of total_equity

        tracing::debug!("RiskState updated from ExecutionReport at {}", timestamp.to_rfc3339());

        Ok(())
    }


    // Update state based on a received MarketDataEvent
    // Primarily used to update the last known price cache and recalculate unrealized P/L
    pub fn apply_market_data_event(&mut self, event: &crate::proto::market_data::MarketDataEvent) -> Result<(), String> {
         let timestamp = event.timestamp.as_ref().ok_or("MarketDataEvent missing timestamp")?.to_chrono().map_err(|e| format!("Invalid timestamp in MarketDataEvent: {}", e))?;

         let (symbol_opt, price_str_opt) =
             if let Some(trade) = &event.event_type.trade {
                 (Some(trade.symbol.clone()), Some(trade.price.clone()))
             } else if let Some(quote) = &event.event_type.quote {
                 let bid_price_res = DecimalResult::from_str(&quote.bid_price);
                 let ask_price_res = DecimalResult::from_str(&quote.ask_price);
                 match (bid_price_res, ask_price_res) {
                     (Ok(bid), Ok(ask)) if bid >= Decimal::ZERO && ask >= Decimal::ZERO => {
                          let midpoint = if bid.is_zero() && ask.is_zero() then Decimal::ZERO else (bid + ask) / dec!(2);
                          (Some(quote.symbol.clone()), Some(midpoint.to_string()))
                     },
                     _ => {
                         tracing::warn!("Failed to parse bid/ask prices in MarketDataEvent for {}: {}", quote.symbol, quote.bid_price);
                         (Some(quote.symbol.clone()), None)
                     }
                 }
             } else if let Some(ob_update) = &event.event_type.order_book_update {
                  if let (Some(best_bid), Some(best_ask)) = (ob_update.bids.first(), ob_update.asks.first()) {
                      let bid_price_res = DecimalResult::from_str(&best_bid.price);
                      let ask_price_res = DecimalResult::from_str(&best_ask.price);
                       match (bid_price_res, ask_price_res) {
                           (Ok(bid), Ok(ask)) if bid >= Decimal::ZERO && ask >= Decimal::ZERO => {
                               let midpoint = if bid.is_zero() && ask.is_zero() then Decimal::ZERO else (bid + ask) / dec!(2);
                               (Some(ob_update.symbol.clone()), Some(midpoint.to_string()))
                           },
                           _ => {
                              tracing::warn!("Failed to parse best bid/ask prices in MarketDataEvent for {}: {}", ob_update.symbol, best_bid.price);
                              (Some(ob_update.symbol.clone()), None)
                           }
                       }
                  } else {
                       tracing::debug!("Received OrderBookUpdate with no bids or asks for {}", ob_update.symbol);
                       (Some(ob_update.symbol.clone()), None)
                  }
             }
             else {
                 tracing::debug!("Received unhandled MarketDataEvent type at {}", timestamp.to_rfc3339());
                 (None, None)
             };

         if let (Some(symbol), Some(price_str)) = (symbol_opt, price_str_opt) {
              let current_price = DecimalResult::from_str(&price_str).map_err(|e| format!("Invalid price string in MarketDataEvent for {}: {}", symbol, e))?;
              if current_price > Decimal::ZERO {
                  // Update last known price cache
                  self.last_prices.insert(symbol.clone(), current_price);
                  tracing::debug!("Updated last price for {}: {} at {}", symbol, current_price, timestamp.to_rfc3339());

                  // Recalculate unrealized P/L for the position if it exists and has quantity/entry price
                  if let Some(position) = self.positions.get_mut(&symbol) {
                       if position.quantity.abs() > Decimal::ZERO && position.avg_entry_price.abs() > Decimal::ZERO {
                            position.unrealized_pnl = (current_price - position.avg_entry_price) * position.quantity;
                            // Note: Total equity is not updated here; rely on AccountUpdate stream for total equity.
                       }
                  }
              } else {
                 tracing::debug!("Ignoring non-positive price in MarketDataEvent for {}: {}", symbol, current_price);
              }
         } else {
             tracing::debug!("MarketDataEvent with no symbol or price processed at {}", timestamp.to_rfc3339());
         }


         Ok(())
    }


    pub fn check_and_update_drawdown_halt(&mut self, config: &config::AppConfig) {
         if self.total_equity > Decimal::ZERO { // Check against total_equity from AccountUpdate
             let drawdown = self.peak_equity - self.total_equity;
             let drawdown_percent = if self.peak_equity.abs() > Decimal::ZERO {
                 (drawdown / self.peak_equity) * dec!(100)
             } else { dec!(0) };

             let config_drawdown_percent = Decimal::from_f64(config.risk_limits.max_drawdown_percent * 100.0).unwrap_or(Decimal::MAX);

             if drawdown_percent > config_drawdown_percent {
                 self.trading_halted = true;
                 tracing::warn!("Trading halted due to drawdown breach: {:.2}% (Limit: {:.2}%) at Equity {}", drawdown_percent, config_drawdown_percent, self.total_equity);
             }
         } else if self.total_equity < Decimal::ZERO {
             self.trading_halted = true;
             tracing::warn!("Trading halted due to negative equity: {} at {}. Peak was {}.", self.total_equity, Utc::now().to_rfc3339(), self.peak_equity);
         }
    }

    pub fn check_and_update_daily_loss_halt(&mut self, config: &config::AppConfig) {
         let config_daily_loss_limit = config.risk_limits.max_daily_realized_loss.abs();

         if self.daily_realized_loss.abs() > config_daily_loss_limit && config_daily_loss_limit > Decimal::ZERO {
             self.trading_halted = true;
             tracing::warn!("Trading halted due to daily realized loss breach: {} (Limit: {}) at {}", self.daily_realized_loss, config_daily_loss_limit, Utc::now().to_rfc3339());
         }
    }

     // --- Coq Formalization Notes ---
     // Verification of `apply_execution_report` logic could focus on proving
     // properties about `total_realized_loss` and `daily_realized_loss` updates
     // based on the `realized_pnl` field in the report.
     // E.g., "If an ExecutionReport has a positive `realized_pnl`, applying it
     // increases `total_realized_loss` by that amount (assuming loss is negative)".
     // (The sign convention needs to be clear). We are using negative for loss,
     // so adding a positive realized P/L would actually decrease the total_realized_loss (making it less negative or positive).
     // A positive 'realized_pnl' from the exchange means profit, so we'd add it.
     // If the field *is* P/L (profit or loss), the sign should be consistent. Let's assume positive is profit, negative is loss.
     // `self.total_realized_loss += realized_pnl;` is correct if `realized_pnl` is signed Profit/Loss.
     // `self.daily_realized_loss += realized_pnl;` as well.

}