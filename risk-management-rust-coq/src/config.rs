// src/config.rs
use serde::Deserialize;
use std::fs;
use std::io::Read;
use parking_lot::RwLock;
use rust_decimal::Decimal;
use std::collections::HashMap;
use std::sync::Arc;
use rust_decimal_macros::dec;


#[derive(Debug, Deserialize, Clone)]
pub struct GrpcConfig {
    pub listen_address: String,
    pub data_service_address: String,
    pub strategy_engine_address: String, // **NEW:** Address for Strategy Engine
}

#[derive(Debug, Deserialize, Clone)]
pub struct RiskLimitsConfig {
    #[serde(default)]
    pub max_symbol_exposure_quote: HashMap<String, Decimal>,
    #[serde(default = "default_max_total_net_exposure_quote")]
    pub max_total_net_exposure_quote: Decimal,
    #[serde(default = "default_max_drawdown_percent")]
    pub max_drawdown_percent: f64, // 0.0 to 1.0
    #[serde(default)]
    pub max_order_quantity: HashMap<String, Decimal>,
    #[serde(default = "default_max_order_value_quote")]
    pub max_order_value_quote: Decimal,
    #[serde(default = "default_max_daily_realized_loss")]
    pub max_daily_realized_loss: Decimal,
    // Add other risk limit fields
}

// Default values
fn default_max_total_net_exposure_quote() -> Decimal { Decimal::MAX }
fn default_max_drawdown_percent() -> f64 { 1.0 }
fn default_max_order_value_quote() -> Decimal { Decimal::MAX }
fn default_max_daily_realized_loss() -> Decimal { Decimal::MAX }


#[derive(Debug, Deserialize, Clone)]
pub struct AppConfig {
    pub grpc: GrpcConfig,
    pub default_account_id: String,
    pub risk_limits: RiskLimitsConfig,
    // Add other top-level config
}

pub struct ConfigContainer {
    config: RwLock<Option<AppConfig>>,
}

impl ConfigContainer {
    pub fn new() -> Self {
        ConfigContainer {
            config: RwLock::new(None),
        }
    }

    pub fn load_from_file(&self, file_path: &str) -> Result<(), Box<dyn std::error::Error>> {
        let mut file = fs::File::open(file_path)?;
        let mut contents = String::new();
        file.read_to_string(&mut contents)?;
        let config: AppConfig = serde_yaml::from_str(&contents)?;

        let mut config_guard = self.config.write();
        *config_guard = Some(config);
        tracing::info!("Configuration loaded successfully from {}", file_path);
        Ok(())
    }

    pub fn get_config(&self) -> Option<parking_lot::RwLockReadGuard<'_, Option<AppConfig>>> {
         let config_guard = self.config.read();
         if config_guard.is_some() {
             Some(config_guard)
         } else {
             None
         }
    }
}

lazy_static::lazy_static! {
    pub static ref GLOBAL_CONFIG: Arc<ConfigContainer> = Arc::new(ConfigContainer::new());
}

pub fn get_loaded_config() -> Option<parking_lot::RwLockReadGuard<'static, Option<AppConfig>>> {
     GLOBAL_CONFIG.get_config()
}