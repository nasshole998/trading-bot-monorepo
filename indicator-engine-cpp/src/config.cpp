#include "config.h"
#include "utils/indicator_engine_error.h"
#include <yaml-cpp/yaml.h> // Include again for implementation
#include <spdlog/spdlog.h> // For logging
#include <stdexcept> // For runtime_error, though we use Result now
#include <algorithm> // For std::min/max

namespace IndicatorEngine {

IndicatorEngine::VoidResult Config::load(const std::string& filepath) {
    try {
        YAML::Node config_node = YAML::LoadFile(filepath);
        spdlog::info("Loading configuration from: {}", filepath);

        // Load gRPC config
        if (config_node["grpc"]) {
            if (config_node["grpc"]["listen_address"]) {
                grpc.listen_address = config_node["grpc"]["listen_address"].as<std::string>();
                 if (grpc.listen_address.empty()) {
                    spdlog::error("Config error: grpc.listen_address cannot be empty.");
                    return Error::IndicatorEngineErrc::InvalidConfig;
                }
            } else {
                spdlog::error("Config error: Missing grpc.listen_address.");
                return Error::IndicatorEngineErrc::InvalidConfig;
            }
        } else {
            spdlog::error("Config error: Missing 'grpc' section.");
            return Error::IndicatorEngineErrc::InvalidConfig;
        }

        // Load Health Check config (optional, provide default)
         if (config_node["health_check"]) {
            if (config_node["health_check"]["listen_address"]) {
                 health_check.listen_address = config_node["health_check"]["listen_address"].as<std::string>();
                 if (health_check.listen_address.empty()) {
                     spdlog::warn("Config warning: health_check.listen_address is empty, defaulting.");
                     health_check.listen_address = "0.0.0.0:8080"; // Default
                 }
            } else {
                 spdlog::warn("Config warning: Missing health_check.listen_address, defaulting.");
                 health_check.listen_address = "0.0.0.0:8080"; // Default
            }
        } else {
             spdlog::warn("Config warning: Missing 'health_check' section, defaulting.");
             health_check.listen_address = "0.0.0.0:8080"; // Default
        }


        // Load general settings
         if (config_node["max_history_size"]) {
             max_history_size = config_node["max_history_size"].as<int>();
             if (max_history_size <= 0) {
                 spdlog::warn("Config warning: Invalid 'max_history_size' ({}), defaulting to 1000.", max_history_size);
                 max_history_size = 1000; // Default
             }
         } else {
              spdlog::warn("Config warning: Missing 'max_history_size', defaulting to 1000.");
              max_history_size = 1000; // Default
         }


        // Load indicator configurations
        if (config_node["indicators"]) {
            if (config_node["indicators"]["sma"]) {
                sma_configs = config_node["indicators"]["sma"].as<std::vector<Indicators::SmaConfig>>();
                 // Basic validation for loaded configs
                 for(const auto& cfg : sma_configs) {
                     if (cfg.period <= 0 || cfg.symbol.empty() || cfg.name.empty()) {
                         spdlog::error("Config error: Invalid SMA config entry (period <= 0 or empty symbol/name)");
                         return Error::IndicatorEngineErrc::InvalidConfig;
                     }
                 }
            }
            if (config_node["indicators"]["ema"]) {
                ema_configs = config_node["indicators"]["ema"].as<std::vector<Indicators::EmaConfig>>();
                 for(const auto& cfg : ema_configs) {
                     if (cfg.period <= 0 || cfg.symbol.empty() || cfg.name.empty()) {
                         spdlog::error("Config error: Invalid EMA config entry (period <= 0 or empty symbol/name)");
                         return Error::IndicatorEngineErrc::InvalidConfig;
                     }
                 }
            }
            if (config_node["indicators"]["rsi"]) {
                rsi_configs = config_node["indicators"]["rsi"].as<std::vector<Indicators::RsiConfig>>();
                 for(const auto& cfg : rsi_configs) {
                     if (cfg.period <= 1 || cfg.symbol.empty() || cfg.name.empty()) { // RSI needs period > 1
                         spdlog::error("Config error: Invalid RSI config entry (period <= 1 or empty symbol/name)");
                         return Error::IndicatorEngineErrc::InvalidConfig;
                     }
                 }
            }
             if (config_node["indicators"]["macd"]) {
                macd_configs = config_node["indicators"]["macd"].as<std::vector<Indicators::MacdConfig>>();
                 for(const auto& cfg : macd_configs) {
                     if (cfg.fast_period <= 0 || cfg.slow_period <= 0 || cfg.signal_period <= 0 || cfg.fast_period >= cfg.slow_period || cfg.symbol.empty() || cfg.name.empty()) {
                         spdlog::error("Config error: Invalid MACD config entry (invalid periods or empty symbol/name)");
                         return Error::IndicatorEngineErrc::InvalidConfig;
                     }
                 }
            }
            // Load other indicators...
        } else {
             spdlog::warn("Config warning: Missing 'indicators' section.");
        }

        spdlog::info("Configuration loaded successfully.");
        return Error::IndicatorEngineErrc::Success; // Indicate success

    } catch (const YAML::Exception& e) {
        spdlog::error("YAML parsing error: {}", e.what());
        return Error::IndicatorEngineErrc::InvalidConfig;
    } catch (const std::exception& e) {
        spdlog::error("Unknown config loading error: {}", e.what());
        return Error::IndicatorEngineErrc::UnknownError;
    }
}

// Implement YAML parsing for indicator config structures
namespace YAML {
    bool convert<Indicators::SmaConfig>::decode(const Node& node, Indicators::SmaConfig& config) {
        if (!node.IsMap()) return false;
        // Use try_as for safer conversion, though error handling is primarily in Config::load
        config.name = node["name"].as<std::string>("");
        config.symbol = node["symbol"].as<std::string>("");
        config.period = node["period"].as<int>(0); // Default to 0 for easier validation

        return true; // Indicate parsing succeeded (validation is separate)
    }
     bool convert<Indicators::EmaConfig>::decode(const Node& node, Indicators::EmaConfig& config) {
        if (!node.IsMap()) return false;
        config.name = node["name"].as<std::string>("");
        config.symbol = node["symbol"].as<std::string>("");
        config.period = node["period"].as<int>(0);
        return true;
    }
     bool convert<Indicators::RsiConfig>::decode(const Node& node, Indicators::RsiConfig& config) {
        if (!node.IsMap()) return false;
        config.name = node["name"].as<std::string>("");
        config.symbol = node["symbol"].as<std::string>("");
        config.period = node["period"].as<int>(0);
        return true;
    }
     bool convert<Indicators::MacdConfig>::decode(const Node& node, Indicators::MacdConfig& config) {
        if (!node.IsMap()) return false;
        config.name = node["name"].as<std::string>("");
        config.symbol = node["symbol"].as<std::string>("");
        config.fast_period = node["fast_period"].as<int>(0);
        config.slow_period = node["slow_period"].as<int>(0);
        config.signal_period = node["signal_period"].as<int>(0);
        return true;
    }
}

} // namespace IndicatorEngine