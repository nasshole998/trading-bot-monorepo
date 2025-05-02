#include "config.h"
#include <iostream>
#include <stdexcept> // For runtime_error

void Config::load(const std::string& filepath) {
    try {
        YAML::Node config = YAML::LoadFile(filepath);

        // Load gRPC config
        if (config["grpc"]) {
            grpc.listen_address = config["grpc"]["listen_address"].as<std::string>();
        } else {
            throw std::runtime_error("Missing 'grpc' section in config");
        }

        // Load Health Check config
         if (config["health_check"]) {
            health_check.listen_address = config["health_check"]["listen_address"].as<std::string>();
        } else {
            // Optional: provide a default or make it required
            health_check.listen_address = "0.0.0.0:8080"; // Default
            std::cerr << "Warning: Missing 'health_check' section in config, using default address." << std::endl;
        }


        // Load general settings
         if (config["max_history_size"]) {
             max_history_size = config["max_history_size"].as<int>();
             if (max_history_size <= 0) {
                 max_history_size = 1000; // Default
                 std::cerr << "Warning: Invalid 'max_history_size', using default 1000." << std::endl;
             }
         }


        // Load indicator configurations
        if (config["indicators"]) {
            if (config["indicators"]["sma"]) {
                sma_configs = config["indicators"]["sma"].as<std::vector<Indicators::SmaConfig>>();
            }
            if (config["indicators"]["rsi"]) {
                rsi_configs = config["indicators"]["rsi"].as<std::vector<Indicators::RsiConfig>>();
            }
             if (config["indicators"]["macd"]) {
                macd_configs = config["indicators"]["macd"].as<std::vector<Indicators::MacdConfig>>();
            }
            // Load other indicators...
        } else {
             std::cerr << "Warning: Missing 'indicators' section in config." << std::endl;
        }


    } catch (const YAML::Exception& e) {
        throw std::runtime_error("YAML parsing error: " + std::string(e.what()));
    } catch (const std::runtime_error& e) {
         throw e; // Re-throw specific runtime errors
    } catch (const std::exception& e) {
        throw std::runtime_error("Unknown config loading error: " + std::string(e.what()));
    }
}

// Implement YAML parsing for indicator config structures
namespace YAML {
    bool convert<Indicators::SmaConfig>::decode(const Node& node, Indicators::SmaConfig& config) {
        if (!node.IsMap()) return false;
        if (node["name"] && node["symbol"] && node["period"]) {
            config.name = node["name"].as<std::string>();
            config.symbol = node["symbol"].as<std::string>();
            config.period = node["period"].as<int>();
            return true;
        }
        return false;
    }
     bool convert<Indicators::RsiConfig>::decode(const Node& node, Indicators::RsiConfig& config) {
        if (!node.IsMap()) return false;
        if (node["name"] && node["symbol"] && node["period"]) {
            config.name = node["name"].as<std::string>();
            config.symbol = node["symbol"].as<std::string>();
            config.period = node["period"].as<int>();
            return true;
        }
        return false;
    }
     bool convert<Indicators::MacdConfig>::decode(const Node& node, Indicators::MacdConfig& config) {
        if (!node.IsMap()) return false;
        if (node["name"] && node["symbol"] && node["fast_period"] && node["slow_period"] && node["signal_period"]) {
            config.name = node["name"].as<std::string>();
            config.symbol = node["symbol"].as<std::string>();
            config.fast_period = node["fast_period"].as<int>();
            config.slow_period = node["slow_period"].as<int>();
            config.signal_period = node["signal_period"].as<int>();
            return true;
        }
        return false;
    }
}