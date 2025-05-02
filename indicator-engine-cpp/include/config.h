#ifndef INDICATOR_ENGINE_CONFIG_H
#define INDICATOR_ENGINE_CONFIG_H

#include <string>
#include <vector>
#include <yaml-cpp/yaml.h> // Using YAML-CPP for config parsing
#include "indicators/sma.h" // Include indicator config types
#include "indicators/rsi.h"
#include "indicators/macd.h"
#include "indicators/ema.h" // Include EMA config type
#include "utils/indicator_engine_error.h" // Include custom error type

// Configuration structures
struct GrpcConfig {
    std::string listen_address; // Address for this gRPC server
};

struct HealthCheckConfig {
    std::string listen_address; // Address for the HTTP health check server
};


// Main application configuration
struct Config {
    GrpcConfig grpc;
    HealthCheckConfig health_check;
    int max_history_size = 1000; // Max data points per symbol buffer

    // Indicator configurations
    std::vector<Indicators::SmaConfig> sma_configs;
    std::vector<Indicators::RsiConfig> rsi_configs;
    std::vector<Indicators::MacdConfig> macd_configs;
    std::vector<Indicators::EmaConfig> ema_configs;
    // Add vectors for other indicator configs

    // Method to load configuration from a file (e.g., YAML)
    // Returns VoidResult indicating success or failure with error code
    IndicatorEngine::VoidResult load(const std::string& filepath);

private:
    // Helper functions to parse indicator configs from YAML
    // These return bool indicating success/failure directly as required by YAML-CPP
    // Validation *after* parsing is done in Config::load
    namespace YAML {
        template<> struct convert<Indicators::SmaConfig> {
            static bool decode(const Node& node, Indicators::SmaConfig& config);
        };
         template<> struct convert<Indicators::RsiConfig> {
            static bool decode(const Node& node, Indicators::RsiConfig& config);
        };
         template<> struct convert<Indicators::MacdConfig> {
            static bool decode(const Node& node, Indicators::MacdConfig& config);
        };
         template<> struct convert<Indicators::EmaConfig> {
            static bool decode(const Node& node, Indicators::EmaConfig& config);
        };
    }
};

#endif // INDICATOR_ENGINE_CONFIG_H