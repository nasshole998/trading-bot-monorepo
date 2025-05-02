#ifndef INDICATOR_ENGINE_CONFIG_H
#define INDICATOR_ENGINE_CONFIG_H

#include <string>
#include <vector>
#include <yaml-cpp/yaml.h> // Using YAML-CPP for config parsing
#include "indicators/sma.h" // Include indicator config types
#include "indicators/rsi.h"
#include "indicators/macd.h"

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
    // Add vectors for other indicator configs

    // Method to load configuration from a file (e.g., YAML)
    void load(const std::string& filepath);
};

// Helper functions to parse indicator configs from YAML
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
}

#endif // INDICATOR_ENGINE_CONFIG_H