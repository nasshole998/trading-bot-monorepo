// indicator-engine-cpp/include/config.h
#pragma once

#include <string>
#include <vector>
#include <cstdint>

struct EngineConfig {
    uint16_t grpc_port = 50051; // Default gRPC port
    int thread_pool_size = 0;  // 0 means auto-detect based on hardware concurrency
    std::string log_level = "info";
    std::string log_file = ""; // Empty means log only to console
    bool use_ta_lib = true;    // Corresponds to CMake option default
    bool enable_gpu = false;   // Corresponds to CMake option default
    // Add other config parameters as needed (e.g., default indicator periods)
};

// Function to parse command line arguments (or load from file/env vars)
bool parse_config(int argc, char* argv[], EngineConfig& config);


