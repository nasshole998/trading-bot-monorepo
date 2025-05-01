// indicator-engine-cpp/src/config.cpp
#include "config.h"
#include "logging.h" // Use logging within config parsing
#include <iostream> // For basic error output before logging is up
#include <string>
#include <stdexcept> // For std::stoi, std::stoul

// Basic command line argument parser
bool parse_config(int argc, char* argv[], EngineConfig& config) {
    for (int i = 1; i < argc; ++i) {
        std::string arg = argv[i];
        try {
            if ((arg == "-p" || arg == "--port") && i + 1 < argc) {
                config.grpc_port = static_cast<uint16_t>(std::stoul(argv[++i]));
            } else if ((arg == "-t" || arg == "--threads") && i + 1 < argc) {
                config.thread_pool_size = std::stoi(argv[++i]);
            } else if ((arg == "-l" || arg == "--loglevel") && i + 1 < argc) {
                config.log_level = argv[++i];
            } else if ((arg == "-f" || arg == "--logfile") && i + 1 < argc) {
                config.log_file = argv[++i];
            } else if (arg == "--no-talib") {
                config.use_ta_lib = false;
            } else if (arg == "--gpu") {
                config.enable_gpu = true;
            }
             else if (arg == "-h" || arg == "--help") {
                std::cerr << "Usage: " << argv[0] << " [options]\n";
                std::cerr << "Options:\n";
                std::cerr << "  -p, --port PORT        gRPC server port (default: 50051)\n";
                std::cerr << "  -t, --threads N        Number of worker threads (default: auto)\n";
                std::cerr << "  -l, --loglevel LEVEL   Log level (trace, debug, info, warn, error, critical, off) (default: info)\n";
                std::cerr << "  -f, --logfile PATH     Path to log file (default: console only)\n";
                std::cerr << "  --no-talib             Disable TA-Lib usage (if compiled in)\n";
                std::cerr << "  --gpu                  Enable GPU usage (if compiled in)\n";
                std::cerr << "  -h, --help             Show this help message\n";
                return false; // Indicate help message was shown, program should exit
            }
            else {
                std::cerr << "Warning: Ignoring unknown argument: " << arg << std::endl;
            }
        } catch (const std::exception& e) {
             std::cerr << "Error parsing argument for " << arg << ": " << e.what() << std::endl;
             return false; // Indicate parsing failure
        }
    }

    // Basic validation
    if (config.thread_pool_size < 0) {
        std::cerr << "Error: Thread pool size cannot be negative." << std::endl;
        return false;
    }

    // Note: Further validation (e.g., log level string validity) can be added here
    // or handled by the logging initialization function.

    return true; // Indicate successful parsing
}

