// indicator-engine-cpp/src/main.cpp
#include "config.h"
#include "logging.h"
#include "interface.h" // Includes the service implementation
#include <grpcpp/grpcpp.h>
#include <iostream>
#include <string>
#include <memory>
#include <csignal> // For signal handling
#include <atomic>

// Global atomic flag for shutdown signal
std::atomic<bool> g_shutdown_requested(false);
std::unique_ptr<grpc::Server> g_server_ptr = nullptr; // Global server pointer for shutdown

// Signal handler function
void signal_handler(int signal) {
    if (signal == SIGINT || signal == SIGTERM) {
        LOG_WARN("Shutdown signal received (Signal {}). Initiating graceful shutdown...", signal);
        g_shutdown_requested.store(true);
        // Trigger server shutdown if it's running
        if (g_server_ptr) {
            // Shutdown deadline (e.g., 5 seconds)
            g_server_ptr->Shutdown(std::chrono::system_clock::now() + std::chrono::seconds(5));
        }
    }
}


int main(int argc, char* argv[]) {
    // --- Configuration ---
    EngineConfig config;
    if (!parse_config(argc, argv, config)) {
        // Parsing failed or help message shown
        return (argc > 1 && (std::string(argv[1]) == "-h" || std::string(argv[1]) == "--help")) ? 0 : 1;
    }

    // --- Logging ---
    Logging::Initialize(config.log_level, config.log_file);

    // Log configuration details
    LOG_INFO("--- Indicator Engine Configuration ---");
    LOG_INFO("gRPC Port: {}", config.grpc_port);
    LOG_INFO("Worker Threads: {} (0=auto)", config.thread_pool_size);
    LOG_INFO("Log Level: {}", config.log_level);
    LOG_INFO("Log File: '{}'", config.log_file.empty() ? "None (Console Only)" : config.log_file);
    LOG_INFO("TA-Lib Enabled (Runtime): {}", config.use_ta_lib);
    LOG_INFO("GPU Enabled (Runtime): {}", config.enable_gpu);
    LOG_INFO("------------------------------------");


    // --- Service Implementation ---
    MarketDataServiceImpl service(config);

    // --- gRPC Server Setup ---
    grpc::EnableDefaultHealthCheckService(true);
    // grpc::reflection::InitProtoReflectionServerBuilder(&builder); // Optional: for server reflection
    grpc::ServerBuilder builder;

    // Listen on the specified port without SSL/TLS encryption
    std::string server_address = "0.0.0.0:" + std::to_string(config.grpc_port);
    builder.AddListeningPort(server_address, grpc::InsecureServerCredentials());

    // Register the service implementation
    builder.RegisterService(&service);

    // --- Start Server ---
    std::unique_ptr<grpc::Server> server(builder.BuildAndStart());
    if (!server) {
        LOG_CRITICAL("Failed to start gRPC server on address {}", server_address);
        Logging::Shutdown();
        return 1;
    }
    LOG_INFO("gRPC server listening on {}", server_address);

    // Assign to global pointer for signal handler
    g_server_ptr = std::move(server);

    // --- Signal Handling ---
    signal(SIGINT, signal_handler);
    signal(SIGTERM, signal_handler);

    // --- Wait for Shutdown ---
    // Keep the main thread alive until shutdown is requested
    // server->Wait() blocks until server is shut down.
    LOG_INFO("Indicator Engine running. Press Ctrl+C to shut down.");
    g_server_ptr->Wait(); // Block here until Shutdown() is called

    // --- Cleanup ---
    LOG_INFO("gRPC server shut down.");
    Logging::Shutdown(); // Ensure async logs are flushed

    return 0;
}

