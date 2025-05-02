#include "indicator_service_impl.h"
#include "data_manager.h"
#include "indicators/sma.h"
#include "indicators/rsi.h"
#include "indicators/macd.h"
#include "indicators/ema.h" // Include EMA header
#include "config.h"
#include "health_check.h"
#include <grpcpp/server.h>
#include <grpcpp/server_builder.h>
#include <grpcpp/health_check_service_factory.h> // Optional health check service
#include <iostream>
#include <string>
#include <vector>
#include <memory> // For shared_ptr, unique_ptr
#include <csignal> // For signal handling
#include <atomic> // For atomic flag
#include <thread> // For health check thread

// Use a global atomic flag for graceful shutdown triggered by signal handler
std::atomic<bool> global_shutdown_requested(false);

// Signal handler function (safe to set atomic flag)
void signal_handler(int signal) {
    std::cout << "Received signal " << signal << ". Setting shutdown flag..." << std::endl;
    global_shutdown_requested.store(true);
}


int main(int argc, char** argv) {
    // --- Setup Signal Handling ---
    // Register signal handlers for graceful shutdown
    std::signal(SIGINT, signal_handler);  // Handle Ctrl+C
    std::signal(SIGTERM, signal_handler); // Handle termination signal
    // Add other signals if needed


    // --- Load Configuration ---
    Config config;
    try {
        // Assume config file is named settings.yaml and is in a 'config' subdirectory
        // relative to the executable location, or needs path adjusted.
        // In production deployment, use environment variables or mount a config file.
        config.load("config/settings.yaml");
        std::cout << "Configuration loaded successfully." << std::endl;
        // Debug print loaded indicator configs
        std::cout << "  " << config.sma_configs.size() << " SMA configs loaded." << std::endl;
        std::cout << "  " << config.ema_configs.size() << " EMA configs loaded." << std::endl;
        std::cout << "  " << config.rsi_configs.size() << " RSI configs loaded." << std::endl;
        std::cout << "  " << config.macd_configs.size() << " MACD configs loaded." << std::endl;

    } catch (const std::exception& e) {
        std::cerr << "Failed to load configuration: " << e.what() << std::endl;
        return 1; // Exit on config failure
    }


    // --- Initialize Data Manager ---
    // Use shared_ptr as DataManager will be accessed by multiple threads (gRPC handlers)
    auto data_manager = std::make_shared<IndicatorEngine::DataManager>(config.max_history_size);


    // --- Register Indicators (based on config) ---
    // Create unique_ptr instances and register them with the DataManager
    for (const auto& sma_cfg : config.sma_configs) {
        data_manager->registerSMA(std::make_unique<Indicators::SMA>(sma_cfg));
    }
     for (const auto& ema_cfg : config.ema_configs) {
        data_manager->registerEMA(std::make_unique<Indicators::EMA>(ema_cfg));
    }
    for (const auto& rsi_cfg : config.rsi_configs) {
        data_manager->registerRSI(std::make_unique<Indicators::RSI>(rsi_cfg));
    }
     for (const auto& macd_cfg : config.macd_configs) {
        data_manager->registerMACD(std::make_unique<Indicators::MACD>(macd_cfg));
    }
    // Register other indicators...


    // --- Setup gRPC Server ---
    // The service implementation needs access to the DataManager
    IndicatorEngine::IndicatorServiceImpl service(data_manager);

    grpc::ServerBuilder builder;
    // Listen on the configured address
    // 0.0.0.0 allows binding to all network interfaces
    builder.AddListeningPort(config.grpc.listen_address, grpc::InsecureServerCredentials()); // Use Insecure for local testing, Secure in production

    // Register the service implementations
    builder.RegisterService(&service);

    // Optional: Register gRPC health check service (different from HTTP one)
    // builder.RegisterService(&grpc::HealthCheckServiceFactory().GetHealthCheckService());

    // Build and start the server
    std::unique_ptr<grpc::Server> server(builder.BuildAndStart());
    if (!server) {
         std::cerr << "Failed to start gRPC server on " << config.grpc.listen_address << std::endl;
         return 1; // Exit if server failed to build/start
    }
    std::cout << "gRPC server listening on " << config.grpc.listen_address << std::endl;


    // --- Start Health Check Server (HTTP) ---
    std::atomic<bool> health_check_stop_flag(false); // Flag specific to health check thread
    std::thread health_check_thread([&]() {
        IndicatorEngine::start_health_check_server(config.health_check.listen_address, health_check_stop_flag);
    });
    // Do NOT detach the health check thread; join it later for graceful shutdown.


    // --- Wait for Server Termination or Global Shutdown Flag ---
    // The main thread will now block, waiting for either the gRPC server to be shut down
    // *from another thread* (e.g., signal handler) or for the global_shutdown_requested flag.
    // A safe way is to poll the flag and call server->Shutdown() when it's set.
    // The server->Wait() call should ideally be replaced with logic that exits when Shutdown is complete.
    // gRPC's `Server::Shutdown()` initiates the shutdown but doesn't block until it's finished.
    // The recommended way is to call `server->Wait()` after calling `server->Shutdown()` from another thread.
    // However, our signal handler approach sets a flag, and main thread polls.

    std::cout << "Service running. Waiting for shutdown signal..." << std::endl;

    // Wait for the global_shutdown_requested flag to be set
    while (!global_shutdown_requested.load()) {
        std::this_thread::sleep_for(std::chrono::milliseconds(50)); // Sleep briefly to avoid busy-waiting
    }

    // --- Initiate Graceful Shutdown ---
    std::cout << "Initiating graceful shutdown..." << std::endl;

    // 1. Signal the gRPC server to stop accepting new RPCs and start draining active ones.
    std::cout << "Shutting down gRPC server..." << std::endl;
    server->Shutdown(); // This is non-blocking relative to this thread.

    // 2. Signal other threads (like health check) to stop.
    std::cout << "Shutting down health check server..." << std::endl;
    health_check_stop_flag.store(true); // Set the flag for the health check thread.

    // 3. Wait for the gRPC server to fully stop.
    // After calling Shutdown(), server->Wait() can be used to block until shutdown is complete.
    // In this case, the main thread *is* the one that would have potentially called Wait(),
    // but we interrupted that with the polling loop.
    // The `server` unique_ptr going out of scope *will* call its destructor, which blocks
    // until the server is fully shut down. So, we just need to ensure this happens after
    // signaling all other threads and joining them.

    // 4. Join other threads to ensure they finish cleanly.
    if (health_check_thread.joinable()) {
        std::cout << "Joining health check thread..." << std::endl;
        health_check_thread.join();
    }
    // The IndicatorServiceImpl destructor should handle joining any subscriber threads it manages.

    // 5. The gRPC server object (unique_ptr `server`) will be destroyed when main exits,
    // which will block until the server is fully stopped.

    std::cout << "gRPC server shut down." << std::endl;
    std::cout << "Indicator Engine Service shut down completely." << std::endl;

    return 0;
}