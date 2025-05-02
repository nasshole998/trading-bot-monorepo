#include "indicator_service_impl.h"
#include "data_manager.h"
#include "indicators/sma.h"
#include "indicators/rsi.h"
#include "indicators/macd.h"
#include "indicators/ema.h"
#include "config.h"
#include "health_check.h"
#include "utils/indicator_engine_error.h" // Include error header
#include <grpcpp/server.h>
#include <grpcpp/server_builder.h>
// #include <grpcpp/health_check_service_factory.h> // Optional gRPC health check service
#include <spdlog/spdlog.h> // For logging
#include <spdlog/sinks/stdout_color_sinks.h> // For colorized stdout
#include <csignal> // For signal handling
#include <atomic> // For atomic flag
#include <thread> // For health check thread
#include <iostream> // Used by spdlog setup


// Use a global atomic flag for graceful shutdown triggered by signal handler
std::atomic<bool> global_shutdown_requested(false);

// Signal handler function (safe to set atomic flag)
void signal_handler(int signal) {
    spdlog::info("Received signal {}. Setting shutdown flag...", signal);
    global_shutdown_requested.store(true);
}


int main(int argc, char** argv) {
    // --- Setup Logging (spdlog) ---
    // Create a colorized stdout sink
    auto stdout_sink = std::make_shared<spdlog::sinks::stdout_color_sink_mt>();
    // Set the default logger
    spdlog::set_default_logger(std::make_shared<spdlog::logger>("indicator_engine", stdout_sink));
    spdlog::set_level(spdlog::level::info); // Set default logging level (e.g., info, debug, trace)
    spdlog::set_pattern("[%Y-%m-%d %H:%M:%S.%e] [%l] [%t] [%@] %v"); // Example pattern

    spdlog::info("Starting Indicator Engine Service...");


    // --- Setup Signal Handling ---
    std::signal(SIGINT, signal_handler);  // Handle Ctrl+C
    std::signal(SIGTERM, signal_handler); // Handle termination signal
    // Ignore SIGPIPE which can happen with broken network connections
    std::signal(SIGPIPE, SIG_IGN);


    // --- Load Configuration ---
    Config config;
    // Assume config file is named settings.yaml and is in a 'config' subdirectory
    // relative to the executable location, or needs path adjusted.
    IndicatorEngine::VoidResult config_load_result = config.load("config/settings.yaml");
    if (config_load_result.has_error()) {
        spdlog::error("Failed to load configuration: {}. Exiting.", config_load_result.error().message());
        return 1; // Exit on config failure
    }
    // Debug print loaded indicator configs
    spdlog::info("Configuration loaded successfully.");
    spdlog::info("  Max history size: {}", config.max_history_size);
    spdlog::info("  gRPC listen address: {}", config.grpc.listen_address);
    spdlog::info("  Health check listen address: {}", config.health_check.listen_address);
    spdlog::info("  SMA configs loaded: {}", config.sma_configs.size());
    spdlog::info("  EMA configs loaded: {}", config.ema_configs.size());
    spdlog::info("  RSI configs loaded: {}", config.rsi_configs.size());
    spdlog::info("  MACD configs loaded: {}", config.macd_configs.size());


    // --- Initialize Data Manager ---
    auto data_manager = std::make_shared<IndicatorEngine::DataManager>(config.max_history_size);


    // --- Register Indicators (based on config) ---
    // Create unique_ptr instances and register them with the DataManager
    // Handle potential errors during registration (e.g., invalid indicator config)
    bool registration_failed = false;
    for (const auto& sma_cfg : config.sma_configs) {
        if (data_manager->registerSMA(std::make_unique<Indicators::SMA>(sma_cfg)).has_error()) {
             registration_failed = true; // Error logged in DataManager
        }
    }
     for (const auto& ema_cfg : config.ema_configs) {
        if (data_manager->registerEMA(std::make_unique<Indicators::EMA>(ema_cfg)).has_error()) {
             registration_failed = true;
        }
    }
    for (const auto& rsi_cfg : config.rsi_configs) {
        if (data_manager->registerRSI(std::make_unique<Indicators::RSI>(rsi_cfg)).has_error()) {
             registration_failed = true;
        }
    }
     for (const auto& macd_cfg : config.macd_configs) {
        if (data_manager->registerMACD(std::make_unique<Indicators::MACD>(macd_cfg)).has_error()) {
             registration_failed = true;
        }
    }
    // Register other indicators...

    if (registration_failed) {
        spdlog::error("Indicator registration failed. Exiting.");
        return 1; // Exit if any registration failed
    }


    // --- Setup gRPC Server ---
    IndicatorEngine::IndicatorServiceImpl service(data_manager);

    grpc::ServerBuilder builder;
    builder.AddListeningPort(config.grpc.listen_address, grpc::InsecureServerCredentials());

    // Register the service implementations
    builder.RegisterService(&service);

    // Optional: Register gRPC health check service (different from HTTP one)
    // builder.RegisterService(&grpc::HealthCheckServiceFactory().GetHealthCheckService());

    // Build and start the server
    std::unique_ptr<grpc::Server> server(builder.BuildAndStart());
    if (!server) {
         spdlog::error("Failed to start gRPC server on {}. Exiting.", config.grpc.listen_address);
         return 1; // Exit if server failed to build/start
    }
    spdlog::info("gRPC server listening on {}", config.grpc.listen_address);


    // --- Start Health Check Server (HTTP) ---
    std::atomic<bool> health_check_stop_flag(false); // Flag specific to health check thread
    std::thread health_check_thread([&]() {
        IndicatorEngine::start_health_check_server(config.health_check.listen_address, health_check_stop_flag);
    });
    // Do NOT detach the health check thread; join it later for graceful shutdown.


    // --- Wait for Global Shutdown Flag ---
    spdlog::info("Service running. Waiting for shutdown signal...");

    // Wait for the global_shutdown_requested flag to be set by a signal handler
    while (!global_shutdown_requested.load()) {
        std::this_thread::sleep_for(std::chrono::milliseconds(50)); // Sleep briefly to avoid busy-waiting
    }

    // --- Initiate Graceful Shutdown ---
    spdlog::info("Initiating graceful shutdown...");

    // 1. Signal the gRPC server to stop accepting new RPCs and start draining active ones.
    // Shutdown is non-blocking. server->Wait() (called implicitly by unique_ptr destructor)
    // will block until shutdown is complete.
    spdlog::info("Shutting down gRPC server...");
    server->Shutdown();

    // 2. Signal other threads (like health check) to stop.
    spdlog::info("Shutting down health check server...");
    health_check_stop_flag.store(true); // Set the flag for the health check thread.
    // The health check thread's Asio accept loop watches this flag.

    // 3. Join other threads to ensure they finish cleanly.
    if (health_check_thread.joinable()) {
        spdlog::info("Joining health check thread...");
        health_check_thread.join();
    }
    // The IndicatorServiceImpl destructor should handle joining any subscriber threads it manages.
    // In our design, subscriber "threads" are the RPC handler threads themselves, managed by gRPC.

    // 4. The gRPC server object (unique_ptr `server`) will be destroyed when main exits.
    // Its destructor calls `server->Wait()`, which blocks until the gRPC server is fully stopped,
    // including draining ongoing RPCs (like the market data ingestion stream).

    spdlog::info("Indicator Engine Service shut down complete.");

    // spdlog::shutdown(); // Optional: Clean up spdlog resources if needed

    return 0;
}