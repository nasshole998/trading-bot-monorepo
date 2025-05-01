// indicator-engine-cpp/include/logging.h
#pragma once

#include <memory>
#include "spdlog/spdlog.h"
#include "spdlog/sinks/stdout_color_sinks.h"
#include "spdlog/sinks/basic_file_sink.h"
#include "spdlog/async.h" // Optional: for async logging

namespace Logging {

inline void Initialize(const std::string& level = "info", const std::string& log_file = "") {
    try {
        // Default console logger (thread-safe)
        auto console_sink = std::make_shared<spdlog::sinks::stdout_color_sink_mt>();
        console_sink->set_level(spdlog::level::trace); // Log all levels to console initially
        console_sink->set_pattern("[%Y-%m-%d %H:%M:%S.%e] [%^%l%$] [%t] [%s:%#] %v"); // Example pattern

        std::vector<spdlog::sink_ptr> sinks;
        sinks.push_back(console_sink);

        // Optional file logger
        if (!log_file.empty()) {
            auto file_sink = std::make_shared<spdlog::sinks::basic_file_sink_mt>(log_file, true); // true = truncate
            file_sink->set_level(spdlog::level::trace); // Log all levels to file
             file_sink->set_pattern("[%Y-%m-%d %H:%M:%S.%e] [%l] [%t] [%s:%#] %v"); // File pattern without color
            sinks.push_back(file_sink);
        }

        // Create a multi-sink logger (async or sync)
        // Option 1: Synchronous logger
        // auto logger = std::make_shared<spdlog::logger>("indicator_engine", sinks.begin(), sinks.end());

        // Option 2: Asynchronous logger (better performance but needs careful shutdown)
        spdlog::init_thread_pool(8192, 1); // Queue size, number of threads
        auto logger = std::make_shared<spdlog::async_logger>("indicator_engine", sinks.begin(), sinks.end(),
                                                             spdlog::thread_pool(), spdlog::async_overflow_policy::block);


        // Register the logger as the default logger
        spdlog::set_default_logger(logger);

        // Set the global logging level based on config
        spdlog::set_level(spdlog::level::from_str(level));
        spdlog::flush_on(spdlog::level::warn); // Flush automatically on warning/error

        spdlog::info("Logging initialized. Level: {}, File: '{}'", level, log_file);

    } catch (const spdlog::spdlog_ex& ex) {
        fprintf(stderr, "Log initialization failed: %s\n", ex.what());
        exit(EXIT_FAILURE);
    }
}

// Optional: Function to properly shut down async logger if used
inline void Shutdown() {
    spdlog::info("Shutting down logging.");
    spdlog::shutdown();
}

} // namespace Logging

// Macros for easy logging (optional, but convenient)
#define LOG_TRACE(...)    spdlog::trace(__VA_ARGS__)
#define LOG_DEBUG(...)    spdlog::debug(__VA_ARGS__)
#define LOG_INFO(...)     spdlog::info(__VA_ARGS__)
#define LOG_WARN(...)     spdlog::warn(__VA_ARGS__)
#define LOG_ERROR(...)    spdlog::error(__VA_ARGS__)
#define LOG_CRITICAL(...) spdlog::critical(__VA_ARGS__)

