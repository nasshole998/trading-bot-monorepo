#include "health_check.h"
#include <asio.hpp>
#include <chrono>
#include <thread>
#include <spdlog/spdlog.h> // For logging
#include <vector> // Needed by Asio headers sometimes

namespace IndicatorEngine {

// Simple HTTP health check server implementation using Asio
void start_health_check_server(const std::string& listen_address, const std::atomic<bool>& stop_flag) {
    spdlog::info("Starting Health Check server on {} (Asio)...", listen_address);

    try {
        asio::io_context io_context;

        // Parse address and port
        asio::ip::tcp::resolver resolver(io_context);
        asio::ip::tcp::endpoint endpoint;
        try {
             // Attempt to parse as IPv6 first, then IPv4 if fails
             endpoint = *resolver.resolve(asio::ip::tcp::v6(), listen_address.substr(0, listen_address.find(':')), listen_address.substr(listen_address.find(':') + 1)).begin();
        } catch (const asio::system_error&) {
             endpoint = *resolver.resolve(asio::ip::tcp::v4(), listen_address.substr(0, listen_address.find(':')), listen_address.substr(listen_address.find(':') + 1)).begin();
        }


        asio::ip::tcp::acceptor acceptor(io_context, endpoint);

        spdlog::info("Health Check server listening on {}", acceptor.local_endpoint());

        // Set acceptor to non-blocking mode for graceful shutdown
        std::error_code ec_nonblock;
        acceptor.non_blocking(true, ec_nonblock);
         if (ec_nonblock) {
             spdlog::error("Failed to set non-blocking mode for health check acceptor: {}", ec_nonblock.message());
             return; // Exit thread on failure
         }


        // Accept connections in a loop, checking the stop flag
        while (!stop_flag.load()) {
            asio::ip::tcp::socket socket(io_context);
            std::error_code ec_accept;

            // Attempt to accept a connection (non-blocking)
            acceptor.accept(socket, ec_accept);

            if (!ec_accept) {
                // Connection accepted successfully
                // Handle the connection (very simple HTTP response)
                std::string http_response = "HTTP/1.1 200 OK\r\nContent-Length: 2\r\nConnection: close\r\n\r\nOK"; // Add Connection: close
                std::error_code ec_write;
                asio::write(socket, asio::buffer(http_response), ec_write);
                 if (ec_write) {
                   spdlog::error("Error writing health check response: {}", ec_write.message());
                 }
                 std::error_code ec_close; // Use a new error code for close operations
                 socket.shutdown(asio::ip::tcp::socket::shutdown_both, ec_close);
                 socket.close(ec_close);

            } else if (ec_accept == asio::error::would_block) {
                // No pending connection, continue the loop
                // Sleep briefly to avoid tight polling loop
                std::this_thread::sleep_for(std::chrono::milliseconds(10));
            } else {
                // A real error occurred during accept
                spdlog::error("Health check accept error: {}", ec_accept.message());
                // Depending on error, might break loop or continue
                std::this_thread::sleep_for(std::chrono::seconds(1)); // Sleep before retrying accept
            }
        }

        // Clean up acceptor on stop request
        std::error_code ec_close;
        acceptor.close(ec_close);
         if (ec_close) {
             spdlog::error("Error closing health check acceptor: {}", ec_close.message());
         }

    } catch (const std::exception& e) {
        spdlog::error("Health Check server error: {}", e.what());
    }

    spdlog::info("Health Check server stopped.");
}

} // namespace IndicatorEngine