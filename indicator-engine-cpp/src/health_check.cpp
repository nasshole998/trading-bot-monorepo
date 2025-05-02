#include "health_check.h"
#include <iostream>
#include <string>
#include <vector>
#include <asio.hpp> // Using Asio for basic socket
#include <chrono> // For std::chrono::seconds
#include <thread> // For std::this_thread::sleep_for

namespace IndicatorEngine {

// Simple HTTP health check server implementation using Asio
void start_health_check_server(const std::string& listen_address, const std::atomic<bool>& stop_flag) {
    std::cout << "Starting Health Check server on " << listen_address << " (Asio)..." << std::endl;

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

        std::cout << "Health Check server listening on " << acceptor.local_endpoint() << std::endl;

        // Set acceptor to non-blocking mode for graceful shutdown
        std::error_code ec_nonblock;
        acceptor.non_blocking(true, ec_nonblock);
         if (ec_nonblock) {
             std::cerr << "Failed to set non-blocking mode for health check acceptor: " << ec_nonblock.message() << std::endl;
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
                   std::cerr << "Error writing health check response: " << ec_write.message() << std::endl;
                 }
                 socket.shutdown(asio::ip::tcp::socket::shutdown_both, ec_accept); // Use ec_accept for shutdown
                 socket.close(ec_accept); // Use ec_accept for close

            } else if (ec_accept == asio::error::would_block) {
                // No pending connection, continue the loop
                // Sleep briefly to avoid tight polling loop
                std::this_thread::sleep_for(std::chrono::milliseconds(10));
            } else {
                // A real error occurred during accept
                std::cerr << "Health check accept error: " << ec_accept.message() << std::endl;
                // Depending on error, might break loop or continue
                std::this_thread::sleep_for(std::chrono::seconds(1)); // Sleep before retrying accept
            }
        }

        // Clean up acceptor on stop request
        std::error_code ec_close;
        acceptor.close(ec_close);
         if (ec_close) {
             std::cerr << "Error closing health check acceptor: " << ec_close.message() << std::endl;
         }

    } catch (const std::exception& e) {
        std::cerr << "Health Check server error: " << e.what() << std::endl;
    }

    std::cout << "Health Check server stopped." << std::endl;
}

} // namespace IndicatorEngine