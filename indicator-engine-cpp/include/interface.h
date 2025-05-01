// indicator-engine-cpp/include/interface.h
#pragma once

#include <grpcpp/grpcpp.h> // Include gRPC headers first
#include "../../proto/market_data.grpc.pb.h" // Include generated gRPC service code
#include "indicators/IndicatorBase.h"
#include "CircularBuffer.h"
#include "ThreadPool.h"
#include "config.h"
#include <string>
#include <vector>
#include <memory>
#include <unordered_map>
#include <shared_mutex> // For read-write locks (C++17)

// Forward declarations
class SMA;
class RSI;
class EMA;

// Structure to hold data and indicators for a single symbol
struct SymbolData {
    std::string symbol;
    CircularBuffer<double> price_buffer; // Store recent prices

    // Store indicators associated with this symbol
    std::vector<std::shared_ptr<IndicatorBase>> indicators;

    // Mutex to protect this specific symbol's data during updates
    std::shared_mutex data_mutex; // Read-write lock: many readers (calculate), one writer (update)

    explicit SymbolData(const std::string& sym, size_t buffer_capacity = 200) // Default buffer size
        : symbol(sym), price_buffer(buffer_capacity) {}

    // Add methods to initialize indicators for this symbol
    void add_sma(size_t period);
    void add_rsi(size_t period, bool use_ta_lib);
    void add_ema(size_t period, bool use_ta_lib);
};


// The gRPC service implementation class
class MarketDataServiceImpl final : public marketdata::MarketDataBroadcaster::CallbackService {
public:
    explicit MarketDataServiceImpl(const EngineConfig& config);
    ~MarketDataServiceImpl() override;

    // Override the gRPC service methods
    grpc::ServerReadReactor<marketdata::Tick>* StreamTicks(
        grpc::CallbackServerContext* context,
        grpc::ServerWriteReactor<marketdata::TickAck>* writer) override;

    grpc::ServerReadReactor<marketdata::Quote>* StreamQuotes(
        grpc::CallbackServerContext* context,
        grpc::ServerWriteReactor<marketdata::QuoteAck>* writer) override;

    // Add methods for potential future extensions, e.g., getting indicator values
    // grpc::Status GetIndicatorValues(grpc::ServerContext* context, const IndicatorRequest* request, IndicatorResponse* response) override;

private:
    // Map from symbol string to its data and indicators
    std::unordered_map<std::string, std::shared_ptr<SymbolData>> m_symbol_data_map;
    // Mutex to protect access to the map itself (adding/removing symbols)
    std::shared_mutex m_map_mutex;
    // Thread pool for processing updates/calculations
    ThreadPool m_thread_pool;
    // Engine configuration
    const EngineConfig& m_config;

    // Helper to get or create SymbolData entry
    std::shared_ptr<SymbolData> get_or_create_symbol_data(const std::string& symbol);

    // Helper function to process a received tick
    void process_tick(const marketdata::Tick& tick);
    // Helper function to process a received quote
    void process_quote(const marketdata::Quote& quote);
};


// --- Reactor Implementations ---
// Reactor for handling the StreamTicks RPC
class TickReactor : public grpc::ServerReadReactor<marketdata::Tick> {
public:
    TickReactor(MarketDataServiceImpl* service, grpc::ServerWriteReactor<marketdata::TickAck>* writer);
    void OnReadDone(bool ok) override;
    void OnDone() override;
    void SendAck(const std::string& correlation_id);

private:
    MarketDataServiceImpl* m_service;
    grpc::ServerWriteReactor<marketdata::TickAck>* m_writer;
    marketdata::Tick m_tick; // Buffer for incoming tick
    std::mutex m_ack_mutex; // Mutex for writer access
};

// Reactor for handling the StreamQuotes RPC
class QuoteReactor : public grpc::ServerReadReactor<marketdata::Quote> {
public:
     QuoteReactor(MarketDataServiceImpl* service, grpc::ServerWriteReactor<marketdata::QuoteAck>* writer);
    void OnReadDone(bool ok) override;
    void OnDone() override;
    void SendAck(const std::string& correlation_id);

private:
    MarketDataServiceImpl* m_service;
    grpc::ServerWriteReactor<marketdata::QuoteAck>* m_writer;
    marketdata::Quote m_quote; // Buffer for incoming quote
    std::mutex m_ack_mutex; // Mutex for writer access
};
