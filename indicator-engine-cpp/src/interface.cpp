// indicator-engine-cpp/src/interface.cpp
#include "interface.h"
#include "logging.h"
#include "indicators/SMA.h" // Include specific indicator headers
#include "indicators/RSI.h"
#include "indicators/EMA.h"
#include <google/protobuf/timestamp.pb.h> // For timestamp conversion
#include <chrono>
#include <future> // For futures from thread pool

// --- SymbolData Methods ---

void SymbolData::add_sma(size_t period) {
    // Consider acquiring write lock if indicators can be added dynamically
    // std::unique_lock lock(data_mutex);
    indicators.push_back(std::make_shared<SMA>(period));
    LOG_INFO("Added SMA({}) for symbol {}", period, symbol);
}

void SymbolData::add_rsi(size_t period, bool use_ta_lib) {
    // std::unique_lock lock(data_mutex);
    #ifndef USE_TA_LIB
    if (use_ta_lib) {
        LOG_WARN("TA-Lib requested for RSI({}) on symbol {} but engine not compiled with TA-Lib support. Using manual calculation.", period, symbol);
    }
    #endif
    indicators.push_back(std::make_shared<RSI>(period));
    LOG_INFO("Added RSI({}) for symbol {}", period, symbol);
}

void SymbolData::add_ema(size_t period, bool use_ta_lib) {
    // std::unique_lock lock(data_mutex);
     #ifndef USE_TA_LIB
    if (use_ta_lib) {
        LOG_WARN("TA-Lib requested for EMA({}) on symbol {} but engine not compiled with TA-Lib support. Using manual calculation.", period, symbol);
    }
    #endif
    indicators.push_back(std::make_shared<EMA>(period));
    LOG_INFO("Added EMA({}) for symbol {}", period, symbol);
}


// --- MarketDataServiceImpl Methods ---

MarketDataServiceImpl::MarketDataServiceImpl(const EngineConfig& config)
    : m_thread_pool(config.thread_pool_size), m_config(config)
{
    LOG_INFO("MarketDataServiceImpl created.");
    // Pre-initialize data structures for expected symbols? Or create on demand.
    // Example: Initialize indicators for a default set of symbols/periods
    // auto spy_data = get_or_create_symbol_data("SPY");
    // spy_data->add_sma(50);
    // spy_data->add_rsi(14, config.use_ta_lib);
}

MarketDataServiceImpl::~MarketDataServiceImpl() {
    LOG_INFO("MarketDataServiceImpl destroyed.");
    // Thread pool destructor handles joining threads
}

// Helper to get or create SymbolData entry safely
std::shared_ptr<SymbolData> MarketDataServiceImpl::get_or_create_symbol_data(const std::string& symbol) {
    // Try read lock first for common case (symbol exists)
    {
        std::shared_lock read_lock(m_map_mutex);
        auto it = m_symbol_data_map.find(symbol);
        if (it != m_symbol_data_map.end()) {
            return it->second;
        }
    } // Release read lock

    // Symbol not found, acquire write lock to potentially create it
    std::unique_lock write_lock(m_map_mutex);
    // Double-check if another thread created it while waiting for the write lock
    auto it = m_symbol_data_map.find(symbol);
    if (it != m_symbol_data_map.end()) {
        return it->second;
    }

    // Create new entry
    LOG_INFO("Creating data structures for new symbol: {}", symbol);
    auto new_data = std::make_shared<SymbolData>(symbol);

    // --- Add Default Indicators Here ---
    // Example: Add SMA(20) and RSI(14) for every new symbol
    new_data->add_sma(20);
    new_data->add_rsi(14, m_config.use_ta_lib);
    new_data->add_ema(10, m_config.use_ta_lib);
    // Add more based on configuration or strategy needs

    m_symbol_data_map[symbol] = new_data;
    return new_data;
}


// Helper function to process a received tick
void MarketDataServiceImpl::process_tick(const marketdata::Tick& tick) {
    LOG_TRACE("Processing tick for {}", tick.symbol());

    // Get data structure for the symbol
    auto symbol_data_ptr = get_or_create_symbol_data(tick.symbol());
    if (!symbol_data_ptr) return; // Should not happen with get_or_create

    // --- Submit update task to thread pool ---
    // Capture necessary data by value for the lambda
    double price = tick.price();
    std::string symbol = tick.symbol(); // Copy symbol string

    m_thread_pool.enqueue([symbol_data_ptr, price, symbol] {
        // This code runs in a worker thread
        LOG_TRACE("Worker thread processing tick for {} - Price: {}", symbol, price);

        // Acquire write lock for this specific symbol's data
        std::unique_lock write_lock(symbol_data_ptr->data_mutex);

        // Update price buffer
        symbol_data_ptr->price_buffer.push(price);

        // Update all indicators for this symbol
        for (const auto& indicator : symbol_data_ptr->indicators) {
            try {
                indicator->update(price);

                // Optional: Calculate immediately after update and log/store/publish
                if(indicator->is_ready()) {
                    auto value = indicator->calculate();
                    if(value.has_value()) {
                        LOG_DEBUG("Indicator calculated: Symbol={}, Name={}, Value={:.4f}",
                                 symbol, indicator->get_name(), value.value());
                        // TODO: Store or publish the calculated indicator value
                        // This could involve another gRPC call, writing to DB, or publishing to NATS/Kafka
                    }
                }
            } catch (const std::exception& e) {
                LOG_ERROR("Error updating/calculating indicator {} for symbol {}: {}",
                          indicator->get_name(), symbol, e.what());
            }
        }
    }); // End of lambda submitted to thread pool
}

// Helper function to process a received quote (similar structure to process_tick)
void MarketDataServiceImpl::process_quote(const marketdata::Quote& quote) {
     LOG_TRACE("Processing quote for {}", quote.symbol());
     // Currently, most common indicators use trade prices (close price).
     // If you have indicators that use bid/ask/mid-price, implement similar logic here.
     // Example: Update a mid-price buffer
     /*
     auto symbol_data_ptr = get_or_create_symbol_data(quote.symbol());
     if (!symbol_data_ptr) return;

     double mid_price = (quote.bid_price() + quote.ask_price()) / 2.0;
     std::string symbol = quote.symbol();

     m_thread_pool.enqueue([symbol_data_ptr, mid_price, symbol] {
         std::unique_lock write_lock(symbol_data_ptr->data_mutex);
         // symbol_data_ptr->mid_price_buffer.push(mid_price); // If you have such a buffer
         // Update relevant indicators...
         LOG_DEBUG("Mid-price updated for {}: {:.4f}", symbol, mid_price);
     });
     */
}


// --- gRPC Method Implementations ---

grpc::ServerReadReactor<marketdata::Tick>* MarketDataServiceImpl::StreamTicks(
    grpc::CallbackServerContext* context,
    grpc::ServerWriteReactor<marketdata::TickAck>* writer)
{
    LOG_INFO("Client connected to StreamTicks.");
    auto* reactor = new TickReactor(this, writer);
    // Start reading the first tick from the client stream
    reactor->StartRead(&reactor->m_tick);
    return reactor;
}

grpc::ServerReadReactor<marketdata::Quote>* MarketDataServiceImpl::StreamQuotes(
    grpc::CallbackServerContext* context,
    grpc::ServerWriteReactor<marketdata::QuoteAck>* writer)
{
     LOG_INFO("Client connected to StreamQuotes.");
     auto* reactor = new QuoteReactor(this, writer);
     reactor->StartRead(&reactor->m_quote);
     return reactor;
}


// --- TickReactor Methods ---

TickReactor::TickReactor(MarketDataServiceImpl* service, grpc::ServerWriteReactor<marketdata::TickAck>* writer)
    : m_service(service), m_writer(writer) {}

void TickReactor::OnReadDone(bool ok) {
    if (ok) {
        // Process the received tick
        m_service->process_tick(m_tick);

        // Optional: Send an acknowledgement back
        // SendAck(m_tick.correlation_id()); // If ticks have correlation IDs

        // Start reading the next tick
        StartRead(&m_tick);
    } else {
        // Stream closed by client or error
        LOG_INFO("StreamTicks read failed or stream closed by client.");
        Finish(grpc::Status::OK); // Finish the RPC on this side
    }
}

void TickReactor::OnDone() {
    // Called when the RPC is finished (e.g., client disconnects, error, Finish called)
    LOG_INFO("StreamTicks RPC finished.");
    delete this; // Clean up the reactor object
}

void TickReactor::SendAck(const std::string& correlation_id) {
     // This needs careful synchronization if called from multiple threads
     // (e.g., the service's worker threads if they send acks)
     // If only called from OnReadDone (gRPC thread), mutex might be simpler.
     std::lock_guard<std::mutex> lock(m_ack_mutex);
     marketdata::TickAck ack;
     ack.set_correlation_id(correlation_id);
     ack.set_received(true);
     m_writer->Write(ack); // Write is thread-safe, but managing state might need mutex
}


// --- QuoteReactor Methods ---

QuoteReactor::QuoteReactor(MarketDataServiceImpl* service, grpc::ServerWriteReactor<marketdata::QuoteAck>* writer)
    : m_service(service), m_writer(writer) {}

void QuoteReactor::OnReadDone(bool ok) {
     if (ok) {
        m_service->process_quote(m_quote);
        // SendAck(m_quote.correlation_id()); // Optional Ack
        StartRead(&m_quote);
    } else {
        LOG_INFO("StreamQuotes read failed or stream closed by client.");
        Finish(grpc::Status::OK);
    }
}

void QuoteReactor::OnDone() {
    LOG_INFO("StreamQuotes RPC finished.");
    delete this;
}

void QuoteReactor::SendAck(const std::string& correlation_id) {
     std::lock_guard<std::mutex> lock(m_ack_mutex);
     marketdata::QuoteAck ack;
     ack.set_correlation_id(correlation_id);
     ack.set_received(true);
     m_writer->Write(ack);
}

