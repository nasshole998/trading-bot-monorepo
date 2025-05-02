#ifndef INDICATOR_ENGINE_DATA_MANAGER_H
#define INDICATOR_ENGINE_DATA_MANAGER_H

#include <string>
#include <vector>
#include <unordered_map>
#include <deque>
#include <memory> // For std::shared_ptr, unique_ptr
#include <atomic> // For atomic flags
#include <mutex> // For std::mutex
#include <condition_variable> // For std::condition_variable
#include <optional> // For std::optional
#include <chrono> // For std::chrono::system_clock::time_point

#include "utils/conversions.h" // For DecimalLike
#include "indicators/sma.h" // Need indicator config types and classes
#include "indicators/rsi.h"
#include "indicators/macd.h"
#include "indicators/ema.h" // Need EMA config and class
#include "generated/market_data.grpc.pb.h" // Generated Protobuf types
#include "generated/indicator_data.grpc.pb.h.cc" // Include generated code for types
#include <grpcpp/server_context.h>
#include <grpcpp/server_reader.h>
#include <grpcpp/server_writer.h>


namespace IndicatorEngine {

// Structure to hold recent market data for a single symbol
struct SymbolData {
    std::string symbol;
    // Store price along with its timestamp
    std::deque<std::pair<DecimalLike, std::chrono::system_clock::time_point>> recent_prices;

    std::mutex mutex; // Protect access to this symbol's data buffer

    // Add buffers for other data types if indicators need them directly (less common)
    // std::deque<market_data::Trade> recent_trades;
    // std::deque<market_data::Quote> recent_quotes;
    // std::deque<market_data::OrderBookUpdate> recent_order_book_updates;
};

// Forward declarations for indicator calculators
namespace Indicators {
    class SMA;
    class RSI;
    class MACD;
    class EMA;
}

// Manages market data received via gRPC and stores it per symbol.
// Triggers indicator computation and holds latest indicator values.
// Notifies subscribers when new values are available.
class DataManager {
public:
    DataManager(int max_history_size = 1000);
    ~DataManager(); // Destructor for cleanup

    // Process an incoming MarketDataEvent from the gRPC stream
    void processMarketDataEvent(const market_data::MarketDataEvent& event);

    // Get the most recent price and timestamp for a symbol
    std::optional<std::pair<DecimalLike, std::chrono::system_clock::time_point>> getLatestPriceWithTimestamp(const std::string& symbol) const;

    // Get a window of recent prices for a symbol (only prices)
    std::vector<DecimalLike> getRecentPrices(const std::string& symbol, int count) const;

     // Get a window of recent prices with timestamps for a symbol
    std::vector<std::pair<DecimalLike, std::chrono::system_clock::time_point>> getRecentPricesWithTimestamps(const std::string& symbol, int count) const;


    // Register indicator instances (takes ownership)
    void registerSMA(std::unique_ptr<Indicators::SMA> indicator);
    void registerRSI(std::unique_ptr<Indicators::RSI> indicator);
    void registerMACD(std::unique_ptr<Indicators::MACD> indicator);
    void registerEMA(std::unique_ptr<Indicators::EMA> indicator);
    // Add methods for other indicator types

    // Get the latest computed value for a specific indicator on a symbol
    std::optional<indicator_data::IndicatorValue> getLatestIndicatorValue(
        const std::string& symbol,
        const std::string& indicator_name
    ) const;

    // Get all latest indicator values matching a subscription request
    // Returns a vector of values that are newer than the last sent timestamp for this subscriber.
    // Requires tracking per-subscriber state.
    std::vector<indicator_data::IndicatorValue> getNewIndicatorValues(
        const std::string& subscriber_id,
        const indicator_data::IndicatorSubscriptionRequest& request
    );

     // Notify subscriber threads that new indicator data is available
     void notifySubscribers();

     // Add/Remove subscriber state (called by IndicatorServiceImpl)
     void addSubscriber(const std::string& subscriber_id);
     void removeSubscriber(const std::string& subscriber_id);


    // Condition variable and mutex for notifying subscriber threads
    std::condition_variable new_indicator_cv_;
    std::mutex latest_values_mutex_; // Protects latest_indicator_values_ and last_sent_timestamps_

private:
    int max_history_size_;
    std::unordered_map<std::string, std::unique_ptr<SymbolData>> symbol_data_; // Store data per symbol
    std::mutex symbol_data_mutex_; // Protect access to the symbol_data_ map

    // Store registered indicator instances, grouped by type for easier triggering
    std::vector<std::unique_ptr<Indicators::SMA>> sma_calculators_;
    std::vector<std::unique_ptr<Indicators::RSI>> rsi_calculators_;
    std::vector<std::unique_ptr<Indicators::MACD>> macd_calculators_;
    std::vector<std::unique_ptr<Indicators::EMA>> ema_calculators_;
    std::mutex calculators_mutex_; // Protect access to calculator lists

    // Store the latest computed indicator values per symbol and indicator name
    std::unordered_map<std::string, // Symbol
                       std::unordered_map<std::string, // Indicator Name
                                          indicator_data::IndicatorValue // Latest Value
                                         >> latest_indicator_values_;


    // Track the last timestamp of the value sent to each subscriber for each indicator
    // This prevents resending old values.
    std::unordered_map<std::string, // Subscriber ID
                       std::unordered_map<std::string, // Indicator Name (e.g., "symbol:indicator_name")
                                          std::chrono::system_clock::time_point // Last sent timestamp
                                         >> last_sent_timestamps_;


    // Internal method to trigger computation for relevant indicators
    void triggerIndicatorComputation(const std::string& symbol);

    // Internal method to store a computed indicator value and notify subscribers
    void storeAndPublishIndicatorValue(indicator_data::IndicatorValue value);
};

} // namespace IndicatorEngine

#endif // INDICATOR_ENGINE_DATA_MANAGER_H