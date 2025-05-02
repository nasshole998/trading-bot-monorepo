#include "data_manager.h"
#include "utils/conversions.h"
#include "indicators/sma.h"
#include "indicators/rsi.h"
#include "indicators/macd.h"
#include "indicators/ema.h"
#include <iostream> // For debug output
#include <algorithm> // For std::min
#include <cmath> // For std::isnan
#include <numeric> // For std::accumulate


namespace IndicatorEngine {

DataManager::DataManager(int max_history_size) : max_history_size_(max_history_size) {
    if (max_history_size_ <= 0) {
        max_history_size_ = 1000; // Default if invalid size provided
    }
    std::cout << "DataManager initialized with max history size: " << max_history_size_ << std::endl;
}

DataManager::~DataManager() {
    // Destructor
    std::cout << "DataManager destroying." << std::endl;
    // Ensure all subscribers are removed? Or rely on ServiceImpl destructor?
    // The ServiceImpl destructor should remove subscribers from DataManager.
}


void DataManager::processMarketDataEvent(const market_data::MarketDataEvent& event) {
    std::string symbol;
    std::optional<DecimalLike> price_opt;
    std::chrono::system_clock::time_point timestamp;

    if (event.has_trade()) {
        const auto& trade = event.trade();
        symbol = trade.symbol();
        price_opt = Utils::Conversions::StringToDecimalLike(trade.price());
        timestamp = Utils::Conversions::ProtoTimestampToTimePoint(trade.timestamp());
        // Add trade to history if needed directly by some indicator
    } else if (event.has_quote()) {
        const auto& quote = event.quote();
        symbol = quote.symbol();
        // Use midpoint price?
        auto bid_opt = Utils::Conversions::StringToDecimalLike(quote.bid_price());
        auto ask_opt = Utils::Conversions::StringToDecimalLike(quote.ask_price());
        if (bid_opt && ask_opt) {
             price_opt = (bid_opt.value() + ask_opt.value()) / 2.0; // Example: use midpoint
        }
        timestamp = Utils::Conversions::ProtoTimestampToTimePoint(quote.timestamp());
        // Add quote to history if needed
    } else if (event.has_order_book_update()) {
        const auto& ob_update = event.order_book_update();
        symbol = ob_update.symbol();
         if (!ob_update.bids().empty() && !ob_update.asks().empty()) {
             auto best_bid_opt = Utils::Conversions::StringToDecimalLike(ob_update.bids(0).price());
             auto best_ask_opt = Utils::Conversions::StringToDecimalLike(ob_update.asks(0).price());
             if (best_bid_opt && best_ask_opt) {
                 price_opt = (best_bid_opt.value() + best_ask_opt.value()) / 2.0;
             }
         }
        timestamp = Utils::Conversions::ProtoTimestampToTimePoint(ob_update.timestamp());
        // Need to manage order book state if full depth is required by indicators
    } else {
        // Unhandled event type
        // std::cerr << "Received unhandled market data event type." << std::endl;
        return;
    }

    if (symbol.empty()) {
         // std::cerr << "Received market data event with empty symbol." << std::endl;
         return;
    }
    if (!price_opt) {
         // Conversion failed or no price in event
         // std::cerr << "Could not extract valid price from market data event for symbol: " << symbol << std::endl;
         return;
    }


    // Get or create SymbolData
    std::unique_ptr<SymbolData>* symbol_data_ptr;
    {
        std::lock_guard<std::mutex> map_lock(symbol_data_mutex_);
        if (symbol_data_.find(symbol) == symbol_data_.end()) {
            std::cout << "Creating SymbolData for: " << symbol << std::endl;
            symbol_data_[symbol] = std::make_unique<SymbolData>();
            symbol_data_[symbol]->symbol = symbol;
        }
        symbol_data_ptr = &symbol_data_[symbol];
    } // Release map_lock

    // Add price and timestamp to history
    {
        std::lock_guard<std::mutex> data_lock((*symbol_data_ptr)->mutex);
        (*symbol_data_ptr)->recent_prices.push_back({price_opt.value(), timestamp});
        if ((*symbol_data_ptr)->recent_prices.size() > max_history_size_) {
            (*symbol_data_ptr)->recent_prices.pop_front();
        }
        // std::cout << "Added price for " << symbol << ", history size: " << (*symbol_data_ptr)->recent_prices.size() << std::endl; // Debug
    }

    // Trigger indicator computation for this symbol
    triggerIndicatorComputation(symbol);
}

std::optional<std::pair<DecimalLike, std::chrono::system_clock::time_point>> DataManager::getLatestPriceWithTimestamp(const std::string& symbol) const {
     std::lock_guard<std::mutex> map_lock(symbol_data_mutex_);
     auto it = symbol_data_.find(symbol);
     if (it != symbol_data_.end()) {
         std::lock_guard<std::mutex> data_lock(it->second->mutex);
         if (!it->second->recent_prices.empty()) {
             return it->second->recent_prices.back(); // Return the pair
         }
     }
     return std::nullopt; // Symbol not found or no price data
}


std::vector<DecimalLike> DataManager::getRecentPrices(const std::string& symbol, int count) const {
    std::vector<DecimalLike> prices;
    if (count <= 0) return prices;

     std::lock_guard<std::mutex> map_lock(symbol_data_mutex_);
     auto it = symbol_data_.find(symbol);
     if (it != symbol_data_.end()) {
         std::lock_guard<std::mutex> data_lock(it->second->mutex);
         size_t start_index = 0;
         if (it->second->recent_prices.size() > count) {
             start_index = it->second->recent_prices.size() - count;
         }
         for (size_t i = start_index; i < it->second->recent_prices.size(); ++i) {
             prices.push_back(it->second->recent_prices[i].first); // Get only the price
         }
     }
     return prices; // Return potentially empty vector
}

std::vector<std::pair<DecimalLike, std::chrono::system_clock::time_point>> DataManager::getRecentPricesWithTimestamps(const std::string& symbol, int count) const {
     std::vector<std::pair<DecimalLike, std::chrono::system_clock::time_point>> price_timestamps;
     if (count <= 0) return price_timestamps;

      std::lock_guard<std::mutex> map_lock(symbol_data_mutex_);
     auto it = symbol_data_.find(symbol);
     if (it != symbol_data_.end()) {
         std::lock_guard<std::mutex> data_lock(it->second->mutex);
         size_t start_index = 0;
         if (it->second->recent_prices.size() > count) {
             start_index = it->second->recent_prices.size() - count;
         }
         for (size_t i = start_index; i < it->second->recent_prices.size(); ++i) {
             price_timestamps.push_back(it->second->recent_prices[i]); // Get the pair
         }
     }
     return price_timestamps; // Return potentially empty vector
}


// Register indicator instances (takes ownership)
void DataManager::registerSMA(std::unique_ptr<Indicators::SMA> indicator) {
    if (!indicator) return;
    std::lock_guard<std::mutex> lock(calculators_mutex_);
    std::cout << "Registered SMA: " << indicator->config_.name << " for " << indicator->config_.symbol << " period " << indicator->config_.period << std::endl;
    sma_calculators_.push_back(std::move(indicator));
}

void DataManager::registerRSI(std::unique_ptr<Indicators::RSI> indicator) {
     if (!indicator) return;
    std::lock_guard<std::mutex> lock(calculators_mutex_);
    std::cout << "Registered RSI: " << indicator->config_.name << " for " << indicator->config_.symbol << " period " << indicator->config_.period << std::endl;
    rsi_calculators_.push_back(std::move(indicator));
}

void DataManager::registerMACD(std::unique_ptr<Indicators::MACD> indicator) {
     if (!indicator) return;
    std::lock_guard<std::mutex> lock(calculators_mutex_);
     std::cout << "Registered MACD: " << indicator->config_.name << " for " << indicator->config_.symbol << " (" << indicator->config_.fast_period << "," << indicator->config_.slow_period << "," << indicator->config_.signal_period << ")" << std::endl;
    macd_calculators_.push_back(std::move(indicator));
}

void DataManager::registerEMA(std::unique_ptr<Indicators::EMA> indicator) {
     if (!indicator) return;
    std::lock_guard<std::mutex> lock(calculators_mutex_);
     std::cout << "Registered EMA: " << indicator->config_.name << " for " << indicator->config_.symbol << " period " << indicator->config_.period << std::endl;
    ema_calculators_.push_back(std::move(indicator));
}


// Internal method to trigger computation for relevant indicators for a given symbol
void DataManager::triggerIndicatorComputation(const std::string& symbol) {
    // This method is called after new data arrives for 'symbol'.
    // Iterate through all registered indicators that match this symbol and compute.

    // The computation itself should not block this thread for too long if it's expensive.
    // For complex SIMD/GPU or if doing heavy batching, this could queue work for a worker pool.
    // Currently, indicator->compute() runs synchronously.

    // Note: SIMD/GPU acceleration would likely batch computations *across* symbols or periods,
    // so triggering per-symbol might not be the final model for highly optimized calculation.
    // A batching mechanism would collect data for multiple symbols, send to GPU, compute all,
    // then update results for all.

    std::lock_guard<std::mutex> calc_lock(calculators_mutex_); // Lock calculators list

    // SMA
    for (auto& sma_calc : sma_calculators_) { // Use mutable reference for stateful indicators
        if (sma_calc->config_.symbol == symbol) {
            DecimalLike value;
            if (sma_calc->compute(*this, &value)) { // Pass reference to DataManager
                indicator_data::IndicatorValue proto_value;
                proto_value.set_exchange("binance"); // Assuming binance for now
                proto_value.set_symbol(symbol);
                proto_value.set_indicator_name(sma_calc->config_.name);
                proto_value.set_value(Utils::Conversions::DecimalLikeToString(value));

                // Get timestamp of the data point the indicator was computed for
                // This is the timestamp of the *last* data point used (the current one)
                auto latest_price_ts_opt = getLatestPriceWithTimestamp(symbol);
                if (latest_price_ts_opt) {
                     *proto_value.mutable_timestamp() = Utils::Conversions::TimePointToProtoTimestamp(latest_price_ts_opt.value().second);
                } else {
                     // Fallback timestamp if somehow no price/timestamp found
                     *proto_value.mutable_timestamp() = Utils::Conversions::TimePointToProtoTimestamp(std::chrono::system_clock::now());
                }

                storeAndPublishIndicatorValue(std::move(proto_value)); // Store and notify
            }
        }
    }

    // EMA
     for (auto& ema_calc : ema_calculators_) { // Use mutable reference for stateful indicators
        if (ema_calc->config_.symbol == symbol) {
            DecimalLike value;
            if (ema_calc->compute(*this, &value)) { // Pass reference to DataManager
                indicator_data::IndicatorValue proto_value;
                 proto_value.set_exchange("binance");
                proto_value.set_symbol(symbol);
                proto_value.set_indicator_name(ema_calc->config_.name);
                proto_value.set_value(Utils::Conversions::DecimalLikeToString(value));
                 auto latest_price_ts_opt = getLatestPriceWithTimestamp(symbol);
                if (latest_price_ts_opt) {
                     *proto_value.mutable_timestamp() = Utils::Conversions::TimePointToProtoTimestamp(latest_price_ts_opt.value().second);
                } else {
                     *proto_value.mutable_timestamp() = Utils::Conversions::TimePointToProtoTimestamp(std::chrono::system_clock::now());
                }

                storeAndPublishIndicatorValue(std::move(proto_value)); // Store and notify
            }
        }
    }


    // RSI
     for (auto& rsi_calc : rsi_calculators_) { // Use mutable reference for stateful indicators
        if (rsi_calc->config_.symbol == symbol) {
            DecimalLike value;
            if (rsi_calc->compute(*this, &value)) { // Pass reference to DataManager
                indicator_data::IndicatorValue proto_value;
                 proto_value.set_exchange("binance");
                proto_value.set_symbol(symbol);
                proto_value.set_indicator_name(rsi_calc->config_.name);
                proto_value.set_value(Utils::Conversions::DecimalLikeToString(value));
                 auto latest_price_ts_opt = getLatestPriceWithTimestamp(symbol);
                if (latest_price_ts_opt) {
                     *proto_value.mutable_timestamp() = Utils::Conversions::TimePointToProtoTimestamp(latest_price_ts_opt.value().second);
                } else {
                     *proto_value.mutable_timestamp() = Utils::Conversions::TimePointToProtoTimestamp(std::chrono::system_clock::now());
                }

                storeAndPublishIndicatorValue(std::move(proto_value)); // Store and notify
            }
        }
    }

    // MACD
     for (auto& macd_calc : macd_calculators_) { // Use mutable reference for stateful indicators
        if (macd_calc->config_.symbol == symbol) {
            DecimalLike macd_line, signal_line, histogram;
            if (macd_calc->compute(*this, &macd_line, &signal_line, &histogram)) { // Pass reference to DataManager
                // MACD publishes 3 values per computation
                 indicator_data::IndicatorValue macd_value, signal_value, hist_value;
                 std::string base_name = macd_calc->config_.name; // e.g., "MACD_12_26_9"
                 auto latest_price_ts_opt = getLatestPriceWithTimestamp(symbol);
                 google::protobuf::Timestamp timestamp;
                 if (latest_price_ts_opt) {
                      timestamp = Utils::Conversions::TimePointToProtoTimestamp(latest_price_ts_opt.value().second);
                 } else {
                      timestamp = Utils::Conversions::TimePointToProtoTimestamp(std::chrono::system_clock::now());
                 }


                 // MACD Line
                 macd_value.set_exchange("binance");
                 macd_value.set_symbol(symbol);
                 macd_value.set_indicator_name(base_name + "_Line");
                 macd_value.set_value(Utils::Conversions::DecimalLikeToString(macd_line));
                 *macd_value.mutable_timestamp() = timestamp;
                 storeAndPublishIndicatorValue(std::move(macd_value));

                 // Signal Line
                 signal_value.set_exchange("binance");
                 signal_value.set_symbol(symbol);
                 signal_value.set_indicator_name(base_name + "_Signal");
                 signal_value.set_value(Utils::Conversions::DecimalLikeToString(signal_line));
                 *signal_value.mutable_timestamp() = timestamp;
                 storeAndPublishIndicatorValue(std::move(signal_value));

                 // Histogram
                 hist_value.set_exchange("binance");
                 hist_value.set_symbol(symbol);
                 hist_value.set_indicator_name(base_name + "_Hist");
                 hist_value.set_value(Utils::Conversions::DecimalLikeToString(histogram));
                 *hist_value.mutable_timestamp() = timestamp;
                 storeAndPublishIndicatorValue(std::move(hist_value));
            }
        }
    }

}

// Internal method to store a computed indicator value and notify subscribers
void DataManager::storeAndPublishIndicatorValue(indicator_data::IndicatorValue value) {
    // Store the latest value
    {
        // Lock covers both latest_indicator_values_ and last_sent_timestamps_
        std::lock_guard<std::mutex> lock(latest_values_mutex_);
        latest_indicator_values_[value.symbol()][value.indicator_name()] = value;
        // std::cout << "Stored latest indicator value: " << value.symbol() << ":" << value.indicator_name() << " = " << value.value() << std::endl; // Debug
    }

    // Notify subscriber threads waiting on the condition variable
    new_indicator_cv_.notify_all();
}


std::optional<indicator_data::IndicatorValue> DataManager::getLatestIndicatorValue(
    const std::string& symbol,
    const std::string& indicator_name
) const {
    std::lock_guard<std::mutex> lock(latest_values_mutex_);
    auto symbol_it = latest_indicator_values_.find(symbol);
    if (symbol_it != latest_indicator_values_.end()) {
        auto indicator_it = symbol_it->second.find(indicator_name);
        if (indicator_it != symbol_it->second.end()) {
            return indicator_it->second; // Return a copy
        }
    }
    return std::nullopt; // Value not found
}

// Get all latest indicator values matching a subscription request
// Returns a vector of values that are newer than the last sent timestamp for this subscriber.
std::vector<indicator_data::IndicatorValue> DataManager::getNewIndicatorValues(
    const std::string& subscriber_id,
    const indicator_data::IndicatorSubscriptionRequest& request
) {
    std::vector<indicator_data::IndicatorValue> new_values;
    std::lock_guard<std::mutex> lock(latest_values_mutex_); // Lock the latest values and subscriber state

    // Find or create the subscriber's last sent timestamps map
    auto& subscriber_timestamps = last_sent_timestamps_[subscriber_id];


    // Iterate through the latest stored values
    for (const auto& symbol_pair : latest_indicator_values_) {
        const std::string& current_symbol = symbol_pair.first;
        // Check if symbol is in subscription (if symbols list is not empty)
        bool symbol_matches = request.symbols().empty() ||
                              std::find(request.symbols().begin(), request.symbols().end(), current_symbol) != request.symbols().end();

        if (symbol_matches) {
            for (const auto& indicator_pair : symbol_pair.second) {
                const std::string& current_indicator_name = indicator_pair.first;
                const auto& indicator_value = indicator_pair.second;
                 const auto& indicator_timestamp = Utils::Conversions::ProtoTimestampToTimePoint(indicator_value.timestamp());


                // Check if indicator name is in subscription (if names list is not empty)
                 bool indicator_matches = request.indicator_names().empty() ||
                                          std::find(request.indicator_names().begin(), request.indicator_names().end(), current_indicator_name) != request.indicator_names().end();

                if (indicator_matches) {
                    // Check if this value is newer than the last one sent to THIS subscriber for THIS indicator
                    std::string key = current_symbol + ":" + current_indicator_name;
                    auto last_sent_it = subscriber_timestamps.find(key);

                    bool is_newer = true; // Assume newer if never sent before
                    if (last_sent_it != subscriber_timestamps.end()) {
                         // Compare current indicator value's timestamp with the last sent timestamp
                         if (indicator_timestamp <= last_sent_it->second) {
                             is_newer = false; // Not newer than the last one sent
                         }
                    }

                    if (is_newer) {
                         // Add to the list of values to send
                         new_values.push_back(indicator_value); // Copy the value

                         // Update the last sent timestamp for this subscriber and indicator
                         subscriber_timestamps[key] = indicator_timestamp;
                    }
                }
            }
        }
    }

    return new_values; // Return the list of new values
}

void DataManager::addSubscriber(const std::string& subscriber_id) {
    // No explicit state needed here beyond the last_sent_timestamps_ entry,
    // which is created lazily in getNewIndicatorValues.
     std::cout << "DataManager: Added subscriber state for ID: " << subscriber_id << std::endl;
     // Could initialize last_sent_timestamps_[subscriber_id] here if preferred.
}

void DataManager::removeSubscriber(const std::string& subscriber_id) {
     // Clean up the subscriber's state
    std::lock_guard<std::mutex> lock(latest_values_mutex_); // Lock access to subscriber state
    if (last_sent_timestamps_.erase(subscriber_id)) {
        std::cout << "DataManager: Removed subscriber state for ID: " << subscriber_id << std::endl;
    } else {
         std::cerr << "DataManager: Warning - tried to remove non-existent subscriber state for ID: " << subscriber_id << std::endl;
    }
}


} // namespace IndicatorEngine