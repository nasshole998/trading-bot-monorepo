#include "indicators/ema.h"
#include "utils/conversions.h" // For DecimalLike, NaN check
#include <numeric> // For std::accumulate
#include <iostream> // For debug output
#include <limits> // For numeric_limits

namespace Indicators {

EMA::EMA(const EmaConfig& config) : config_(config) {
    if (config_.period <= 0) {
        std::cerr << "Error: EMA period must be positive. Config: " << config_.name << std::endl;
        // Handle invalid config
    }
    // Calculate alpha: 2 / (period + 1)
    if (config_.period > 0) {
        alpha_ = 2.0 / (static_cast<DecimalLike>(config_.period) + 1.0);
    } else {
        alpha_ = 0.0; // Avoid division by zero
    }
}

// Compute the first EMA value (SMA-like average over the initial period)
bool EMA::compute_initial_value(const IndicatorEngine::DataManager& data_manager) {
    // Need 'period' data points to calculate the initial SMA
    auto recent_prices = data_manager.getRecentPrices(config_.symbol, config_.period);
    if (recent_prices.size() >= config_.period) {
         DecimalLike sum = std::accumulate(recent_prices.begin(), recent_prices.end(), 0.0);
         current_value_ = sum / static_cast<DecimalLike>(config_.period);
         data_count_ = config_.period; // Mark as having processed the initial period
        // std::cout << "Computed Initial EMA (" << config_.name << "): " << current_value_ << std::endl; // Debug
         return true;
    }
    return false; // Not enough data for initial EMA
}


bool EMA::compute(const IndicatorEngine::DataManager& data_manager, DecimalLike* output_value) {
    if (!output_value) {
        std::cerr << "Error: output_value pointer is null for EMA." << std::endl;
        return false;
    }
     if (config_.period <= 0) return false;

    // Get the latest price for the symbol
    auto latest_price_opt = data_manager.getLatestPrice(config_.symbol);

    if (!latest_price_opt) {
        // No data yet
        return false;
    }

    DecimalLike current_price = latest_price_opt.value();

    // Handle initialization
    if (std::isnan(current_value_)) {
         // Attempt to compute the initial value (SMA over the first 'period' data points)
         if (!compute_initial_value(data_manager)) {
             // Still not enough data after trying initialization
            data_count_++; // Count data points towards the initial period
             return false; // Cannot compute EMA yet
         }
         // If initial value computed, data_count_ is updated and current_value_ is set.
         // Now proceed to standard smoothing for this latest price.
    }


    // Standard EMA smoothing: EMA = (Close - PrevEMA) * alpha + PrevEMA
    // Ensure we have processed at least 'period' data points before standard smoothing,
    // including the one that finished the initial SMA.
    if (data_count_ >= config_.period) {
        current_value_ = (current_price - current_value_) * alpha_ + current_value_;
        // std::cout << "Computed EMA (" << config_.name << "): " << current_value_ << std::endl; // Debug
         *output_value = current_value_;
         data_count_++; // Increment count
         return true; // EMA computed
    } else {
        // Still accumulating data for the initial period or just finished it and need the next data point
        data_count_++; // Increment count
        return false; // Not enough data for standard smoothing yet
    }
}

} // namespace Indicators