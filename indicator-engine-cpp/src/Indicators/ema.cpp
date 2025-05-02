#include "indicators/ema.h"
#include "utils/conversions.h" // For DecimalLike, NaN check
#include <numeric> // For std::accumulate
#include <limits> // For numeric_limits
#include <spdlog/spdlog.h> // For logging


namespace Indicators {

EMA::EMA(const EmaConfig& config) : config_(config) {
    if (config_.period <= 0) {
        spdlog::error("EMA config error: Period must be positive. Config: {}", config_.name);
        // Handle invalid config
    }
    // Calculate alpha: 2 / (period + 1)
    if (config_.period > 0) {
        alpha_ = 2.0 / (static_cast<DecimalLike>(config_.period) + 1.0);
    } else {
        alpha_ = 0.0; // Avoid division by zero, won't be used with period <= 0
    }
     spdlog::debug("EMA {} initialized with period {}", config_.name, config_.period);
}

// Compute the first EMA value (SMA-like average over the initial period)
bool EMA::compute_initial_value(const IndicatorEngine::DataManager& data_manager) {
    // Need 'period' data points to calculate the initial SMA
    auto recent_prices = data_manager.getRecentPrices(config_.symbol, config_.period);
    if (recent_prices.size() >= static_cast<size_t>(config_.period)) {
         DecimalLike sum = std::accumulate(recent_prices.begin(), recent_prices.end(), 0.0);
         current_value_ = sum / static_cast<DecimalLike>(config_.period);
         data_count_ = config_.period; // Mark as having processed the initial period
         spdlog::debug("EMA {} computed initial value: {}", config_.name, current_value_);
         return true;
    }
    return false; // Not enough data for initial EMA
}


bool EMA::compute(const IndicatorEngine::DataManager& data_manager, DecimalLike* output_value) {
    if (!output_value) {
        spdlog::error("EMA compute error: output_value pointer is null for {}", config_.name);
        return false;
    }
     if (config_.period <= 0) {
         spdlog::error("EMA compute error: Invalid period ({}) for {}", config_.period, config_.name);
         return false; // Cannot compute with invalid period
    }

    // Get the latest price for the symbol
    auto latest_price_opt = data_manager.getLatestPrice(config_.symbol);

    if (!latest_price_opt) {
        spdlog::debug("EMA compute info: No latest price for {}", config_.symbol);
        return false; // No data yet
    }

    DecimalLike current_price = latest_price_opt.value();

    // Handle initialization
    if (data_count_ < config_.period) {
        // We are still accumulating data for the initial period
        // Try to compute initial value if we have enough data now
        if (!compute_initial_value(data_manager)) {
             // Still not enough data for initial averages after trying initialization
            data_count_++; // Count the current price towards the initial period
            spdlog::trace("EMA compute info: Accumulating data ({}/{}) for initial value for {}", data_count_, config_.period, config_.name);
             return false; // Cannot compute EMA yet
         }
         // If initial value computed, data_count_ is updated and current_value_ is set.
         // Now proceed to standard smoothing for this latest price.
    }


    // --- EMA Computation (Standard Smoothing) ---

    // Ensure we have processed at least 'period' data points before standard smoothing,
    // including the one that finished the initial SMA.
    if (data_count_ >= config_.period) {
        current_value_ = (current_price - current_value_) * alpha_ + current_value_;
        spdlog::debug("Computed {}: {}", config_.name, current_value_);
         *output_value = current_value_;
         data_count_++; // Increment count
         return true; // EMA computed
    } else {
        // This case should ideally only be hit on the data point right after
        // the initial compute_initial_value succeeds, if compute_initial_value
        // doesn't count the last price used for the SMA itself.
        // Our compute_initial_value counts the 'period' data points, so data_count_
        // will be >= period right after. This 'else' branch is likely unreachable
        // with the current logic flow, but keeping it doesn't hurt.
         data_count_++; // Increment count
         return false; // Not enough data for standard smoothing yet
    }
}

} // namespace Indicators