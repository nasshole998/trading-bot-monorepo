#include "indicators/rsi.h"
#include "utils/conversions.h" // For DecimalLike, NaN check
#include <cmath> // For std::abs, std::isnan, std::isinf
#include <limits> // For numeric_limits
#include <numeric> // For std::accumulate
#include <vector> // For getRecentPrices return
#include <spdlog/spdlog.h> // For logging


namespace Indicators {

RSI::RSI(const RsiConfig& config) : config_(config) {
    if (config_.period <= 1) { // RSI needs at least 2 data points for the first gain/loss calculation
         spdlog::error("RSI config error: Period must be greater than 1. Config: {}", config_.name);
         // Handle invalid config
    }
     state_ = RsiState(); // Initialize state
     spdlog::debug("RSI {} initialized with period {}", config_.name, config_.period);
}

// Compute the first average gain/loss over the initial period (SMA-like average)
bool RSI::compute_initial_averages(const IndicatorEngine::DataManager& data_manager) {
    // Need 'period' data points with differences, which requires period + 1 prices
    auto initial_prices = data_manager.getRecentPrices(config_.symbol, config_.period + 1);

    if (initial_prices.size() >= static_cast<size_t>(config_.period + 1)) {
        DecimalLike total_gain = 0.0;
        DecimalLike total_loss = 0.0;

        for (size_t i = 0; i < static_cast<size_t>(config_.period); ++i) {
            DecimalLike diff = initial_prices[i + 1] - initial_prices[i];
            if (diff > 0) {
                total_gain += diff;
            } else {
                total_loss += std::abs(diff);
            }
        }

        state_.avg_gain = total_gain / static_cast<DecimalLike>(config_.period);
        state_.avg_loss = total_loss / static_cast<DecimalLike>(config_.period);
        state_.last_price = initial_prices.back(); // Store the last price of this initial window
        state_.data_count = config_.period; // Mark as having processed the initial period (period diffs = period+1 prices)
        spdlog::debug("RSI {} computed initial averages - Gain: {}, Loss: {}", config_.name, state_.avg_gain, state_.avg_loss);
        return true;
    }
    return false; // Not enough data for initial averages
}


bool RSI::compute(const IndicatorEngine::DataManager& data_manager, DecimalLike* output_value) {
    if (!output_value) {
        spdlog::error("RSI compute error: output_value pointer is null for {}", config_.name);
        return false;
    }
     if (config_.period <= 1) {
         spdlog::error("RSI compute error: Invalid period ({}) for {}", config_.period, config_.name);
         return false; // Cannot compute with invalid period
     }

    // Get the latest price for the symbol
    auto latest_price_opt = data_manager.getLatestPrice(config_.symbol);

    if (!latest_price_opt) {
        spdlog::debug("RSI compute info: No latest price for {}", config_.symbol);
        return false; // No data yet
    }

    DecimalLike current_price = latest_price_opt.value();

    // Handle initialization
    if (state_.data_count < config_.period) {
        // We are still accumulating data for the initial period
        // Try to compute initial averages if we have enough data now
        if (!compute_initial_averages(data_manager)) {
             // Still not enough data for initial averages after trying initialization
             state_.data_count++; // Count the current price towards the initial period
             spdlog::trace("RSI compute info: Accumulating data ({}/{}) for initial averages for {}", state_.data_count, config_.period, config_.name);
             return false; // Cannot compute RSI yet
         }
         // If initial averages computed, data_count_ is updated and state_.avg_gain/loss are set.
         // The last price used for the initial averages is stored in state_.last_price.
         // Now proceed to standard smoothing for this latest price that arrived *after* the initial period.
    }


    // --- RSI Computation (Standard Smoothing) ---

    // We need the previous price to calculate the latest gain/loss
    // This check is crucial if compute_initial_averages failed but data_count >= period.
    if (std::isnan(state_.last_price)) {
         spdlog::error("RSI compute error: last_price is NaN for {} after initialization attempt. Logic error?", config_.name);
         state_.last_price = current_price; // Reset last price to current to attempt next time
         return false;
    }

    DecimalLike diff = current_price - state_.last_price;
    DecimalLike current_gain = (diff > 0) ? diff : 0.0;
    DecimalLike current_loss = (diff < 0) ? std::abs(diff) : 0.0;

    // Exponential Moving Average smoothing using the standard alpha = 2 / (period + 1)
    // The formula is: Avg = (PrevAvg * (period - 1) + CurrentValue) / period
    // Or equivalently: Avg = PrevAvg + alpha * (CurrentValue - PrevAvg) where alpha = 1 / period (Wilder's original) or 2 / (period + 1) (Standard EMA)
    // The formula (PrevAvg * (period - 1) + CurrentValue) / period is equivalent to EMA with alpha = 1/period.
    // Let's stick to the (PrevAvg * (period - 1) + CurrentValue) / period formula which is commonly cited for RSI.
    DecimalLike period_decimal = static_cast<DecimalLike>(config_.period);
    DecimalLike avg_gain = (state_.avg_gain * (period_decimal - 1.0) + current_gain) / period_decimal;
    DecimalLike avg_loss = (state_.avg_loss * (period_decimal - 1.0) + current_loss) / period_decimal;


    state_.avg_gain = avg_gain;
    state_.avg_loss = avg_loss;
    state_.last_price = current_price; // Update last price for the next iteration
    state_.data_count++; // Increment count

    DecimalLike rs = 0.0;
    if (state_.avg_loss == 0.0) {
        // Handle division by zero when there are no losses. Results in infinite RS, 100 RSI.
        if (state_.avg_gain > 0) {
            rs = std::numeric_limits<DecimalLike>::infinity();
        } else {
             // Both gain and loss are 0. RS is undefined. Often treated as 0 or 50.
             // Let's return 0 RS (RSI 50) in this specific case (no change for period)
            rs = 0.0;
        }
    } else {
        rs = state_.avg_gain / state_.avg_loss;
    }

    // Calculate RSI
    DecimalLike rsi_value = 0.0;
    if (std::isinf(rs)) {
        rsi_value = 100.0; // If only gains (infinite RS)
    } else {
        // Ensure 1.0 + rs is not zero or negative, although RS should be >= 0
        DecimalLike denominator = 1.0 + rs;
        if (denominator <= 0.0) { // Should not happen if RS >= 0
             spdlog::error("RSI compute error: Unexpected denominator ({}) for {}", denominator, config_.name);
             return false; // Indicate computation failed
        }
        rsi_value = 100.0 - (100.0 / denominator);
    }

    *output_value = rsi_value;

    spdlog::debug("Computed {}: {}", config_.name, *output_value);

    return true; // RSI computed
}

} // namespace Indicators