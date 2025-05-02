#include "indicators/rsi.h"
#include "utils/conversions.h" // For DecimalLike, NaN check
#include <cmath> // For std::abs
#include <iostream> // For debug output
#include <limits> // For numeric_limits
#include <numeric> // For std::accumulate

namespace Indicators {

RSI::RSI(const RsiConfig& config) : config_(config) {
    if (config_.period <= 1) { // RSI needs at least 2 data points for the first gain/loss calculation
         std::cerr << "Error: RSI period must be greater than 1. Config: " << config_.name << std::endl;
         // Handle invalid config
    }
     state_ = RsiState(); // Initialize state
}

// Compute the first average gain/loss over the initial period (SMA-like average)
bool RSI::compute_initial_averages(const IndicatorEngine::DataManager& data_manager) {
    // Need 'period' data points with differences, which requires period + 1 prices
    auto initial_prices = data_manager.getRecentPrices(config_.symbol, config_.period + 1);

    if (initial_prices.size() >= config_.period + 1) {
        DecimalLike total_gain = 0.0;
        DecimalLike total_loss = 0.0;

        for (size_t i = 0; i < config_.period; ++i) {
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
        state_.data_count = config_.period; // Mark as having processed the initial period
        // std::cout << "RSI Initial Averages (" << config_.name << ") - Gain: " << state_.avg_gain << ", Loss: " << state_.avg_loss << std::endl; // Debug
        return true;
    }
    return false; // Not enough data for initial averages
}


bool RSI::compute(const IndicatorEngine::DataManager& data_manager, DecimalLike* output_value) {
    if (!output_value) {
        std::cerr << "Error: output_value pointer is null for RSI." << std::endl;
        return false;
    }
     if (config_.period <= 1) return false;

    // Get the latest price for the symbol
    auto latest_price_opt = data_manager.getLatestPrice(config_.symbol);

    if (!latest_price_opt) {
        // No data yet
        return false;
    }

    DecimalLike current_price = latest_price_opt.value();

    // Handle initialization
    if (state_.data_count < config_.period) {
        // We are still accumulating data for the initial period
        // Try to compute initial averages if we have enough data now
        if (compute_initial_averages(data_manager)) {
             // If initial averages were just computed, we can't compute the final RSI yet
             // for the *latest* price. The next data point triggers the first smoothing calculation.
            state_.data_count++; // Count the current price
             return false; // Cannot compute RSI yet for *this* price
        } else {
             // Still not enough data for initial averages
             state_.data_count++; // Count the current price
             return false; // Cannot compute RSI yet
        }
    }


    // --- RSI Computation (Smoothing) ---

    // We need the previous price to calculate the latest gain/loss
    if (std::isnan(state_.last_price)) {
         // This should not happen if initialization was successful, but handle defensively
         state_.last_price = current_price;
         return false;
    }

    DecimalLike diff = current_price - state_.last_price;
    DecimalLike current_gain = (diff > 0) ? diff : 0.0;
    DecimalLike current_loss = (diff < 0) ? std::abs(diff) : 0.0;

    // Exponential Moving Average (Wilder's smoothing typically uses alpha = 1 / period)
    // Common implementation uses alpha = 2 / (period + 1), which is standard EMA.
    // Let's use 2/(N+1) as per the EMA definition already used.
    // alpha = 2 / (period + 1)
    DecimalLike alpha = 2.0 / (static_cast<DecimalLike>(config_.period) + 1.0); // Use same alpha as EMA

    DecimalLike avg_gain = (state_.avg_gain * (static_cast<DecimalLike>(config_.period) - 1.0) + current_gain) / static_cast<DecimalLike>(config_.period);
    DecimalLike avg_loss = (state_.avg_loss * (static_cast<DecimalLike>(config_.period) - 1.0) + current_loss) / static_cast<DecimalLike>(config_.period);

    state_.avg_gain = avg_gain;
    state_.avg_loss = avg_loss;
    state_.last_price = current_price; // Update last price for the next iteration
    state_.data_count++; // Increment count

    DecimalLike rs = 0.0;
    if (state_.avg_loss == 0.0) {
        // Handle division by zero when there are no losses in the window
        // This can happen initially or during strong uptrends.
        rs = std::numeric_limits<DecimalLike>::infinity();
    } else {
        rs = state_.avg_gain / state_.avg_loss;
    }

    // Calculate RSI
    DecimalLike rsi_value = 0.0;
    if (std::isinf(rs)) {
        rsi_value = 100.0; // If only gains
    } else {
        rsi_value = 100.0 - (100.0 / (1.0 + rs));
    }

    *output_value = rsi_value;

    // std::cout << "Computed RSI(" << config_.name << ") - Gain: " << avg_gain << ", Loss: " << avg_loss << ", RS: " << rs << ", RSI: " << *output_value << std::endl; // Debug

    return true; // RSI computed
}

} // namespace Indicators