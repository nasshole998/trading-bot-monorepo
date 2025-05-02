#include "indicators/macd.h"
#include "utils/conversions.h" // For DecimalLike, NaN check
#include <cmath> // For std::abs, std::isnan, std::isinf
#include <limits> // For numeric_limits
#include <numeric> // For std::accumulate
#include <deque> // For macd_line_history_
#include <vector> // For getRecentPrices return
#include <spdlog/spdlog.h> // For logging


namespace Indicators {

MACD::MACD(const MacdConfig& config) : config_(config) {
    if (config_.fast_period <= 0 || config_.slow_period <= 0 || config_.signal_period <= 0 || config_.fast_period >= config_.slow_period) {
        spdlog::error("MACD config error: Invalid periods in config {}. Fast: {}, Slow: {}, Signal: {}",
                      config_.name, config_.fast_period, config_.slow_period, config_.signal_period);
        // Handle invalid config
    }

    // MACD requires three EMAs. The first two (fast and slow) are based on price.
    // The third (signal) is an EMA of the MACD line itself.
    // The standard alpha = 2 / (period + 1) is used for all EMAs here.
    alpha_fast_ = (config_.fast_period > 0) ? 2.0 / (static_cast<DecimalLike>(config_.fast_period) + 1.0) : 0.0;
    alpha_slow_ = (config_.slow_period > 0) ? 2.0 / (static_cast<DecimalLike>(config_.slow_period) + 1.0) : 0.0;
    alpha_signal_ = (config_.signal_period > 0) ? 2.0 / (static_cast<DecimalLike>(config_.signal_period) + 1.0) : 0.0;


    // Internal state for MACD calculation
    state_.fast_ema = std::numeric_limits<DecimalLike>::quiet_NaN();
    state_.slow_ema = std::numeric_limits<DecimalLike>::quiet_NaN();
    state_.signal_ema = std::numeric_limits<DecimalLike>::quiet_NaN();
    state_.last_price = std::numeric_limits<DecimalLike>::quiet_NaN();
    state_.data_count = 0; // Count data points processed

    // MACD line history size needed for the initial SMA of the Signal EMA
    // Signal EMA (period S) starts with an SMA of the first S MACD values.
    // MACD values are only computed after Slow EMA is ready (needs SlowPeriod prices).
    // So, total prices needed for the very first Signal EMA value: SlowPeriod (for Slow EMA) + SignalPeriod (for MACD values) - 1 (overlap)
    // This state management based on data_count is tricky.
    // A history buffer for MACD line values is cleaner.
    macd_line_history_size_ = config_.signal_period; // Need 'signal_period' MACD values for Signal EMA initialization


    spdlog::debug("MACD {} initialized with periods F:{}, S:{}, Sig:{}",
                  config_.name, config_.fast_period, config_.slow_period, config_.signal_period);
}

// Helper to compute EMA value based on latest price and previous EMA state
// This version works for Fast and Slow EMAs based on price.
bool MACD::compute_ema_price(const IndicatorEngine::DataManager& data_manager,
                             int period,
                             DecimalLike alpha,
                             DecimalLike latest_price,
                             DecimalLike& prev_ema_state, // Reference to the state variable
                             int& data_count_state) // Reference to the state variable
{
    if (period <= 0) return false; // Cannot compute with invalid period

    // Handle initialization (first EMA value is SMA-like)
    if (data_count_state < period) {
        // We are still accumulating data for the initial period
        // Try to compute initial value if we have enough data now
        auto recent_prices = data_manager.getRecentPrices(config_.symbol, period);
        if (recent_prices.size() >= static_cast<size_t>(period)) {
             DecimalLike sum = std::accumulate(recent_prices.begin(), recent_prices.end(), 0.0);
             prev_ema_state = sum / static_cast<DecimalLike>(period); // Store initial SMA as the first EMA value
             data_count_state = period; // Mark as having processed the initial period

             // std::cout << "Computed Initial EMA (period " << period << "): " << prev_ema_state << std::endl; // Debug
             return true; // Initial value computed
        } else {
             // Still not enough data for initial averages
             data_count_state++; // Count the current price towards the initial period
             return false; // Cannot compute EMA yet
        }
    }


    // --- EMA Computation (Standard Smoothing) ---
    // Ensure we have processed at least 'period' data points before standard smoothing
    if (data_count_state >= period) {
        prev_ema_state = (latest_price - prev_ema_state) * alpha + prev_ema_state;
        data_count_state++; // Increment count
        return true; // EMA computed
    } else {
        // Should not happen with the current logic flow if compute_initial_value is called first
        data_count_state++; // Increment count
        return false; // Not enough data for standard smoothing yet
    }
}


// Helper to compute EMA value based on MACD line and previous EMA state
// This version works for the Signal EMA based on MACD line values.
bool MACD::compute_ema_macd_line(
                             int period,
                             DecimalLike alpha,
                             DecimalLike latest_macd_line,
                             DecimalLike& prev_ema_state, // Reference to the state variable
                             int& data_count_state, // Reference to the state variable
                             const std::deque<DecimalLike>& macd_line_history // History needed for initial SMA
                             )
{
    if (period <= 0) return false; // Cannot compute with invalid period

    // Handle initialization (first Signal EMA value is SMA-like over first 'period' MACD values)
    if (data_count_state < period) {
        // Need 'period' MACD line values in the history buffer for the initial SMA
        if (macd_line_history.size() >= static_cast<size_t>(period)) {
             DecimalLike sum_macd = std::accumulate(macd_line_history.begin(), macd_line_history.end(), 0.0);
             prev_ema_state = sum_macd / static_cast<DecimalLike>(period); // Store initial SMA as the first Signal EMA value
             data_count_state = period; // Mark as having processed the initial period

             // std::cout << "Computed Initial Signal EMA (period " << period << "): " << prev_ema_state << std::endl; // Debug
             return true; // Initial value computed
         } else {
             data_count_state++; // Count MACD values towards initial period
             return false; // Not enough MACD values for initial Signal EMA yet
         }
    }


    // --- EMA Computation (Standard Smoothing) ---
     if (data_count_state >= period) {
        prev_ema_state = (latest_macd_line - prev_ema_state) * alpha + prev_ema_state;
        data_count_state++; // Increment count
        return true; // EMA computed
    } else {
        // Should not happen with the current logic flow
        data_count_state++; // Increment count
        return false; // Not enough data for standard smoothing yet
    }
}


bool MACD::compute(const IndicatorEngine::DataManager& data_manager,
                 DecimalLike* macd_line_out,
                 DecimalLike* signal_line_out,
                 DecimalLike* histogram_out) {

    if (!macd_line_out || !signal_line_out || !histogram_out) {
         spdlog::error("MACD compute error: Output pointers are null for {}", config_.name);
         return false;
    }
     if (config_.fast_period <= 0 || config_.slow_period <= 0 || config_.signal_period <= 0 || config_.fast_period >= config_.slow_period) {
         spdlog::error("MACD compute error: Invalid periods for {}", config_.name);
         return false; // Cannot compute with invalid periods
     }


    // Get the latest price for the symbol
    auto latest_price_opt = data_manager.getLatestPrice(config_.symbol);

    if (!latest_price_opt) {
        spdlog::debug("MACD compute info: No latest price for {}", config_.symbol);
        return false; // No data yet
    }

    DecimalLike current_price = latest_price_opt.value();

    // Update Fast and Slow EMAs (based on price)
    // Use state_.data_count for overall progress tracking, pass by reference to helpers
    bool fast_computed = compute_ema_price(data_manager, config_.fast_period, alpha_fast_, current_price, state_.fast_ema, state_.data_count);
    bool slow_computed = compute_ema_price(data_manager, config_.slow_period, alpha_slow_, current_price, state_.slow_ema, state_.data_count);

    // Need both fast and slow EMA computed to calculate MACD line
    if (std::isnan(state_.fast_ema) || std::isnan(state_.slow_ema)) {
         spdlog::trace("MACD compute info: Fast or Slow EMA not ready for {}", config_.name);
         state_.last_price = current_price; // Update last price for next EMA calculation
         return false; // Not enough data for both EMAs yet
    }

    // Compute MACD line: FastEMA - SlowEMA
    DecimalLike macd_line = state_.fast_ema - state_.slow_ema;
    *macd_line_out = macd_line; // Output MACD line

    // Store MACD line value for Signal line calculation history
    macd_line_history_.push_back(macd_line);
    if (macd_line_history_.size() > static_cast<size_t>(macd_line_history_size_)) {
        macd_line_history_.pop_front();
    }


    // Compute Signal line (EMA of the MACD line)
    // Use compute_ema_macd_line helper. It manages its own state (signal_ema_value_, signal_ema_data_count_)
    // within the MACD instance's state.
    bool signal_computed = compute_ema_macd_line(
                                                 config_.signal_period,
                                                 alpha_signal_,
                                                 macd_line, // The latest value is the current MACD line
                                                 state_.signal_ema,
                                                 state_.signal_ema_data_count, // Need a separate count for signal EMA
                                                 macd_line_history_ // Pass history for initial SMA
                                                 );


    // Need both MACD line and Signal line computed to calculate Histogram
    if (!signal_computed || std::isnan(state_.signal_ema)) {
        spdlog::trace("MACD compute info: Signal EMA not ready for {}", config_.name);
        state_.last_price = current_price; // Update last price
        return false; // Not enough data for Signal line yet
    }

    *signal_line_out = state_.signal_ema; // Output Signal line


    // Compute Histogram: MACD Line - Signal Line
    DecimalLike histogram = *macd_line_out - *signal_line_out;
    *histogram_out = histogram; // Output Histogram

    // Update last price for next EMA calculation
    state_.last_price = current_price;

    spdlog::debug("Computed {}: MACD={}, Signal={}, Hist={}", config_.name, *macd_line_out, *signal_line_out, *histogram_out);

    return true; // MACD values computed
}

} // namespace Indicators