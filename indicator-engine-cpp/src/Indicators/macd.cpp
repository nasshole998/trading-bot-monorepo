#include "indicators/macd.h"
#include "utils/conversions.h" // For DecimalLike, NaN check
#include <iostream> // For debug output
#include <limits> // For numeric_limits
#include <numeric> // For std::accumulate

namespace Indicators {

MACD::MACD(const MacdConfig& config) : config_(config) {
    if (config_.fast_period <= 0 || config_.slow_period <= 0 || config_.signal_period <= 0 || config_.fast_period >= config_.slow_period) {
        std::cerr << "Error: Invalid MACD periods in config. Config: " << config_.name << std::endl;
        // Handle invalid config
    }

    // Create internal EMA calculators
    fast_ema_calc_ = std::make_unique<EMA>(EmaConfig{config_.name + "_FastEMA", config_.symbol, config_.fast_period});
    slow_ema_calc_ = std::make_unique<EMA>(EmaConfig{config_.name + "_SlowEMA", config_.symbol, config_.slow_period});
    // Signal EMA is an EMA of the MACD line, not the price. Its period is config_.signal_period.
    // The calculation is handled within MACD::compute, potentially using a helper function.
    // The signal EMA requires the *values* of the MACD line.

    // Need a history buffer for MACD line values to compute the signal EMA (if using SMA initialization)
    // Or, manage the signal EMA state directly. Let's manage signal EMA state directly.
     signal_ema_calc_ = std::make_unique<EMA>(EmaConfig{config_.name + "_SignalEMA", config_.symbol, config_.signal_period});

    // The size of the history buffer for MACD line values needed for the Signal EMA's initial SMA
    // depends on whether the Signal EMA uses an SMA or just starts smoothing immediately.
    // If the Signal EMA starts with an SMA over the first 'signal_period' MACD values,
    // we need a buffer of size 'signal_period'.
    macd_line_history_size_ = config_.signal_period;
}


bool MACD::compute(const IndicatorEngine::DataManager& data_manager,
                 DecimalLike* macd_line_out,
                 DecimalLike* signal_line_out,
                 DecimalLike* histogram_out) {

    if (!macd_line_out || !signal_line_out || !histogram_out) {
         std::cerr << "Error: Output pointers are null for MACD." << std::endl;
         return false;
    }
     if (config_.fast_period <= 0 || config_.slow_period <= 0 || config_.signal_period <= 0 || config_.fast_period >= config_.slow_period) return false; // Invalid config


    // Compute Fast and Slow EMAs
    DecimalLike fast_ema_value, slow_ema_value;
    bool fast_computed = fast_ema_calc_->compute(data_manager, &fast_ema_value);
    bool slow_computed = slow_ema_calc_->compute(data_manager, &slow_ema_value);

    // Need both fast and slow EMA computed to calculate MACD line
    if (!fast_computed || !slow_computed) {
         return false; // Not enough data for both EMAs yet
    }

    // Compute MACD line: FastEMA - SlowEMA
    DecimalLike macd_line = fast_ema_value - slow_ema_value;
    *macd_line_out = macd_line; // Output MACD line

    // Store MACD line value for Signal line calculation
    macd_line_history_.push_back(macd_line);
    if (macd_line_history_.size() > macd_line_history_size_) {
        macd_line_history_.pop_front();
    }


    // Compute Signal line (EMA of the MACD line)
    // This requires enough *MACD line values* in the history buffer for the Signal EMA's initial value
    // (if it uses an SMA initialization) or simply enough to start smoothing.
    // Using the internal signal_ema_calc_ (which is an EMA of *price*, not MACD line) was incorrect.
    // We need an EMA calculation function that works on the `macd_line_history_` buffer.

    // Let's reuse the EMA logic conceptually but apply it to the MACD line history.
    // The signal EMA calculation needs its own state (previous value, data count).

     // --- Signal Line Computation ---
     DecimalLike signal_line_value;
     // Need enough MACD line values to compute the signal EMA
     if (macd_line_history_.size() < config_.signal_period) {
          return false; // Not enough MACD values for Signal line yet
     }

     // Compute the signal EMA value based on macd_line_history_
     // This requires implementing EMA logic here or having a dedicated helper that works on deques.
     // Let's implement a simple sequential EMA update here for the signal line state.
     // We need a separate state variable for the signal EMA.
     // Let's add `signal_ema_value_` and `signal_ema_data_count_` to MACD's state.

     // We are calculating the signal EMA for the *latest* macd_line value.
     // This is an EMA calculation on a series where the "price" is the MACD line value.
     // The period is config_.signal_period.
     // The alpha is alpha_signal_.

     // Need to track the previous signal EMA value.
     // Let's add `DecimalLike signal_ema_value_ = std::numeric_limits<DecimalLike>::quiet_NaN();`
     // and `int signal_ema_data_count_ = 0;` to MACD's private members.

     // Check if signal EMA needs initialization (SMA over first signal_period MACD values)
     if (std::isnan(signal_ema_value_) && signal_ema_data_count_ < config_.signal_period) {
         // Need 'signal_period' MACD line values in the history buffer for initial SMA
         if (macd_line_history_.size() >= config_.signal_period) {
              DecimalLike sum_macd = std::accumulate(macd_line_history_.begin(), macd_line_history_.end(), 0.0);
              signal_ema_value_ = sum_macd / static_cast<DecimalLike>(config_.signal_period);
              signal_ema_data_count_ = config_.signal_period; // Mark as initialized
             // std::cout << "Computed Initial Signal EMA (" << config_.name << "): " << signal_ema_value_ << std::endl; // Debug
             // The first actual histogram is computed after the first signal value.
             // We might not output values until both MACD and Signal are ready.
         } else {
             signal_ema_data_count_++; // Count MACD values towards signal init
             return false; // Not enough MACD values for Signal line yet
         }
     }

    // If signal EMA is initialized (or standard smoothing after init)
    if (!std::isnan(signal_ema_value_)) {
        // Standard EMA smoothing on the latest MACD line value
        signal_ema_value_ = (macd_line - signal_ema_value_) * alpha_signal_ + signal_ema_value_;
        signal_ema_data_count_++; // Increment count
        *signal_line_out = signal_ema_value_; // Output Signal line
    } else {
        return false; // Still not enough data for Signal line
    }


    // Need both MACD line and Signal line computed to calculate Histogram
    if (std::isnan(*macd_line_out) || std::isnan(*signal_line_out)) {
        return false; // Should not happen if we reached here, but defensive
    }

    // Compute Histogram: MACD Line - Signal Line
    DecimalLike histogram = *macd_line_out - *signal_line_out;
    *histogram_out = histogram; // Output Histogram

    // std::cout << "Computed MACD (" << config_.name << ") - MACD: " << *macd_line_out << ", Signal: " << *signal_line_out << ", Hist: " << *histogram_out << std::endl; // Debug


    return true; // MACD values computed
}

} // namespace Indicators