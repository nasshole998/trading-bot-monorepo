#ifndef INDICATOR_ENGINE_INDICATORS_EMA_H
#define INDICATOR_ENGINE_INDICATORS_EMA_H

#include "../data_manager.h" // Need access to data buffers
#include <vector>
#include <string>
#include <cmath> // For std::exp (alpha)

namespace Indicators {

// Configuration for an EMA indicator instance
struct EmaConfig {
    std::string name;     // e.g., "EMA_50"
    std::string symbol;   // e.g., "btc_usdt"
    int period;           // e.g., 50
};

// Exponential Moving Average Calculator
class EMA {
public:
    EMA(const EmaConfig& config);

    // Compute EMA for the latest data point
    // EMA is stateful (uses previous value).
    // Returns true if a new value was computed, false otherwise (not enough data for initial value)
    bool compute(const IndicatorEngine::DataManager& data_manager, DecimalLike* output_value);

    // Get the current state (needed for chained indicators like MACD)
    DecimalLike getCurrentValue() const { return current_value_; }


private:
    EmaConfig config_;

    // EMA smoothing alpha: 2 / (period + 1)
    DecimalLike alpha_;

    // State for EMA calculation
    DecimalLike current_value_ = std::numeric_limits<DecimalLike>::quiet_NaN(); // Current EMA value
    int data_count_ = 0; // Count of data points processed for this instance

    // Helper to compute the first EMA value (SMA-like)
    bool compute_initial_value(const IndicatorEngine::DataManager& data_manager);
};

} // namespace Indicators

#endif // INDICATOR_ENGINE_INDICATORS_EMA_H