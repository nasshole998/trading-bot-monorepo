#ifndef INDICATOR_ENGINE_INDICATORS_RSI_H
#define INDICATOR_ENGINE_INDICATORS_RSI_H

#include "../data_manager.h" // Need access to data buffers
#include <vector>
#include <string>
#include <cmath> // For std::abs
#include <limits> // For numeric_limits

namespace Indicators {

// Configuration for an RSI indicator instance
struct RsiConfig {
    std::string name;     // e.g., "RSI_14"
    std::string symbol;   // e.g., "btc_usdt"
    int period;           // e.g., 14
};

// Relative Strength Index Calculator
class RSI {
public:
    RSI(const RsiConfig& config);

    // Compute RSI for the latest data point
    // Requires previous average gain/loss, so it's stateful.
    // Returns true if a new value was computed, false otherwise (not enough data)
    bool compute(const IndicatorEngine::DataManager& data_manager, DecimalLike* output_value);

private:
    RsiConfig config_;

    // State for RSI calculation (Wilder's smoothing)
    struct RsiState {
        DecimalLike avg_gain = 0.0; // Exponentially smoothed average gain
        DecimalLike avg_loss = 0.0; // Exponentially smoothed average loss
        DecimalLike last_price = std::numeric_limits<DecimalLike>::quiet_NaN(); // Last price processed

        int data_count = 0; // Count of data points processed since initialization
    };

    RsiState state_;

    // Helper to compute the first average gain/loss over the initial period (SMA-like)
    bool compute_initial_averages(const IndicatorEngine::DataManager& data_manager);
};

} // namespace Indicators

#endif // INDICATOR_ENGINE_INDICATORS_RSI_H