#ifndef INDICATOR_ENGINE_INDICATORS_MACD_H
#define INDICATOR_ENGINE_INDICATORS_MACD_H

#include "../data_manager.h" // Need access to data buffers
#include "ema.h" // MACD uses EMAs
#include <string>
#include <vector>
#include <cmath> // For std::exp (EMA alpha)
#include <memory> // For unique_ptr

namespace Indicators {

// Configuration for a MACD indicator instance
struct MacdConfig {
    std::string name;     // e.g., "MACD_12_26_9"
    std::string symbol;   // e.g., "btc_usdt"
    int fast_period;      // e.g., 12
    int slow_period;      // e.g., 26
    int signal_period;    // e.g., 9
};

// Moving Average Convergence Divergence Calculator
// Manages internal EMA calculators.
class MACD {
public:
    MACD(const MacdConfig& config);

    // Compute MACD line, Signal line, and Histogram for the latest data point
    // Returns true if values were computed, false otherwise (not enough data)
    // Output parameters: macd_line, signal_line, histogram
    bool compute(const IndicatorEngine::DataManager& data_manager,
                 DecimalLike* macd_line_out,
                 DecimalLike* signal_line_out,
                 DecimalLike* histogram_out);

private:
    MacdConfig config_;

    // Internal EMA calculators
    std::unique_ptr<EMA> fast_ema_calc_;
    std::unique_ptr<EMA> slow_ema_calc_;
    std::unique_ptr<EMA> signal_ema_calc_; // EMA of the MACD line

    // State for tracking MACD line values needed for the signal line EMA
    std::deque<DecimalLike> macd_line_history_;
    int macd_line_history_size_; // Size needed is signal_period

    // Helper to compute the Signal line EMA using the history buffer
    bool compute_signal_ema(DecimalLike* signal_line_out);
};

} // namespace Indicators

#endif // INDICATOR_ENGINE_INDICATORS_MACD_H