#ifndef INDICATOR_ENGINE_INDICATORS_SMA_H
#define INDICATOR_ENGINE_INDICATORS_SMA_H

#include "../data_manager.h" // Need access to data buffers
#include <vector>
#include <string>

namespace Indicators {

// Configuration for an SMA indicator instance
struct SmaConfig {
    std::string name;     // e.g., "SMA_50"
    std::string symbol;   // e.g., "btc_usdt"
    int period;           // e.g., 50
};

// Simple Moving Average Calculator
class SMA {
public:
    SMA(const SmaConfig& config);

    // Compute SMA for the latest data point
    // Returns true if a new value was computed, false otherwise (e.g., not enough data)
    bool compute(const IndicatorEngine::DataManager& data_manager, DecimalLike* output_value);

private:
    SmaConfig config_;
    // SMA is memoryless over the window, state is implicitly in DataManager's buffer
};

} // namespace Indicators

#endif // INDICATOR_ENGINE_INDICATORS_SMA_H