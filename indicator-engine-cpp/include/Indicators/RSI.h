// indicator-engine-cpp/include/indicators/RSI.h
#pragma once

#include "IndicatorBase.h"
#include "../CircularBuffer.h"
#include <string>
#include <optional>
#include <vector>

#ifdef USE_TA_LIB // Conditional compilation based on CMake option
#include "ta_libc.h"
#endif

class RSI : public IndicatorBase {
public:
    explicit RSI(size_t period);
    ~RSI(); // Destructor needed if using TA-Lib state

    void update(IndicatorValue price) override;
    std::optional<IndicatorValue> calculate() const override;
    const std::string& get_name() const override;
    size_t get_period() const override;
    bool is_ready() const override;

private:
    const size_t m_period;
    const std::string m_name;

#ifdef USE_TA_LIB
    // TA-Lib specific members
    mutable TA_Real* m_outReal; // Output buffer for TA-Lib
    mutable int m_outBegIdx;
    mutable int m_outNbElement;
    // TA-Lib lookback is period - 1 for EMA-based RSI, but calculation needs period prices
    const int m_lookback;
#else
    // Manual calculation members
    CircularBuffer<IndicatorValue> m_prices;
    IndicatorValue m_prev_avg_gain = 0.0;
    IndicatorValue m_prev_avg_loss = 0.0;
    size_t m_update_count = 0;
#endif // USE_TA_LIB

    // Common members
    mutable std::vector<double> m_price_history; // Store recent prices for calculation
    mutable std::mutex m_calc_mutex; // Mutex to protect calculation state if needed
};
