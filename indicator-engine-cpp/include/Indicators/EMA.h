// indicator-engine-cpp/include/indicators/EMA.h
#pragma once

#include "IndicatorBase.h"
#include <string>
#include <optional>
#include <vector> // Needed for TA-Lib input

#ifdef USE_TA_LIB
#include "ta_libc.h"
#endif

class EMA : public IndicatorBase {
public:
    explicit EMA(size_t period);
    ~EMA(); // Destructor needed if using TA-Lib state

    void update(IndicatorValue price) override;
    std::optional<IndicatorValue> calculate() const override;
    const std::string& get_name() const override;
    size_t get_period() const override;
    bool is_ready() const override;

private:
    const size_t m_period;
    const std::string m_name;
    const IndicatorValue m_smoothing_factor; // Alpha = 2 / (period + 1)

#ifdef USE_TA_LIB
    mutable TA_Real* m_outReal;
    mutable int m_outBegIdx;
    mutable int m_outNbElement;
    const int m_lookback;
    mutable std::vector<double> m_price_history; // Store recent prices for TA-Lib
#else
    IndicatorValue m_current_ema = std::numeric_limits<IndicatorValue>::quiet_NaN();
    size_t m_update_count = 0;
#endif // USE_TA_LIB

    mutable std::mutex m_calc_mutex;
};
