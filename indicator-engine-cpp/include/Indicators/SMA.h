// indicator-engine-cpp/include/indicators/SMA.h
#pragma once

#include "IndicatorBase.h"
#include "../CircularBuffer.h" // Include the circular buffer
#include <string>
#include <optional>

class SMA : public IndicatorBase {
public:
    explicit SMA(size_t period);

    void update(IndicatorValue price) override;
    std::optional<IndicatorValue> calculate() const override;
    const std::string& get_name() const override;
    size_t get_period() const override;
    bool is_ready() const override;

private:
    const size_t m_period;
    const std::string m_name;
    CircularBuffer<IndicatorValue> m_prices;
};