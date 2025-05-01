// indicator-engine-cpp/src/indicators/SMA.cpp
#include "indicators/SMA.h"
#include <limits> // For NaN

SMA::SMA(size_t period) :
    m_period(period),
    m_name("SMA(" + std::to_string(period) + ")"),
    m_prices(period) // Initialize circular buffer with the period size
{
    if (period == 0) {
        throw std::invalid_argument("SMA period must be positive.");
    }
}

void SMA::update(IndicatorValue price) {
    m_prices.push(price); // Add the new price to the circular buffer
}

std::optional<IndicatorValue> SMA::calculate() const {
    if (!is_ready()) {
        return std::nullopt; // Not enough data yet
    }
    // Use the efficient mean calculation from the circular buffer
    return m_prices.mean();
}

const std::string& SMA::get_name() const {
    return m_name;
}

size_t SMA::get_period() const {
    return m_period;
}

bool SMA::is_ready() const {
    // SMA is ready when the circular buffer is full
    return m_prices.is_full();
}

