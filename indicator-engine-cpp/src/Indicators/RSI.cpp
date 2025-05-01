// indicator-engine-cpp/src/indicators/RSI.cpp
#include "indicators/RSI.h"
#include <cmath> // For std::max, std::fabs
#include <limits> // For NaN
#include <stdexcept>
#include <numeric> // For std::accumulate
#include "logging.h" // Use logging

RSI::RSI(size_t period) :
    m_period(period),
    m_name("RSI(" + std::to_string(period) + ")")
#ifdef USE_TA_LIB
    , m_outReal(nullptr)
    , m_outBegIdx(0)
    , m_outNbElement(0)
    // TA-Lib's RSI lookback is 'period'. Calculation needs 'period+1' prices for first value.
    , m_lookback(static_cast<int>(period))
#else
    , m_prices(period + 1) // Need one extra price to calculate the first change
#endif
{
    if (period == 0) {
        throw std::invalid_argument("RSI period must be positive.");
    }
    m_price_history.reserve(period + 10); // Reserve some space

#ifdef USE_TA_LIB
    // Allocate output buffer for TA-Lib (only needs to hold 1 value)
    m_outReal = new TA_Real[1];
    if (!m_outReal) {
         throw std::runtime_error("Failed to allocate memory for TA-Lib output");
    }
#endif
}

RSI::~RSI() {
#ifdef USE_TA_LIB
    delete[] m_outReal; // Free TA-Lib output buffer
#endif
}


void RSI::update(IndicatorValue price) {
    std::lock_guard<std::mutex> lock(m_calc_mutex); // Lock for thread safety during update

#ifdef USE_TA_LIB
    // Store price history for TA-Lib calculation
    m_price_history.push_back(price);
    // Optional: Trim history to avoid unbounded growth, keep slightly more than needed
    // e.g., keep 2 * period prices
    if (m_price_history.size() > m_period * 2 + 5) { // Keep a bit extra margin
       m_price_history.erase(m_price_history.begin(), m_price_history.begin() + (m_price_history.size() - (m_period * 2)));
    }
#else
    // Manual calculation update
    m_prices.push(price);
    m_update_count++;
#endif
}

bool RSI::is_ready() const {
#ifdef USE_TA_LIB
    std::lock_guard<std::mutex> lock(m_calc_mutex);
    // TA-Lib needs 'period' prices to calculate the first RSI value.
    // The lookback returned by TA_RSI_Lookback is 'period'.
    return m_price_history.size() >= static_cast<size_t>(m_lookback + 1);
#else
    // Manual calculation needs 'period' price *changes*, which requires 'period + 1' prices.
    // It also needs the initial average gain/loss calculation to complete.
    return m_update_count >= m_period + 1;
#endif
}


std::optional<IndicatorValue> RSI::calculate() const {
    std::lock_guard<std::mutex> lock(m_calc_mutex); // Lock for thread safety during calculation

    if (!is_ready()) {
        return std::nullopt;
    }

#ifdef USE_TA_LIB
    // --- TA-Lib Calculation ---
    if (m_price_history.empty()) return std::nullopt; // Should not happen if is_ready is true

    // TA-Lib calculates over a range. We only need the latest value.
    // Provide enough data for TA-Lib to calculate at least one value.
    int startIdx = 0;
    int endIdx = static_cast<int>(m_price_history.size()) - 1;

    // Ensure we have enough data points based on lookback
    if (endIdx < m_lookback) {
         LOG_TRACE("RSI ({}) calculation skipped: Not enough data for TA-Lib ({} < {})", m_period, endIdx + 1, m_lookback + 1);
         return std::nullopt;
    }

    TA_RetCode retCode = TA_RSI(
        startIdx,           // Start index for input data
        endIdx,             // End index for input data
        m_price_history.data(), // Input price data array
        static_cast<int>(m_period), // RSI period
        &m_outBegIdx,       // Output: Index of the first output value
        &m_outNbElement,    // Output: Number of output values calculated
        m_outReal           // Output buffer (size 1 is sufficient for latest value)
    );

    if (retCode == TA_SUCCESS && m_outNbElement > 0) {
        // TA-Lib calculation succeeded, return the last calculated value
        // The output array `m_outReal` contains `m_outNbElement` values.
        // The last value corresponds to the input `endIdx`.
        // Since our output buffer has size 1, TA-Lib *should* only write the last value if called correctly.
        // Let's assume TA_RSI with endIdx calculates the RSI for that index.
        return m_outReal[0]; // Return the single value calculated
    } else {
        LOG_WARN("TA_RSI calculation failed for period {}. RetCode: {}, NbElement: {}, InputSize: {}",
                 m_period, static_cast<int>(retCode), m_outNbElement, m_price_history.size());
        return std::nullopt; // Calculation failed or produced no output
    }

#else
    // --- Manual Calculation (Wilder's Smoothing) ---
    if (!m_prices.is_full()) { // Need period+1 prices
        return std::nullopt;
    }

    IndicatorValue current_avg_gain = 0.0;
    IndicatorValue current_avg_loss = 0.0;

    // Calculate initial average gain/loss only once
    if (m_update_count == m_period + 1) {
        IndicatorValue total_gain = 0.0;
        IndicatorValue total_loss = 0.0;
        for (size_t i = 1; i <= m_period; ++i) {
            // CircularBuffer[0] is latest, [1] is previous, etc.
            // We need change from [i] to [i-1]
             IndicatorValue change = m_prices[i-1] - m_prices[i];
            if (change > 0) {
                total_gain += change;
            } else {
                total_loss += std::fabs(change);
            }
        }
         // Use mutable members here - const_cast is generally bad practice,
         // but needed if calculate() must be const and update internal state
         // for the *first* calculation. A better design might make calculate() non-const
         // or separate initialization. Let's use const_cast carefully here.
        auto* mutable_self = const_cast<RSI*>(this);
        mutable_self->m_prev_avg_gain = total_gain / m_period;
        mutable_self->m_prev_avg_loss = total_loss / m_period;
        current_avg_gain = m_prev_avg_gain;
        current_avg_loss = m_prev_avg_loss;

    } else { // Subsequent calculations use Wilder's smoothing
        IndicatorValue current_change = m_prices[0] - m_prices[1];
        IndicatorValue current_gain = std::max(0.0, current_change);
        IndicatorValue current_loss = std::max(0.0, -current_change); // std::fabs(std::min(0.0, current_change));

        // Use mutable members again with const_cast
        auto* mutable_self = const_cast<RSI*>(this);
        mutable_self->m_prev_avg_gain = (m_prev_avg_gain * (m_period - 1) + current_gain) / m_period;
        mutable_self->m_prev_avg_loss = (m_prev_avg_loss * (m_period - 1) + current_loss) / m_period;
        current_avg_gain = m_prev_avg_gain;
        current_avg_loss = m_prev_avg_loss;
    }


    if (current_avg_loss == 0.0) {
        return 100.0; // Avoid division by zero, RSI is 100
    }

    IndicatorValue rs = current_avg_gain / current_avg_loss;
    IndicatorValue rsi = 100.0 - (100.0 / (1.0 + rs));

    return rsi;
#endif // USE_TA_LIB
}


const std::string& RSI::get_name() const {
    return m_name;
}

size_t RSI::get_period() const {
    return m_period;
}
