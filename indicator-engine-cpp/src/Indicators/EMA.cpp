// indicator-engine-cpp/src/indicators/EMA.cpp
#include "indicators/EMA.h"
#include <limits>
#include <stdexcept>
#include "logging.h" // Use logging

EMA::EMA(size_t period) :
    m_period(period),
    m_name("EMA(" + std::to_string(period) + ")"),
    m_smoothing_factor(2.0 / (static_cast<IndicatorValue>(period) + 1.0))
#ifdef USE_TA_LIB
    , m_outReal(nullptr)
    , m_outBegIdx(0)
    , m_outNbElement(0)
    , m_lookback(static_cast<int>(period) - 1) // TA-Lib lookback for EMA is period-1
#endif
{
    if (period == 0) {
        throw std::invalid_argument("EMA period must be positive.");
    }
     m_price_history.reserve(period + 10); // Reserve some space

#ifdef USE_TA_LIB
    m_outReal = new TA_Real[1]; // Allocate output buffer
     if (!m_outReal) {
         throw std::runtime_error("Failed to allocate memory for TA-Lib output");
    }
#endif
}

EMA::~EMA() {
#ifdef USE_TA_LIB
    delete[] m_outReal;
#endif
}

void EMA::update(IndicatorValue price) {
     std::lock_guard<std::mutex> lock(m_calc_mutex);

#ifdef USE_TA_LIB
    m_price_history.push_back(price);
    // Optional trimming of history
    if (m_price_history.size() > m_period * 2 + 5) {
       m_price_history.erase(m_price_history.begin(), m_price_history.begin() + (m_price_history.size() - (m_period * 2)));
    }
#else
    m_update_count++;
    if (m_update_count == m_period) {
        // First EMA value is the SMA of the first 'period' prices
        // This requires storing the first 'period' prices temporarily or using a buffer.
        // For simplicity here, let's assume the *first* update call after enough data
        // will establish the initial SMA. A better way uses the circular buffer.
        // Let's refine: EMA needs the *previous* EMA. The first EMA uses an SMA.
         if (std::isnan(m_current_ema)) { // Calculate initial SMA if not already done
             // This part is tricky without storing initial prices.
             // Let's assume the first call to calculate() handles initialization.
             m_current_ema = price; // Seed with the first price needed for SMA calc in calculate()
         } else {
              m_current_ema = (price - m_current_ema) * m_smoothing_factor + m_current_ema;
         }

    } else if (m_update_count > m_period) {
         // Standard EMA calculation using previous EMA
         m_current_ema = (price - m_current_ema) * m_smoothing_factor + m_current_ema;
    } else {
         // Not enough data yet, just store price implicitly for initial SMA calculation later
         // If we had a CircularBuffer here, we'd push to it.
         m_current_ema = price; // Keep track of latest price for potential SMA calculation
    }
#endif
}

bool EMA::is_ready() const {
#ifdef USE_TA_LIB
    std::lock_guard<std::mutex> lock(m_calc_mutex);
    // TA-Lib needs 'period' prices to calculate the first EMA value (lookback is period-1)
    return m_price_history.size() >= m_period;
#else
    // Manual calculation needs 'period' prices for the initial SMA.
    return m_update_count >= m_period;
#endif
}

std::optional<IndicatorValue> EMA::calculate() const {
    std::lock_guard<std::mutex> lock(m_calc_mutex);

    if (!is_ready()) {
        return std::nullopt;
    }

#ifdef USE_TA_LIB
    // --- TA-Lib Calculation ---
    if (m_price_history.empty()) return std::nullopt;

    int startIdx = 0;
    int endIdx = static_cast<int>(m_price_history.size()) - 1;

     // Ensure we have enough data points based on lookback
    if (endIdx < m_lookback) {
         LOG_TRACE("EMA ({}) calculation skipped: Not enough data for TA-Lib ({} < {})", m_period, endIdx + 1, m_lookback + 1);
         return std::nullopt;
    }

    TA_RetCode retCode = TA_EMA(
        startIdx,
        endIdx,
        m_price_history.data(),
        static_cast<int>(m_period),
        &m_outBegIdx,
        &m_outNbElement,
        m_outReal
    );

    if (retCode == TA_SUCCESS && m_outNbElement > 0) {
        return m_outReal[0]; // Return the latest calculated EMA
    } else {
         LOG_WARN("TA_EMA calculation failed for period {}. RetCode: {}, NbElement: {}, InputSize: {}",
                 m_period, static_cast<int>(retCode), m_outNbElement, m_price_history.size());
        return std::nullopt;
    }

#else
    // --- Manual Calculation ---
    // The update function now maintains m_current_ema directly after the initial period.
    // However, the very first calculation needs the SMA.
    if (m_update_count == m_period) {
        // This is where the initial SMA should be calculated.
        // This requires access to the first 'period' prices, which are not
        // stored in this simplified manual version.
        // A CircularBuffer would be needed here for a correct manual implementation.
        // Returning NaN or nullopt until the *next* update is safer here.
        LOG_DEBUG("EMA({}) manual calculation needs one more update for first value.", m_period);
        return std::nullopt; // Cannot calculate initial SMA without history
    } else if (m_update_count > m_period) {
         if (std::isnan(m_current_ema)) {
             // Should not happen if update logic is correct
             LOG_ERROR("EMA({}) internal state inconsistent: NaN EMA after ready.", m_period);
             return std::nullopt;
         }
         return m_current_ema;
    } else {
        // Should be caught by is_ready()
        return std::nullopt;
    }
#endif // USE_TA_LIB
}


const std::string& EMA::get_name() const {
    return m_name;
}

size_t EMA::get_period() const {
    return m_period;
}
