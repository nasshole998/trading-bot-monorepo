#include "indicators/sma.h"
#include "utils/conversions.h" // For DecimalLike
#include "utils/indicator_engine_error.h" // For Result
#include <numeric> // For std::accumulate
#include <vector> // Need vector for SIMD example
#include <spdlog/spdlog.h> // For logging
// Include SIMD headers if needed, e.g., <immintrin.h> for AVX


namespace Indicators {

SMA::SMA(const SmaConfig& config) : config_(config) {
    if (config_.period <= 0) {
        spdlog::error("SMA config error: Period must be positive. Config: {}", config_.name);
        // Note: In a real system, the constructor might throw or return an error
        // indication if validation fails, preventing creation of an invalid object.
    }
}

bool SMA::compute(const IndicatorEngine::DataManager& data_manager, DecimalLike* output_value) {
    if (!output_value) {
        spdlog::error("SMA compute error: output_value pointer is null for {}", config_.name);
        return false;
    }
     if (config_.period <= 0) {
         spdlog::error("SMA compute error: Invalid period ({}) for {}", config_.period, config_.name);
         return false; // Cannot compute with invalid period
    }

    // Get the recent price data for the symbol
    auto recent_prices = data_manager.getRecentPrices(config_.symbol, config_.period);

    if (recent_prices.size() < static_cast<size_t>(config_.period)) {
        // Not enough data yet to compute SMA
        spdlog::debug("SMA compute info: Not enough data ({}/{}) for {}", recent_prices.size(), config_.period, config_.name);
        return false;
    }

    // --- SMA Computation ---
    DecimalLike sum = 0.0;

    // --- SIMD Optimization (Conceptual Sketch for Summation) ---
    // This requires the data to be in a contiguous array/vector like std::vector or raw array.
    // std::vector<DecimalLike> data_window = recent_prices; // Copy or work directly on deque's underlying buffer if possible

    // if (!data_window.empty()) {
    //     // Example for AVX (Advanced Vector Extensions) working on doubles (4 doubles per __m256d)
    //     #ifdef __AVX__
    //     const int vector_size = 4;
    //     __m256d sum_vec = _mm256_setzero_pd(); // Initialize vector sum to zeros

    //     int i = 0;
    //     // Process chunks of 'vector_size' elements using SIMD
    //     for (; i + vector_size <= data_window.size(); i += vector_size) {
    //         __m256d data_vec = _mm256_loadu_pd(&data_window[i]); // Load unaligned data (use _mm256_load_pd for aligned)
    //         sum_vec = _mm256_add_pd(sum_vec, data_vec); // Vector addition
    //     }

    //     // Sum the elements in the vector sum (horizontal sum)
    //     // This typically requires multiple steps depending on SIMD width
    //     // For AVX: sum_vec[0]+sum_vec[1]+sum_vec[2]+sum_vec[3]
    //     // Or use a helper like _mm256_hadd_pd (less common) or a custom reduction.
    //     DecimalLike temp_sum[vector_size];
    //     _mm256_storeu_pd(temp_sum, sum_vec);
    //     sum = temp_sum[0] + temp_sum[1] + temp_sum[2] + temp_sum[3];

    //     // Handle remaining elements without SIMD
    //     for (; i < data_window.size(); ++i) {
    //         sum += data_window[i];
    //     }
    //     #else
    //     // Fallback to standard summation if AVX is not available
    //     sum = std::accumulate(recent_prices.begin(), recent_prices.end(), 0.0);
    //     #endif
    // }

    // For simple sum on a potentially non-contiguous deque, std::accumulate is often the most
    // practical approach. Compilers are good at optimizing it, potentially using SIMD automatically
    // if the underlying data structure allows (vector) or with runtime checks.
    sum = std::accumulate(recent_prices.begin(), recent_prices.end(), 0.0);

    *output_value = sum / static_cast<DecimalLike>(config_.period);

    spdlog::debug("Computed {}: {}", config_.name, *output_value);

    return true;
}

} // namespace Indicators