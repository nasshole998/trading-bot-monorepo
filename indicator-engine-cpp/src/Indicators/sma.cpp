#include "indicators/sma.h"
#include "utils/conversions.h" // For DecimalLike
#include <numeric> // For std::accumulate
#include <iostream> // For debug output
#include <vector> // Need vector for SIMD example
// Include SIMD headers if needed, e.g., <immintrin.h> for AVX


namespace Indicators {

SMA::SMA(const SmaConfig& config) : config_(config) {
    if (config_.period <= 0) {
        std::cerr << "Error: SMA period must be positive. Config: " << config_.name << std::endl;
        // Handle invalid config, e.g., throw exception or mark as invalid
    }
}

bool SMA::compute(const IndicatorEngine::DataManager& data_manager, DecimalLike* output_value) {
    if (!output_value) {
        std::cerr << "Error: output_value pointer is null for SMA." << std::endl;
        return false;
    }
     if (config_.period <= 0) return false; // Don't compute for invalid config

    // Get the recent price data for the symbol
    auto recent_prices = data_manager.getRecentPrices(config_.symbol, config_.period);

    if (recent_prices.size() < config_.period) {
        // Not enough data yet to compute SMA
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

    // std::cout << "Computed SMA(" << config_.symbol << ", " << config_.period << "): " << *output_value << std::endl; // Debug

    return true;
}

} // namespace Indicators