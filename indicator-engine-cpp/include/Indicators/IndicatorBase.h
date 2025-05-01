// indicator-engine-cpp/include/indicators/IndicatorBase.h
#pragma once

#include <vector>
#include <string>
#include <optional> // Requires C++17
#include <Eigen/Core> // For potential Eigen usage in indicators

// Using double for indicator calculations for precision
using IndicatorValue = double;
using PriceVector = Eigen::VectorXd; // Example using Eigen for price series input

class IndicatorBase {
public:
    virtual ~IndicatorBase() = default;

    /**
     * @brief Updates the indicator with a new price value.
     *
     * @param price The latest price to add.
     */
    virtual void update(IndicatorValue price) = 0;

    /**
     * @brief Calculates the current value of the indicator.
     *
     * @return std::optional<IndicatorValue> The calculated indicator value,
     * or std::nullopt if the indicator doesn't have enough data yet.
     */
    virtual std::optional<IndicatorValue> calculate() const = 0;

    /**
     * @brief Gets the name of the indicator (e.g., "SMA", "RSI").
     * @return const std::string& The indicator name.
     */
    virtual const std::string& get_name() const = 0;

    /**
     * @brief Gets the period used by the indicator (e.g., 14 for RSI(14)).
     * @return size_t The period length.
     */
    virtual size_t get_period() const = 0;

     /**
      * @brief Checks if the indicator has enough data to produce a value.
      * @return true if the indicator is ready, false otherwise.
      */
     virtual bool is_ready() const = 0;

    // Potential future extensions:
    // - Methods to calculate based on a full price vector (for batch processing)
    // virtual std::vector<std::optional<IndicatorValue>> calculate_batch(const PriceVector& prices) = 0;
    // - Methods to get multiple output values (e.g., MACD line and signal line)
    // virtual std::optional<std::vector<IndicatorValue>> calculate_multi() const = 0;
};
