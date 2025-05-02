// Example indicator_tests.cpp using Google Test
#include "gtest/gtest.h"
#include "indicators/sma.h"
#include "indicators/rsi.h"
#include "indicators/macd.h"
#include "indicators/ema.h" // Include EMA header
#include "data_manager.h" // Need DataManager or mock it
#include "utils/conversions.h" // Need DecimalLike
#include "utils/indicator_engine_error.h" // Include error type
#include <vector>
#include <deque> // Mock DataManager uses deque
#include <chrono> // For timestamps
#include <numeric> // For std::accumulate in mock
#include <memory> // For unique_ptr
#include <spdlog/spdlog.h> // For logging
#include <spdlog/sinks/null_sink.h> // To discard logs during tests


// Mock DataManager for testing indicators in isolation
// This mock provides a simple way to inject historical data.
class MockDataManager : public IndicatorEngine::DataManager {
public:
    MockDataManager() : IndicatorEngine::DataManager(1000) {
         // Suppress logging from base class and methods during tests
         if (!test_logger_) {
             test_logger_ = spdlog::create<spdlog::sinks::null_sink_mt>("test_logger");
             spdlog::set_default_logger(test_logger_);
         }
     }

    // Allow setting mock prices
    void setMockPrices(const std::deque<std::pair<DecimalLike, std::chrono::system_clock::time_point>>& prices) {
        std::lock_guard<std::mutex> lock(symbol_data_mutex_); // Lock the mock's internal map
        auto& symbol_data = symbol_data_["TEST_SYMBOL"]; // Use a fixed symbol for mock
        if (!symbol_data) {
             symbol_data = std::make_unique<IndicatorEngine::SymbolData>();
             symbol_data->symbol = "TEST_SYMBOL";
        }
        std::lock_guard<std::mutex> data_lock(symbol_data->mutex); // Lock the symbol data
        symbol_data->recent_prices = prices;
    }

    // Override getRecentPricesWithTimestamps to return predefined data
    std::vector<std::pair<DecimalLike, std::chrono::system_clock::time_point>> getRecentPricesWithTimestamps(const std::string& symbol, int count) const override {
        if (symbol == "TEST_SYMBOL") {
            std::lock_guard<std::mutex> lock(symbol_data_mutex_);
            auto it = symbol_data_.find(symbol);
            if (it != symbol_data_.end()) {
                 std::lock_guard<std::mutex> data_lock(it->second->mutex);
                 size_t start_index = 0;
                if (count > 0 && it->second->recent_prices.size() > static_cast<size_t>(count)) {
                    start_index = it->second->recent_prices.size() - static_cast<size_t>(count);
                }
                 std::vector<std::pair<DecimalLike, std::chrono::system_clock::time_point>> result;
                 result.reserve(it->second->recent_prices.size() - start_index);
                for (size_t i = start_index; i < it->second->recent_prices.size(); ++i) {
                     result.push_back(it->second->recent_prices[i]);
                }
                return result;
            }
        }
        return {}; // Return empty for other symbols or if mock data not set
    }

    // Override getRecentPrices
     std::vector<DecimalLike> getRecentPrices(const std::string& symbol, int count) const override {
         auto prices_with_ts = getRecentPricesWithTimestamps(symbol, count);
         std::vector<DecimalLike> prices;
         prices.reserve(prices_with_ts.size());
         for(const auto& pair : prices_with_ts) {
             prices.push_back(pair.first);
         }
         return prices;
     }


    // Override getLatestPriceWithTimestamp
     std::optional<std::pair<DecimalLike, std::chrono::system_clock::time_point>> getLatestPriceWithTimestamp(const std::string& symbol) const override {
         if (symbol == "TEST_SYMBOL") {
             std::lock_guard<std::mutex> lock(symbol_data_mutex_);
             auto it = symbol_data_.find(symbol);
              if (it != symbol_data_.end()) {
                  std::lock_guard<std::mutex> data_lock(it->second->mutex);
                  if (!it->second->recent_prices.empty()) {
                       return it->second->recent_prices.back();
                  }
              }
         }
         return std::nullopt; // Latest price not set
     }

      // Override getLatestPrice
     std::optional<DecimalLike> getLatestPrice(const std::string& symbol) const override {
         auto latest_pair = getLatestPriceWithTimestamp(symbol);
         if (latest_pair) {
             return latest_pair->first;
         }
         return std::nullopt;
     }

     // Override methods called during processing/publishing if testing full flow
     // VoidResult processMarketDataEvent(...) override { return IndicatorEngine::Error::IndicatorEngineErrc::Success; }
     // void storeAndPublishIndicatorValue(...) override {} // No-op for mock publishing

private:
     // Use a null logger for tests
     static std::shared_ptr<spdlog::logger> test_logger_;
};

// Initialize the static logger pointer
std::shared_ptr<spdlog::logger> MockDataManager::test_logger_ = nullptr;


// Test fixture for indicator tests
class IndicatorTest : public ::testing::Test {
protected:
    MockDataManager mock_data_manager;
    std::string test_symbol = "TEST_SYMBOL";

    void SetUp() override {
        // Ensure default logger is set for tests if it wasn't already
         if (!spdlog::default_logger()) {
             auto null_sink = std::make_shared<spdlog::sinks::null_sink_mt>();
             spdlog::set_default_logger(std::make_shared<spdlog::logger>("test_logger_setup", null_sink));
         }
        // Setup code if needed before each test
        mock_data_manager.setMockPrices({}); // Clear mock data before each test
    }

    void TearDown() override {
        // Teardown code if needed after each test
    }

    // Helper to create timestamp sequence
    std::deque<std::pair<DecimalLike, std::chrono::system_clock::time_point>> createPriceSequence(const std::vector<DecimalLike>& prices) {
        std::deque<std::pair<DecimalLike, std::chrono::system_clock::time_point>> sequence;
        auto now = std::chrono::system_clock::now();
        for (size_t i = 0; i < prices.size(); ++i) {
            // Add a small delay for distinct timestamps
            sequence.push_back({prices[i], now + std::chrono::milliseconds(i * 100)});
        }
        return sequence;
    }
};

// Test case for SMA
TEST_F(IndicatorTest, SMACalculation) {
    Indicators::SmaConfig config = {"SMA_5", test_symbol, 5};
    Indicators::SMA sma(config);
    DecimalLike output_value;

    // Set mock prices (more than the period)
    std::vector<DecimalLike> prices = {1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0, 10.0};
    mock_data_manager.setMockPrices(createPriceSequence(prices));

    // Test with enough data
    // DataManager should provide the last 5: 6.0, 7.0, 8.0, 9.0, 10.0
    // SMA(5) = (6+7+8+9+10) / 5 = 40.0 / 5.0 = 8.0
    bool computed = sma.compute(mock_data_manager, &output_value);
    ASSERT_TRUE(computed);
    EXPECT_DOUBLE_EQ(output_value, 8.0);

    // Test with not enough data (less than period)
    std::vector<DecimalLike> short_prices = {1.0, 2.0, 3.0}; // Period is 5
    mock_data_manager.setMockPrices(createPriceSequence(short_prices));
    computed = sma.compute(mock_data_manager, &output_value);
    ASSERT_FALSE(computed); // Should not compute

    // Test with exactly enough data (period)
    std::vector<DecimalLike> exact_prices = {1.0, 2.0, 3.0, 4.0, 5.0}; // Period is 5
    mock_data_manager.setMockPrices(createPriceSequence(exact_prices));
    // SMA(5) = (1+2+3+4+5) / 5 = 15 / 5 = 3.0
    computed = sma.compute(mock_data_manager, &output_value);
    ASSERT_TRUE(computed);
    EXPECT_DOUBLE_EQ(output_value, 3.0);
}

// Test case for EMA
TEST_F(IndicatorTest, EMACalculation) {
    Indicators::EmaConfig config = {"EMA_3", test_symbol, 3};
    Indicators::EMA ema(config);
    DecimalLike output_value;

    // Simulate sequential data points to test stateful computation
    // EMA(3) alpha = 2 / (3+1) = 0.5
    // Prices: 10, 12, 14, 16, 18
    // Need 3 prices for initial SMA: (10+12+14)/3 = 36/3 = 12.0
    // Data point 4 (price 16): EMA = (16 - 12.0) * 0.5 + 12.0 = 4.0 * 0.5 + 12.0 = 2.0 + 12.0 = 14.0
    // Data point 5 (price 18): EMA = (18 - 14.0) * 0.5 + 14.0 = 4.0 * 0.5 + 14.0 = 2.0 + 14.0 = 16.0

    std::vector<DecimalLike> prices_vec = {10.0, 12.0, 14.0, 16.0, 18.0};
    std::deque<std::pair<DecimalLike, std::chrono::system_clock::time_point>> price_sequence;
    auto now = std::chrono::system_clock::now();

    // Feed sequentially
    for (size_t i = 0; i < prices_vec.size(); ++i) {
        price_sequence.push_back({prices_vec[i], now + std::chrono::seconds(i)});
        mock_data_manager.setMockPrices(price_sequence); // Set increasing history
        bool computed = ema.compute(mock_data_manager, &output_value);

        // First 2 data points (indices 0, 1): not enough for init
        if (i < 2) {
            ASSERT_FALSE(computed);
        }
        // 3rd data point (index 2, price 14): enough for initial SMA
        else if (i == 2) {
            ASSERT_TRUE(computed); // Initial value is returned by compute
             // The initial EMA(3) is SMA(3) of {10, 12, 14} which is 12.0
             EXPECT_DOUBLE_EQ(output_value, 12.0);
        }
        // 4th data point (index 3, price 16): first smoothed EMA
         else if (i == 3) {
            ASSERT_TRUE(computed);
            EXPECT_DOUBLE_EQ(output_value, 14.0); // (16 - 12.0) * 0.5 + 12.0 = 14.0
        }
        // 5th data point (index 4, price 18): second smoothed EMA
        else if (i == 4) {
            ASSERT_TRUE(computed);
            EXPECT_DOUBLE_EQ(output_value, 16.0); // (18 - 14.0) * 0.5 + 14.0 = 16.0
        }
    }
}


// Test case for RSI
TEST_F(IndicatorTest, RSICalculation) {
    Indicators::RsiConfig config = {"RSI_3", test_symbol, 3}; // Period 3
    Indicators::RSI rsi(config);
    DecimalLike output_value;

    // Prices: 10, 11, 12, 11, 13, 14
    // Period 3 -> need 4 prices for initial avg (10, 11, 12, 11)
    // Diffs: +1, +1, -1
    // Initial Gains: 1, 1, 0 -> Avg Gain = (1+1+0)/3 = 2/3
    // Initial Losses: 0, 0, 1 -> Avg Loss = (0+0+1)/3 = 1/3
    // Next price: 13 (gain of 2)
    // New Avg Gain = (2/3 * (3-1) + 2) / 3 = (4/3 + 6/3)/3 = (10/3)/3 = 10/9
    // New Avg Loss = (1/3 * (3-1) + 0) / 3 = (2/3)/3 = 2/9
    // RS = (10/9) / (2/9) = 10/2 = 5
    // RSI = 100 - (100 / (1 + 5)) = 100 - 100/6 = 100 - 16.666... = 83.333...

    std::vector<DecimalLike> prices_vec = {10.0, 11.0, 12.0, 11.0, 13.0, 14.0};
    std::deque<std::pair<DecimalLike, std::chrono::system_clock::time_point>> price_sequence;
    auto now = std::chrono::system_clock::now();


    // Feed sequentially
    for (size_t i = 0; i < prices_vec.size(); ++i) {
        price_sequence.push_back({prices_vec[i], now + std::chrono::seconds(i)});
        mock_data_manager.setMockPrices(price_sequence); // Set increasing history
        bool computed = rsi.compute(mock_data_manager, &output_value);

        // First 3 data points (indices 0, 1, 2): not enough for initial averages
        if (i < 3) {
            ASSERT_FALSE(computed);
        }
        // 4th data point (index 3, price 11): enough for initial averages (uses prices 10,11,12,11). Should compute initial, not final RSI.
        else if (i == 3) {
            ASSERT_FALSE(computed); // Initial averages computed, but not final RSI yet
        }
         // 5th data point (index 4, price 13): enough for first smoothed RSI.
         else if (i == 4) {
            ASSERT_TRUE(computed);
            // Expected RSI = 83.333...
            EXPECT_NEAR(output_value, 83.333333333, 1e-9); // Use higher precision for comparison
         }
         // 6th data point (index 5, price 14): second smoothed RSI.
         else if (i == 5) {
            ASSERT_TRUE(computed);
             // Expected RSI = 87.8787...
            EXPECT_NEAR(output_value, 87.878787878, 1e-9); // Use higher precision for comparison
         }
    }
}

// Test case for MACD
TEST_F(IndicatorTest, MACDCalculation) {
    Indicators::MacdConfig config = {"MACD_3_5_2", test_symbol, 3, 5, 2}; // Small periods
    Indicators::MACD macd(config);
    DecimalLike macd_line, signal_line, histogram;

    // Simulate sequential data points
    // Need enough data for Slow EMA (5 prices) + Signal EMA init (2 MACD values after Slow EMA is ready)
    // Slow EMA needs 5 prices for init SMA. Fast EMA needs 3 prices for init SMA.
    // Fast EMA ready after 3 prices. Slow EMA after 5 prices.
    // MACD line = FastEMA - SlowEMA. MACD line ready after 5 prices.
    // Signal EMA needs 2 MACD values for init SMA.
    // So, MACD, Signal, Hist should be computed starting from price index 5 (6th price).

    // Using a sequence of prices and calculating expected values using a reference MACD calculator or manual calc
    // Prices: 10, 11, 12, 13, 14, 15, 16, 17, 18
    // EMA(3) alpha = 0.5
    // EMA(5) alpha = 2/6 = 1/3
    // EMA(2) alpha = 2/3 (for signal)

    std::vector<DecimalLike> prices_vec = {10.0, 11.0, 12.0, 13.0, 14.0, 15.0, 16.0, 17.0, 18.0};
     std::deque<std::pair<DecimalLike, std::chrono::system_clock::time_point>> price_sequence;
    auto now = std::chrono::system_clock::now();

    // Feed sequentially
    for (size_t i = 0; i < prices_vec.size(); ++i) {
        price_sequence.push_back({prices_vec[i], now + std::chrono::seconds(i)});
        mock_data_manager.setMockPrices(price_sequence);
        bool computed = macd.compute(mock_data_manager, &macd_line, &signal_line, &histogram);

        if (i < 5) {
            // Not enough data for MACD line yet (requires Slow EMA, period 5)
            ASSERT_FALSE(computed);
        } else if (i == 5) {
             // MACD line ready (index 5 price), but Signal EMA might not be ready yet (needs 2 MACD values)
             // MACD line value at index 5 is based on EMA(3) and EMA(5) up to index 5.
             // Signal EMA requires MACD line values at indices 4 and 5 for its initial SMA.
             // So, Signal/Hist are computed at index 5.
             ASSERT_TRUE(computed);
             // TODO: Calculate expected values manually or using a reference library for prices 10..15, MACD(3,5,2)
             // Example trace (simplified calculation assuming standard EMA):
             // i=0..2: No EMA(3), No EMA(5)
             // i=2 (P=12): EMA(3) init = (10+11+12)/3 = 11.0
             // i=3 (P=13): EMA(3) = (13 - 11.0)*0.5 + 11.0 = 12.0
             // i=4 (P=14): EMA(3) = (14 - 12.0)*0.5 + 12.0 = 13.0, EMA(5) init = (10+11+12+13+14)/5 = 12.0
             // MACD line (i=4) = 13.0 - 12.0 = 1.0. History: [1.0]
             // i=5 (P=15): EMA(3) = (15 - 13.0)*0.5 + 13.0 = 14.0, EMA(5) = (15 - 12.0)*(1/3) + 12.0 = 3*1/3 + 12 = 13.0
             // MACD line (i=5) = 14.0 - 13.0 = 1.0. History: [1.0, 1.0]
             // Signal EMA(2) init = SMA(2) of MACD history {1.0, 1.0} = (1.0+1.0)/2 = 1.0. Signal=1.0
             // Histogram (i=5) = MACD - Signal = 1.0 - 1.0 = 0.0
             EXPECT_NEAR(macd_line, 1.0, 1e-9);
             EXPECT_NEAR(signal_line, 1.0, 1e-9);
             EXPECT_NEAR(histogram, 0.0, 1e-9);

         } else if (i == 6) {
             // MACD line ready (index 6 price), Signal/Hist computed (based on MACD values at index 5, 6)
             ASSERT_TRUE(computed);
              // i=6 (P=16): EMA(3) = (16 - 14.0)*0.5 + 14.0 = 15.0, EMA(5) = (16 - 13.0)*(1/3) + 13.0 = 3*1/3 + 13 = 14.0
              // MACD line (i=6) = 15.0 - 14.0 = 1.0. History: [1.0, 1.0] (Pop 1.0 from front, Push 1.0 to back)
              // Signal EMA(2) = (MACD[i=6] - Signal[i=5]) * alpha_signal + Signal[i=5]
              // Signal EMA(2) alpha = 2/3
              // Signal EMA = (1.0 - 1.0)*(2/3) + 1.0 = 1.0
              // Histogram = MACD - Signal = 1.0 - 1.0 = 0.0
              EXPECT_NEAR(macd_line, 1.0, 1e-9);
              EXPECT_NEAR(signal_line, 1.0, 1e-9);
              EXPECT_NEAR(histogram, 0.0, 1e-9);
         }
         // Note: MACD values seem constant here because price diffs are constant (1.0)
         // A better test would use a more varied price sequence.
    }
}

// TODO: Add more test cases for edge cases, different periods, zero values, NaN handling, etc.
// TODO: Add tests for DataManager methods (getRecentPrices, getNewIndicatorValues)
// TODO: Add integration tests for gRPC (more complex)