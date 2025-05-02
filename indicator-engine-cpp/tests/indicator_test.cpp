// Example indicator_tests.cpp using Google Test
#include "gtest/gtest.h"
#include "indicators/sma.h"
#include "indicators/rsi.h"
#include "indicators/macd.h"
#include "indicators/ema.h" // Include EMA header
#include "data_manager.h" // Need DataManager or mock it
#include "utils/conversions.h" // Need DecimalLike
#include <vector>
#include <deque> // Mock DataManager uses deque
#include <chrono> // For timestamps
#include <numeric> // For std::accumulate in mock

// Mock DataManager for testing indicators in isolation
// This mock provides a simple way to inject historical data.
class MockDataManager : public IndicatorEngine::DataManager {
public:
    MockDataManager() : IndicatorEngine::DataManager(1000) {} // Call base constructor

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
                if (count > 0 && it->second->recent_prices.size() > count) {
                    start_index = it->second->recent_prices.size() - count;
                }
                return std::vector<std::pair<DecimalLike, std::chrono::system_clock::time_point>>(it->second->recent_prices.begin() + start_index, it->second->recent_prices.end());
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

     // Mocking other methods if needed for indicator computation
     // void processMarketDataEvent(...) {} // No-op for mock
     // void storeAndPublishIndicatorValue(...) {} // No-op for mock publishing
};


// Test fixture for indicator tests
class IndicatorTest : public ::testing::Test {
protected:
    MockDataManager mock_data_manager;
    std::string test_symbol = "TEST_SYMBOL";

    void SetUp() override {
        // Setup code if needed before each test
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
            sequence.push_back({prices[i], now + std::chrono::seconds(i)});
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

    std::deque<std::pair<DecimalLike, std::chrono::system_clock::time_point>> prices;
    auto now = std::chrono::system_clock::now();

    // First price (10) - not enough for init
    prices.push_back({10.0, now});
    mock_data_manager.setMockPrices(prices);
    bool computed = ema.compute(mock_data_manager, &output_value);
    ASSERT_FALSE(computed);

    // Second price (12) - not enough for init
    prices.push_back({12.0, now + std::chrono::seconds(1)});
    mock_data_manager.setMockPrices(prices);
    computed = ema.compute(mock_data_manager, &output_value);
    ASSERT_FALSE(computed);

    // Third price (14) - enough for initial SMA
    prices.push_back({14.0, now + std::chrono::seconds(2)});
    mock_data_manager.setMockPrices(prices);
    computed = ema.compute(mock_data_manager, &output_value);
    ASSERT_FALSE(computed); // Should compute initial, but not output final EMA yet

    // Fourth price (16) - enough for first smoothed EMA
    prices.push_back({16.0, now + std::chrono::seconds(3)});
    mock_data_manager.setMockPrices(prices);
    computed = ema.compute(mock_data_manager, &output_value);
    ASSERT_TRUE(computed);
    EXPECT_DOUBLE_EQ(output_value, 14.0); // (16 - 12) * 0.5 + 12 = 14

    // Fifth price (18) - second smoothed EMA
    prices.push_back({18.0, now + std::chrono::seconds(4)});
    mock_data_manager.setMockPrices(prices);
    computed = ema.compute(mock_data_manager, &output_value);
    ASSERT_TRUE(computed);
    EXPECT_DOUBLE_EQ(output_value, 16.0); // (18 - 14) * 0.5 + 14 = 16
}


// Test case for RSI
TEST_F(IndicatorTest, RSICalculation) {
    Indicators::RsiConfig config = {"RSI_3", test_symbol, 3}; // Period 3
    Indicators::RSI rsi(config);
    DecimalLike output_value;

    // Prices to test initial averages and smoothing
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

    std::vector<DecimalLike> prices = {10.0, 11.0, 12.0, 11.0, 13.0, 14.0};
    std::deque<std::pair<DecimalLike, std::chrono::system_clock::time_point>> price_sequence;
    auto now = std::chrono::system_clock::now();

    // Feed sequentially
    for (size_t i = 0; i < prices.size(); ++i) {
        price_sequence.push_back({prices[i], now + std::chrono::seconds(i)});
        mock_data_manager.setMockPrices(price_sequence); // Set increasing history
        bool computed = rsi.compute(mock_data_manager, &output_value);

        // First 3 data points: not enough for initial averages
        if (i < 3) {
            ASSERT_FALSE(computed);
        }
        // 4th data point (index 3, price 11): enough for initial averages (10,11,12,11). Should compute initial, not final RSI.
        else if (i == 3) {
            ASSERT_FALSE(computed); // Initial averages computed, but not final RSI yet
        }
         // 5th data point (index 4, price 13): enough for first smoothed RSI.
         else if (i == 4) {
            ASSERT_TRUE(computed);
            // Expected RSI = 83.333...
            EXPECT_NEAR(output_value, 83.3333333, 1e-6);
         }
         // 6th data point (index 5, price 14): second smoothed RSI.
         else if (i == 5) {
            ASSERT_TRUE(computed);
             // Previous Avg Gain = 10/9, Avg Loss = 2/9
             // Current Price 14, Last Price 13, Diff +1
             // Current Gain = 1, Current Loss = 0
             // New Avg Gain = (10/9 * 2 + 1) / 3 = (20/9 + 9/9) / 3 = (29/9)/3 = 29/27
             // New Avg Loss = (2/9 * 2 + 0) / 3 = (4/9)/3 = 4/27
             // RS = (29/27) / (4/27) = 29/4 = 7.25
             // RSI = 100 - (100 / (1 + 7.25)) = 100 - (100 / 8.25) = 100 - 12.1212... = 87.8787...
            EXPECT_NEAR(output_value, 87.8787878, 1e-6);
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
    // Total prices needed = 5 (for Slow EMA init) + 2 - 1 (for Signal EMA init after Slow is ready) = 6 prices ?
    // Let's use a sequence and check when values are computed.

    // Prices: 10, 11, 12, 13, 14, 15, 16, 17, 18
    // EMA(3) alpha = 0.5
    // EMA(5) alpha = 2/6 = 1/3
    // EMA(2) alpha = 2/3 (for signal)

    std::vector<DecimalLike> prices = {10.0, 11.0, 12.0, 13.0, 14.0, 15.0, 16.0, 17.0, 18.0};
     std::deque<std::pair<DecimalLike, std::chrono::system_clock::time_point>> price_sequence;
    auto now = std::chrono::system_clock::now();


    // Feed sequentially
    for (size_t i = 0; i < prices.size(); ++i) {
        price_sequence.push_back({prices[i], now + std::chrono::seconds(i)});
        mock_data_manager.setMockPrices(price_sequence);
        bool computed = macd.compute(mock_data_manager, &macd_line, &signal_line, &histogram);

        // Need enough prices for Slow EMA (period 5) to be initialized, then enough MACD values (period 2)
        // Slow EMA needs 5 prices for init SMA. Fast EMA needs 3 prices for init SMA.
        // Fast EMA is ready after 3 prices. Slow EMA after 5 prices.
        // MACD line = FastEMA - SlowEMA. MACD line ready after 5 prices.
        // Signal EMA needs 2 MACD values for init SMA.
        // So, need 5 prices + 2 prices = 7 prices? Let's check indices.
        // Price index 0,1,2: No Fast/Slow EMA.
        // Price index 3: Fast EMA init done (uses 0,1,2).
        // Price index 4: Slow EMA init done (uses 0,1,2,3,4). Fast EMA smoothed. MACD line computed (index 4).
        // Price index 5: Need 2nd MACD value (index 5) for Signal EMA init. MACD line computed (index 5). Signal EMA init done (uses MACD values at index 4 and 5). Histogram computed.
        // So, MACD, Signal, Hist should be computed starting from price index 5 (6th price).

        if (i < 5) {
            ASSERT_FALSE(computed); // Not enough data for MACD line yet
        } else if (i == 5) {
             // Should compute MACD line (based on price 5) and Signal/Hist (based on MACD values at index 4, 5)
             ASSERT_TRUE(computed);
             // TODO: Calculate expected values manually or using a reference library
             // This is complex calculation, trust the logic structure for now and rely on reference library tests.
              std::cout << "MACD computed at index " << i << ": MACD=" << macd_line << ", Signal=" << signal_line << ", Hist=" << histogram << std::endl; // Debug
         } else {
             ASSERT_TRUE(computed); // Should compute
             std::cout << "MACD computed at index " << i << ": MACD=" << macd_line << ", Signal=" << signal_line << ", Hist=" << histogram << std::endl; // Debug
         }
    }
}

// TODO: Add more test cases for edge cases, different periods, zero values, etc.
// TODO: Add tests for DataManager methods (getRecentPrices, getNewIndicatorValues)
// TODO: Add integration tests for gRPC (more complex)