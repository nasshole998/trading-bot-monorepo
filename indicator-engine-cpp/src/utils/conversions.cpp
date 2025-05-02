#include "utils/conversions.h"
#include "utils/indicator_engine_error.h"
#include <stdexcept> // For std::invalid_argument, std::out_of_range
#include <cmath>     // For std::round, std::pow, std::isnan, std::isinf
#include <limits>    // For numeric_limits
#include <iomanip>   // For std::fixed, std::setprecision
#include <sstream>   // For std::stringstream
#include <algorithm> // For std::max, std::min
#include <spdlog/spdlog.h> // For logging


namespace Utils {
namespace Conversions {

IndicatorEngine::Result<DecimalLike> StringToDecimalLike(const std::string& s) {
    if (s.empty()) {
         spdlog::warn("StringToDecimalLike received empty string.");
        return IndicatorEngine::Error::IndicatorEngineErrc::InvalidInputData; // Or ConversionError
    }
    try {
        size_t processed_chars;
        DecimalLike value = std::stod(s, &processed_chars);
        if (processed_chars != s.length()) {
             spdlog::warn("StringToDecimalLike did not convert entire string: '{}'", s);
            // Depending on strictness, this could be an error or just a warning.
            // Let's treat it as an error indicating invalid format.
             return IndicatorEngine::Error::IndicatorEngineErrc::InvalidInputData;
        }
        // Check for infinity or NaN after conversion, though stod might throw for some inputs.
        if (std::isinf(value) || std::isnan(value)) {
             spdlog::warn("StringToDecimalLike resulted in infinity or NaN for string: '{}'", s);
             return IndicatorEngine::Error::IndicatorEngineErrc::ConversionError;
        }
        return value; // Success
    } catch (const std::invalid_argument& e) {
        spdlog::error("Error converting string to double (invalid argument): '{}' - {}", s, e.what());
        return IndicatorEngine::Error::IndicatorEngineErrc::InvalidInputData;
    } catch (const std::out_of_range& e) {
         spdlog::error("Error converting string to double (out of range): '{}' - {}", s, e.what());
         return IndicatorEngine::Error::IndicatorEngineErrc::ConversionError;
    }
}

std::string DecimalLikeToString(DecimalLike d) {
    if (std::isnan(d)) return "NaN";
    if (std::isinf(d)) return d > 0 ? "Infinity" : "-Infinity";

    std::stringstream ss;
    // Use fixed precision for financial data. Choose an appropriate precision.
    // This is a simplified approach; a proper decimal type is better.
    // Printing with high precision first, then trimming.
    ss << std::fixed << std::setprecision(17) << d; // std::numeric_limits<double>::max_digits10 for full precision
    std::string s = ss.str();

    // Trim trailing zeros and decimal point if possible
    // Find the decimal point
    size_t decimal_pos = s.find('.');
    if (decimal_pos == std::string::npos) {
        // No decimal point, no trimming needed
        return s;
    }

    // Find the last non-zero character after the decimal point
    size_t last_digit_pos = s.find_last_not_of('0');

    if (last_digit_pos != std::string::npos && last_digit_pos > decimal_pos) {
        // Trailing zeros exist after the decimal
        s.erase(last_digit_pos + 1);
    }

    // Remove decimal point if no digits are left after it
    if (s.back() == '.') {
        s.pop_back();
    }

    return s;
}

std::chrono::system_clock::time_point
    ProtoTimestampToTimePoint(const google::protobuf::Timestamp& ts) {
    // Handle potential invalid timestamps (e.g., negative seconds, nanos out of range)
    if (ts.seconds() < 0 && ts.nanos() > 0) {
         spdlog::warn("Received Protobuf Timestamp with negative seconds ({}) but positive nanos ({}) for conversion.", ts.seconds(), ts.nanos());
         // This indicates a potential issue with the timestamp source.
         // Return epoch or throw error based on desired behavior.
         return std::chrono::system_clock::time_point(); // Return epoch
    }
     if (ts.nanos() < 0 || ts.nanos() >= 1'000'000'000) {
        spdlog::warn("Received Protobuf Timestamp with nanos out of range ({}) for conversion.", ts.nanos());
        // Attempt to clamp nanos, but this might lose information.
         google::protobuf::Timestamp valid_ts = ts;
         valid_ts.set_nanos(std::max(0, std::min(999'999'999, valid_ts.nanos())));
         return std::chrono::system_clock::time_point(
            std::chrono::seconds(valid_ts.seconds()) +
            std::chrono::nanoseconds(valid_ts.nanos())
        );
     }

    // Convert seconds and nanos to duration, then to time_point
    auto duration = std::chrono::seconds(ts.seconds()) + std::chrono::nanoseconds(ts.nanos());
    return std::chrono::system_clock::time_point(duration);
}

google::protobuf::Timestamp
    TimePointToProtoTimestamp(const std::chrono::system_clock::time_point& tp) {
    auto duration = tp.time_since_epoch();
    // Ensure duration is not negative if time_point is before epoch
    if (duration < std::chrono::nanoseconds::zero()) {
         spdlog::warn("Converting time_point before epoch to Protobuf Timestamp.");
         // Protobuf Timestamp spec is ambiguous for pre-epoch, often treated as error source.
         // Clamp to epoch or handle as error depending on convention.
         // For simplicity, convert to epoch timestamp (0, 0).
         google::protobuf::Timestamp ts;
         ts.set_seconds(0);
         ts.set_nanos(0);
         return ts;
    }

    auto seconds = std::chrono::duration_cast<std::chrono::seconds>(duration);
    auto nanos = std::chrono::duration_cast<std::chrono::nanoseconds>(duration - seconds);

    google::protobuf::Timestamp ts;
    ts.set_seconds(seconds.count());
    ts.set_nanos(static_cast<int32_t>(nanos.count())); // nanos should fit in i32
    return ts;
}

} // namespace Conversions
} // namespace Utils