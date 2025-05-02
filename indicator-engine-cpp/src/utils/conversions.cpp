#include "utils/conversions.h"
#include <stdexcept> // For std::invalid_argument, std::out_of_range
#include <cmath>     // For std::round, std::pow, std::isnan, std::isinf
#include <limits>    // For numeric_limits
#include <iomanip>   // For std::fixed, std::setprecision
#include <sstream>   // For std::stringstream
#include <iostream>  // For std::cerr

namespace Utils {
namespace Conversions {

std::optional<DecimalLike> StringToDecimalLike(const std::string& s) {
    if (s.empty()) return std::nullopt;
    try {
        size_t processed_chars;
        DecimalLike value = std::stod(s, &processed_chars);
        if (processed_chars != s.length()) {
             // Part of the string was not converted (e.g., "123.45abc")
             std::cerr << "Warning: StringToDecimalLike did not convert entire string: '" << s << "'" << std::endl;
             return std::nullopt; // Consider this an error
        }
        return value;
    } catch (const std::invalid_argument& e) {
        std::cerr << "Error converting string to double (invalid argument): '" << s << "' - " << e.what() << std::endl;
        return std::nullopt; // Conversion failed
    } catch (const std::out_of_range& e) {
         std::cerr << "Error converting string to double (out of range): '" << s << "' - " << e.what() << std::endl;
         return std::nullopt; // Conversion failed
    }
}

std::string DecimalLikeToString(DecimalLike d) {
    if (std::isnan(d)) return "NaN";
    if (std::isinf(d)) return d > 0 ? "Infinity" : "-Infinity";

    std::stringstream ss;
    // Use fixed precision for financial data. Choose an appropriate precision.
    // This is a simplified approach; a proper decimal type is better.
    ss << std::fixed << std::setprecision(10) << d; // Example: 10 decimal places for slightly better representation
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
    // Handle potential invalid timestamps (e.g., negative seconds)
    if (ts.seconds() < 0 && ts.nanos() > 0) {
         // Not strictly valid, might indicate an issue
         std::cerr << "Warning: Received invalid Protobuf Timestamp (negative seconds, positive nanos)." << std::endl;
         // Return a default or error indicator?
         return std::chrono::system_clock::time_point(); // Return epoch or similar
    }
     if (ts.nanos() < 0 || ts.nanos() >= 1'000'000'000) {
        std::cerr << "Warning: Received invalid Protobuf Timestamp (nanos out of range): " << ts.nanos() << std::endl;
        // Return a default or adjust nanos? Adjusting is risky.
         google::protobuf::Timestamp valid_ts = ts;
         valid_ts.set_nanos(std::max(0, std::min(999'999'999, valid_ts.nanos())));
         return std::chrono::system_clock::time_point(
            std::chrono::seconds(valid_ts.seconds()) +
            std::chrono::nanoseconds(valid_ts.nanos())
        );
     }


    return std::chrono::system_clock::time_point(
        std::chrono::seconds(ts.seconds()) +
        std::chrono::nanoseconds(ts.nanos())
    );
}

google::protobuf::Timestamp
    TimePointToProtoTimestamp(const std::chrono::system_clock::time_point& tp) {
    auto duration = tp.time_since_epoch();
    auto seconds = std::chrono::duration_cast<std::chrono::seconds>(duration);
    auto nanos = std::chrono::duration_cast<std::chrono::nanoseconds>(duration - seconds);

    google::protobuf::Timestamp ts;
    ts.set_seconds(seconds.count());
    ts.set_nanos(nanos.count());
    return ts;
}

} // namespace Conversions
} // namespace Utils