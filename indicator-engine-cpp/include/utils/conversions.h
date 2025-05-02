#ifndef INDICATOR_ENGINE_UTILS_CONVERSIONS_H
#define INDICATOR_ENGINE_UTILS_CONVERSIONS_H

#include <string>
#include <google/protobuf/timestamp.pb.h>
#include <chrono>
#include <optional> // Still used internally by Result
#include "utils/indicator_engine_error.h" // Include custom error type

// Use double for indicator calculations, acknowledge precision limitations
// WARNING: Using 'double' for financial calculations can lead to precision errors.
// For production systems requiring high accuracy, consider using a fixed-point decimal library.
using DecimalLike = double;

namespace Utils {
namespace Conversions {

// Convert string (from protobuf) to numerical type
// Returns Result<DecimalLike> indicating success or a ConversionError
IndicatorEngine::Result<DecimalLike> StringToDecimalLike(const std::string& s);

// Convert numerical type to string (for protobuf)
std::string DecimalLikeToString(DecimalLike d);

// Convert Protobuf Timestamp to std::chrono time point
// Returns the time point, logs a warning for invalid timestamps but attempts conversion.
std::chrono::system_clock::time_point
    ProtoTimestampToTimePoint(const google::protobuf::Timestamp& ts);

// Convert std::chrono time point to Protobuf Timestamp
google::protobuf::Timestamp
    TimePointToProtoTimestamp(const std::chrono::system_clock::time_point& tp);

} // namespace Conversions
} // namespace Utils

#endif // INDICATOR_ENGINE_UTILS_CONVERSIONS_H