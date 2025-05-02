#ifndef INDICATOR_ENGINE_UTILS_CONVERSIONS_H
#define INDICATOR_ENGINE_UTILS_CONVERSIONS_H

#include <string>
#include <google/protobuf/timestamp.pb.h>
#include <chrono>
#include <optional> // For std::optional

// Use double for indicator calculations, acknowledge precision limitations
using DecimalLike = double;

namespace Utils {
namespace Conversions {

// Convert string (from protobuf) to numerical type
// Returns std::nullopt if conversion fails
std::optional<DecimalLike> StringToDecimalLike(const std::string& s);

// Convert numerical type to string (for protobuf)
std::string DecimalLikeToString(DecimalLike d);

// Convert Protobuf Timestamp to std::chrono time point
std::chrono::system_clock::time_point
    ProtoTimestampToTimePoint(const google::protobuf::Timestamp& ts);

// Convert std::chrono time point to Protobuf Timestamp
google::protobuf::Timestamp
    TimePointToProtoTimestamp(const std::chrono::system_clock::time_point& tp);

} // namespace Conversions
} // namespace Utils

#endif // INDICATOR_ENGINE_UTILS_CONVERSIONS_H