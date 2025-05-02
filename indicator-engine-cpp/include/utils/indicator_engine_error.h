#ifndef INDICATOR_ENGINE_UTILS_INDICATOR_ENGINE_ERROR_H
#define INDICATOR_ENGINE_UTILS_INDICATOR_ENGINE_ERROR_H

#include <string>
#include <system_error> // For std::error_code, std::error_category, std::is_error_code_enum

namespace IndicatorEngine {
namespace Error {

enum class IndicatorEngineErrc {
    Success = 0,
    InvalidConfig,
    InvalidInputData,
    ComputationError,
    DataNotAvailable,
    ConversionError,
    RpcError, // gRPC specific error
    IoError, // File, network, etc.
    ShutdownRequested,
    UnknownError,
};

// Define a custom error category
std::error_category& indicator_engine_category();

// Make IndicatorEngineErrc compatible with std::error_code
inline std::error_code make_error_code(IndicatorEngineErrc errc) {
    return std::error_code(static_cast<int>(errc), indicator_engine_category());
}

// Allow implicit conversion to std::error_code
} // namespace Error
} // namespace IndicatorEngine

namespace std {
// Specialize std::is_error_code_enum to enable enum to std::error_code conversion
template <> struct is_error_code_enum<IndicatorEngine::Error::IndicatorEngineErrc> : std::true_type {};
} // namespace std


namespace IndicatorEngine {
namespace Error {

// Simple Result type (can be replaced by Boost.Outcome or C++23 std::expected)
template <typename T>
struct Result {
    Result(T val) : value_(std::move(val)), has_value_(true) {}
    Result(std::error_code ec) : error_(ec), has_value_(false) {}

    bool has_value() const { return has_value_; }
    bool has_error() const { return !has_value_; }

    T& value() {
        if (!has_value_) throw std::system_error(error_, "Result does not hold a value");
        return value_.value();
    }
     const T& value() const {
        if (!has_value_) throw std::system_error(error_, "Result does not hold a value");
        return value_.value();
    }

    const std::error_code& error() const {
        if (has_value_) throw std::logic_error("Result does not hold an error");
        return error_;
    }

private:
    std::optional<T> value_;
    std::error_code error_;
    bool has_value_;
};

// Result type for functions that return void
template <>
struct Result<void> {
    Result() : error_(IndicatorEngineErrc::Success) {}
    Result(std::error_code ec) : error_(ec) {}

    bool is_success() const { return error_ == IndicatorEngineErrc::Success; }
    bool has_error() const { return !is_success(); }

    const std::error_code& error() const {
        if (is_success()) throw std::logic_error("Result does not hold an error");
        return error_;
    }

private:
    std::error_code error_;
};


} // namespace Error

// Define a convenient alias
template <typename T>
using Result = Error::Result<T>;
using VoidResult = Error::Result<void>;

} // namespace IndicatorEngine

#endif // INDICATOR_ENGINE_UTILS_INDICATOR_ENGINE_ERROR_H