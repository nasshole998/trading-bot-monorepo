#include "utils/indicator_engine_error.h"
#include <string>

namespace IndicatorEngine {
namespace Error {

struct IndicatorEngineCategory : std::error_category {
    const char* name() const noexcept override { return "indicator_engine"; }
    std::string message(int ev) const override {
        switch (static_cast<IndicatorEngineErrc>(ev)) {
            case IndicatorEngineErrc::Success: return "Success";
            case IndicatorEngineErrc::InvalidConfig: return "Invalid configuration";
            case IndicatorEngineErrc::InvalidInputData: return "Invalid input data";
            case IndicatorEngineErrc::ComputationError: return "Indicator computation error";
            case IndicatorEngineErrc::DataNotAvailable: return "Data not available for computation";
            case IndicatorEngineErrc::ConversionError: return "Data conversion error";
            case IndicatorEngineErrc::RpcError: return "gRPC error";
            case IndicatorEngineErrc::IoError: return "I/O error";
            case IndicatorEngineErrc::ShutdownRequested: return "Shutdown requested";
            case IndicatorEngineErrc::UnknownError: return "Unknown error";
            default: return "Unknown indicator engine error";
        }
    }
};

std::error_category& indicator_engine_category() {
    static IndicatorEngineCategory category;
    return category;
}

} // namespace Error
} // namespace IndicatorEngine