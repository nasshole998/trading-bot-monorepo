#ifndef INDICATOR_ENGINE_INDICATOR_SERVICE_IMPL_H
#define INDICATOR_ENGINE_INDICATOR_SERVICE_IMPL_H

#include "generated/market_data.grpc.pb.h" // Generated Protobuf types
#include "generated/indicator_data.grpc.pb.h.cc" // Include generated code for types
#include "data_manager.h" // Need access to DataManager
#include <grpcpp/grpcpp.h>
#include <thread>
#include <atomic>
#include <unordered_map> // For managing subscriber states if not in DataManager
#include <mutex> // For managing subscriber states if not in DataManager
#include <memory> // For shared_ptr
#include <string> // For subscriber ID
#include <condition_variable> // Needed for waiting on CV
#include <chrono> // For timeouts

namespace IndicatorEngine {

// Implementation of the gRPC services
class IndicatorServiceImpl final :
    public market_data::MarketDataService::Service, // Receives market data
    public indicator_data::IndicatorService::Service // Streams indicator data
{
public:
    IndicatorServiceImpl(std::shared_ptr<DataManager> data_manager);
    ~IndicatorServiceImpl(); // Clean up threads (though handler threads are joined by gRPC library usually)

    // Implementation of market_data.proto StreamMarketData (Client Streaming)
    // The client (Data Ingestion) streams requests (MarketDataEvent), we receive them.
    grpc::Status StreamMarketData(
        grpc::ServerContext* context,
        grpc::ServerReader<market_data::MarketDataEvent>* reader
    ) override;

    // Implementation of indicator_data.proto SubscribeToIndicators (Server Streaming)
    // A client (Strategy Engine) sends one request, we stream responses (IndicatorValue).
    grpc::Status SubscribeToIndicators(
        grpc::ServerContext* context,
        const indicator_data::IndicatorSubscriptionRequest* request,
        grpc::ServerWriter<indicator_data::IndicatorValue>* writer
    ) override;


private:
    std::shared_ptr<DataManager> data_manager_; // Shared access to data manager

    // Helper to generate a unique subscriber ID (can use context pointer)
    std::string GenerateSubscriberId(grpc::ServerContext* context);
};

} // namespace IndicatorEngine

#endif // INDICATOR_ENGINE_INDICATOR_SERVICE_IMPL_H