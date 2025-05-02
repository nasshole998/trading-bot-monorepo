#include "indicator_service_impl.h"
#include <grpcpp/create_channel.h>
#include <iostream>
#include <chrono>
#include <sstream> // For generating subscriber ID
#include <algorithm> // For std::find
#include <vector> // For storing results from DataManager

namespace IndicatorEngine {

IndicatorServiceImpl::IndicatorServiceImpl(std::shared_ptr<DataManager> data_manager)
    : data_manager_(data_manager)
{
    std::cout << "IndicatorServiceImpl created." << std::endl;
}

IndicatorServiceImpl::~IndicatorServiceImpl() {
    std::cout << "IndicatorServiceImpl destroying." << std::endl;
    // Subscriber threads are tied to the RPC context and should exit when the RPC finishes.
    // The gRPC library manages their lifecycle. Explicitly joining detached threads
    // is not needed here if they are tied to the RPC context.
}


// Implementation of market_data.proto StreamMarketData (Client Streaming)
grpc::Status IndicatorServiceImpl::StreamMarketData(
    grpc::ServerContext* context,
    grpc::ServerReader<market_data::MarketDataEvent>* reader
) {
    std::cout << "StreamMarketData RPC started from client: " << context->peer() << std::endl;
    market_data::MarketDataEvent event;
    // Read events until the client stream is closed or an error occurs
    while (reader->Read(&event)) {
        // Process each incoming market data event
        data_manager_->processMarketDataEvent(event);

        // Check if the client has cancelled the stream
        if (context->IsCancelled()) {
            std::cerr << "StreamMarketData RPC cancelled by client: " << context->peer() << std::endl;
            return grpc::Status::CANCELLED;
        }
    }
    // The loop finishes when the client closes the stream gracefully
    std::cout << "StreamMarketData RPC finished from client: " << context->peer() << std::endl;
    return grpc::Status::OK;
}


// Implementation of indicator_data.proto SubscribeToIndicators (Server Streaming)
grpc::Status IndicatorServiceImpl::SubscribeToIndicators(
    grpc::ServerContext* context,
    const indicator_data::IndicatorSubscriptionRequest* request,
    grpc::ServerWriter<indicator_data::IndicatorValue>* writer
) {
    std::string subscriber_id = GenerateSubscriberId(context);
    std::cout << "SubscribeToIndicators RPC started for subscriber: " << subscriber_id << " from " << context->peer() << std::endl;

    // Add subscriber state to DataManager
    data_manager_->addSubscriber(subscriber_id);

    // Get a reference to the DataManager's condition variable and mutex
    // Note: Accessing members like this requires them to be public in DataManager
    // Alternatively, DataManager can provide public methods like `waitForUpdates(lock, timeout)`
    // or `getLatestValuesAndClearFlags()`. Using public members is simpler for this example.
    std::condition_variable& cv = data_manager_->new_indicator_cv_;
    std::mutex& mutex = data_manager_->latest_values_mutex_; // Mutex protecting the latest values cache

    // The handler thread *is* the subscriber thread.
    // It waits for a signal from DataManager (or polls) and sends updates.

    while (!context->IsCancelled()) {
        std::unique_lock<std::mutex> lock(mutex); // Lock the latest values cache

        // Wait for a signal from DataManager (new_indicator_cv_) or a timeout.
        // The timeout is important to periodically check for client cancellation (context->IsCancelled()).
        // The lambda predicate prevents spurious wakeups or waking up if already cancelled.
        cv.wait_for(lock, std::chrono::milliseconds(100), // Wait timeout (e.g., 100ms)
                    [&]{ return context->IsCancelled(); || !data_manager_->getNewIndicatorValues(subscriber_id, *request).empty(); } // Predicate: wake if cancelled or there are new values
        );


        // Check if the context was cancelled while waiting
        if (context->IsCancelled()) {
            std::cout << "Subscriber " << subscriber_id << " stopping due to client cancellation." << std::endl;
            break; // Exit the loop
        }

        // Get the new indicator values that match the subscription request and haven't been sent yet
        // This method in DataManager handles filtering and updates the last_sent_timestamps_
        std::vector<indicator_data::IndicatorValue> new_values = data_manager_->getNewIndicatorValues(subscriber_id, *request);

        // Release the lock on the latest values cache before sending over the network
        lock.unlock();

        // Send the new values to the client
        for (const auto& value : new_values) {
             // std::cout << "Subscriber " << subscriber_id << ": Sending " << value.indicator_name() << " = " << value.value() << std::endl; // Debug
            if (!writer->Write(value)) {
                // Client disconnected or error writing
                std::cerr << "Failed to write to subscriber " << subscriber_id << ". Client likely disconnected." << std::endl;
                // context->IsCancelled() will likely become true soon, the loop will exit.
                break; // Exit sending loop for this subscriber
            }
        }
         // If writing failed, context->IsCancelled() will be true on the next loop check,
         // and the outer loop will break.
    }

    // Clean up state associated with this subscriber in DataManager
    data_manager_->removeSubscriber(subscriber_id);

    std::cout << "SubscribeToIndicators RPC finished for subscriber: " << subscriber_id << "." << std::endl;

    // The RPC handler returning signals the end of the server stream.
    // gRPC library handles cleaning up the writer.
    return grpc::Status::OK; // Indicate stream ended gracefully
}


// Helper to generate a unique subscriber ID (example using context pointer and a counter)
std::string IndicatorServiceImpl::GenerateSubscriberId(grpc::ServerContext* context) {
    // Use a static counter to ensure uniqueness even if context pointers are reused or peers are the same
    static std::atomic<int> subscriber_counter{0};
    std::stringstream ss;
    ss << "sub-" << subscriber_counter.fetch_add(1) << "-" << context; // Unique per RPC context
    return ss.str();
}


} // namespace IndicatorEngine