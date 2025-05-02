#include "indicator_service_impl.h"
#include <grpcpp/create_channel.h>
#include <spdlog/spdlog.h> // For logging
#include <chrono>
#include <sstream> // For generating subscriber ID
#include <algorithm> // For std::find
#include <vector> // For storing results from DataManager


namespace IndicatorEngine {

IndicatorServiceImpl::IndicatorServiceImpl(std::shared_ptr<DataManager> data_manager)
    : data_manager_(data_manager)
{
    spdlog::info("IndicatorServiceImpl created.");
}

IndicatorServiceImpl::~IndicatorServiceImpl() {
    spdlog::info("IndicatorServiceImpl destroying.");
    // Subscriber threads are tied to the RPC context and should exit when the RPC finishes.
    // The gRPC library manages their lifecycle. Explicitly joining detached threads
    // is not needed here if they are tied to the RPC context.
}


// Implementation of market_data.proto StreamMarketData (Client Streaming)
grpc::Status IndicatorServiceImpl::StreamMarketData(
    grpc::ServerContext* context,
    grpc::ServerReader<market_data::MarketDataEvent>* reader
) {
    spdlog::info("StreamMarketData RPC started from client: {}", context->peer());
    market_data::MarketDataEvent event;
    // Read events until the client stream is closed or an error occurs
    while (reader->Read(&event)) {
        // Process each incoming market data event
        // Catch errors from DataManager processing
        if (data_manager_->processMarketDataEvent(event).has_error()) {
             // Depending on error, might log and continue, or return error status
             // For critical errors like conversion failure, returning error is appropriate.
             // For this example, we log the error inside DataManager and continue.
        }

        // Check if the client has cancelled the stream
        if (context->IsCancelled()) {
            spdlog::warn("StreamMarketData RPC cancelled by client: {}", context->peer());
            return grpc::Status::CANCELLED;
        }
    }
    // The loop finishes when the client closes the stream gracefully
    spdlog::info("StreamMarketData RPC finished from client: {}", context->peer());
    return grpc::Status::OK;
}


// Implementation of indicator_data.proto SubscribeToIndicators (Server Streaming)
grpc::Status IndicatorServiceImpl::SubscribeToIndicators(
    grpc::ServerContext* context,
    const indicator_data::IndicatorSubscriptionRequest* request,
    grpc::ServerWriter<indicator_data::IndicatorValue>* writer
) {
    std::string subscriber_id = GenerateSubscriberId(context);
    spdlog::info("SubscribeToIndicators RPC started for subscriber: {} from {}", subscriber_id, context->peer());

    // Add subscriber state to DataManager
    data_manager_->addSubscriber(subscriber_id);

    // Get a reference to the DataManager's condition variable and mutex
    std::condition_variable& cv = data_manager_->new_indicator_cv_;
    std::mutex& mutex = data_manager_->latest_values_mutex_; // Mutex protecting the latest values cache

    // The handler thread *is* the subscriber thread.
    // It waits for a signal from DataManager (or polls) and sends updates.

    while (!context->IsCancelled()) {
        std::unique_lock<std::mutex> lock(mutex); // Lock the latest values cache

        // Wait for a signal from DataManager (new_indicator_cv_) or a timeout.
        // The timeout is important to periodically check for client cancellation (context->IsCancelled()).
        // The lambda predicate prevents spurious wakeups or waking up if already cancelled.
        // It also checks if there might be new data available, although getNewIndicatorValues
        // needs the lock, so we check for new values *after* the wait. A better predicate
        // might involve a simple flag in DataManager set by storeAndPublishIndicatorValue.
        cv.wait_for(lock, std::chrono::milliseconds(100), // Wait timeout (e.g., 100ms)
                    [&]{ return context->IsCancelled(); } // Predicate: wake if cancelled
        );


        // Check if the context was cancelled while waiting
        if (context->IsCancelled()) {
            spdlog::info("Subscriber {} stopping due to client cancellation.", subscriber_id);
            break; // Exit the loop
        }

        // Get the new indicator values that match the subscription request and haven't been sent yet
        // This method in DataManager handles filtering and updates the last_sent_timestamps_
        // Note: This call happens *after* the wait, *under the lock*.
        // The DataManager::getNewIndicatorValues method needs the lock internally anyway.
        // We can release the lock *before* calling getNewIndicatorValues if DataManager handles its own lock.
        // Let's adjust DataManager::getNewIndicatorValues to handle its own lock.

        // Re-get lock if it was unlocked by wait_for timeout without a signal
        // This is handled by unique_lock automatically.

        // Call DataManager method to get new values (it handles its own lock internally now)
        lock.unlock(); // Release the lock before calling DataManager method that acquires its own

        std::vector<indicator_data::IndicatorValue> new_values = data_manager_->getNewIndicatorValues(subscriber_id, *request);


        // Send the new values to the client
        for (const auto& value : new_values) {
             // spdlog::trace("Subscriber {}: Attempting to send {}:{} = {}", subscriber_id, value.symbol(), value.indicator_name(), value.value()); // Debug
            if (!writer->Write(value)) {
                // Client disconnected or error writing
                spdlog::warn("Failed to write to subscriber {}. Client likely disconnected.", subscriber_id);
                // context->IsCancelled() will likely become true soon, the loop will exit.
                // If not, breaking the sending loop will cause the while(!context->IsCancelled())
                // loop to re-evaluate and potentially exit or spin. Exiting the RPC is cleaner.
                 goto end_rpc; // Jump to cleanup
            }
             // spdlog::trace("Subscriber {}: Sent {}:{}", subscriber_id, value.symbol(), value.indicator_name()); // Debug
        }
         // If writing failed for any value, we jump to end_rpc.
    }

end_rpc:
    // Clean up state associated with this subscriber in DataManager
    data_manager_->removeSubscriber(subscriber_id);

    spdlog::info("SubscribeToIndicators RPC finished for subscriber: {}.", subscriber_id);

    // The RPC handler returning signals the end of the server stream.
    // gRPC library handles cleaning up the writer.
    return grpc::Status::OK; // Indicate stream ended gracefully
}


// Helper to generate a unique subscriber ID (example using context pointer and a counter)
std::string IndicatorServiceImpl::GenerateSubscriberId(grpc::ServerContext* context) {
    // Use a static counter to ensure uniqueness across different RPCs and threads
    static std::atomic<int> subscriber_counter{0};
    std::stringstream ss;
    // Include thread ID and a counter for high likelihood of uniqueness
    ss << "sub-" << std::this_thread::get_id() << "-" << subscriber_counter.fetch_add(1);
    // Could also include context pointer address, but thread ID + counter is often sufficient
    return ss.str();
}


} // namespace IndicatorEngine