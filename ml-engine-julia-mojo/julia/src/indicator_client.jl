# MLEngine.jl/src/indicator_client.jl

using gRPC # For gRPC client functionality
using ProtoBuf # Needed for Protobuf types
using Logging # For logging

# Function to start the gRPC client task for the Indicator Engine
function start_indicator_client(indicator_engine_address::String, dm::DataManager, config::Config)
    @info "Starting Indicator Engine gRPC client connecting to $(indicator_engine_address)..."

    # Connect to the Indicator Engine gRPC server
    channel = gRPC.client_channel(indicator_engine_address)

    # Create the client stub
    indicator_service_stub = indicator_data.IndicatorServiceBlockingStub(channel)

    # Prepare the subscription request
    # Subscribe to all indicators for all symbols for simplicity, or configure specific ones.
    request = indicator_data.IndicatorSubscriptionRequest()
    # Populate request.symbols and request.indicator_names if you want to filter
    # request.symbols = ["btc_usdt", "eth_btc"]
    # request.indicator_names = ["SMA_20_BTCUSDT", "RSI_14_BTCUSDT"]

    @info "Subscribing to indicators with request: $(request)"

    try
        # Call the server-streaming RPC SubscribeToIndicators
        # This returns an iterable stream of IndicatorValue messages
        indicator_stream = indicator_service_stub.SubscribeToIndicators(request)

        # Iterate over the incoming stream of messages
        for indicator_value in indicator_stream
            # Process each received IndicatorValue
            handle_indicator_value(indicator_value, dm) # Feed into DataManager

            # No need to check for cancellation here explicitly,
            # the iterator should terminate when the server stream ends or the connection is broken.
        end

        @info "Indicator Engine gRPC client stream finished."

    catch e
        # Catch errors during stream processing (e.g., connection errors, server errors)
        @error "Error in Indicator Engine gRPC client stream: $(e)"
        # Implement retry logic here if the client should try to reconnect
        # Example (simple retry after a delay):
        # @warn "Retrying Indicator Engine connection in 5 seconds..."
        # sleep(5)
        # @async start_indicator_client(indicator_engine_address, dm, config) # Restart the task (careful with infinite retries)
    end

    # Close the channel when done (or on shutdown)
    gRPC.close(channel)
    @info "Indicator Engine gRPC client finished."

    return nothing # Task returns nothing on completion/error
end

# The handle_indicator_value function is implemented in data_manager.jl