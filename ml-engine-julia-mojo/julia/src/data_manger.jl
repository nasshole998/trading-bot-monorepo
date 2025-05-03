# MLEngine.jl/src/data_manager.jl

using CircularArrays # For efficient data storage
using Dates # For timestamps
using ProtoBuf # Needed for Protobuf types
using Base.Threads # For thread-safe access (ReentrantLock, atomic, Condition)
using Logging # For logging
using JSON # For parsing metadata
using Statistics # For mean, etc. (example usage)


# Define a structure to hold data for a single symbol
# This needs to store price and indicator values together, aligned by timestamp.
# Since indicator streams might not align perfectly with market data streams,
# we need a strategy to combine them based on timestamps.
# A simple approach: store price and indicator values separately, then when
# building sequences, find the closest indicator value for each price timestamp.
# A more complex approach: resample data to a fixed frequency.
# Let's use the simpler approach for this task.

mutable struct SymbolData
    symbol::String
    # Store price data (timestamp, price)
    recent_market_data::CircularVector{Tuple{DateTime, Float64}} # Use DateTime first for sorting

    # Store indicator data (indicator key -> buffer of (timestamp, value))
    # Indicator key: "indicator_name" (assuming symbol is implicit from SymbolData)
    recent_indicators::Dict{String, CircularVector{Tuple{DateTime, Float64}}}

    lock::ReentrantLock # Lock for thread-safe access to this symbol's data

end

# Constructor for SymbolData
function SymbolData(symbol::String, max_history_size::Int)
    SymbolData(
        symbol,
        CircularVector{Tuple{DateTime, Float64}}(max_history_size),
        Dict{String, CircularVector{Tuple{DateTime, Float64}}}(), # Indicators dict
        ReentrantLock()
    )
end


# The main DataManager struct
mutable struct DataManager
    # Store data for each symbol
    symbol_data::Dict{String, SymbolData}
    map_lock::ReentrantLock # Lock for thread-safe access to the symbol_data Dict

    max_history_size::Atomic{Int} # Use Atomic for mutable fields accessed across threads

    # Store the latest computed predictions per symbol and prediction type
    # Prediction key: "symbol:prediction_type" -> PredictionValue
    latest_predictions::Dict{String, ml_prediction.PredictionValue}
    predictions_lock::ReentrantLock # Lock for thread-safe access to latest_predictions

    # Track the last timestamp of the value sent to each subscriber
    # Subscriber ID -> Dictionary (Prediction Key -> Last Sent Timestamp)
    # Prediction Key: "symbol:prediction_type"
    subscriber_timestamps::Dict{String, Dict{String, DateTime}}
    subscribers_lock::ReentrantLock # Lock for thread-safe access to subscriber_timestamps

    # Condition variable to notify subscriber tasks when new predictions are available
    new_prediction_cv::Condition # Condition variable for notifying subscribers

end

# Constructor for DataManager
function DataManager()
    DataManager(
        Dict{String, SymbolData}(),
        ReentrantLock(),
        Atomic{Int}(1000), # Default history size
        Dict{String, ml_prediction.PredictionValue}(),
        ReentrantLock(),
        Dict{String, Dict{String, DateTime}}(),
        ReentrantLock(),
        Condition()
    )
end

# Helper to get or create SymbolData instance
function get_or_create_symbol_data(dm::DataManager, symbol::String)::SymbolData
    lock(dm.map_lock) do
        if !haskey(dm.symbol_data, symbol)
            @info "Creating SymbolData for symbol: $(symbol)"
            dm.symbol_data[symbol] = SymbolData(symbol, dm.max_history_size[]) # Use Atomic value
        end
        dm.symbol_data[symbol]
    end
end


# --- Methods for handling incoming data ---

# Handle a single MarketDataEvent (called by gRPC server handler)
function handle_market_data_event(event::market_data.MarketDataEvent, dm::DataManager)
    symbol::String = ""
    price::Union{Float64, Nothing} = nothing
    timestamp_proto::Union{google.protobuf.Timestamp, Nothing} = nothing

    if hasproperty(event, :trade) && !isnothing(event.trade)
        trade = event.trade
        symbol = trade.symbol
        price_str = trade.price
        price = tryparse(Float64, price_str)
        timestamp_proto = trade.timestamp
        # @debug "Received Trade: $(symbol) $(price_str)"
    elseif hasproperty(event, :quote) && !isnothing(event.quote)
        quote_data = event.quote
        symbol = quote_data.symbol
        # Example: Use midpoint price
        bid_price_str = quote_data.bid_price
        ask_price_str = quote_data.ask_price
        bid_price = tryparse(Float64, bid_price_str)
        ask_price = tryparse(Float64, ask_price_str)
        if !isnothing(bid_price) && !isnothing(ask_price)
            price = (bid_price + ask_price) / 2.0
        end
         timestamp_proto = quote_data.timestamp
        # @debug "Received Quote: $(symbol) $(bid_price_str)/$(ask_price_str)"
    elseif hasproperty(event, :order_book_update) && !isnothing(event.order_book_update)
        ob_update = event.order_book_update
        symbol = ob_update.symbol
         # Example: Use midpoint of best bid/ask if available
         if !isempty(ob_update.bids) && !isempty(ob_update.asks)
            best_bid_str = ob_update.bids[1].price
            best_ask_str = ob_update.asks[1].price
            best_bid = tryparse(Float64, best_bid_str)
            best_ask = tryparse(Float64, best_ask_str)
             if !isnothing(best_bid) && !isnothing(best_ask)
                 price = (best_bid + best_ask) / 2.0
             end
         end
         timestamp_proto = ob_update.timestamp
    else
        @warn "Received unhandled MarketDataEvent type."
        return # Do nothing for unhandled types
    end

    if isempty(symbol)
        @warn "Received MarketDataEvent with empty symbol."
        return
    end
    if isnothing(timestamp_proto)
        @warn "Received MarketDataEvent with no timestamp for symbol: $(symbol)."
        return
    end

     timestamp = Dates.unix2datetime(timestamp_proto.seconds + timestamp_proto.nanos / 1e9)


    # Get or create SymbolData
    symbol_data_instance = get_or_create_symbol_data(dm, symbol)

    # Add price and timestamp to history if price is valid
    if !isnothing(price) && !isnan(price)
        lock(symbol_data_instance.lock) do
            push!(symbol_data_instance.recent_market_data, (timestamp, price))
            # CircularVector handles size limit automatically
        end
        @trace "Added price $(price) @ $(timestamp) for $(symbol). History size: $(length(symbol_data_instance.recent_market_data))"

        # Trigger prediction calculation for this symbol (if prediction depends on market data)
        # Prediction is triggered whenever *any* relevant data arrives (market or indicator)
        trigger_prediction_computation(symbol, timestamp, dm, model_manager) # Pass DataManager and ModelManager
    else
         @warn "Could not parse or derive valid price from MarketDataEvent for symbol: $(symbol)"
    end
end

# Handle a single IndicatorValue (called by indicator client task)
function handle_indicator_value(value::indicator_data.IndicatorValue, dm::DataManager)
    symbol = value.symbol
    indicator_name = value.indicator_name
    indicator_key = indicator_name # Use name as key within symbol data
    value_str = value.value
    indicator_value = tryparse(Float64, value_str)
    timestamp_proto = value.timestamp

     if isempty(symbol) || isempty(indicator_name) || isnothing(timestamp_proto)
        @warn "Received invalid IndicatorValue (empty symbol/name or no timestamp)."
        return
    end
     if isnothing(indicator_value) || isnan(indicator_value)
         @warn "Could not parse valid value from IndicatorValue for $(symbol):$(indicator_name)."
         return
     end

     timestamp = Dates.unix2datetime(timestamp_proto.seconds + timestamp_proto.nanos / 1e9)


    # Get or create SymbolData
    symbol_data_instance = get_or_create_symbol_data(dm, symbol)

    # Get or create indicator buffer within SymbolData
    lock(symbol_data_instance.lock) do
         if !haskey(symbol_data_instance.recent_indicators, indicator_key)
             @info "Creating indicator buffer for $(symbol):$(indicator_key)"
             # Indicator buffer size can be same as market data history or indicator-specific
             # Let's use the same max history size for simplicity
             symbol_data_instance.recent_indicators[indicator_key] = CircularVector{Tuple{DateTime, Float64}}(dm.max_history_size[])
         end
         # Add the indicator value and timestamp
         push!(symbol_data_instance.recent_indicators[indicator_key], (timestamp, indicator_value))
          # @trace "Added indicator $(symbol):$(indicator_key) = $(indicator_value) @ $(timestamp). History size: $(length(symbol_data_instance.recent_indicators[indicator_key]))"
    end

    # Trigger prediction calculation for this symbol (if prediction depends on indicator data)
    trigger_prediction_computation(symbol, timestamp, dm, model_manager) # Pass DataManager and ModelManager
end


# Handler for MarketDataService.StreamMarketData (Client Streaming)
function handle_stream_market_data(ctx, reader_stream, dm::DataManager)
    @info "StreamMarketData RPC started from client: $(ctx.peer)"
    try
        # Iterate over the incoming stream of MarketDataEvent messages
        for event in reader_stream
            handle_market_data_event(event, dm)
             # No need to check ctx.is_cancelled explicitly in the loop body usually,
             # gRPC.jl's iterator should handle stream closure/cancellation.
        end
        @info "StreamMarketData RPC finished from client: $(ctx.peer)"
        return nothing # Or gRPC.RPCStatus(:OK) if using status returns
    catch e
        # Catch errors during stream processing (e.g., deserialization errors, internal logic errors)
        @error "Error in StreamMarketData RPC for client $(ctx.peer): $(e)"
        # Return an error status if gRPC.jl supports it in handlers, otherwise rethrow.
        # return gRPC.RPCStatus(:Internal, "Error processing market data: $(e)") # Example error status
         rethrow(e)
    end
end


# --- Methods for accessing data (for predictors and trainers) ---

# Get a sequence of aligned data points for a symbol up to a given timestamp.
# This is crucial for time series models like LSTM.
# Features order needs to be consistent: [price, indicator1, indicator2, ...]
# Missing values (e.g., indicator value slightly older than price) need handling (e.g., forward fill, nearest).
# For simplicity, this implementation will find the *latest* indicator value *at or before* the timestamp
# for each point in the price sequence.
function get_aligned_data_sequence(dm::DataManager, symbol::String, required_data_points::Int, end_timestamp::DateTime)::Union{Vector{Vector{Float64}}, Nothing}

    symbol_data_instance = lock(dm.map_lock) do
        get(dm.symbol_data, symbol, nothing)
    end

    if isnothing(symbol_data_instance)
        @debug "No data available for symbol $(symbol)."
        return nothing
    end

    lock(symbol_data_instance.lock) do
        price_data = symbol_data_instance.recent_market_data
        indicator_data_dict = symbol_data_instance.recent_indicators
        indicator_keys = collect(keys(indicator_data_dict)) # Get keys once

        # Find the price data point at or just before the end_timestamp
        # This is the end of our lookback window
        end_idx = findlast(item -> item[1] <= end_timestamp, price_data) # Find index by timestamp

        if isnothing(end_idx) # No price data at or before end_timestamp
             @debug "No price data found at or before $(end_timestamp) for symbol $(symbol)."
             return nothing
        end

        # Determine the start index of the lookback window
        start_idx = max(1, end_idx - required_data_points + 1)

        # Check if we have enough data points in the lookback window
        if (end_idx - start_idx + 1) < required_data_points
             @debug "Not enough price data points ($(end_idx - start_idx + 1)/$(required_data_points)) for sequence for symbol $(symbol) ending at $(end_timestamp)."
            return nothing # Not enough data for the requested sequence length
        end

        # Build the aligned sequence
        sequence = Vector{Vector{Float64}}(undef, required_data_points) # Vector of feature vectors

        for i in 0:(required_data_points - 1)
            price_tuple = price_data[start_idx + i] # Get the i-th price tuple in the window
            price_ts = price_tuple[1]
            price_value = price_tuple[2]

            # Create the feature vector for this timestamp
            # Start with the price
            feature_vector = [price_value]

            # Add corresponding indicator values
            for indicator_key in indicator_keys
                 indicator_hist = indicator_data_dict[indicator_key]
                 # Find the latest indicator value AT or BEFORE the current price timestamp
                 indicator_idx = findlast(item -> item[1] <= price_ts, indicator_hist)

                 if isnothing(indicator_idx)
                     # No indicator data available up to this timestamp for this indicator
                     # Handle as missing: use 0.0, NaN, or the first available value (more complex)
                     push!(feature_vector, 0.0) # Simple handling: use 0.0
                     @trace "No indicator data found for $(indicator_key) <= $(price_ts). Using 0.0"
                 else
                     push!(feature_vector, indicator_hist[indicator_idx][2]) # Add indicator value
                 end
            end

            sequence[i + 1] = feature_vector # Store the feature vector (Julia is 1-indexed)
        end

        return sequence
    end # Lock released
end


# --- Methods for handling predictions ---

# Store a computed prediction value and notify subscribers
function store_and_publish_prediction(dm::DataManager, prediction::ml_prediction.PredictionValue)
    prediction_key = "$(prediction.symbol):$(prediction.prediction_type)"

    lock(dm.predictions_lock) do # Lock covers both latest_predictions and subscriber_timestamps
        dm.latest_predictions[prediction_key] = prediction # Store the latest value
        # @trace "Stored latest prediction: $(prediction_key) = $(prediction.value)"
    end

    # Notify subscriber tasks waiting on the condition variable
    notify(dm.new_prediction_cv)
end

# Get all latest prediction values matching a subscription request
# Returns a vector of values that are newer than the last sent timestamp for this subscriber.
function get_new_prediction_values(dm::DataManager, subscriber_id::String, request::ml_prediction.PredictionSubscriptionRequest)::Vector{ml_prediction.PredictionValue}
    new_values = Vector{ml_prediction.PredictionValue}()

    lock(dm.predictions_lock) do # Lock the latest predictions and subscriber state
        # Find or create the subscriber's last sent timestamps dictionary
        subscriber_timestamps_dict = get!(dm.subscriber_timestamps, subscriber_id, Dict{String, DateTime}())

        # Iterate through the latest stored predictions
        for (prediction_key, prediction_value) in dm.latest_predictions
            # Prediction key is "symbol:prediction_type"
            parts = split(prediction_key, ":")
            if length(parts) != 2
                @warn "Invalid prediction key format in storage: $(prediction_key)"
                continue
            end
            current_symbol = parts[1]
            current_prediction_type = parts[2]
            prediction_timestamp = Dates.unix2datetime(prediction_value.timestamp.seconds + prediction_value.timestamp.nanos / 1e9)


            # Check if symbol matches subscription (if symbols list is not empty)
            symbol_matches = isempty(request.symbols) || current_symbol in request.symbols

            if symbol_matches
                # Check if prediction type matches subscription (if types list is not empty)
                 type_matches = isempty(request.prediction_types) || current_prediction_type in request.prediction_types

                if type_matches
                    # Check if this value is newer than the last one sent to THIS subscriber for THIS prediction key
                    last_sent_timestamp = get(subscriber_timestamps_dict, prediction_key, Dates.DateTime(0)) # Default to epoch if never sent

                    is_newer = prediction_timestamp > last_sent_timestamp

                    if is_newer
                         # Add to the list of values to send
                         push!(new_values, prediction_value) # Copy the value

                         # Update the last sent timestamp for this subscriber and prediction key
                         subscriber_timestamps_dict[prediction_key] = prediction_timestamp
                         # @trace "DataManager: Found new value for sub $(subscriber_id) key $(prediction_key)"
                    end
                end
            end
        end
    end # Lock is released here

    return new_values
end

# Add/Remove subscriber state (called by PredictionService handler)
function add_subscriber(dm::DataManager, subscriber_id::String)
    lock(dm.predictions_lock) do # Lock the subscriber state map
        # Initialize an empty dictionary for the subscriber's timestamps
        get!(dm.subscriber_timestamps, subscriber_id, Dict{String, DateTime}())
    end
    @info "DataManager: Added subscriber state for ID: $(subscriber_id)"
end

function remove_subscriber(dm::DataManager, subscriber_id::String)
    lock(dm.predictions_lock) do # Lock access to subscriber state
        if haskey(dm.subscriber_timestamps, subscriber_id)
            delete!(dm.subscriber_timestamps, subscriber_id)
            @info "DataManager: Removed subscriber state for ID: $(subscriber_id)"
        else
             @warn "DataManager: Warning - tried to remove non-existent subscriber state for ID: $(subscriber_id)"
        end
    end
end


# --- Trigger Prediction Computation and Publish ---

# Internal method to trigger prediction computation for relevant predictions/models
# This is called whenever *any* relevant data arrives (market or indicator).
# It finds relevant model configs and calls the predictor.
function trigger_prediction_computation(symbol::String, timestamp::DateTime, dm::DataManager, model_manager::ModelManager)
    # Iterate through exposed prediction configs to see which ones need computing
    # based on this incoming data (symbol, timestamp).

    # For simplicity, we'll check all exposed predictions that match the symbol.
    # A real system might map incoming data types/symbols to specific prediction triggers.

    for pred_cfg in model_manager.prediction_configs # Use prediction_configs stored in ModelManager after loading config
        if pred_cfg.symbol == symbol
            prediction_type = pred_cfg.type
            # Find the relevant model config for this prediction type and symbol
            model_cfg = find_model_config_for_prediction(model_manager, symbol, prediction_type)

            if isnothing(model_cfg)
                @warn "No model config found for prediction type $(prediction_type) on symbol $(symbol). Cannot predict."
                continue
            end

            # Trigger the prediction logic (e.g., call the Predictor)
            # Pass necessary context: DataManager, ModelManager, specific config
            prediction_value = get_prediction(dm, model_manager, model_cfg, timestamp) # Pass model_cfg and timestamp

            if !isnothing(prediction_value)
                # Create a Protobuf PredictionValue message
                proto_timestamp = google.protobuf.Timestamp()
                seconds = floor(Int64, Dates.datetime2unix(timestamp))
                nanos = floor(Int32, (Dates.datetime2unix(timestamp) - seconds) * 1e9)
                proto_timestamp.seconds = seconds
                proto_timestamp.nanos = nanos


                prediction_msg = ml_prediction.PredictionValue(
                    exchange="binance", # Assuming exchange
                    symbol=symbol,
                    prediction_type=prediction_type,
                    value=string(prediction_value), # Convert numerical prediction to string
                    timestamp=proto_timestamp
                    # metadata="{}", # Add metadata if available
                )

                # Store and publish the new prediction
                store_and_publish_prediction(dm, prediction_msg)
            else
                # Prediction not ready or failed for this data point/config
                @trace "Prediction $(prediction_type) for $(symbol) not computed for data at $(timestamp)."
            end
        end
    end
end

# --- Handler for ml_prediction.PredictionService.SubscribeToPredictions (Server Streaming) ---

function handle_subscribe_to_predictions(ctx, request::ml_prediction.PredictionSubscriptionRequest, dm::DataManager, model_manager::ModelManager, config::Config)
     # This function runs in a task managed by gRPC.jl for each subscriber.
     # It needs to send predictions to the client using `put!(ctx.stream, prediction_msg)`.
     # It should block, waiting for new predictions, and check for cancellation.

     # Generate a unique subscriber ID using thread ID and context object ID
     subscriber_id = string(Threads.threadid(), "-", objectid(ctx))

     @info "SubscribeToPredictions RPC started for subscriber: $(subscriber_id) from $(ctx.peer)"

     add_subscriber(dm, subscriber_id) # Add subscriber state to DataManager

     try
         # Loop while the RPC is active and not cancelled
         while true
             # Use a lock and condition variable to wait for new predictions.
             # The lock protects access to the DataManager's latest_predictions and subscriber_timestamps.
             # The Condition variable is signaled when store_and_publish_prediction is called.
             lock(dm.predictions_lock) do
                 # Wait for a signal OR a timeout. Timeout allows checking cancellation.
                 # `wait` releases the lock while waiting and reacquires it before returning.
                 wait(dm.new_prediction_cv, dm.predictions_lock, 0.1) # Wait with 100ms timeout
             end # Lock is released by wait/upon returning

             # Check for RPC cancellation *after* waiting (the timeout ensures we check periodically)
             if !isnothing(ctx.is_cancelled) && ctx.is_cancelled()
                 @info "Subscriber $(subscriber_id) detected client cancellation."
                 break # Exit loop
             end

             # Get new prediction values that match the subscription and haven't been sent
             # This method handles filtering and updating the last_sent_timestamps for this subscriber.
             # It acquires its own lock internally.
             new_predictions = get_new_prediction_values(dm, subscriber_id, request)

             # Send the new predictions to the client
             for prediction_value in new_predictions
                 # Use `put!` to send messages to the server stream (non-blocking)
                 # This call can throw if the client disconnects
                 try
                     put!(ctx.stream, prediction_value)
                     # @trace "Subscriber $(subscriber_id): Sent $(prediction_value.prediction_type) for $(prediction_value.symbol)"
                 catch e
                     # Catch error during write (likely client disconnected)
                     @warn "Failed to write to subscriber $(subscriber_id) stream: $(e). Client likely disconnected."
                     # If writing fails, assume client is gone and exit the handler.
                     throw(e) # Rethrow to be caught by the outer catch block
                 end
             end

             # Check for cancellation again after sending (important if sending takes time)
             if !isnothing(ctx.is_cancelled) && ctx.is_cancelled()
                 @info "Subscriber $(subscriber_id) detected client cancellation after sending."
                 break # Exit loop
             end
         end # end while true

     catch e
         # Catch any exceptions that occurred in the loop (including write errors)
         @error "Exception in SubscribeToPredictions RPC for subscriber $(subscriber_id): $(e)"
         # The exception will likely cause the gRPC framework to terminate the RPC with an error status.
     finally
         # Clean up subscriber state in DataManager
         remove_subscriber(dm, subscriber_id)
         @info "SubscribeToPredictions RPC handler finished for subscriber: $(subscriber_id)."
     end

    return nothing # Return nothing on successful exit or handled error
end

# Implement other DataManager methods as needed
# e.g., loading historical data for training from persistent storage (files/DB).
# This would be a separate function not directly tied to the real-time stream processing.
# function load_historical_data_for_training(dm::DataManager, symbol::String, start_time::DateTime, end_time::DateTime)::Union{Vector{Tuple{DateTime, Float64}}, Nothing}
#     # Implement logic to load historical data from storage (e.g., CSV, database)
#     @warn "Placeholder: load_historical_data_for_training not implemented."
#     return nothing
# end