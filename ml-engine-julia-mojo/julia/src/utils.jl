# ml-engine-julia-mojo/julia/src/utils.jl
module Utils

using Logging
using LoggingExtras
using Dates
using ProtoBuf # For timestamp conversion

"""
    parse_loglevel(level_str::AbstractString) -> LogLevel

Converts a log level string (case-insensitive) to a Julia LogLevel object.
Defaults to Logging.Info if the string is invalid.
"""
function parse_loglevel(level_str::AbstractString)::LogLevel
    level = lowercase(strip(level_str))
    if level == "trace"
        Logging.Debug # Julia's Debug level is closest to Trace
    elseif level == "debug"
        Logging.Debug
    elseif level == "info"
        Logging.Info
    elseif level == "warn" || level == "warning"
        Logging.Warn
    elseif level == "error"
        Logging.Error
    else
        @warn "Invalid log level string '$level_str'. Defaulting to Info."
        Logging.Info
    end
end

"""
    create_console_logger(min_level::LogLevel) -> AbstractLogger

Creates a logger that logs messages to the console (stderr) with timestamp and level.
"""
function create_console_logger(min_level::LogLevel)::AbstractLogger
    ConsoleLogger(stderr, min_level) |> FormatLogger(timestamp_logger_formatter)
end

"""
    create_file_logger(min_level::LogLevel, filepath::String) -> AbstractLogger

Creates a logger that logs messages to a file. Appends to the file if it exists.
Includes basic rotation (though external tools like logrotate are better).
"""
function create_file_logger(min_level::LogLevel, filepath::String)::AbstractLogger
    log_dir = dirname(filepath)
    if !isdir(log_dir)
        try
            mkpath(log_dir)
        catch e
            @error "Failed to create log directory: $log_dir" exception=(e, catch_backtrace())
            # Fallback to console logger
            return create_console_logger(min_level)
        end
    end

    # TeeLogger combines multiple loggers. Here, console + file.
    TeeLogger(
        ConsoleLogger(stderr, min_level) |> FormatLogger(timestamp_logger_formatter),
        # File logger with rotation (simple size-based rotation)
        MinLevelLogger(
            FileLogger(filepath; append=true, filesize_limit=10*1024*1024, n_keep=3), # 10MB limit, keep 3 backups
            min_level
        ) |> FormatLogger(timestamp_logger_formatter) # Apply formatter to file logger too
    )

    # For only file logging:
    # MinLevelLogger(FileLogger(filepath; append=true), min_level) |> FormatLogger(timestamp_logger_formatter)
end

"""
    timestamp_logger_formatter(log_stream, log_args)

Custom log message formatter including timestamp, level, module, file, line.
"""
function timestamp_logger_formatter(log_stream, log_args)
    timestamp = Dates.format(now(), "yyyy-mm-dd HH:MM:SS.sss")
    log_level_color = Logging.default_logcolor(log_args.level)
    module_name = log_args._module # Can be long, consider shortening
    filepath = replace(something(log_args.file, "<?>"), Base.source_path() => "") # Relative path
    line_num = log_args.line

    # Format: [TIMESTAMP] [LEVEL] [Module File:Line] Message
    # Use printstyled for colored level output
    print(log_stream, "[", timestamp, "] [")
    printstyled(log_stream, uppercase(string(log_args.level)); color = log_level_color)
    # print(log_stream, "] [", module_name, " ", filepath, ":", line_num, "] ")
    print(log_stream, "] [", filepath, ":", line_num, "] ") # Shorter format
    println(log_stream, log_args.message)
end


# --- Timestamp Conversion ---

"""
    now_proto() -> Google.Protobuf.Timestamp

Returns the current UTC time as a Google Protobuf Timestamp.
Requires `ProtoBuf.jl` to have generated code for `google/protobuf/timestamp.proto`.
Make sure `google/protobuf/timestamp.proto` is available during `build.jl`.
"""
function now_proto()
    # Ensure the generated module is accessible. Adjust path if needed.
    # Assumes build.jl placed google protos correctly.
    try
        # Dynamically check if the module exists to avoid errors if protos weren't compiled
        if isdefined(Main, :Google) && isdefined(Main.Google.Protobuf, :Timestamp)
            dt = now(Dates.UTC)
            secs = round(Int64, datetime2unix(dt))
            nanos = Dates.value(dt % Dates.Second) ÷ 1_000_000 # Milliseconds to Nanoseconds
            return Main.Google.Protobuf.Timestamp(seconds=secs, nanos=nanos)
        else
            @warn "Google.Protobuf.Timestamp not found. Proto compilation might be missing."
            # Return a default or error timestamp
             return ProtoBuf.google.protobuf.Timestamp(seconds=0, nanos=0) # Requires ProtoBuf v1.0+ namespace
        end
     catch e
         @error "Error creating protobuf timestamp:" exception=(e, catch_backtrace())
         # Return a default or error timestamp
         return ProtoBuf.google.protobuf.Timestamp(seconds=0, nanos=0)
     end
end

"""
    proto_to_datetime(ts::ProtoBuf.google.protobuf.Timestamp) -> DateTime

Converts a Google Protobuf Timestamp to a Julia DateTime object (UTC).
"""
function proto_to_datetime(ts::ProtoBuf.google.protobuf.Timestamp)::DateTime
    try
        # Combine seconds and nanoseconds, then convert to DateTime
        dt_unix = unix2datetime(ts.seconds + ts.nanos / 1e9)
        return DateTime(dt_unix, Dates.UTC) # Ensure it's UTC
    catch e
        @error "Error converting protobuf timestamp to DateTime:" exception=(e, catch_backtrace())
        return DateTime(0) # Return epoch or handle error appropriately
    end
end


end # module Utils
```


```julia
# ml-engine-julia-mojo/julia/src/features.jl
module Features

using DataFrames
using Statistics
using Logging
using ..Utils # For logging

# Define feature calculation functions or feature extraction logic here.
# This module would typically:
# 1. Define how raw data (prices, volumes, indicators) is transformed into features.
# 2. Handle missing values (imputation).
# 3. Perform normalization or scaling if required by the model.

"""
    extract_features(data::Dict{String, Float64}, expected_features::Vector{String}) -> Union{Vector{Float64}, Nothing}

Extracts and orders features from an input dictionary based on the list
of feature names expected by a specific model.

Handles missing features and potentially performs basic validation/scaling.

# Arguments
- `data::Dict{String, Float64}`: Dictionary containing available feature names and their values.
- `expected_features::Vector{String}`: Ordered list of feature names the model requires.

# Returns
- `Vector{Float64}`: Ordered vector of feature values ready for the model.
- `nothing`: If required features are missing or data is invalid.
"""
function extract_features(data::Dict{String, Float64}, expected_features::Vector{String})::Union{Vector{Float64}, Nothing}
    feature_vector = Vector{Float64}(undef, length(expected_features))

    for (i, feature_name) in enumerate(expected_features)
        if haskey(data, feature_name)
            value = data[feature_name]
            # Basic validation (e.g., check for NaN, Inf)
            if !isfinite(value)
                @warn "Invalid non-finite value for feature '$feature_name': $value. Symbol: $(get(data, "symbol", "N/A"))"
                # Options: return nothing, impute, or let the model handle it
                return nothing # Fail fast for now
            end
            feature_vector[i] = value
        else
            @warn "Missing expected feature '$feature_name'. Symbol: $(get(data, "symbol", "N/A"))"
            # Options: return nothing, use a default value, impute based on other features
            return nothing # Fail fast if features are missing
        end
    end

    # Optional: Apply scaling/normalization here if needed
    # scaler = load_scaler_for_model(...)
    # feature_vector = apply_scaling(feature_vector, scaler)

    return feature_vector
end

# Example placeholder for a feature calculation function
"""
    calculate_price_change_pct(prices::AbstractVector{<:Real}, period::Int) -> Vector{Float64}

Calculates the percentage change over a given period.
"""
function calculate_price_change_pct(prices::AbstractVector{<:Real}, period::Int)::Vector{Float64}
    n = length(prices)
    pct_change = fill(NaN, n) # Initialize with NaN
    if n <= period
        return pct_change # Not enough data
    end
    for i in (period + 1):n
        if prices[i-period] != 0 # Avoid division by zero
            pct_change[i] = (prices[i] - prices[i-period]) / prices[i-period]
        else
            pct_change[i] = 0.0 # Or NaN, depending on desired handling
        end
    end
    return pct_change
end


# Add more complex feature engineering functions as needed:
# - Rolling window calculations (volatility, skewness)
# - Indicator combinations (e.g., SMA difference, RSI divergence)
# - Lagged features
# - Time-based features (hour of day, day of week)

end # module Features
```


```julia
# ml-engine-julia-mojo/julia/src/models.jl
module Models

using Flux # Assuming Flux models primarily
using BSON # For loading models saved as BSON files
using ONNX # Example if using ONNX models (requires ONNX.jl package)
using Logging
using Printf
using ..ConfigLoader: ModelConfig # Use the config struct
using ..Utils

# Define an abstract type for models if needed for dispatch
# abstract type AbstractPredictiveModel end

# Simple struct to hold a loaded model and its config
struct LoadedModel
    id::String
    model::Any # Can be a Flux chain, ONNX model, etc.
    config::ModelConfig
    # Add scaler/preprocessor info if needed
end

"""
    load_model(config::ModelConfig)::Union{LoadedModel, Nothing}

Loads a single model based on its configuration.
Supports different model types (Flux/BSON, ONNX).
"""
function load_model(config::ModelConfig)::Union{LoadedModel, Nothing}
    model_dir = joinpath(@__DIR__, "..", "..", "models") # Path relative to src/
    model_path = joinpath(model_dir, config.path)
    @info "Attempting to load model '$(config.id)' from: $(model_path)"

    if !isfile(model_path)
        @error "Model file not found for id '$(config.id)' at path: $(model_path)"
        return nothing
    end

    try
        if config.type == "flux_bson" || config.type == "flux_nn" # Handle BSON saved Flux models
            # BSON.load returns a Dict, model is usually under a key like :model
            loaded_data = BSON.load(model_path)
            if haskey(loaded_data, :model)
                 model = loaded_data[:model]
                 # Optional: Put model in eval mode if using Flux/CUDA
                 # model = Flux.testmode(model)
                 # if CUDA.functional() # Move model to GPU if available
                 #    model = model |> gpu
                 # end
                 @info "Successfully loaded Flux/BSON model '$(config.id)'."
                 # Type check? Ensure model is callable?
                 return LoadedModel(config.id, model, config)
            else
                @error "BSON file loaded for '$(config.id)', but key ':model' not found."
                return nothing
            end
        elseif config.type == "onnx"
            # Requires ONNX.jl package
             @warn "ONNX model loading not fully implemented yet. Requires ONNX.jl."
             # model = ONNX.load(model_path) # Placeholder
             # @info "Successfully loaded ONNX model '$(config.id)' (Placeholder)."
             # return LoadedModel(config.id, model, config) # Placeholder
             return nothing # Remove when ONNX.jl is added and tested
        else
            @error "Unsupported model type '$(config.type)' for model id '$(config.id)'."
            return nothing
        end
    catch e
        @error "Failed to load model '$(config.id)' from $(model_path):" exception=(e, catch_backtrace())
        return nothing
    end
end

"""
    load_all_models(model_configs::Vector{ModelConfig})::Dict{String, LoadedModel}

Loads all models specified in the configuration list.
"""
function load_all_models(model_configs::Vector{ModelConfig})::Dict{String, LoadedModel}
    loaded_models = Dict{String, LoadedModel}()
    if isempty(model_configs)
        @warn "No models specified in the configuration."
        return loaded_models
    end

    for config in model_configs
        loaded_model = load_model(config)
        if !isnothing(loaded_model)
            if haskey(loaded_models, config.id)
                 @warn "Duplicate model ID found: '$(config.id)'. Overwriting previous entry."
            end
            loaded_models[config.id] = loaded_model
        else
             @error "Skipping model due to loading failure: ID='$(config.id)', Path='$(config.path)'"
        end
    end
    return loaded_models
end


"""
    predict(loaded_model::LoadedModel, features::Vector{Float64})::Union{Any, Nothing}

Performs prediction using the loaded model and prepared feature vector.
Handles potential model-specific input formatting.
"""
function predict(loaded_model::LoadedModel, features::Vector{Float64})::Union{Any, Nothing}
    model = loaded_model.model
    config = loaded_model.config

    try
        # --- Input Formatting ---
        # Reshape features if needed (e.g., Flux expects batch dimension)
        # Example: Add batch dimension (assuming features is a column vector)
        # input_data = reshape(features, length(features), 1)

        # Example: If features need to be Float32
        input_data = Float32.(features)
        input_data = reshape(input_data, length(input_data), 1) # Batch dim

        # Move input data to GPU if model is on GPU
        # if model isa Flux.Chain && CUDA.functional() && !(input_data isa CuArray)
        #     input_data = input_data |> gpu
        # end

        # --- Prediction ---
        @debug "Running prediction for model '$(config.id)'..."
        raw_prediction = model(input_data)

        # --- Output Formatting ---
        # Move prediction back to CPU if it was on GPU
        # if raw_prediction isa CuArray
        #     raw_prediction = raw_prediction |> cpu
        # end

        # Extract desired output (e.g., probability from softmax, single value)
        # This depends heavily on the model's output layer.
        # Example: If output is a 1x1 matrix (single value)
        if size(raw_prediction) == (1, 1)
            prediction_value = raw_prediction[1, 1]
        # Example: If output is probability distribution (e.g., 2 classes)
        elseif size(raw_prediction, 2) == 1 && size(raw_prediction, 1) > 1
             # Assuming prediction is a column vector of probabilities
             # Return the probability of the second class (e.g., "up")
             # prediction_value = raw_prediction[2, 1] # Or return the whole vector/dict
             prediction_value = Dict("raw_output" => vec(raw_prediction)) # Return raw vector in a dict
        else
             # Handle other output shapes or return raw output
             @warn "Model '$(config.id)' produced unexpected output shape: $(size(raw_prediction)). Returning raw output."
             prediction_value = raw_prediction # Return the raw output
        end

        @debug "Prediction successful for model '$(config.id)'."
        return prediction_value

    catch e
        @error "Error during prediction for model '$(config.id)':" exception=(e, catch_backtrace())
        return nothing
    end
end


end # module Models
```


```julia
# ml-engine-julia-mojo/julia/src/inference.jl
module Inference

using Logging
using Libdl # For FFI to call Mojo shared library
using ..ConfigLoader: MojoConfig
using ..Models: LoadedModel, predict as model_predict
using ..Features: extract_features
using Printf

# --- Mojo Integration (Placeholder) ---

# Define the expected signature of the Mojo inference function
# This MUST match the signature exported by the Mojo shared library
# Example: Takes features pointer, feature count, output buffer pointer, returns status code
const MOJO_INFER_SIGNATURE = Ptr{Cvoid}, (Ptr{Float64}, Cint, Ptr{Float64})

mutable struct MojoHandle
    lib_handle::Ptr{Cvoid}
    infer_func_ptr::Ptr{Cvoid}

    MojoHandle() = new(C_NULL, C_NULL) # Initialize with null pointers
end

# Global handle for the loaded Mojo library
const MOJO_LIB = MojoHandle()

"""
    load_mojo_library(config::MojoConfig) -> Bool

Attempts to load the Mojo shared library and find the inference function.
Returns true on success, false otherwise.
"""
function load_mojo_library(config::MojoConfig)::Bool
    if !config.enabled
        @info "Mojo integration is disabled in config."
        return false
    end
    if MOJO_LIB.lib_handle != C_NULL
        @info "Mojo library already loaded."
        return true # Already loaded
    end

    lib_path = abspath(joinpath(@__DIR__, "..", config.lib_path)) # Get absolute path
    @info "Attempting to load Mojo library from: $(lib_path)"

    if !isfile(lib_path)
        @error "Mojo shared library not found at path: $(lib_path)"
        return false
    end

    try
        # Load the shared library
        MOJO_LIB.lib_handle = dlopen(lib_path, RTLD_LAZY | RTLD_GLOBAL)

        # Find the inference function symbol within the library
        # Replace "mojo_predict_model_v1" with the actual exported function name
        func_name = :mojo_predict_model_v1
        MOJO_LIB.infer_func_ptr = dlsym(MOJO_LIB.lib_handle, func_name)

        @info "Successfully loaded Mojo library and found function '$func_name'."
        return true
    catch e
        @error "Failed to load Mojo library or find function:" exception=(e, catch_backtrace())
        if MOJO_LIB.lib_handle != C_NULL
            dlclose(MOJO_LIB.lib_handle) # Close handle if symbol lookup failed
        end
        MOJO_LIB.lib_handle = C_NULL
        MOJO_LIB.infer_func_ptr = C_NULL
        return false
    end
end

"""
    is_mojo_loaded() -> Bool

Checks if the Mojo library handle and function pointer are valid.
"""
function is_mojo_loaded()::Bool
    return MOJO_LIB.lib_handle != C_NULL && MOJO_LIB.infer_func_ptr != C_NULL
end

"""
    predict_with_mojo(features::Vector{Float64}, output_buffer::Vector{Float64}) -> Int

Calls the loaded Mojo inference function via C FFI.

# Arguments
- `features::Vector{Float64}`: Input feature vector.
- `output_buffer::Vector{Float64}`: Pre-allocated buffer to store Mojo's output.

# Returns
- `Int`: Status code returned by the Mojo function (e.g., 0 for success).
"""
function predict_with_mojo(features::Vector{Float64}, output_buffer::Vector{Float64})::Int
    if !is_mojo_loaded()
        @error "Mojo library not loaded, cannot perform Mojo prediction."
        return -1 # Indicate error
    end

    n_features = length(features)
    n_outputs = length(output_buffer) # Mojo function needs to know output size

    # Ensure output buffer is writable (should be by default)
    # output_buffer .= 0.0 # Optional: zero out buffer before call

    try
        # Call the C function pointer
        status = ccall(
            MOJO_LIB.infer_func_ptr, # Function pointer
            Cint,                    # Return type (status code)
            # Argument types tuple - MUST match MOJO_INFER_SIGNATURE
            (Ptr{Float64}, Cint, Ptr{Float64}),
            # Arguments
            features,       # Pointer to input features (Julia array converts automatically)
            n_features,     # Number of features
            output_buffer   # Pointer to output buffer
        )
        return Int(status)
    catch e
        @error "Error calling Mojo function via ccall:" exception=(e, catch_backtrace())
        return -2 # Indicate ccall error
    end
end


# --- Inference Context ---

struct InferenceContext
    models::Dict{String, LoadedModel}
    mojo_config::MojoConfig
    mojo_loaded::Bool

    function InferenceContext(models::Dict{String, LoadedModel}, mojo_config::MojoConfig)
        mojo_was_loaded = load_mojo_library(mojo_config)
        new(models, mojo_config, mojo_was_loaded)
    end
end

is_mojo_loaded(ctx::InferenceContext) = ctx.mojo_loaded

"""
    run_inference(ctx::InferenceContext, model_id::String, feature_dict::Dict{String, Float64}) -> Union{Dict{String, Float64}, Nothing}

Main inference function. Selects model, extracts features, runs prediction (Julia or Mojo).
"""
function run_inference(ctx::InferenceContext, model_id::String, feature_dict::Dict{String, Float64})::Union{Dict{String, Float64}, Nothing}
    # --- Get Model ---
    if !haskey(ctx.models, model_id)
        @error "Model ID '$model_id' not found in loaded models."
        return nothing
    end
    loaded_model = ctx.models[model_id]
    model_config = loaded_model.config

    # --- Extract Features ---
    @debug "Extracting features for model '$model_id': $(model_config.features)"
    feature_vector = extract_features(feature_dict, model_config.features)
    if isnothing(feature_vector)
        @error "Failed to extract features for model '$model_id'."
        return nothing # Error already logged by extract_features
    end
    @debug "Feature vector extracted: $(feature_vector)"


    # --- Select Inference Path (Mojo or Julia) ---
    prediction_result = nothing
    if ctx.mojo_config.enabled && ctx.mojo_loaded # && model_config.type == "mojo_compatible" # Optional: Check if model *should* use Mojo
        @debug "Attempting prediction with Mojo for model '$model_id'..."
        # Determine expected output size for Mojo model
        expected_output_size = 1 # Example: Assume single output value
        output_buffer = Vector{Float64}(undef, expected_output_size)

        status = predict_with_mojo(feature_vector, output_buffer)

        if status == 0 # Success code from Mojo
            @debug "Mojo prediction successful. Status: $status"
            # Assume output_buffer now contains the result(s)
            # Map Mojo output to the expected format (e.g., a dictionary)
            # This mapping depends on the specific Mojo model's output contract
            prediction_result = Dict("mojo_output_$(i)" => val for (i, val) in enumerate(output_buffer)) # Example mapping
        else
            @error "Mojo prediction failed for model '$model_id'. Status code: $status. Falling back to Julia model if available."
            # Fallback logic: Try Julia model if Mojo failed? Only if applicable.
            # prediction_result = model_predict(loaded_model, feature_vector) # Fallback
            prediction_result = nothing # Or return specific error
        end
    else
        # Use Julia model prediction
        @debug "Performing prediction with Julia model '$model_id'..."
        prediction_result = model_predict(loaded_model, feature_vector)
    end

    # --- Format Output ---
    if isnothing(prediction_result)
        @error "Prediction failed for model '$model_id'."
        return nothing
    end

    # Convert prediction result to the standard Dict{String, Float64} format
    output_dict = Dict{String, Float64}()
    if prediction_result isa Dict
        for (k, v) in prediction_result
            if v isa Number # Handle Dict output from Mojo/Julia
                output_dict[string(k)] = Float64(v)
            else
                @warn "Non-numeric value in prediction result dict for key '$k': $(typeof(v))"
            end
        end
     elseif prediction_result isa Number
         # Assume single output value, use a default key
         output_dict["prediction"] = Float64(prediction_result)
     elseif prediction_result isa AbstractArray{<:Number}
         # Handle array output (e.g., probabilities)
         if length(prediction_result) == 1
             output_dict["prediction"] = Float64(prediction_result[1])
         else
             # Example: map index to key
             for (i, val) in enumerate(prediction_result)
                 output_dict["prediction_$(i)"] = Float64(val)
             end
         end
    else
        @error "Unhandled prediction result type: $(typeof(prediction_result)) for model '$model_id'."
        return nothing
    end

    @info "Inference completed for model '$model_id'. Output keys: $(keys(output_dict))"
    return output_dict

end


end # module Inference
```


```julia
# ml-engine-julia-mojo/julia/src/grpc_server.jl
module GrpcServer

using gRPC
using Sockets
using Logging
using ..Inference: InferenceContext, run_inference
using ..Utils: now_proto, proto_to_datetime # Timestamp utils

# Include the generated protobuf code (adjust path if needed)
include("generated/ml_predictions_pb.jl")
import .ml_predictions_pb: PredictionServiceBlockingStub, PredictionService, GetPrediction, PredictionRequest, PredictionResponse

# Define the service implementation struct
# Needs to be mutable if it holds state that changes (like stats)
mutable struct PredictionServiceImpl <: PredictionService
    inference_ctx::InferenceContext
    # Add any other state needed, e.g., request counters
end

# Implement the gRPC service methods
function GetPrediction(impl::PredictionServiceImpl, req::PredictionRequest)
    @info "Received gRPC GetPrediction request: ID=$(req.feature_vector.request_id), Symbol=$(req.feature_vector.symbol)"
    resp = PredictionResponse()
    resp.request_id = req.feature_vector.request_id
    resp.symbol = req.feature_vector.symbol
    resp.prediction_timestamp = now_proto() # Record prediction time

    # --- Input Validation ---
    if isempty(req.feature_vector.symbol)
        @warn "Prediction request missing symbol. ID: $(req.request_id)"
        resp.error_message = "Missing symbol in request"
        return resp # Return response with error
    end
    if isempty(req.feature_vector.features)
         @warn "Prediction request missing features. ID: $(req.request_id), Symbol: $(req.symbol)"
         resp.error_message = "Missing features in request"
         return resp
    end

    # Determine model ID (use default if not specified, requires config)
    # For now, assume client *must* specify or we use a hardcoded default/first model
    model_id_to_use = if !isempty(req.model_id)
        req.model_id
    elseif !isempty(impl.inference_ctx.models)
        # Use the ID of the first loaded model as a default
        first_model_id = first(keys(impl.inference_ctx.models))
        @debug "No model_id specified in request, using default: $first_model_id"
        first_model_id
    else
        @error "No model_id specified and no models loaded."
        resp.error_message = "No model available for prediction"
        return resp
    end
    resp.model_id = model_id_to_use


    # --- Run Inference ---
    # feature_dict comes directly from the proto map
    feature_dict = req.feature_vector.features
    @debug "Running inference for Model: $(model_id_to_use), Features: $(feature_dict)"

    prediction_outputs = run_inference(impl.inference_ctx, model_id_to_use, feature_dict)

    # --- Process Results ---
    if isnothing(prediction_outputs)
        @error "Inference failed for request ID: $(req.request_id), Symbol: $(req.symbol), Model: $(model_id_to_use)"
        resp.error_message = "Inference computation failed"
        # prediction_outputs is already nothing
    else
        @info "Inference successful for request ID: $(req.request_id), Symbol: $(req.symbol), Model: $(model_id_to_use)"
        # Populate the predictions map in the response
        resp.predictions = prediction_outputs
        # Add confidence if available
        # resp.confidence = get(prediction_outputs, "confidence_score", NaN)
    end

    return resp
end


"""
    run_server(inference_ctx::InferenceContext, port::Int) -> Task

Starts the gRPC server in a new Task.
"""
function run_server(inference_ctx::InferenceContext, port::Int)::Task
    # Create service implementation instance
    service_impl = PredictionServiceImpl(inference_ctx)

    # Create gRPC server
    server_task = @async begin
        try
            gRPC.serve(service_impl, IPv4(0,0,0,0), port) # Listen on all interfaces
        catch e
            @error "gRPC server failed:" exception=(e, catch_backtrace())
            # Potentially rethrow or handle specific errors (e.g., port unavailable)
            if e isa Base.IOError && occursin("bind", e.msg)
                 @error "Failed to bind gRPC server to port $port. Is it already in use?"
            end
            # Rethrow to make the task fail
            rethrow(e)
        end
    end

    @info "gRPC server task started on port $port."
    return server_task
end

end # module GrpcServer
```


```julia
# ml-engine-julia-mojo/julia/src/training.jl
# Conceptual outline for the training pipeline logic.
# This would typically be run offline, not as part of the main service.
module Training

using Flux
using MLJ # Or other ML frameworks
using DataFrames
using CSV
using BSON
using Statistics
using Logging
using Printf
using ..ConfigLoader: Settings, ModelConfig # Use config structs
using ..Features: calculate_price_change_pct # Example feature function
using ..Models: LoadedModel # To potentially load base models for transfer learning
using ..Utils

# --- Data Loading & Preparation ---

"""
    load_training_data(data_path::String, symbol::String) -> DataFrame

Loads historical price/indicator data for a specific symbol.
Placeholder: Needs actual implementation based on data source (CSV, DB).
"""
function load_training_data(data_path::String, symbol::String)::DataFrame
    @info "Loading training data for $symbol from $data_path..."
    # Example: Load from CSV assuming specific columns
    try
        # Adjust path and column names as needed
        df = CSV.read(joinpath(data_path, "$(symbol)_1m.csv"), DataFrame)
        # Ensure DateTime column is parsed correctly
        # df.timestamp = DateTime.(df.timestamp_str, "yyyy-mm-dd HH:MM:SS")
        @info "Loaded $(nrow(df)) rows for $symbol."
        return df
    catch e
        @error "Failed to load training data for $symbol:" exception=(e, catch_backtrace())
        rethrow(e)
    end
end

"""
    preprocess_data(df::DataFrame, feature_configs, target_config) -> DataFrame

Performs feature engineering and target variable creation.
"""
function preprocess_data(df::DataFrame; feature_periods=[14, 50], target_period=5)::DataFrame
    @info "Preprocessing data: Calculating features and target..."
    df = copy(df) # Avoid modifying original dataframe

    # Feature examples:
    df.rsi_14 = calculate_rsi(df.close, 14) # Assume calculate_rsi exists
    df.sma_50 = calculate_sma(df.close, 50) # Assume calculate_sma exists
    df.vol_20d = calculate_volatility(df.close, 20) # Assume exists
    df.sma_10_diff = df.close - calculate_sma(df.close, 10)

    # Target example: Future price change % over 'target_period' minutes
    df.target_pct_change = lead(calculate_price_change_pct(df.close, target_period), target_period)

    # Handle NaNs generated by indicators/targets (e.g., drop rows)
    df_processed = dropmissing(df, disallowmissing=true) # Drop rows with any NaN/Missing

    @info "Preprocessing complete. Final data size: $(nrow(df_processed)) rows."
    return df_processed
end

# Placeholder indicator functions (replace with robust implementations or use packages like Indicators.jl)
function calculate_sma(prices::AbstractVector, period::Int) fill(NaN, length(prices)) end
function calculate_rsi(prices::AbstractVector, period::Int) fill(NaN, length(prices)) end
function calculate_volatility(prices::AbstractVector, period::Int) fill(NaN, length(prices)) end
function lead(vector::AbstractVector, n::Int) vcat(vector[n+1:end], fill(NaN, n)) end


# --- Model Definition & Training ---

"""
    define_model(input_dim::Int, output_dim::Int, model_type::String) -> Any

Defines the model structure (e.g., a Flux Chain).
"""
function define_model(input_dim::Int, output_dim::Int, model_type::String="flux_nn")
    @info "Defining model structure (Type: $model_type)..."
    if model_type == "flux_nn"
        # Example simple Feedforward Neural Network
        return Chain(
            Dense(input_dim => 64, relu),
            Dropout(0.2),
            Dense(64 => 32, relu),
            Dense(32 => output_dim) # Output layer (e.g., sigmoid for probability, linear for regression)
            # Add softmax if doing multi-class classification
            # softmax # Example
        )
    else
        @error "Unsupported model type for definition: $model_type"
        return nothing
    end
end

"""
    train_model!(model, X_train, y_train, X_val, y_val; epochs=10, lr=0.001)

Trains the model using provided data. Modifies the model in-place.
"""
function train_model!(model, X_train, y_train, X_val, y_val; epochs=10, lr=0.001)
    @info "Starting model training... Epochs: $epochs, LR: $lr"

    # --- Data Preparation (Flux requires specific format) ---
    # Assuming X are matrices (features x samples), y are targets
    # Convert to Float32, potentially move to GPU
    X_train_f32 = Float32.(X_train)
    y_train_f32 = Float32.(y_train) # Adjust shape/type based on loss function
    X_val_f32 = Float32.(X_val)
    y_val_f32 = Float32.(y_val)

    # Example: Reshape target if needed (e.g., for Flux loss functions)
    # y_train_f32 = reshape(y_train_f32, 1, :) # If target is single value per sample
    # y_val_f32 = reshape(y_val_f32, 1, :)

    # Move data to GPU if available and model is on GPU
    # if CUDA.functional() && model isa Flux.Chain # Check if model is already on GPU?
    #     @info "Moving training data to GPU..."
    #     X_train_f32, y_train_f32 = X_train_f32 |> gpu, y_train_f32 |> gpu
    #     X_val_f32, y_val_f32 = X_val_f32 |> gpu, y_val_f32 |> gpu
    # end

    train_data = Flux.Data.DataLoader((X_train_f32, y_train_f32), batchsize=64, shuffle=true)
    val_data = Flux.Data.DataLoader((X_val_f32, y_val_f32), batchsize=128) # No shuffle for validation

    # --- Loss Function & Optimizer ---
    # Choose loss based on task (regression: mse, classification: logitbinarycrossentropy/crossentropy)
    loss_func(x, y) = Flux.Losses.mse(model(x), y) # Example: Mean Squared Error
    # loss_func(x, y) = Flux.Losses.logitbinarycrossentropy(model(x), y) # Example: Binary Classification

    optimizer = Flux.Optimise.Adam(lr)
    params_to_train = Flux.params(model)

    # --- Training Loop ---
    best_val_loss = Inf
    epochs_no_improve = 0
    patience = 3 # Early stopping patience

    for epoch in 1:epochs
        epoch_train_loss = 0.0
        batch_count = 0
        Flux.trainmode!(model) # Set model to training mode (for Dropout, BatchNorm)

        for (x_batch, y_batch) in train_data
            batch_loss, grads = Flux.withgradient(params_to_train) do
                loss_func(x_batch, y_batch)
            end
            Flux.Optimise.update!(optimizer, params_to_train, grads)
            epoch_train_loss += batch_loss
            batch_count += 1
        end
        avg_train_loss = epoch_train_loss / batch_count

        # --- Validation ---
        Flux.testmode!(model) # Set model to testing mode
        epoch_val_loss = 0.0
        val_batch_count = 0
        for (x_val_batch, y_val_batch) in val_data
             epoch_val_loss += loss_func(x_val_batch, y_val_batch) # Calculate loss on validation set
             val_batch_count += 1
        end
        avg_val_loss = epoch_val_loss / val_batch_count

        @info @sprintf("Epoch %d/%d: Train Loss=%.6f, Val Loss=%.6f", epoch, epochs, avg_train_loss, avg_val_loss)

        # --- Early Stopping ---
        if avg_val_loss < best_val_loss
            best_val_loss = avg_val_loss
            epochs_no_improve = 0
            # Optional: Save the best model checkpoint here
            # save_model_checkpoint(model, "best_model.bson")
        else
            epochs_no_improve += 1
            if epochs_no_improve >= patience
                @warn "Validation loss did not improve for $patience epochs. Stopping early."
                break
            end
        end
    end # End epoch loop

    @info "Training finished. Best Validation Loss: $best_val_loss"
    # Optional: Load the best checkpoint if saved
    # model = load_model_checkpoint("best_model.bson")
end

"""
    save_model(model, filepath::String)

Saves the trained model (and potentially scaler/metadata) to a file.
"""
function save_model(model, filepath::String)
    @info "Saving model to: $filepath"
    try
        # Move model to CPU before saving if it was on GPU
        # model_cpu = model |> cpu
        model_cpu = model # Assume model is already on CPU after training

        # Save using BSON - include model and potentially other metadata
        # It's good practice to save metadata like feature names, training date etc.
        bson(filepath, Dict(:model => model_cpu, :training_date => now()))
        @info "Model saved successfully."
    catch e
        @error "Failed to save model to $filepath:" exception=(e, catch_backtrace())
    end
end


# --- Main Training Pipeline Function ---

"""
    run_training_pipeline(config::Settings, data_dir::String, symbol::String, model_id_to_train::String)

Orchestrates the loading, preprocessing, training, and saving for a specific model.
"""
function run_training_pipeline(config::Settings, data_dir::String, symbol::String, model_id_to_train::String)
    @info "--- Starting Training Pipeline for Model '$model_id_to_train', Symbol '$symbol' ---"

    # Find model config
    model_config = nothing
    for mc in config.models
        if mc.id == model_id_to_train
            model_config = mc
            break
        end
    end
    if isnothing(model_config)
        @error "Configuration for model ID '$model_id_to_train' not found."
        return
    end

    # 1. Load Data
    raw_data = load_training_data(data_dir, symbol)
    if isempty(raw_data) return end

    # 2. Preprocess Data (Feature Engineering, Target Creation)
    processed_data = preprocess_data(raw_data) # Add specific configs if needed
    if isempty(processed_data)
        @error "No data remaining after preprocessing for $symbol."
        return
    end

    # 3. Split Data (Train/Validation/Test)
    # Example simple split (needs refinement, e.g., time-series split)
    n_rows = nrow(processed_data)
    train_end = floor(Int, 0.7 * n_rows)
    val_end = floor(Int, 0.85 * n_rows)

    train_df = processed_data[1:train_end, :]
    val_df = processed_data[train_end+1:val_end, :]
    test_df = processed_data[val_end+1:end, :] # Test set for final evaluation

    @info "Data Split: Train=$(nrow(train_df)), Val=$(nrow(val_df)), Test=$(nrow(test_df))"

    # 4. Prepare Features (X) and Target (y)
    feature_names = model_config.features
    target_name = "target_pct_change" # Example target column name

    # Convert to matrices (features x samples)
    X_train = Matrix(train_df[:, feature_names])'
    y_train = Matrix(train_df[:, [target_name]])' # Ensure correct shape for loss
    X_val = Matrix(val_df[:, feature_names])'
    y_val = Matrix(val_df[:, [target_name]])'
    X_test = Matrix(test_df[:, feature_names])'
    y_test = Matrix(test_df[:, [target_name]])'

    # Optional: Scale features (fit scaler on train data only)
    # scaler = fit_scaler(X_train)
    # X_train = apply_scaler(X_train, scaler)
    # X_val = apply_scaler(X_val, scaler)
    # X_test = apply_scaler(X_test, scaler)
    # save_scaler(scaler, ...) # Save scaler with model

    # 5. Define Model
    input_dim = length(feature_names)
    output_dim = 1 # Example: predicting single value (pct change)
    model = define_model(input_dim, output_dim, model_config.type)
    if isnothing(model) return end

    # 6. Train Model
    train_model!(model, X_train, y_train, X_val, y_val; epochs=20, lr=0.001) # Adjust params

    # 7. Evaluate Model (on test set)
    @info "Evaluating model on test set..."
    Flux.testmode!(model)
    # test_loss = Flux.Losses.mse(model(Float32.(X_test) |> gpu), Float32.(y_test) |> gpu) |> cpu # Example evaluation
    test_loss = Flux.Losses.mse(model(Float32.(X_test)), Float32.(y_test)) # CPU version
    @info "Test Set MSE: $test_loss"
    # Add other evaluation metrics (R^2, MAE, classification metrics)

    # 8. Save Model
    model_dir = joinpath(@__DIR__, "..", "..", "models")
    mkpath(model_dir) # Ensure directory exists
    save_filepath = joinpath(model_dir, model_config.path)
    save_model(model, save_filepath)

    @info "--- Training Pipeline Finished for Model '$model_id_to_train' ---"
end


end # module Training
```


```julia
# ml-engine-julia-mojo/julia/scripts/run_training.jl
# Example script to execute the training pipeline.

# Activate the project environment
using Pkg
if isfile("Project.toml") && isfile("Manifest.toml")
    Pkg.activate(".")
else
     # Try activating from parent directory if run from scripts/
     parent_proj = joinpath(@__DIR__, "..", "Project.toml")
     if isfile(parent_proj)
         Pkg.activate(joinpath(@__DIR__, ".."))
     else
         @warn "Could not find Project.toml. Ensure you run this script from the 'julia' directory or its 'scripts' subdirectory."
     end
end

# Load the main module code (adjust path if needed)
include(joinpath(@__DIR__, "..", "src", "MLEngine.jl"))

using .MLEngine.ConfigLoader
using .MLEngine.Training
using Logging

# --- Configuration ---
# Set environment or pass arguments to select config/mode
config_path = joinpath(@__DIR__, "..", "config", "default.toml") # Or load production/local
data_directory = joinpath(@__DIR__, "..", "..", "data", "historical") # Example path to historical data
symbol_to_train = "AAPL" # Example symbol
model_id_to_train = "direction_v1" # Example model ID from config

# --- Setup Logging ---
# Configure logging specifically for the training script
logger = MLEngine.Utils.create_console_logger(Logging.Debug) # Use Debug level for training
global_logger(logger)
@info "--- Training Script Started ---"
@info "Config Path: $config_path"
@info "Data Directory: $data_directory"
@info "Symbol: $symbol_to_train"
@info "Model ID: $model_id_to_train"

# --- Load Config ---
config = load_config(config_path)

# --- Run Pipeline ---
try
    Training.run_training_pipeline(config, data_directory, symbol_to_train, model_id_to_train)
catch e
    @error "Training pipeline failed:" exception=(e, catch_backtrace())
    exit(1) # Exit with error code
end

@info "--- Training Script Finished Successfully ---"
exit(0)

