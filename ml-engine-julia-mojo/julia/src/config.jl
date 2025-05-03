# MLEngine.jl/src/config.jl

using YAML
using Logging # For logging configuration errors
using ArgParse # For parsing command-line arguments

# Define the configuration structures
Base.@kwdef struct GrpcConfig
    listen_address::String
end

Base.@kwdef struct HealthCheckConfig
    listen_address::String
end

Base.@kwdef struct DataConfig
    max_history_size::Int # How many recent data points to keep per symbol
end

Base.@kwdef struct ModelInstanceConfig
    name::String     # Unique name for this instance
    type::String     # Type of model (e.g., "FluxLSTM", "RuleBased", "MojoModel")
    symbol::String   # Symbol this model is trained for / predicts for
    required_data_points::Int = 100 # Lookback window size needed for prediction/training
    # Add other model-specific parameters here (e.g., architecture params, file name)
    model_file::String = "$(name).bson" # Default model file name
end

Base.@kwdef struct ModelConfig
    artifacts_path::String = "models" # Directory to load/save trained models from/to
    instances::Vector{ModelInstanceConfig} = [] # List of model instances
end

Base.@kwdef struct PredictionConfig
    type::String   # Prediction type (e.g., "buy_signal", "price_forecast_1h")
    symbol::String # Symbol this prediction relates to
    # Optional: Map to a specific model if not implicit by symbol/type
    # model_name::String = ""
end

Base.@kwdef struct PredictionsConfig
    expose::Vector{PredictionConfig} = [] # List of prediction types to expose via gRPC
end

Base.@kwdef struct TrainingConfig
    enabled::Bool = true # Enable or disable online training
    interval_sec::Float64 = 3600.0 # How often to trigger training (in seconds)
    initial_delay_sec::Float64 = 60.0 # Delay before first training trigger
    data_size::Int = 1000 # How many data points to use for each retraining cycle (e.g., use last N points)
    # Add other training parameters (learning rate, epochs, loss function, optimizer)
    epochs::Int = 10 # Example: number of epochs for retraining
    learning_rate::Float64 = 0.001 # Example learning rate
end

Base.@kwdef struct IndicatorEngineConfig
    grpc_address::String # Address of the Indicator Engine gRPC server
end


# Define the overall configuration structure
Base.@kwdef mutable struct Config
    grpc::GrpcConfig
    health_check::HealthCheckConfig
    data::DataConfig
    model::ModelConfig
    predictions::PredictionsConfig
    training::TrainingConfig
    indicator_engine::IndicatorEngineConfig
    # Add other top-level parameters
end

# Function to load configuration from a YAML file
function load_config(filepath::String)::Union{Config, Nothing}
    if !isfile(filepath)
        @error "Config file not found: $(filepath)"
        return nothing
    end

    try
        config_dict = YAML.load_file(filepath)

        # Helper to get nested struct, returning Nothing if key missing
        function get_nested(dict, key, type)
            haskey(dict, key) ? type(; dict[key]...) : nothing
        end

        # Helper to get vector of structs
        function get_vector_of_structs(dict, key, type)
             if haskey(dict, key) && dict[key] isa Vector
                 return [type(; item...) for item in dict[key]]
             end
             return []
        end

        grpc_cfg = get_nested(config_dict, "grpc", GrpcConfig)
        if isnothing(grpc_cfg) || isnothing(grpc_cfg.listen_address) || isempty(grpc_cfg.listen_address)
            @error "Config error: Missing or invalid 'grpc' configuration."
            return nothing
        end

        health_check_cfg = get_nested(config_dict, "health_check", HealthCheckConfig)
         if isnothing(health_check_cfg) || isnothing(health_check_cfg.listen_address) || isempty(health_check_cfg.listen_address)
             @warn "Config warning: Missing or invalid 'health_check' configuration, defaulting."
             health_check_cfg = HealthCheckConfig(listen_address="0.0.0.0:8080")
         end

         data_cfg = get_nested(config_dict, "data", DataConfig)
          if isnothing(data_cfg) || data_cfg.max_history_size <= 0
              @warn "Config warning: Missing or invalid 'data' configuration, defaulting max_history_size."
              data_cfg = DataConfig(max_history_size=1000)
          end

        model_cfg_dict = get(config_dict, "model", Dict{String, Any}())
        model_instances = get_vector_of_structs(model_cfg_dict, "instances", ModelInstanceConfig)
        model_cfg = ModelConfig(
             artifacts_path = get(model_cfg_dict, "artifacts_path", "models"),
             instances = model_instances
         )


        predictions_cfg_dict = get(config_dict, "predictions", Dict{String, Any}())
        predictions_expose = get_vector_of_structs(predictions_cfg_dict, "expose", PredictionConfig)
        predictions_cfg = PredictionsConfig(expose = predictions_expose)


        training_cfg_dict = get(config_dict, "training", Dict{String, Any}())
         training_cfg = TrainingConfig(
             enabled = get(training_cfg_dict, "enabled", true),
             interval_sec = get(training_cfg_dict, "interval_sec", 3600.0),
             initial_delay_sec = get(training_cfg_dict, "initial_delay_sec", 60.0),
             data_size = get(training_cfg_dict, "data_size", 1000),
             epochs = get(training_cfg_dict, "epochs", 10),
             learning_rate = get(training_cfg_dict, "learning_rate", 0.001)
         )

        indicator_engine_cfg = get_nested(config_dict, "indicator_engine", IndicatorEngineConfig)
         if isnothing(indicator_engine_cfg) || isnothing(indicator_engine_cfg.grpc_address) || isempty(indicator_engine_cfg.grpc_address)
             @error "Config error: Missing or invalid 'indicator_engine' configuration."
             return nothing
         end


        # Basic validation for loaded configs (more specific validation can be done in managers)
        if isempty(model_cfg.instances)
             @warn "No model instances defined in config."
        end
        if isempty(predictions_cfg.expose)
             @warn "No prediction types exposed in config."
        end


        config = Config(
            grpc = grpc_cfg,
            health_check = health_check_cfg,
            data = data_cfg,
            model = model_cfg,
            predictions = predictions_cfg,
            training = training_cfg,
            indicator_engine = indicator_engine_cfg
        )

        @info "Configuration loaded successfully."
        return config

    catch e
        @error "Error loading configuration from $(filepath): $(e)"
        return nothing # Indicate failure
    end
end

# Helper function to configure an existing DataManager instance
function configure!(dm::DataManager, max_history_size::Int)
    dm.max_history_size[] = max_history_size # Update atomic field
    @info "DataManager configured with max_history_size=$(max_history_size)."
end

# Helper function to configure an existing ModelManager instance
function configure!(mm::ModelManager, artifacts_path::String)
     mm.model_artifacts_path = artifacts_path
     @info "ModelManager configured with artifacts_path=$(artifacts_path)."
     # Ensure artifacts path exists
     if !isdir(artifacts_path)
         @info "Creating model artifacts directory: $(artifacts_path)"
         mkpath(artifacts_path) # Create directory and parent directories
     end
end