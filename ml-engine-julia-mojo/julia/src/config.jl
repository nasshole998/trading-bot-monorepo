# ml-engine-julia-mojo/julia/src/config.jl
module ConfigLoader

using Configurations
using ..Utils # Access logging utilities if needed

# Define configuration structs using Configurations.jl macros
# These structs mirror the structure of the TOML config file

@option struct ModelConfig
    id::String          # Unique identifier for the model
    path::String        # Path to the saved model file (relative to models/ dir)
    type::String        # Type of model (e.g., "flux_nn", "xgboost", "onnx")
    features::Vector{String} # List of feature names this model expects
    # Add other model-specific params (e.g., output_name)
end

@option struct ServerConfig
    host::String = "0.0.0.0"
    port::Int = 50053
end

@option struct LoggingConfig
    level::String = "info" # trace, debug, info, warn, error
    log_file::String = ""  # Empty means console only, otherwise relative path to julia/ dir
end

@option struct MojoConfig
    enabled::Bool = false # Whether to attempt using Mojo
    lib_path::String = "../mojo/libmodel_inference.so" # Relative path to shared library
end

# Main configuration struct
@option struct Settings
    server::ServerConfig = ServerConfig()
    logging::LoggingConfig = LoggingConfig()
    models::Vector{ModelConfig} = [] # List of models to load
    mojo::MojoConfig = MojoConfig()
    # Add other sections as needed (e.g., feature_engineering params)
end

"""
    load_config(path::String)

Loads configuration from the specified TOML file path.
"""
function load_config(path::String)::Settings
    try
        # `from_toml` reads the file and parses it into the Settings struct
        return from_toml(Settings, path)
    catch e
        println(stderr, "Error loading configuration from $(path): $e")
        rethrow(e)
    end
end

end # module ConfigLoader


# ml-engine-julia-mojo/julia/config/default.toml
# Default configuration for the ML Engine service

[server]
host = "0.0.0.0" # Listen on all interfaces inside the container
port = 50053     # Default gRPC port

[logging]
level = "info"   # Default log level (debug, info, warn, error)
log_file = "logs/ml_engine.log" # Relative path to julia/ directory for log file (optional)

[mojo]
enabled = false # Set to true to enable experimental Mojo integration
# Path relative to julia/ directory where the built .so file is expected
lib_path = "../mojo/libmodel_inference.so"

# List of models to load on startup
[[models]]
id = "direction_v1" # Unique name for this model
path = "direction_model_v1.bson" # Filename within the models/ directory
type = "flux_nn" # Type indicator (used by model loader)
features = ["rsi_14", "sma_10_diff", "vol_20d"] # Expected input features
# output_name = "direction_prob_up" # Optional: name for the output prediction

[[models]]
id = "volatility_v1"
path = "volatility_model_v1.bson"
type = "flux_nn"
features = ["price_std_5m", "atr_14"]
# output_name = "volatility_forecast_1h"

# Add more models as needed
# [[models]]
# id = "other_model"
# path = "other.onnx"
# type = "onnx" # Example for ONNX model
# features = [...]


# --- ml-engine-julia-mojo/julia/config/local.toml (Gitignored) ---
# Example local overrides (e.g., for testing specific models)
# [logging]
# level = "debug"

# [[models]]
# id = "test_model"
# path = "test.bson"
# type = "flux_nn"
# features = ["feature1", "feature2"]

```
**Note:** Create the `config` directory and the `default.toml` file. Add `local.toml` for local overrides and ensure it's in `.gitignor