# MLEngine.jl/src/model_manager.jl

using Flux # Example ML library
using BSON # For saving/loading models
using Logging # For logging
using CUDA # For GPU acceleration

# Define a structure to hold trained models and the currently active ones
mutable struct ModelManager
    # Store models, ideally the currently active ones ready for prediction.
    # Use a dictionary where key is the model name (from config)
    active_models::Dict{String, Any} # Model Name -> loaded_model_object
    lock::ReentrantLock # Lock for thread-safe access to active_models

    # Configuration path
    model_artifacts_path::String

    # Store the configuration of the models we should be managing/loading
    model_configs::Vector{ModelInstanceConfig}
     prediction_configs::Vector{PredictionConfig} # Also store prediction configs to map predictions to models
end

# Constructor
function ModelManager()
    ModelManager(
        Dict{String, Any}(),
        ReentrantLock(),
        "models", # Default artifact path
        [], # Default empty model configs
        [] # Default empty prediction configs
    )
end

# Helper function to configure an existing ModelManager instance (used by main/MLEngine)
function configure!(mm::ModelManager, artifacts_path::String)
     mm.model_artifacts_path = artifacts_path
     @info "ModelManager configured with artifacts_path=$(artifacts_path)."
     # Ensure artifacts path exists
     if !isdir(artifacts_path)
         @info "Creating model artifacts directory: $(artifacts_path)"
         mkpath(artifacts_path) # Create directory and parent directories
     end
end


# Function to load models based on configuration
# This loads the *initial* models when the service starts.
function load_models!(mm::ModelManager, model_configs::Vector{ModelInstanceConfig})
    mm.model_configs = model_configs # Store configs

    lock(mm.lock) do
        empty!(mm.active_models) # Clear any previously loaded models

        for model_cfg in model_configs
            model_name = model_cfg.name
            model_type = model_cfg.type
            symbol = model_cfg.symbol
            model_file = joinpath(mm.model_artifacts_path, model_cfg.model_file) # Use file name from config or default

            if model_type == "RuleBased"
                 @info "Registering RuleBased model placeholder: $(model_name) for $(symbol)."
                 # Rule-based models don't need loading, maybe just register a marker
                 mm.active_models[model_name] = "RuleBasedModel_$(model_name)" # Placeholder marker
                 continue
            end
             if model_type == "MojoModel"
                 @info "Registering MojoModel model placeholder: $(model_name) for $(symbol)."
                 # Mojo models are external, just register a marker indicating it should be used
                 mm.active_models[model_name] = "MojoModel_$(model_name)" # Placeholder marker
                 continue
             end


            # For learnable models (like FluxLSTM), try loading from file
            if !isfile(model_file)
                @warn "Model file not found for $(model_name) ($(model_type}) for $(symbol): $(model_file). Will attempt to train a new one."
                # Do not add to active_models yet, it will be added after initial training
                continue
            end

            try
                # Load the model using BSON.jl
                # Assumes the model was saved using BSON.bson(filepath, model=model_object)
                loaded_data = BSON.load(model_file)
                if haskey(loaded_data, :model)
                    loaded_model = loaded_data[:model]

                    # Move model to GPU if CUDA is available and enabled
                    if CUDA.functional() # Check if CUDA is available
                         @info "Moving model $(model_name) to GPU."
                         loaded_model = loaded_model |> gpu # Move to GPU
                    else
                        @info "CUDA not functional. Model $(model_name) remains on CPU."
                         loaded_model = loaded_model |> cpu # Ensure on CPU if not on GPU
                    end

                    mm.active_models[model_name] = loaded_model
                    @info "Loaded model: $(model_name) ($(model_type}) for $(symbol) from $(model_file)."
                else
                     @error "BSON file $(model_file) does not contain ':model' key."
                end

            catch e
                @error "Error loading model file $(model_file): $(e)"
            end
        end
    end # Lock released
    @info "Model loading complete. $(length(mm.active_models)) models are active."
end

# Function to update an active model after retraining
function update_active_model!(mm::ModelManager, model_name::String, new_model)
    lock(mm.lock) do
        # Move new model to GPU if CUDA is available
        if CUDA.functional()
             new_model = new_model |> gpu
        else
            new_model = new_model |> cpu
        end
        mm.active_models[model_name] = new_model
        @info "Updated active model: $(model_name)."
    end # Lock released
end

# Function to get a specific loaded model by name
function get_model(mm::ModelManager, model_name::String)
    lock(mm.lock) do
        get(mm.active_models, model_name, nothing)
    end
end

# Helper to find the relevant model config for a given prediction type and symbol
function find_model_config_for_prediction(mm::ModelManager, symbol::String, prediction_type::String)::Union{ModelInstanceConfig, Nothing}
    # This mapping can be complex. A simple approach:
    # Find the first model instance config that matches the symbol.
    # Or, assume model name is derived from prediction type and symbol.
    # For this implementation, let's assume prediction configs implicitly map to model configs
    # based on symbol and type, and we need to find the *model config* to get its parameters (like required_data_points).

    # A better approach: the prediction config could explicitly reference the model name.
    # Let's update PredictionConfig to optionally include a model_name field.
    # If model_name is specified in PredictionConfig, use that.
    # If not, try to find a model_instance config that matches the symbol and type somehow.

    # Simpler approach for now: Iterate model configs and see if any match the symbol
    # and are of a type that *could* produce this prediction type.
    # This requires knowing which model *types* produce which prediction types - complex.
    # Let's go back to the simpler mapping: Find the *first* model config that matches the symbol.
    # This assumes one model per symbol, or the first matching one is the right one.

    # Refined simple mapping: Find a model instance config by name, where name is derived from symbol/type.
    # Or, iterate through model configs and see which one's purpose aligns with the prediction type.
    # The configuration structure allows linking predictions to models, but the logic needs implementing.

    # Let's add an optional `model_name` field to `PredictionConfig` and use that mapping.
    # (Updated `config.jl` structure assumes this now)
    # First, find the PredictionConfig for this symbol/type combination to see which model it points to
    pred_cfg = nothing
    for cfg in mm.prediction_configs # Use the stored prediction configs
        if cfg.symbol == symbol && cfg.type == prediction_type
             pred_cfg = cfg
             break
        end
    end

    if isnothing(pred_cfg)
        @error "No PredictionConfig found for symbol $(symbol) and type $(prediction_type)."
        return nothing
    end

    # Now find the corresponding ModelInstanceConfig by name
    # The PredictionConfig should ideally specify the model_name it relies on.
    # If PredictionConfig has `model_name` field:
    # target_model_name = pred_cfg.model_name
    # If not, try to infer (less reliable) or assume 1:1 symbol mapping.
    # Let's use the assumption that the PredictionConfig is linked implicitly via symbol
    # to a ModelInstanceConfig that also matches the symbol, and the prediction type
    # is something that model can produce. Find the first model instance config matching the symbol.
    # A more robust system requires explicit config linking.

    # Find the model instance config that matches the symbol
    model_cfg = nothing
    for cfg in mm.model_configs
        if cfg.symbol == symbol
            model_cfg = cfg # Take the first matching one
            break
        end
    end

    if isnothing(model_cfg)
         @error "No ModelInstanceConfig found for symbol $(symbol)."
         return nothing
    end

    # We found a model config for the symbol. Now check if this model type
    # is capable of producing the requested prediction type. This check is complex.
    # For simplicity, we'll *assume* any model config found for the symbol can produce
    # the requested prediction type. A real system needs a mapping.

    return model_cfg # Return the found model instance config
end

# Function to save a trained model
function save_model(mm::ModelManager, model_name::String, model)
    model_cfg = nothing
    for cfg in mm.model_configs
        if cfg.name == model_name
            model_cfg = cfg
            break
        end
    end
    if isnothing(model_cfg)
        @error "Cannot save model $(model_name): config not found."
        return false
    end

    filepath = joinpath(mm.model_artifacts_path, model_cfg.model_file)

    try
        # Move model to CPU before saving if it's on GPU (BSON might not handle GPU objects)
        model_cpu = model |> cpu

        BSON.bson(filepath, model=model_cpu)
        @info "Model $(model_name) saved to $(filepath)."
        return true
    catch e
        @error "Error saving model $(model_name) to $(filepath): $(e)"
        return false
    end
end

# Add other ModelManager methods as needed (e.g., update models from trainer)