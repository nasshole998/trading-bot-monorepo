# MLEngine.jl/src/model_definition.jl

using Flux # For defining ML models
using NNlib # For activation functions, etc.

# Define the structure for a simple LSTM model for time series forecasting
# This model takes a sequence of feature vectors and outputs a single value (e.g., next price).
# The input sequence will have dimensions (features_per_step, sequence_length, batch_size)
# The output will be (output_features, batch_size), e.g., (1, batch_size) for single price prediction.

function build_lstm_model(input_features::Int, sequence_length::Int, hidden_size::Int, output_features::Int)
    @info "Building LSTM model: input_features=$(input_features), sequence_length=$(sequence_length), hidden_size=$(hidden_size), output_features=$(output_features)"

    # Define the model architecture using Flux.jl
    # Chain layers together.
    # LSTM takes input size and hidden size.
    # flatten converts output to a vector.
    # Dense layer maps hidden state to output features.

    model = Chain(
        LSTM(input_features => hidden_size), # LSTM layer
        x -> x[end], # Get the output from the last time step of the sequence
        Dense(hidden_size, output_features) # Dense layer to map hidden state to output
    )

    # Initialize model parameters (Flux does this by default)
    # You might want custom initialization for better training stability

    @info "LSTM model architecture created."

    return model
end

# Add other model definition functions if you support different model types
# function build_rule_based_model(...) ... end
# function build_cnn_model(...) ... end


# Helper function to get the model architecture based on config type
# This is used by the trainer to define the model before training
function get_model_architecture(model_cfg::ModelInstanceConfig, input_features::Int, sequence_length::Int)
     model_type = model_cfg.type
     model_name = model_cfg.name

     # Add parameters specific to each model type
     # Example: hidden size for LSTM
     lstm_hidden_size = get(model_cfg, "lstm_hidden_size", 64) # Default hidden size
     output_features = get(model_cfg, "output_features", 1) # Default 1 output (e.g., price)


     if model_type == "FluxLSTM"
         return build_lstm_model(input_features, sequence_length, lstm_hidden_size, output_features)
     elseif model_type == "RuleBased"
         @warn "RuleBased model type does not have a learnable architecture."
         # Return a dummy marker or nothing, trainer needs to handle this type differently
         return nothing
     elseif model_type == "MojoModel"
         @warn "MojoModel type architecture is defined in Mojo code."
         # Return a dummy marker or nothing
         return nothing
     else
         @error "Unknown model type for architecture definition: $(model_type) for model $(model_name)."
         return nothing
     end
end