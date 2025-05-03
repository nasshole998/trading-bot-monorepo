# MLEngine.jl/src/trainer.jl

using Flux # For training ML models
using Flux: @epochs # Example macro for training loops
using BSON # For saving/loading models
using Logging # For logging
using CUDA # For GPU acceleration
using Statistics # For mean, std (preprocessing)
using Random # For shuffling data

# Add preprocessing helpers
function preprocess_training_data(sequence::Vector{Vector{Float64}}, target_idx::Int=1)
    # Take a sequence of feature vectors and prepare it for training.
    # Assume we are predicting the NEXT value of the feature at target_idx (e.g., price).
    # Input features for the model: the sequence up to the second-to-last element.
    # Target label for the model: the feature at target_idx in the last element of the sequence.
    #
    # Example: Sequence is [x_1, x_2, ..., x_T].
    # Input to model: [x_1, ..., x_{T-1}]
    # Target label: x_T[target_idx]
    #
    # This requires sequence_length >= 2.

    seq_len = length(sequence)
    if seq_len < 2
        @warn "Sequence too short for training preprocessing: length $(seq_len)."
        return nothing, nothing # Not enough data
    end

    # Input features: sequence from start to second-to-last
    input_sequence = sequence[1 : (seq_len - 1)]
    input_features_per_step = length(input_sequence[1])
    # Reshape input for LSTM: (features_per_step, sequence_length - 1, batch_size=1)
    input_tensor = reshape(reduce(vcat, input_sequence), input_features_per_step, seq_len - 1, 1)


    # Target label: the value at target_idx in the last element
    target_label = sequence[seq_len][target_idx]

    # Add normalization/scaling here if needed.
    # Example: return input_tensor, [target_label] # Return label as a vector for loss function

    return input_tensor, [target_label] # Return input tensor and target label vector
end


# Function to trigger training for all configured learnable models
# This function is called periodically by the training_trigger_task.
function train_models(dm::DataManager, mm::ModelManager, config::Config)
    if !config.training.enabled
        @info "Training is disabled in configuration. Skipping training cycle."
        return
    end

    @info "Starting a training cycle..."

    # Iterate through all configured model instances
    for model_cfg in mm.model_configs
        model_name = model_cfg.name
        model_type = model_cfg.type
        symbol = model_cfg.symbol

        # Only train learnable models (e.g., FluxLSTM)
        if model_type == "FluxLSTM"
            @info "Training model $(model_name) ($(model_type}) for $(symbol)."

            # --- Data Preparation for Training ---
            # Get recent data for training. Use the size specified in training config.
            # This gets the *latest* N data points, which might not be a single contiguous sequence.
            # We need to generate multiple overlapping sequences from this chunk for training.
            training_data_chunk = get_aligned_data_sequence(dm, symbol, config.training.data_size, now()) # Get a long sequence ending now

            if isnothing(training_data_chunk) || length(training_data_chunk) < model_cfg.required_data_points + 1
                 @warn "Not enough data ($(isnothing(training_data_chunk) ? 0 : length(training_data_chunk)) points) for training $(model_name). Requires at least $(model_cfg.required_data_points + 1) points."
                 continue # Skip training this model if not enough data
            end

            # Generate training pairs (input sequence, target label) from the chunk
            # We need sequences of length `required_data_points` to predict the value at the end of the sequence.
            # So, a chunk of size M yields M - required_data_points training pairs.
            training_pairs = []
            for i in 1:(length(training_data_chunk) - model_cfg.required_data_points)
                 # Get a sequence ending at index i + required_data_points
                 sequence_chunk = training_data_chunk[i : (i + model_cfg.required_data_points)]
                 # Preprocess this sequence chunk to get input tensor and target label
                 input_tensor, target_label = preprocess_training_data(sequence_chunk, 1) # Predict price (feature index 1)

                 if !isnothing(input_tensor) && !isnothing(target_label)
                     push!(training_pairs, (input_tensor, target_label))
                 end
            end

             if isempty(training_pairs)
                 @warn "No valid training pairs generated for model $(model_name) from data chunk of size $(length(training_data_chunk)). Skipping training."
                 continue
             end

            # Shuffle training pairs
            Random.shuffle!(training_pairs)


            # --- Model Definition or Loading ---
            # Get the *current* active model instance. We will fine-tune this instance.
            # If no active model exists (first run), build a new one.
            model = get_model(mm, model_name)

            if isnothing(model) || typeof(model) <: AbstractString # Check if it's a placeholder string
                 @info "No active FluxLSTM model found for $(model_name). Building a new one."
                 # Define a new model architecture
                 input_features = length(training_pairs[1][1]) # Features from the generated tensor
                 sequence_length = size(training_pairs[1][1], 2) # Sequence length from the generated tensor
                 model = get_model_architecture(model_cfg, input_features, sequence_length) # Build architecture based on config

                 if isnothing(model) # get_model_architecture might return nothing for unknown types
                     @error "Could not build model architecture for $(model_name) (type: $(model_type)). Skipping training."
                     continue
                 end
            else
                 @info "Fine-tuning existing model $(model_name)."
                 # Use the existing model for fine-tuning
            end

            # Move model and data to GPU if CUDA is available
            if CUDA.functional()
                 @info "Moving model and data to GPU for training."
                 model = model |> gpu
                 training_pairs_gpu = [(x |> gpu, y |> gpu) for (x, y) in training_pairs]
                 data_for_training = training_pairs_gpu # Use GPU data
            else
                @info "CUDA not functional. Training on CPU."
                 data_for_training = training_pairs # Use CPU data
            end


            # --- Training Loop ---
            # Define loss function and optimizer
            loss(x, y) = Flux.mse(model(x), y) # Mean Squared Error for regression
            optimizer = ADAM(config.training.learning_rate) # Adam optimizer

            @info "Starting Flux.train! for $(model_name) for $(config.training.epochs) epochs."
            Flux.train!(loss, Flux.params(model), data_for_training, optimizer, epochs=config.training.epochs)
            @info "Flux.train! finished for $(model_name)."


            # --- Update Active Model ---
            # Update the active model in the ModelManager with the newly trained one
            update_active_model!(mm, model_name, model)

            # --- Save Model ---
            save_model(mm, model_name, model) # Save the updated model to disk


        elseif model_type == "RuleBased"
             @debug "RuleBased model $(model_name) does not require training."
             continue # Skip training for rule-based models

        elseif model_type == "MojoModel"
             @warn "MojoModel training $(model_name) is a conceptual placeholder. Training must be handled externally."
             # This might trigger an external training process or copy trained artifacts
             continue # Skip Julia-based training for Mojo models

        else
            @warn "Unknown model type encountered in trainer: $(model_type) for model $(model_name). Skipping training."
            continue
        end
    end

    @info "Training cycle finished for all configured models."
end

# Helper function to define model architecture (delegates to model_definition.jl)
# This was moved to model_definition.jl and is called there.

# Helper function to save trained model (delegates to model_manager.jl)
# This was moved to model_manager.jl and is called there.