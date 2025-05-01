# ml-engine-julia-mojo/julia/src/MLEngine.jl
module MLEngine

using Logging
using LoggingExtras
using Configurations
using Sockets
using ArgParse # For command line argument parsing

# Include sub-modules/files
include("utils.jl")
include("config.jl")
include("features.jl")
include("models.jl")
include("inference.jl")
include("grpc_server.jl")
# include("training.jl") # Training logic might be run separately

# Global logger configuration (can be overridden by config)
global_logger(Utils.create_console_logger(Logging.Info))

function julia_main()::Cint
    # --- Argument Parsing ---
    arg_table = ArgParseSettings(description="ML Engine Service")
    @add_arg_table! arg_table begin
        "--config", "-c"
            help = "Path to configuration TOML file"
            arg_type = String
            default = joinpath(@__DIR__, "..", "config", "default.toml") # Default relative path
        "--port", "-p"
            help = "gRPC server port override"
            arg_type = Int
            default = -1 # Indicates no override
        "--loglevel"
            help = "Logging level override (debug, info, warn, error)"
            arg_type = String
            default = "" # Indicates no override
        "--threads" # Informational, Julia gets threads from env/flag
             help = "Number of Julia threads to use (set via JULIA_NUM_THREADS or --threads flag)"
             arg_type = String
             default = string(Threads.nthreads())
    end
    parsed_args = parse_args(ARGS, arg_table)

    # --- Configuration Loading ---
    config_path = parsed_args["config"]
    @info "Loading configuration from: $(config_path)"
    try
        global config = load_config(config_path) # Load into global 'config' variable within module

        # Override config with command line args if provided
        if parsed_args["port"] > 0
            config.server.port = parsed_args["port"]
        end
        if !isempty(parsed_args["loglevel"])
            config.logging.level = parsed_args["loglevel"]
        end

        # --- Setup Logging (based on final config) ---
        log_level = Utils.parse_loglevel(config.logging.level)
        if !isempty(config.logging.log_file)
            log_file_path = joinpath(@__DIR__, "..", config.logging.log_file) # Relative path
            global_logger(Utils.create_file_logger(log_level, log_file_path))
            @info "Logging to file: $(log_file_path)"
        else
             global_logger(Utils.create_console_logger(log_level))
        end
        @info "Logging level set to: $(config.logging.level)"
        @info "Julia threads available: $(Threads.nthreads())"


        # --- Initialize Components ---
        @info "Initializing ML models..."
        # Load models specified in config
        loaded_models = Models.load_all_models(config.models)
        if isempty(loaded_models)
            @warn "No models were loaded. Prediction service will not be functional."
        else
            @info "Loaded models: $(keys(loaded_models))"
        end

        # Initialize Inference Context (passes models, potentially Mojo lib)
        inference_ctx = Inference.InferenceContext(loaded_models, config.mojo)
        @info "Inference context initialized."
        if config.mojo.enabled
             @info "Mojo integration enabled (Path: $(config.mojo.lib_path))"
             # Perform check if Mojo library loaded correctly in InferenceContext constructor
             if !Inference.is_mojo_loaded(inference_ctx)
                 @error "Mojo library configured but failed to load. Check path and dependencies."
                 # Decide whether to continue without Mojo or exit
             end
        end


        # --- Start gRPC Server ---
        server_config = config.server
        @info "Starting gRPC server on port $(server_config.port)..."
        grpc_task = GrpcServer.run_server(inference_ctx, server_config.port)

        # Keep main task running, wait for server task or interrupt
        @info "ML Engine service running. Press Ctrl+C to stop."
        try
            wait(grpc_task) # Wait for the server task to finish (e.g., on error)
        catch e
            if e isa InterruptException
                @warn "Interrupt received. Shutting down server..."
                # TODO: Implement graceful shutdown for gRPC server if possible
                # gRPC.jl might require manual handling of shutdown signals
            else
                @error "gRPC server task failed:" exception=(e, catch_backtrace())
                rethrow(e)
            end
        finally
            @info "ML Engine service stopped."
            # Cleanup resources if needed
        end

    catch e
        @error "Initialization or runtime error:" exception=(e, catch_backtrace())
        println(stderr, "Error during service execution: $e")
        return 1 # Indicate error
    end

    return 0 # Indicate success
end

# Entry point if script is run directly
if abspath(PROGRAM_FILE) == @__FILE__ || (isdefined(Main, :SYSTEM_IMAGE_LOADED) && Main.SYSTEM_IMAGE_LOADED)
    julia_main()
end

end # module MLEngine
