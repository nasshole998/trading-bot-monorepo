import MLEngine # Import the module

using Logging # Standard Julia logging
using SignalInterrupt # For handling termination signals
using ArgParse # For parsing command-line arguments

# Configure logging (optional, can use default or setup specific sinks)
# global_logger(ConsoleLogger(stdout, Logging.Info)) # Example

# --- Main entry point function ---
function main()
    # --- Parse Command Line Arguments ---
    s = ArgParseSettings()
    @add_arg_table! s begin
        "--config", "-c"
            help = "Path to the configuration file (YAML)"
            arg_type = String
            default = "config/settings.yaml" # Default path
    end
    parsed_args = parse_args(ARGS, s)
    config_path = parsed_args["config"]

    @info "ML Engine Service Starting..."

    # --- Load Configuration ---
    config = MLEngine.load_config(config_path)

    if isnothing(config)
        @error "Failed to load configuration from $(config_path). Exiting."
        exit(1)
    end

    @info "Configuration loaded successfully from $(config_path)."
    @info "  gRPC listen address: $(config.grpc.listen_address)"
    @info "  Health check listen address: $(config.health_check.listen_address)"
    @info "  Max data history: $(config.data.max_history_size)"
    @info "  Model artifacts path: $(config.model.artifacts_path)"
    @info "  Loaded model configs: $(length(config.model.instances))"
    @info "  Exposed prediction configs: $(length(config.predictions.expose))"
    @info "  Training interval: $(config.training.interval_sec)s (initial delay: $(config.training.initial_delay_sec)s, data size: $(config.training.data_size))"
     @info "  Indicator Engine gRPC address: $(config.indicator_engine.grpc_address)"


    # --- Start Services ---
    services = MLEngine.start_services(config)

    # --- Handle Signals for Graceful Shutdown ---
    # Julia's default signal handling can interrupt tasks.
    # We'll rely on SIGINT/SIGTERM propagating and use a manual channel
    # to orchestrate shutdown if needed for other signals or explicit control.
    # For SIGINT/SIGTERM, gRPC.jl and HTTP.jl servers often get interrupted.
    # A simple `wait` loop on the main server task is often sufficient,
    # coupled with `stop_services` called by a signal handler.

    # Create a channel to wait for termination signals
    termination_signal_ch = Channel{Int}(1)

    # Register signal handlers (e.g., SIGINT, SIGTERM)
    # SignalInterrupt.handle_interrupt registers handlers for common signals.
    # When a signal is received, the registered handler function is called.
    # We will have the handler set a flag or put a value in a channel.

    # Define a handler that just puts a value into the channel
    function handle_signal(sig)
        @info "Received signal $sig."
        put!(termination_signal_ch, sig) # Send signal to the channel
    end

    # Register the handler for SIGINT (Ctrl+C) and SIGTERM (kill command)
    SignalInterrupt.signal_handler_install(C_NULL) # Ensure default handlers are not interfering?
    SignalInterrupt.signal_handler_register(SignalInterrupt.SIGINT, handle_signal)
    SignalInterrupt.signal_handler_register(SignalInterrupt.SIGTERM, handle_signal)

    @info "Service running. Waiting for shutdown signal..."

    # Wait for a termination signal from the handler
    sig = take!(termination_signal_ch)
    @info "Termination signal received ($sig). Initiating graceful shutdown..."

    # --- Initiate Graceful Shutdown ---
    MLEngine.stop_services(services) # Call the stop function from the module

    # Wait for the main gRPC server task to finish after being stopped
    # This blocks until the server is fully shut down.
    @info "Waiting for main gRPC server task to finish..."
    try
        wait(services.grpc_server_task)
    catch e
        # Catch TaskFailedException if the task failed for other reasons before stop
        @error "Main gRPC server task finished with error: $(e)"
    end

    @info "ML Engine Service shut down complete."
    exit(0) # Exit successfully
end

# Execute the main function if this script is run directly
if abspath(PROGRAM_FILE) == @__FILE__
    main()
end