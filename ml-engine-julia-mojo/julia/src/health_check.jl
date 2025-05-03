# MLEngine.jl/src/health_check.jl

using HTTP
using Sockets # For parsing address/port
using Logging # For logging
using Base.Threads # For thread-safe access
using Dates # For timestamps


# Define the health check handler function
function health_check_handler(req::HTTP.Request, dm::DataManager, mm::ModelManager)
    @debug "Health check request received for $(req.target)."

    if req.target == "/healthz"
        # Perform basic health checks
        is_healthy = true
        status_messages = String[]

        # Check DataManager health (e.g., is it receiving recent data?)
        # This check is simplified: just check if it exists and has any data.
        # A better check might look at the timestamp of the latest data point.
        dm_healthy = lock(dm.map_lock) do
            !isempty(dm.symbol_data) # Check if any symbols are being tracked
        end
        if dm_healthy
             push!(status_messages, "DataManager: OK (tracking $(length(dm.symbol_data)) symbols)")
        else
             is_healthy = false
             push!(status_messages, "DataManager: WARNING (no symbols tracked)")
        end

        # Check ModelManager health (e.g., are models loaded?)
        mm_healthy = lock(mm.lock) do
             !isempty(mm.active_models) # Check if any models are active
        end
         if mm_healthy
             push!(status_messages, "ModelManager: OK (loaded $(length(mm.active_models)) models)")
         else
              is_healthy = false
              push!(status_messages, "ModelManager: WARNING (no active models)")
         end

        # Check prediction availability for exposed types (optional, can be slow)
        # For key exposed predictions, check if a recent prediction value exists.
        # This check is too complex for a basic /healthz, better for a /metrics endpoint.

        # Check if training task is running (needs communication with trainer task)
        # This requires a shared atomic flag or channel state - too complex for basic health check.

        if is_healthy
             @debug "Health check status: OK"
             return HTTP.Response(200, "OK\n$(join(status_messages, "\n"))")
        else
             @warn "Health check status: UNHEALTHY"
             return HTTP.Response(503, "Service Unavailable\n$(join(status_messages, "\n"))") # 503 for service unavailable
        end

    else
        # Handle other paths if needed, otherwise return 404
        @debug "Health check received request for unknown target: $(req.target)"
        return HTTP.Response(404, "Not Found")
    end
end

# Function to start the HTTP health check server
# Pass DataManager and ModelManager for checks
function start_health_check_server(listen_address::String, dm::DataManager, mm::ModelManager)
    @info "Starting Health Check server on $(listen_address)..."
    try
        # Parse the address and port
        addr = split(listen_address, ":")
        ip = Sockets.ipaddress(addr[1])
        port = parse(Int, addr[2])

        # Create a handler that captures dm and mm
        handler = req -> health_check_handler(req, dm, mm)

        # Start the server
        # This is a blocking call unless run in an async task.
        # The task will run until interrupted (e.g., process shutdown).
        HTTP.serve(handler, ip, port)

        @info "Health Check server stopped."

    catch e
        @error "Error starting Health Check server on $(listen_address): $(e)"
        # The task will likely terminate due to this error
    end
end