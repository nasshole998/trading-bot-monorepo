# Stage 1: Build/Setup the Julia environment
# Use a Julia image that includes necessary tools and potentially CUDA drivers/toolkit
# Finding a base image with CUDA drivers AND Julia pre-installed is ideal for GPU usage.
# If not available, you might need to start from an NVIDIA CUDA image and install Julia.
# Let's use a standard Julia image and add CUDA toolkit if needed for the build stage,
# assuming a CUDA runtime image will be used for deployment (Stage 2).
# FROM julia:1.9-bookworm as builder # Using a specific Julia version on Debian bookworm
FROM julia:1.10.0 as builder 

# Install system dependencies (for Protoc, and any libs Julia packages might need)
RUN apt-get update && apt-get install -y --no-install-recommends \
    protobuf-compiler \
    # libyaml-cpp-dev # Only if YAML.jl needs this explicitly
    # Add dependencies for CUDA toolkit if building/training on CPU image
    # (e.g., if Flux.jl needs CUDA libraries for compilation even on CPU)
    # Add dependencies for Mojo compilation if happening here (less likely)
    && rm -rf /var/lib/apt/lists/*

# Install CUDA toolkit if building/training with GPU on a non-CUDA base image
# This is complex and depends on the base image.
# If using an NVIDIA base image, CUDA toolkit might be partially available or needs full install.
# Example (conceptual - depends on base image and CUDA version):
# ENV CUDA_VERSION=12.3.1
# RUN apt-get update && apt-get install -y --no-install-recommends \
#    cuda-toolkit-$CUDA_VERSION \
#    && rm -rf /var/lib/apt/lists/*
# ENV PATH="/usr/local/cuda/bin:${PATH}"
# ENV LD_LIBRARY_PATH="/usr/local/cuda/lib64:${LD_LIBRARY_PATH}"


WORKDIR /app

# Copy Julia project files
COPY ml-engine-julia-mojo/julia/Project.toml ./julia/Project.toml
COPY ml-engine-julia-mojo/julia/Manifest.toml ./julia/Manifest.toml 

# Instantiate Julia dependencies (resolves and downloads packages)
# This is crucial for reproducible builds
RUN julia --project=./julia/ -e 'using Pkg; Pkg.instantiate()'

# Copy source code and config
COPY ml-engine-julia-mojo/julia/src/ ./julia/src/
COPY ml-engine-julia-mojo/config/ ./config/
# Copy models directory (initial models)
COPY ml-engine-julia-mojo/models/ ./models/


# --- Protobuf Compilation (Julia) ---
# Need protoc and the Julia gRPC plugin.
# The Julia gRPC plugin is usually part of the gRPC.jl package and found automatically by protoc
# if `julia --project=./julia/ -e 'using gRPC'` works.

# Copy proto files from the monorepo root
COPY proto/ ./proto/

# Command to compile protos using Julia's gRPC build helper
# This typically uses `protoc` with `--julia_out` and `--plugin` pointing to the Julia plugin.
# The exact command and plugin path depend on the gRPC.jl version and installation.
# Example (conceptual - relies on gRPC.jl setting up the plugin path):
RUN mkdir -p ./julia/src/generated # Create output dir
# Need to figure out the correct protoc command and plugin path in the Docker environment.
# A common way is to run a Julia script that uses `gRPC.generate_proto_code`.
# Example Julia script:
# ```julia
# import gRPC
# gRPC.generate_proto_code(["proto/market_data.proto", "proto/indicator_data.proto", "proto/ml_prediction.proto"], output_dir="./julia/src/generated")
# ```
# RUN julia --project=./julia/ -e 'import gRPC; gRPC.generate_proto_code(["proto/market_data.proto", "proto/indicator_data.proto", "proto/ml_prediction.proto"], output_dir="./julia/src/generated")'
# Placeholder command if the above doesn't work directly:
RUN echo "Placeholder: Run protoc manually or via Julia script using gRPC.generate_proto_code"
# A manual command might look like this, but finding the plugin path is tricky in Docker:
# RUN protoc -I. --plugin=protoc-gen-julia=/path/to/julia/grpc/plugin --julia_out=./julia/src/generated proto/market_data.proto proto/indicator_data.proto proto/ml_prediction.proto


# --- Mojo Compilation (Optional/Placeholder) ---
# Copy Mojo files (if building Mojo code)
COPY ml-engine-julia-mojo/mojo/ ./mojo/
# Run the Mojo build script (placeholder)
# Ensure Mojo SDK is available in the builder image if needed for compilation
# RUN ./mojo/build.sh
# COPY mojo/compiled_artifact ./julia/src/ # Copy compiled Mojo artifact


# Stage 2: Create the final small image (Optional for Julia)
# Use an NVIDIA CUDA runtime image if your application needs GPU drivers/runtime at deployment
# If not using GPU, the builder image might be sufficient, or use a smaller base image.
FROM nvidia/cuda:12.3.1-runtime-ubuntu22.04 as runtime 

# Install Julia on the runtime image (if not using a Julia base image with CUDA)
# Example (depends on Julia distribution method):
# RUN apt-get update && apt-get install -y julia=1.10.0 && rm -rf /var/lib/apt/lists/*

# Copy Julia environment, source code, config, and models from the builder stage
COPY --from=builder /app/julia/ ./julia/
COPY --from=builder /app/config/ ./config/
COPY --from=builder /app/models/ ./models/
COPY --from=builder /app/proto/ ./proto/ 


WORKDIR /app

# Set the entrypoint to run your main Julia script
# Use `--threads=auto` to utilize available CPU cores
ENTRYPOINT ["julia", "--project=./julia/", "--threads=auto", "./julia/src/main.jl"]

# Default command (optional)
# CMD []

# Expose ports (optional, for documentation)
EXPOSE 50052 
EXPOSE 8081 