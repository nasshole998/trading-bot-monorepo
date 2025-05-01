# ml-engine-julia-mojo/Dockerfile

# --- Builder Stage ---
# Use an official Julia base image. Pin the version for reproducibility.
# Choose appropriate base (e.g., ubuntu) if needing specific system libs.
# Use NVIDIA CUDA base if GPU support is required.
ARG JULIA_VERSION=1.9.4 # Specify desired Julia version
ARG BASE_IMAGE=julia:${JULIA_VERSION}-bullseye
ARG CUDA_VERSION=12.1.1 # Match CUDA version if using GPU
ARG CUDNN_VERSION=8
ARG USE_GPU=false

FROM ${BASE_IMAGE} as base
FROM nvidia/cuda:${CUDA_VERSION}-cudnn${CUDNN_VERSION}-devel-ubuntu22.04 as gpu_base

FROM ${gpu_base} as builder_gpu_base
FROM base as builder_cpu_base

# Select base based on USE_GPU
FROM builder_${USE_GPU}_base as builder

ARG DEBIAN_FRONTEND=noninteractive
ENV JULIA_DEPOT_PATH="/opt/julia_depot" \
    JULIA_PROJECT="/app"

# Install build dependencies (e.g., for specific Julia packages, protoc)
# protoc might be needed if build.jl runs protoc binary directly
RUN apt-get update && apt-get install -y --no-install-recommends \
    git \
    # protobuf-compiler # Uncomment if build.jl needs it explicitly
    # Add python3 and build tools if building Mojo shared library here
    python3 python3-pip build-essential \
    # Clean up
    && apt-get clean && rm -rf /var/lib/apt/lists/*

# Create app directory and Julia depot path
WORKDIR /app
RUN mkdir -p ${JULIA_DEPOT_PATH}

# Copy Julia project files
COPY ./julia/Project.toml ./julia/Manifest.toml* ./
# If Manifest.toml doesn't exist yet, create dummy to avoid error
RUN touch Manifest.toml

# Pre-install Julia dependencies to leverage Docker layer caching
# Instantiate installs packages, resolve ensures Manifest is up-to-date
RUN julia --project=. -e 'using Pkg; Pkg.instantiate(); Pkg.resolve(); Pkg.precompile()'

# Copy the rest of the Julia source code and build script
COPY ./julia/src ./src
COPY ./julia/build.jl ./build.jl
COPY ./julia/config ./config
# Copy proto definitions needed for build.jl
COPY ./proto /proto

# Run the Julia build script to generate gRPC code
# Ensure necessary proto files (including google/protobuf) are accessible
RUN julia --project=. build.jl

# --- Mojo Build (Optional - Placeholder) ---
# Copy Mojo source and build script
COPY ./mojo /app/mojo
WORKDIR /app/mojo
# Placeholder: Install Mojo SDK (this needs a real installation method)
# RUN curl ... | sh install_mojo.sh
# Placeholder: Build the Mojo shared library
# RUN bash build.sh
WORKDIR /app # Return to main app directory

# --- Runtime Stage ---
# Use a minimal Julia base image (or NVIDIA base if using GPU)
ARG JULIA_VERSION
ARG BASE_IMAGE_RUNTIME=julia:${JULIA_VERSION}-bullseye-slim
ARG CUDA_VERSION_RUNTIME=12.1.1
ARG USE_GPU

FROM ${BASE_IMAGE_RUNTIME} as runtime_base
FROM nvidia/cuda:${CUDA_VERSION_RUNTIME}-base-ubuntu22.04 as gpu_runtime_base

FROM ${gpu_runtime_base} as runtime_gpu_base
FROM runtime_base as runtime_cpu_base

FROM runtime_${USE_GPU}_base as runtime

ARG DEBIAN_FRONTEND=noninteractive
ENV JULIA_DEPOT_PATH="/opt/julia_depot" \
    JULIA_PROJECT="/app" \
    JULIA_NUM_THREADS=auto # Use multiple threads based on CPU cores

# Install runtime dependencies (e.g., libprotobuf, potentially Python for Mojo runtime)
RUN apt-get update && apt-get install -y --no-install-recommends \
    libprotobuf32 \
    # python3 # If Mojo runtime needs it
    # Clean up
    && apt-get clean && rm -rf /var/lib/apt/lists/*

WORKDIR /app

# Copy necessary files from the builder stage
COPY --from=builder ${JULIA_DEPOT_PATH} ${JULIA_DEPOT_PATH}
COPY --from=builder /app/Project.toml /app/Manifest.toml ./
COPY --from=builder /app/src ./src
COPY --from=builder /app/config ./config
# Copy trained models (can also be mounted as a volume)
COPY ./models /app/models
# Copy compiled Mojo shared library (if built)
# COPY --from=builder /app/mojo/libmodel_inference.so /app/mojo/

# Expose the gRPC server port (must match config)
EXPOSE 50053

# Set the entrypoint to run the Julia application
# Use --project to specify the environment
# Add --threads=auto (or specific number) for multi-threading
# Add command line args for config if needed
CMD ["julia", "--project=.", "--threads=$JULIA_NUM_THREADS", "src/MLEngine.jl", "--port=50053"]

