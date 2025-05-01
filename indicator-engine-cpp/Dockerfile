# indicator-engine-cpp/Dockerfile

# --- Builder Stage ---
# Use a base image with C++ compiler, CMake, and essential build tools.
# Using a specific Ubuntu/Debian version is recommended for stability.
# Add NVIDIA CUDA base image if GPU support is enabled.
ARG BASE_IMAGE=ubuntu:22.04
ARG CUDA_VERSION=12.1.1 # Example CUDA version, adjust as needed
ARG CUDNN_VERSION=8 # Example cuDNN version
ARG USE_GPU=false # Build arg to control GPU inclusion

# Select base image based on USE_GPU argument
FROM ${BASE_IMAGE} as base
FROM nvidia/cuda:${CUDA_VERSION}-cudnn${CUDNN_VERSION}-devel-ubuntu22.04 as gpu_base

# Use a conditional stage based on the build argument
FROM ${gpu_base} as builder_gpu_base
FROM base as builder_cpu_base

# Final builder stage selection
FROM builder_${USE_GPU}_base as builder

ARG DEBIAN_FRONTEND=noninteractive

# Install build dependencies
RUN apt-get update && apt-get install -y --no-install-recommends \
    build-essential \
    cmake \
    ninja-build \
    git \
    pkg-config \
    # Protobuf / gRPC dependencies
    libprotobuf-dev protobuf-compiler libgrpc++-dev libgrpc-dev \
    # Eigen
    libeigen3-dev \
    # TA-Lib (Install from source or package manager if available)
    # Example: Installing TA-Lib from source (common method)
    wget \
    autoconf \
    automake \
    libtool \
    && wget http://prdownloads.sourceforge.net/ta-lib/ta-lib-0.4.0-src.tar.gz \
    && tar -xzf ta-lib-0.4.0-src.tar.gz \
    && cd ta-lib/ \
    && ./configure --prefix=/usr/local \
    && make \
    && make install \
    && cd .. \
    && rm -rf ta-lib ta-lib-0.4.0-src.tar.gz \
    # Spdlog dependency (if not using FetchContent or header-only)
    # libspdlog-dev
    # OpenSSL (often needed by gRPC)
    libssl-dev \
    # Clean up
    && apt-get clean && rm -rf /var/lib/apt/lists/*

WORKDIR /build

# Copy source code
COPY . .

# Configure and Build
# Pass CMake options based on build args
ARG USE_GPU
ARG USE_TA_LIB=true
ARG USE_OMP=true
RUN cmake -S . -B build \
    -G Ninja \
    -DCMAKE_BUILD_TYPE=Release \
    -DCMAKE_INSTALL_PREFIX=/usr/local/indicator_engine \
    -DINDICATOR_ENGINE_ENABLE_GPU=${USE_GPU} \
    -DINDICATOR_ENGINE_USE_TA_LIB=${USE_TA_LIB} \
    -DINDICATOR_ENGINE_ENABLE_OMP=${USE_OMP}
RUN cmake --build build --config Release --parallel $(nproc)
# Optional: Install step if needed for cleaner runtime stage
# RUN cmake --install build --config Release


# --- Runtime Stage ---
# Use a minimal base image. If using GPU, needs NVIDIA runtime base.
ARG BASE_IMAGE_RUNTIME=debian:bullseye-slim
ARG CUDA_VERSION_RUNTIME=12.1.1 # Match major version used for build
ARG USE_GPU

FROM ${BASE_IMAGE_RUNTIME} as runtime_base
FROM nvidia/cuda:${CUDA_VERSION_RUNTIME}-base-ubuntu22.04 as gpu_runtime_base

FROM ${gpu_runtime_base} as runtime_gpu_base
FROM runtime_base as runtime_cpu_base

FROM runtime_${USE_GPU}_base as runtime

ARG DEBIAN_FRONTEND=noninteractive

# Install runtime dependencies
# Must include libraries linked by the application
RUN apt-get update && apt-get install -y --no-install-recommends \
    libprotobuf32 \
    libgrpc++1 \
    libssl1.1 \
    # TA-Lib runtime library (needs to be installed same way as in builder)
    # If installed from source to /usr/local, copy from builder or reinstall
    # libta-lib0 # If available as a package
    # OpenMP runtime
    libgomp1 \
    # Clean up
    && apt-get clean && rm -rf /var/lib/apt/lists/*

# Copy TA-Lib runtime library from builder if installed from source
COPY --from=builder /usr/local/lib/libta_lib* /usr/local/lib/
# Update linker cache
RUN ldconfig

WORKDIR /app

# Copy the compiled binary from the builder stage
COPY --from=builder /build/build/indicator_engine_server .

# Copy configuration files (optional, could be mounted)
# COPY config ./config

# Expose the gRPC server port (must match config/command line arg)
EXPOSE 50051

# Set environment variables (e.g., for logging, OMP threads)
# ENV SPDLOG_LEVEL=info
# ENV OMP_NUM_THREADS=4 # Example: Set number of OpenMP threads

# Command to run the application
# Add command line arguments for config if needed
CMD ["./indicator_engine_server", "--port=50051"]