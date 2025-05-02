# Stage 1: Build the application
FROM nvidia/cuda:12.3.1-devel-ubuntu22.04 as builder

# Install common build tools and dependencies
RUN apt-get update && apt-get install -y --no-install-recommends \
    build-essential \
    cmake \
    ninja-build \
    pkg-config \
    libssl-dev \
    zlib1g-dev \
    protobuf-compiler \
    libprotobuf-dev \
    libgrpc++-dev \
    libleveldb-dev \
    libhdf5-dev \
    libeigen3-dev \
    libyaml-cpp-dev \
    libasio-dev \
    libspdlog-dev \
    # libtalib-dev # Uncomment if using TA-Lib from package manager
    && rm -rf /var/lib/apt/lists/*

WORKDIR /app

# Copy Protobuf files - needed by CMake to generate C++ headers
COPY proto/ ./proto/

# Copy CMake files and source code
COPY indicator-engine-cpp/CMakeLists.txt ./CMakeLists.txt
COPY indicator-engine-cpp/include/ ./include/
COPY indicator-engine-cpp/src/ ./src/
COPY indicator-engine-cpp/tests/ ./tests/

# Configure and build with CMake
RUN cmake -B build \
    -DCMAKE_BUILD_TYPE=Release \
    -DCMAKE_INSTALL_PREFIX=/usr/local \
    -DCMAKE_TOOLCHAIN_FILE=${CUDA_TOOLKIT_ROOT_DIR}/share/cmake/CudaSeparateCompilationTargets.cmake \
    -G Ninja
# Note: Adjust CMAKE_TOOLCHAIN_FILE if needed based on CUDA version/installation
# Note: You might need to pass specific flags for gRPC finding depending on installation

RUN cmake --build build --target install

# --- Add Optional Test Build ---
# If you want to build and run tests in the container:
# RUN cmake --build build --target indicator_tests
# RUN ./build/tests/indicator_tests # Example run


# Stage 2: Create the final small image
# Use an NVIDIA runtime image if your application needs GPU drivers/runtime at deployment
FROM nvidia/cuda:12.3.1-runtime-ubuntu22.04 as runtime

# Install necessary runtime dependencies
# These might be included in the runtime image, but explicitly install if not.
# Check dependencies of your compiled binary using `ldd`
RUN apt-get update && apt-get install -y --no-install-recommends \
    libprotobuf23 \
    libgrpc++1 \
    libssl3 \
    zlib1g \
    libyaml-cpp0.7 \
    libasio-dev \
    libspdlog-dev \
    # libtalib0 # Uncomment if TA-Lib runtime is needed
    && rm -rf /var/lib/apt/lists/*

# Copy the compiled binary from the builder stage
# The binary is installed to /usr/local/bin by the install target
COPY --from=builder /usr/local/bin/indicator_engine /usr/local/bin/indicator_engine

# Copy generated headers needed at runtime if your application loads modules dynamically
# (Unlikely for this service unless using plugins) - skip for now.

# Copy configuration file (Example - better to mount or use env vars)
# RUN mkdir /app/config
# COPY indicator-engine-cpp/config/settings.yaml /app/config/settings.yaml
WORKDIR /app

# Set the entrypoint to your binary
ENTRYPOINT ["/usr/local/bin/indicator_engine"]

# Command to run (uses config file or env vars)
CMD []