# data-ingestion-rust/Dockerfile

# --- Builder Stage ---
# Use the official Rust image as a builder
# Choose a specific version tag for reproducibility, e.g., 1.77
# Use slim-bullseye for smaller base image size
FROM rust:1.77-slim-bullseye as builder

# Set working directory
WORKDIR /usr/src/app

# Install build dependencies needed by some crates (e.g., openssl-sys, grpcio-sys)
# Update package lists and install dependencies
RUN apt-get update && apt-get install -y --no-install-recommends \
    pkg-config \
    libssl-dev \
    # Add build-essential or clang if needed for specific C dependencies
    # build-essential
    # Add protobuf compiler if not using pure Rust prost/tonic
    # protobuf-compiler
    # Clean up apt cache
    && rm -rf /var/lib/apt/lists/*

# Copy Cargo manifests and lock file
COPY Cargo.toml Cargo.lock ./
# Copy build script if it exists
COPY build.rs ./build.rs
# Copy proto files needed by build.rs (adjust path as needed)
# Create the directory first to avoid permission issues if it doesn't exist
RUN mkdir -p ../../proto
COPY ../../proto/market_data.proto ../../proto/
COPY ../../proto/orders.proto ../../proto/

# Build dependencies separately to leverage Docker cache
# Create a dummy src/main.rs to allow building dependencies without full source code
RUN mkdir src && echo "fn main() {}" > src/main.rs
# Build only the dependencies
RUN cargo build --release --locked

# Remove dummy main.rs
RUN rm -f src/main.rs

# Copy the actual source code
COPY src ./src

# Build the application in release mode
# Ensure build script runs again if source changed
RUN touch src/main.rs && cargo build --release --locked --verbose


# --- Runtime Stage ---
# Use a minimal base image for the final container
# Debian slim is a good balance between size and having common utilities/libs
FROM debian:bullseye-slim

# Install runtime dependencies (e.g., OpenSSL libraries)
RUN apt-get update && apt-get install -y --no-install-recommends \
    libssl1.1 \
    ca-certificates \
    # Clean up apt cache
    && rm -rf /var/lib/apt/lists/*

# Set working directory
WORKDIR /app

# Copy the compiled binary from the builder stage
COPY --from=builder /usr/src/app/target/release/data-ingestion-rust .

# Copy configuration files (optional, could be mounted as volume)
COPY config ./config

# Set environment variables (optional defaults, can be overridden)
ENV RUN_MODE="production"
ENV CONFIG_DIR="/app/config"
# Set log level via RUST_LOG if not using config file setting primarily
# ENV RUST_LOG="info"

# Expose the gRPC server port defined in the config
# This needs to match `grpc.order_receiver_bind_address` port
EXPOSE 50052

# Command to run the application
CMD ["./data-ingestion-rust"]

