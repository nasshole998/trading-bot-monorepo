# Stage 1: Build the application
FROM rust:1.77-bookworm as builder

WORKDIR /app

# Install necessary build dependencies for crypto libraries etc.
RUN apt-get update && apt-get install -y --no-install-recommends \
    pkg-config \
    libssl-dev \
    zlib1g-dev \
    && rm -rf /var/lib/apt/lists/*


# Copy Cargo.toml and build.rs first to leverage Docker caching
# Assume build.rs is needed for gRPC proto compilation
COPY data-ingestion-rust/Cargo.toml data-ingestion-rust/Cargo.lock ./
COPY data-ingestion-rust/build.rs ./build.rs
# Copy proto files for build.rs - assuming they are at the monorepo root
COPY proto/ ./proto/

# Dummy src dir structure for build.rs dependencies if needed by build script
# If build.rs only needs proto files, this might not be strictly necessary,
# but it protects against future build.rs changes.
RUN mkdir src/
RUN echo "fn main() { println!(\"dummy build\"); }" > src/lib.rs # Dummy lib.rs
RUN echo "mod lib;" > src/main.rs # Dummy main.rs

# Build dependencies only (will fail if build.rs needs more src files)
# This is an optimization: if only src changes, dependencies are cached.
# If build.rs needs real src code, this needs adjustment.
# Alternatively, copy all src first if build.rs needs it.
RUN --mount=type=cache,target=/usr/local/cargo/registry \
    --mount=type=cache,target=/app/target \
    cargo build --locked --release --target x86_64-unknown-linux-gnu

# Copy the rest of the source code
COPY data-ingestion-rust/src/ ./src/

# Rebuild, leveraging cached dependencies. This compiles your actual code.
RUN --mount=type=cache,target=/usr/local/cargo/registry \
    --mount=type=cache,target=/app/target \
    cargo build --locked --release --target x86_64-unknown-linux-gnu


# Stage 2: Create the final small image
FROM debian:bookworm-slim

# Install necessary runtime dependencies (e.g., for openssl)
# Adjust based on your Rust features and OS requirements
RUN apt-get update && apt-get install -y --no-install-recommends \
    ca-certificates \
    libssl3 \
    && rm -rf /var/lib/apt/lists/*

# Copy the compiled binary from the builder stage
COPY --from=builder /app/target/x86_664-unknown-linux-gnu/release/data-ingestion-rust /usr/local/bin/data-ingestion-rust

# Create a directory for configuration
RUN mkdir /app/config
WORKDIR /app
# Copy default config if you have one - consider injecting this via deployment tools instead
# COPY data-ingestion-rust/config/settings.yaml ./config/settings.yaml

# Set the entrypoint to your binary
ENTRYPOINT ["/usr/local/bin/data-ingestion-rust"]

# Command to run (uses config file or env vars)
CMD []