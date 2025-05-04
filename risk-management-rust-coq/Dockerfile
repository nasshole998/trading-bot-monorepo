# Use a Rust build image
FROM rust:1.76-bookworm as builder

# Install protobuf compiler
# Required for tonic-build/prost-build to run the protoc command
RUN apt-get update && apt-get install -y --no-install-recommends protobuf-compiler && rm -rf /var/lib/apt/lists/*

WORKDIR /app

# Copy Cargo.toml and build.rs first to leverage Docker cache
# build.rs contains the protobuf compilation logic
COPY risk-management-rust-coq/Cargo.toml risk-management-rust-coq/Cargo.lock ./
COPY risk-management-rust-coq/build.rs ./

# Create a dummy src directory and main.rs to allow initial dependency build
# This pre-build step caches dependencies, including build-dependencies like tonic-build,
# which will compile the protos in the next step.
RUN mkdir src && echo "fn main() {}" > src/main.rs

# Build dependencies (this implicitly runs build.rs which compiles protos)
# This will fail if proto files are not yet copied, but caches Rust dependencies.
# We'll copy protos and re-run build later.
RUN cargo build --release || true # Allow failure here, we just want to cache deps

# Remove dummy src and copy actual source files
RUN rm -rf src

# Copy the rest of the source code, the shared proto files, and config
# The build.rs script expects protos in the ../../proto path relative to the Rust project root.
COPY risk-management-rust-coq/ ./
COPY proto/ ./proto/ 

# Re-run build to compile the actual source code and protos
RUN cargo build --release

# Publish the application - Creates a self-contained executable in /app/publish
# --no-self-contained means it depends on the .NET runtime in the base image (for .NET apps)
# For Rust, this usually just means compiling statically if possible, or depending on system libs.
# For this setup, it collects the binary and necessary files.
RUN cargo install --path . --root /app/publish --bin risk-management-rust-coq

# Stage 2: Create a smaller runtime image
FROM debian:bookworm-slim

# Install ca-certificates for HTTPS channels if needed (gRPC often uses HTTPS)
# This is required if your gRPC client or server uses TLS/SSL.
RUN apt-get update && apt-get install -y --no-install-recommends ca-certificates && rm -rf /var/lib/apt/lists/*

WORKDIR /app

# Copy the compiled executable from the builder stage
COPY --from=builder /app/publish/bin/risk-management-rust-coq ./

# Copy configuration files
COPY risk-management-rust-coq/config/ ./config/

# Set the entrypoint to the executable
ENTRYPOINT ["./risk-management-rust-coq"]

# Default command (optional, can pass args if needed)
# CMD []

# Expose the gRPC port (replace with your configured listen_address port if different)
EXPOSE 50056