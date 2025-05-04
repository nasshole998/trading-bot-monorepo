# Use a Go build image
FROM golang:1.22-bookworm as builder

# Install protobuf compiler and gRPC Go plugins
RUN apt-get update && apt-get install -y --no-install-recommends protobuf-compiler && rm -rf /var/lib/apt/lists/*
RUN go install google.golang.org/protobuf/cmd/protoc-gen-go@latest
RUN go install google.golang.org/grpc/cmd/protoc-gen-go-grpc@latest

# Set working directory
WORKDIR /app

# Copy go.mod and go.sum and download dependencies
COPY infrastructure-go/go.mod infrastructure-go/go.sum ./
RUN go mod download

# Copy shared proto files
# build process expects them in ../proto relative to the module root
COPY proto/ ../proto/

# Copy the rest of the application source code and configuration data
COPY infrastructure-go/ ./

# Compile all necessary proto files
RUN protoc --go_out=. --go_opt=paths=source_relative \
           --go-grpc_out=. --go-grpc_opt=paths=source_relative \
           proto/config_service.proto \
           proto/discovery_service.proto # **NEW** Compile the discovery service proto

# Build the Go application
RUN go build -o /app/infrastructure-go ./main.go

# --- Stage 2: Create a smaller runtime image ---
FROM debian:bookworm-slim

# Install ca-certificates for HTTPS channels if needed
RUN apt-get update && apt-get install -y --no-install-recommends ca-certificates && rm -rf /var/lib/apt/lists/*

WORKDIR /app

# Copy the built executable from the builder stage
COPY --from=builder /app/infrastructure-go ./infrastructure-go

# Copy configuration files and the configs_data directory
COPY infrastructure-go/config/ ./config/
COPY infrastructure-go/configs_data/ ./configs_data/

# Set the entrypoint to the executable
ENTRYPOINT ["./infrastructure-go"]

# Default command (optional)
# CMD []

# Expose the gRPC port
EXPOSE 50060