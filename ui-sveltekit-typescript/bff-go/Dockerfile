# Use a Go build image
FROM golang:1.22-bookworm as builder

# Install protobuf compiler and gRPC Go plugins
RUN apt-get update && apt-get install -y --no-install-recommends protobuf-compiler && rm -rf /var/lib/apt/lists/*
RUN go install google.golang.org/protobuf/cmd/protoc-gen-go@latest
RUN go install google.golang.org/grpc/cmd/protoc-gen-go-grpc@latest
# Install gqlgen
RUN go install github.com/99designs/gqlgen@latest

# Set working directory
WORKDIR /app

# Copy go.mod and go.sum and download dependencies
COPY ui-sveltekit-typescript/bff-go/go.mod ui-sveltekit-typescript/bff-go/go.sum ./
RUN go mod download

# Copy shared proto files
# build process expects them in ../../proto relative to the BFF module root
# This is needed for the gqlgen generate step which might use proto definitions,
# and for the go build step which needs the generated .pb.go files.
COPY proto/ ../../proto/

# Copy the rest of the application source code and configuration
COPY ui-sveltekit-typescript/bff-go/ ./

# --- Build Steps ---
# 1. Compile proto files to generate Go code
RUN protoc --go_out=. --go_opt=paths=source_relative \
           --go-grpc_out=. --go-grpc_opt=paths=source_relative \
           ../../proto/backtester.proto \
           ../../proto/config_service.proto \
           ../../proto/discovery_service.proto # Compile needed protos

# 2. Run gqlgen to generate GraphQL boilerplate code from schema
# This reads schema.graphqls and writes to graph/schema.generated.go etc.
RUN gqlgen generate --config gqlgen.yml

# 3. Build the Go application
RUN go build -o /app/bff-go ./main.go

# --- Stage 2: Create a smaller runtime image ---
FROM debian:bookworm-slim

# Install ca-certificates for HTTPS channels if needed
RUN apt-get update && apt-get install -y --no-install-recommends ca-certificates && rm -rf /var/lib/apt/lists/*

WORKDIR /app

# Copy the built executable from the builder stage
COPY --from=builder /app/bff-go ./bff-go

# Copy configuration files
COPY ui-sveltekit-typescript/bff-go/config/ ./config/

# Set the entrypoint to the executable
ENTRYPOINT ["./bff-go"]

# Default command (optional)
# CMD []

# Expose the HTTP/GraphQL port
EXPOSE 8080