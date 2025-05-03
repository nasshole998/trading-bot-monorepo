# Stage 1: Build the OCaml application
FROM ocaml/opam:debian-slim-ocaml-4.14 as builder

# Install system dependencies required for protobuf/grpc and build tools
# protobuf-compiler is needed for protoc
# libev-dev is a dependency for ocaml-grpc's Lwt backend
# libgmp-dev might be needed by batteries or other math libraries
RUN apt-get update && apt-get install -y --no-install-recommends \
    protobuf-compiler \
    libev-dev \
    libgmp-dev \
    git \
    && rm -rf /var/lib/apt/lists/*

# Set up opam
ENV OPAMROOT /home/opam/.opam
RUN opam init --reinstall-ocaml --auto-setup --disable-sandboxing --assume-yes
RUN opam update
RUN opam upgrade -y --assume-yes

# Install OCaml dependencies using opam
# Dune is the build tool
# menhir and ocamlllex are for parser/lexer generation
# ocaml-protoc and ocaml-protoc-plugin for Protobuf/gRPC code generation
# lwt and cohttp-lwt-unix for async and http server
# logs and mtime for logging
# yaml-ez for config
# ppx_sexp_conv and sexplib for sexp conversions
# uri for http
# result for Result type
# csexp for sexp parsing/printing
# batteries for Deque etc.
# ppx_lwt for let* syntax
RUN opam install -y dune menhir ocamlllex ocaml-protoc ocaml-protoc-plugin ocaml-grpc-lwt cohttp-lwt-unix logs mtime yaml-ez ppx_sexp_conv sexplib uri result csexp batteries ppx_lwt yojson

WORKDIR /app

# Copy project files, including proto files from the root
COPY proto/ ./proto/
COPY strategy-dsl-ocaml/ ./strategy-dsl-ocaml/

WORKDIR /app/strategy-dsl-ocaml

# Build the project using dune
# This will run protoc, ocamlllex, menhir and compile the OCaml code
RUN dune build @install

# Stage 2: Create a smaller runtime image
# Use a slim Debian image
FROM debian:bookworm-slim

# Install runtime dependencies (if any OCaml libs link to system libs)
# libev4 is needed at runtime for the built ocaml-grpc-lwt
# libgmp10 is the runtime for libgmp-dev
# libstdc++6 might be needed for some compilers/libraries
RUN apt-get update && apt-get install -y --no-install-recommends \
    libev4 \
    libgmp10 \
    libstdc++6 \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /app

# Copy the compiled executable from the builder stage
# Dune installs to _build/install/default/bin/main (adjust path based on dune-project name if different)
COPY --from=builder /app/strategy-dsl-ocaml/_build/install/default/bin/main ./strategy-dsl-engine

# Copy strategy files and configuration
COPY strategy-dsl-ocaml/config/ ./config/
COPY strategy-dsl-ocaml/examples/ ./examples/ 

# Set the entrypoint to your executable
ENTRYPOINT ["./strategy-dsl-engine"]

# Default command (optional, can be used to pass config path)
# CMD ["--config", "config/settings.yml"]

# Expose ports (optional, for documentation)
EXPOSE 50053 
EXPOSE 8082  