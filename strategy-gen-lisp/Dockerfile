# Use a base image with SBCL (Steel Bank Common Lisp)
FROM common-lisp:latest

# Install necessary system dependencies (like protoc)
RUN apt-get update && apt-get install -y --no-install-recommends \
    protobuf-compiler \
    && rm -rf /var/lib/apt/lists/*

# Install Quicklisp
RUN apt-get update && apt-get install -y unzip && rm -rf /var/lib/apt/lists/*
RUN curl -O https://beta.quicklisp.org/quicklisp.lisp
RUN sbcl --load quicklisp.lisp --eval '(quicklisp-quickstart:install :path "/app/quicklisp/")' --quit \
    && rm quicklisp.lisp

# Set working directory
WORKDIR /app

# Copy project files
COPY proto/ ./proto/
COPY strategy-gen-lisp/ ./strategy-gen-lisp/

# Generate Lisp Protobuf bindings
# Include market_data.proto now for OrderUpdates
RUN /usr/bin/protoc -I=proto/ \
    --lisp-out=strategy-gen-lisp/src/grpc/gen/ \
    proto/backtester.proto \
    proto/strategy_engine.proto \
    proto/market_data.proto # Added

WORKDIR /app/strategy-gen-lisp

# Ensure Quicklisp knows about our system and load dependencies
# This needs to be done in the image after copying the project
RUN sbcl --load /app/quicklisp/setup.lisp \
       --eval "(push (truename \".\") asdf:*central-registry*)" \
       --eval "(ql:quickload :cl-protobuf)" \
       (ql:quickload :cl-grpc)" \
       (ql:quickload :cl-yaml)" \
       (ql:quickload :alexandria)" \
       (ql:quickload :local-time)" \
       --eval "(ql:quickload :strategy-gen-lisp)" \
       --quit

# Create output directory
RUN mkdir -p strategies-out

# Set the entrypoint to the run script
ENTRYPOINT ["./run.sh"]