# data-ingestion-rust/shell.nix
# Nix shell environment specifically for this component.
# This is an alternative if you are *not* using the central nix/flake.nix.
# If using the central flake, this file is not needed, and you'd run `nix develop .#data-ingestion-rust` from the repo root.

{ pkgs ? import <nixpkgs> {} }:

pkgs.mkShell {
  name = "data-ingestion-rust-env";

  # Build inputs: Tools and libraries needed at development time
  buildInputs = with pkgs; [
    # Rust toolchain (using rustup for flexibility)
    rustup

    # System libraries often needed by Rust crates
    openssl.dev
    pkg-config

    # Protocol Buffers compiler (needed for build.rs using tonic-build)
    protobuf

    # Other tools
    git
  ];

  # Environment variables needed by build scripts or runtime
  # For openssl-sys crate
  OPENSSL_DIR = "${pkgs.openssl.dev}";
  OPENSSL_LIB_DIR = "${pkgs.openssl.out}/lib";
  OPENSSL_INCLUDE_DIR = "${pkgs.openssl.dev}/include";

  # For prost-build/tonic-build finding protoc
  PROTOC = "${pkgs.protobuf}/bin/protoc";
  PROTOC_INCLUDE = "${pkgs.protobuf}/include";

  # Optional: Set Rust source path for rust-analyzer
  RUST_SRC_PATH = "${pkgs.rustPlatform.rustLibSrc}";

  # Optional: Shell hook executed when entering the shell
  shellHook = ''
    echo "Entered Rust Data Ingestion development shell."
    # Ensure necessary rustup components are installed (optional)
    # rustup component add clippy rustfmt || true
    echo "Rust version: $(rustc --version)"
    echo "Cargo version: $(cargo --version)"
    echo "Protoc version: $(protoc --version)"
    export RUST_LOG="debug" # Set default log level for dev
    echo "Hint: Run 'cargo build' or 'cargo run'"
  '';
}
