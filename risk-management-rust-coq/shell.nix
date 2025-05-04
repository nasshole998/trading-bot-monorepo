# trading-bot-monorepo/risk-management-rust-coq/shell.nix
# This shell.nix provides a reproducible development environment for the Rust component.
# To enter the environment, navigate to this directory in your terminal and run `nix-shell`.
# If using Nix Flakes, you might use `nix develop .#risk-management-rust-coq` from the monorepo root.

{ pkgs ? import <nixpkgs> {} }:

pkgs.mkShell {
  # Build inputs needed for Rust development and Protobuf compilation
  buildInputs = [
    pkgs.rustc # Rust compiler
    pkgs.cargo # Rust package manager
    pkgs.rustfmt # Rust code formatter (optional but recommended)
    pkgs.clippy # Rust linter (optional but recommended)
    pkgs.protobuf # Includes protoc compiler
    # Add other dependencies if needed (e.g., for specific native libraries)
  ];

  # Define environment variables if required by Rust tools or libraries
  # Example: export environment variables needed for gRPC

  # Define a shell hook to run commands when entering the shell
  shellHook = ''
    echo "Entering Rust Risk Management development shell."
    echo "Rust version: $(rustc --version)"
    echo "Cargo version: $(cargo --version)"
    echo "Protoc version: $(protoc --version)"
    echo ""
    echo "To build the project (includes protobuf generation via build.rs): cargo build"
    echo "To run the project: cargo run"
    echo "Remember to update Protobuf definitions in ../../proto/ and regenerate code if .proto files change."
    echo "The build.rs script handles protobuf compilation automatically."
  '';
}