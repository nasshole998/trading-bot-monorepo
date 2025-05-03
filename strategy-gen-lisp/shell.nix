# This shell.nix provides a reproducible development environment for the Lisp component.
# To enter the environment, navigate to this directory in your terminal and run `nix-shell`.
# If using Nix Flakes, you might use `nix develop .#strategy-gen-lisp` from the monorepo root
# if you define this component in the root flake.nix.

{ pkgs ? import <nixpkgs> {} }:

pkgs.mkShell {
  # Build inputs needed for compilation and development
  buildInputs = [
    # A Common Lisp implementation (e.g., SBCL, CCL)
    pkgs.sbcl

    # Protobuf tools
    pkgs.protobuf # Includes protoc

    # Common Lisp libraries needed by protoc plugin or for development
    # cl-protobuf is typically installed via Quicklisp
  ];

  # Define environment variables if required
  # Example: Set environment variables needed by cl-grpc if any

  # Define a shell hook to run commands when entering the shell
  shellHook = ''
    echo "Entering Common Lisp Strategy Generator development shell."
    echo "Lisp implementation: $(sbcl --version)"
    echo "Protoc version: $(protoc --version)"
    echo ""
    echo "Remember to run 'protoc' manually if .proto files change:"
    echo "  protoc -I=../proto/ --lisp-out=src/grpc/gen/ ../proto/backtester.proto ../proto/strategy_engine.proto ../proto/market_data.proto"
    echo "And ensure Quicklisp is installed and dependencies loaded ('sbcl --load quicklisp.lisp')."
    echo "To run the generator: ./run.sh"
  '';
}