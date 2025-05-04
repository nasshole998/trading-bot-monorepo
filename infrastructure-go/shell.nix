# trading-bot-monorepo/infrastructure-go/shell.nix
# This shell.nix provides a reproducible development environment for the Go infrastructure component.
# To enter the environment, navigate to this directory in your terminal and run `nix-shell`.
# If using Nix Flakes, you might use `nix develop .#infrastructure-go` from the monorepo root.

{ pkgs ? import <nixpkgs> {} }:

pkgs.mkShell {
  # Build inputs needed for Go development and Protobuf compilation
  buildInputs = [
    pkgs.go # Go compiler and tools
    pkgs.protobuf # Includes protoc compiler
    # Need Go plugins for protoc. Install via 'go install' or add to buildInputs if available in nixpkgs.
  ];

  # Define shell hook to run commands when entering the shell
  shellHook = ''
    echo "Entering Go Infrastructure development shell."
    echo "Go version: $(go version)"
    echo "Protoc version: $(protoc --version)"
    echo ""
    echo "Ensure Go protobuf plugins are installed (go install google.golang.org/protobuf/cmd/protoc-gen-go@latest google.golang.org/grpc/cmd/protoc-gen-go-grpc@latest)"
    echo "To compile protobufs: protoc --go_out=. --go_opt=paths=source_relative --go-grpc_out=. --go-grpc_opt=paths=source_relative proto/config_service.proto proto/discovery_service.proto" # **NEW** Include discovery proto
    echo "To build the project: go build ./main.go"
    echo "To run the project: go run ./main.go"
    echo ""
  '';
}