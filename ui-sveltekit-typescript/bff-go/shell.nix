# trading-bot-monorepo/ui-sveltekit-typescript/bff-go/shell.nix
# Development environment for the Go BFF.
# Enter with `nix-shell` or `nix develop .#ui-sveltekit-typescript-bff-go` from monorepo root.

{ pkgs ? import <nixpkgs> {} }:

pkgs.mkShell {
  buildInputs = [
    pkgs.go # Go compiler and tools
    pkgs.protobuf # Includes protoc compiler
    # Install go plugins and gqlgen via `go install`
  ];

  # Define shell hook to run commands when entering the shell
  shellHook = ''
    echo "Entering Go BFF development shell."
    echo "Go version: $(go version)"
    echo "Protoc version: $(protoc --version)"
    echo ""
    echo "Ensure Go protobuf plugins are installed (go install google.golang.org/protobuf/cmd/protoc-gen-go@latest google.golang.org/grpc/cmd/protoc-gen-go-grpc@latest)"
    echo "Ensure gqlgen is installed (go install github.com/99designs/gqlgen@latest)"
    echo ""
    echo "To compile protobufs: protoc --go_out=. --go_opt=paths=source_relative --go-grpc_out=. --go-grpc_opt=paths=source_relative ../../proto/backtester.proto ../../proto/config_service.proto ../../proto/discovery_service.proto"
    echo "To generate GraphQL code: gqlgen generate"
    echo "To build the project: go build ./main.go"
    echo "To run the project: go run ./main.go"
    echo ""
  '';
}