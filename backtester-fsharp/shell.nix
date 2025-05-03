# This shell.nix provides a reproducible development environment for the F# component.
# To enter the environment, navigate to this directory in your terminal and run `nix-shell`.
# If using Nix Flakes, you might use `nix develop .#backtester-fsharp` from the monorepo root.

{ pkgs ? import <nixpkgs> {} }:

pkgs.mkShell {
  # Build inputs needed for .NET development
  buildInputs = [
    pkgs.dotnet-sdk_8 # .NET 8 SDK
    pkgs.protobuf # Includes protoc
    # Add other dependencies if needed (e.g., for specific native libraries)
  ];

  # Define environment variables if required by .NET tools or libraries
  # Example: export environment variables needed for gRPC

  # Define a shell hook to run commands when entering the shell
  shellHook = ''
    echo "Entering F# Backtester development shell."
    echo ".NET SDK version: $(dotnet --version)"
    echo "Protoc version: $(protoc --version)"
    echo ""
    echo "To restore dependencies and generate protobuf code: dotnet restore"
    echo "To build the project: dotnet build"
    echo "To run the project: dotnet run"
    echo "Remember to update Protobuf definitions and regenerate code if .proto files change."
  '';
}