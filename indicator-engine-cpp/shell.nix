# This shell.nix provides a reproducible development environment for the C++ component.
# To enter the environment, navigate to this directory in your terminal and run `nix-shell`.
# If using Nix Flakes, you might use `nix develop .#indicator-engine-cpp` from the monorepo root
# if you define this component in the root flake.nix.

{ pkgs ? import <nixpkgs> {} }:

pkgs.mkShell {
  # Build inputs needed for compilation and development
  buildInputs = [
    pkgs.gcc # Or pkgs.clang
    pkgs.cmake
    pkgs.ninja
    pkgs.pkg-config # Needed by some libraries to find dependencies

    # Protobuf and gRPC
    pkgs.protobuf
    pkgs.grpc

    # Linear Algebra library
    pkgs.eigen # Or pkgs.blaze

    # CUDA toolkit (for GPU development/compilation)
    pkgs.cudaPackages.toolkit # Includes nvcc

    # YAML-CPP
    pkgs.yaml-cpp

    # Asio
    pkgs.asio

    # Optional: TA-Lib (if using)
    # pkgs.ta-lib

    # Optional: Google Test (for writing tests)
    # pkgs.googletest

  ];

  # Define a shell hook to run commands when entering the shell
  shellHook = ''
    echo "Entering C++ indicator-engine development shell."
    echo "C++ compiler: $(c++ --version)"
    echo "CMake version: $(cmake --version)"
    echo "Ninja version: $(ninja --version)"
    echo "Protoc version: $(protoc --version)"
    echo "NVCC version: $(nvcc --version)" # Verify CUDA compiler
  '';
}