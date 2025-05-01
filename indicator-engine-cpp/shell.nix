# indicator-engine-cpp/shell.nix
# Nix shell environment specifically for this component.
# Alternative if *not* using the central nix/flake.nix.

{ pkgs ? import <nixpkgs> {
    # Allow unfree packages like CUDA
    config.allowUnfree = true;
  }
}:

let
  # --- Options (match CMake options) ---
  enableGpu = false; # Set to true to include CUDA
  useTaLib = true;  # Set to true to include TA-Lib
  enableOmp = true; # Set to true to include OpenMP

  # TA-Lib derivation (example - might need adjustments based on nixpkgs version)
  # Check if ta-lib exists in your nixpkgs version first: pkgs.lib.meta.availableOn pkgs.stdenv.hostPlatform pkgs.ta-lib
  talibDerivation = if useTaLib then pkgs.ta-lib else null;
  # If ta-lib is not in nixpkgs, you might need to define a custom derivation:
  # talibDerivation = if useTaLib then pkgs.callPackage ./nix/ta-lib.nix {} else null;

in
pkgs.mkShell {
  name = "indicator-engine-cpp-env";

  # Build inputs: Tools and libraries needed at development time
  buildInputs = with pkgs; [
    # C++ Toolchain
    gcc # Or clang
    cmake
    ninja
    gdb # Debugger

    # Core Dependencies
    pkg-config
    protobuf # Protobuf compiler and libs
    grpc # gRPC libs
    eigen # Eigen library

    # Logging
    spdlog # Spdlog library (header only or compiled version)

    # Optional Dependencies based on flags
  ] ++ lib.optional enableGpu cudaPackages.cudatoolkit # CUDA Toolkit
    ++ lib.optional useTaLib talibDerivation          # TA-Lib
    ++ lib.optional enableOmp openmp                  # OpenMP (usually part of gcc/clang)
  ;

  # Environment variables
  # Example: Help CMake find TA-Lib if pkg-config is needed
  PKG_CONFIG_PATH = lib.makeSearchPathOutput "dev" "lib/pkgconfig" (lib.optional useTaLib talibDerivation);
  # Example: Set default OMP threads for development
  OMP_NUM_THREADS = if enableOmp then "4" else null;

  # Optional: Shell hook
  shellHook = ''
    echo "Entered C++ Indicator Engine development shell."
    echo "CXX: $(c++ --version)"
    echo "CMake: $(cmake --version)"
    echo "Protoc: $(protoc --version)"
    export SPDLOG_LEVEL=debug # Default log level for dev
    echo "GPU Enabled: ${toString enableGpu}"
    echo "TA-Lib Enabled: ${toString useTaLib}"
    echo "OpenMP Enabled: ${toString enableOmp}"
    echo "Hint: Run 'cmake -S . -B build -G Ninja' then 'cmake --build build'"
  '';
}

# --- Example nix/ta-lib.nix (if needed) ---
/*
{ stdenv, fetchurl, autoconf, automake, libtool }:

stdenv.mkDerivation rec {
  pname = "ta-lib";
  version = "0.4.0";

  src = fetchurl {
    url = "mirror://sourceforge/ta-lib/ta-lib-${version}-src.tar.gz";
    sha256 = "14j7k8z9z4qgyg1z6wz6z8wz4qgyg1z6wz6z8wz4qgyg1z6wz6z8"; # Replace with actual hash
  };

  nativeBuildInputs = [ autoconf automake libtool ];

  configureFlags = [ "--prefix=${placeholder "out"}" ];

  meta = with stdenv.lib; {
    description = "Technical Analysis Library";
    homepage = "https://ta-lib.org/";
    license = licenses.bsd2; # Check TA-Lib's actual license
    platforms = platforms.unix;
  };
}
*/
