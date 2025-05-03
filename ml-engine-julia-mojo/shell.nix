# This shell.nix provides a reproducible development environment for the Julia/Mojo component.
# To enter the environment, navigate to this directory in your terminal and run `nix-shell`.
# If using Nix Flakes, you might use `nix develop .#ml-engine-julia-mojo` from the monorepo root
# if you define this component in the root flake.nix.

{ pkgs ? import <nixpkgs> {} }:

let
  # Specify the Julia version you want
  juliaVersion = pkgs.julia_1_10; # Example: Use Julia 1.10

  # Find the Julia gRPC protoc plugin. This might be part of the gRPC.jl package
  # or require special handling in Nix. This is a placeholder for its location.
  # grpcJuliaPlugin = "${juliaVersion}/share/julia/site/v${juliaVersion.major}.${juliaVersion.minor}/gRPC/plugin/protoc-gen-julia"; # Example path (verify this)
  # Or maybe it's an executable provided by the package:
  # grpcJuliaPlugin = "${pkgs.juliaPackages.gRPC}/bin/protoc-gen-julia"; # Example if gRPC.jl provides a binary

  # Specify the Mojo SDK path if installed via Nix or available otherwise
  # Check if Mojo is available in nixpkgs or need to fetch it.
  # mojoSdk = pkgs.mojo; # Example if Mojo is in pkgs (unlikely in standard)
  # Or fetch it as in the previous draft (adjust URL/hash):
  # mojoSdk = pkgs.fetchzip { ... };

in
pkgs.mkShell {
  # Build inputs needed for development
  buildInputs = [
    juliaVersion
    pkgs.protobuf # Includes protoc
    # Need a way to get the Julia gRPC plugin for protoc.
    # If the plugin is built when `Pkg.build("gRPC")` runs, you might need to
    # run instantiate *before* trying to compile protos outside of Julia.
    # If it's a separate executable, include it here.
    # grpcJuliaPlugin # Include the Julia gRPC plugin executable

    # Other tools potentially needed
    pkgs.git # For cloning packages
    pkgs.which # Useful for finding executables
    # Add any system libraries that Julia packages might depend on
    # pkgs.libyaml # Example if YAML.jl needed system libyaml
    # pkgs.zlib # Example if some package needs zlib
    # Add CUDA development libraries if you need to build/compile Julia CUDA code
    # pkgs.cudaPackages.toolkit # If building Flux with CUDA support
    # pkgs.cudaPackages.cudnn # If building Flux with CuDNN support

    # mojoSdk # Include Mojo SDK in the environment if needed for development/building
  ];

  # Define environment variables if required by Julia or Mojo
  # JULIA_PROJECT = "${self}/julia"; # Set the project environment
  # MOJO_SDK_PATH = "${mojoSdk}"; # Set Mojo SDK path (needed by mojo command)
  # JULIA_CUDA_USE_BINARY = "true"; # Example env var for CUDA.jl


  # Define a shell hook to run commands when entering the shell
  shellHook = ''
    echo "Entering Julia/Mojo ML-Engine development shell."
    echo "Julia version: $(julia --version)"
    echo "Protoc version: $(protoc --version)"
    # if command -v mojo &> /dev/null; then
    #   echo "Mojo version: $(mojo --version)"
    # fi
    echo ""
    echo "To set up Julia dependencies, run: julia --project=./julia/ -e 'using Pkg; Pkg.instantiate()'"
    echo "To compile Protobuf files, you need 'protoc' and the 'protoc-gen-julia' plugin from gRPC.jl."
    echo "Navigate to the monorepo root and run: protoc -Iproto/ --plugin=protoc-gen-julia=\$(julia --project=ml-engine-julia-mojo/julia/ -e 'using gRPC; print(gRPC.get_plugin_path())') --julia_out=ml-engine-julia-mojo/julia/src/generated/ proto/market_data.proto proto/indicator_data.proto proto/ml_prediction.proto"
    echo ""

    # Optional: Helper to compile protos in the shell
    # compile_protos() {
    #   echo "Compiling Protobuf files..."
    #   # Find the path to the Julia gRPC plugin. This requires Julia to be in the PATH.
    #   PROTOC_GEN_JGRPC=\$(julia --project=./julia/ -e 'using gRPC; print(gRPC.get_plugin_path())')
    #   if [ -z "\$PROTOC_GEN_JGRPC" ]; then
    #     echo "Error: Could not find protoc-gen-julia plugin. Ensure gRPC.jl is installed and built."
    #     return 1
    #   fi
    #   protoc \
    #     -I ${PWD}/../../proto \
    #     --plugin=protoc-gen-julia=\${PROTOC_GEN_JGRPC} \
    #     --julia_out=${PWD}/julia/src/generated \
    #     ${PWD}/../../proto/market_data.proto \
    #     ${PWD}/../../proto/indicator_data.proto \
    #     ${PWD}/../../proto/ml_prediction.proto
    #   echo "Protobuf compilation finished."
    # }
  '';
}