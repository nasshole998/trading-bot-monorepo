# indicator-engine-cpp/package.nix
# Nix package definition for the C++ Indicator Engine.
# Used by the central nix/flake.nix.

{ lib
, stdenv
, cmake
, ninja
, pkg-config
, protobuf
, grpc
, eigen
, spdlog
, ta-lib ? null # Optional TA-Lib dependency
, cudatoolkit ? null # Optional CUDA dependency
, openmp ? null # Optional OpenMP dependency (often part of stdenv.cc)
, fetchFromGitHub ? null # Needed if fetching source from Git
, src # Source directory provided by the flake input
, enableGpu ? false
, useTaLib ? true
, enableOmp ? true
}:

stdenv.mkDerivation rec {
  pname = "indicator-engine-cpp";
  version = "0.1.0";

  inherit src;

  nativeBuildInputs = [
    cmake
    ninja
    pkg-config
    protobuf # For protoc
  ] ++ lib.optional enableGpu cudatoolkit # Need nvcc available
  ;

  buildInputs = [
    grpc # Runtime libs
    eigen
    spdlog
  ] ++ lib.optional useTaLib ta-lib
    ++ lib.optional enableGpu cudatoolkit.lib # CUDA runtime lib
    # OpenMP is usually provided by the C++ compiler (stdenv.cc)
    # ++ lib.optional enableOmp openmp
  ;

  # CMake configuration flags matching options in CMakeLists.txt
  cmakeFlags = [
    "-DCMAKE_BUILD_TYPE=Release"
    "-DINDICATOR_ENGINE_ENABLE_GPU=${if enableGpu then "ON" else "OFF"}"
    "-DINDICATOR_ENGINE_USE_TA_LIB=${if useTaLib then "ON" else "OFF"}"
    "-DINDICATOR_ENGINE_ENABLE_OMP=${if enableOmp then "ON" else "OFF"}"
    # Help CMake find dependencies if needed (Nix environment usually handles this)
    # "-DEIGEN3_INCLUDE_DIR=${eigen}/include/eigen3"
  ] ++ lib.optional enableGpu "-DCMAKE_CUDA_ARCHITECTURES=75;86"; # Example GPU architectures

  # Environment variables needed during build
  # Example: Help find TA-Lib via pkg-config if necessary
  PKG_CONFIG_PATH = lib.makeSearchPathOutput "dev" "lib/pkgconfig" (lib.optional useTaLib ta-lib);

  # If source is fetched via Git and needs submodules:
  # postPatch = ''
  #   git submodule update --init --recursive
  # '';

  # Build Phase (CMake handles this)

  # Install Phase (CMake handles installation to $out)
  # Ensure CMakeLists.txt has install rules:
  # install(TARGETS indicator_engine_server DESTINATION $out/bin)

  # Check Phase (Optional - run tests)
  # doCheck = true;
  # checkPhase = ''
  #   runHook preCheck
  #   ctest --output-on-failure
  #   runHook postCheck
  # '';

  meta = with lib; {
    description = "High-performance technical indicator calculation engine";
    homepage = "https://example.com/trading-bot"; # Replace with actual URL
    license = licenses.mit; # Choose your license
    platforms = platforms.linux; # Add other platforms if supported
    maintainers = [ maintainers.your_github_username ]; # Replace
  };
}

