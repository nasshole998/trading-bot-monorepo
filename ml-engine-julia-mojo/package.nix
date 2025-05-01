# ml-engine-julia-mojo/package.nix
# Nix package definition for the Julia ML Engine.
# Used by the central nix/flake.nix.

{ lib
, stdenv
, julia-bin # Use julia-bin for pre-built Julia
, protobuf # For protoc needed by build.jl
, git # Needed if fetching deps via Git in Julia Pkg
, cacert # For network requests (HTTPS)
, python3 ? null # Optional Python for Mojo build/runtime
, buildEssential ? null # Optional C build tools for Mojo build
, cudaPackages ? null # Optional CUDA toolkit
, fetchFromGitHub ? null # If src is from Git
, src # Source directory provided by flake
, enableGpu ? false
, enableMojo ? false # Flag to control Mojo parts
}:

# Filter out optional null dependencies
let
  buildInputsList = [
    julia-bin
    protobuf # For protoc used in build.jl
    git
    cacert # Often needed by Julia's Pkg/Downloads
  ] ++ lib.optional enableGpu cudaPackages.cudatoolkit # CUDA Toolkit for build/runtime
    ++ lib.optional enableMojo python3 # Python needed for Mojo build/runtime
    ++ lib.optional enableMojo buildEssential # C compiler for Mojo build
  ;
in
stdenv.mkDerivation rec {
  pname = "ml-engine-julia";
  version = "0.1.0";

  inherit src;

  # Dependencies needed to build the Julia package itself (run build.jl, etc.)
  nativeBuildInputs = buildInputsList;
  # Dependencies needed by the final Julia application at runtime
  # Often overlaps significantly with nativeBuildInputs for Julia
  buildInputs = buildInputsList;


  # Environment variables needed during build or runtime
  # Ensure Julia uses the Nix store's SSL certificates
  SSL_CERT_FILE = "${cacert}/etc/ssl/certs/ca-bundle.crt";
  # Point Julia Pkg to a writable directory inside the build sandbox
  JULIA_DEPOT_PATH = "./.julia_depot";
  # Ensure build.jl finds protoc
  PROTOC = "${protobuf}/bin/protoc";


  # Configure Phase (Prepare Julia environment)
  configurePhase = ''
    runHook preConfigure

    # Set JULIA_PROJECT specifically for build steps
    export JULIA_PROJECT=$(pwd)/julia

    echo "Initializing Julia Depot in $JULIA_DEPOT_PATH"
    mkdir -p $JULIA_DEPOT_PATH

    echo "Copying Project.toml/Manifest.toml..."
    cp julia/Project.toml julia/Manifest.toml* .

    echo "Instantiating Julia environment..."
    julia --project=. -e 'using Pkg; Pkg.instantiate(); Pkg.resolve();'

    echo "Running Julia build script (ProtoBuf compilation)..."
    # Ensure protos are accessible relative to build.jl
    cp -r ../proto ./proto # Copy protos into build dir
    julia --project=. julia/build.jl

    # --- Placeholder: Mojo Build ---
    if [ "${toString enableMojo}" == "true" ]; then
      echo "Building Mojo component (Placeholder)..."
      # This needs the actual Mojo SDK installation and build steps
      # Example:
      # pushd mojo
      # bash build.sh # Assuming this creates libmodel_inference.so
      # popd
      echo "Mojo build placeholder complete."
    fi

    runHook postConfigure
  '';

  # Build Phase (Optional - could involve precompilation or PackageCompiler)
  buildPhase = ''
    runHook preBuild
    echo "Running Julia precompilation..."
    export JULIA_PROJECT=$(pwd) # Use top-level Project.toml now
    julia --project=. -e 'using Pkg; Pkg.precompile()'
    runHook postBuild
  '';

  # Install Phase (Copy necessary files to $out)
  installPhase = ''
    runHook preInstall

    # Create installation directory structure
    mkdir -p $out/bin $out/lib/julia $out/config $out/models
    mkdir -p $out/lib/julia/mojo # If using Mojo

    # Copy Julia project files and generated code
    cp Project.toml Manifest.toml $out/lib/julia/
    cp -r src $out/lib/julia/
    # cp -r .julia_depot $out/lib/julia/ # Avoid copying depot, rely on runtime resolution

    # Copy config files
    cp -r julia/config/* $out/config/

    # Copy trained models (or expect them to be mounted)
    # cp -r models/* $out/models/
    touch $out/models/.placeholder # Ensure dir exists

    # Copy Mojo shared library if built
    if [ "${toString enableMojo}" == "true" ] && [ -f "mojo/libmodel_inference.so" ]; then
       cp mojo/libmodel_inference.so $out/lib/julia/mojo/
    fi

    # Create a wrapper script to run the Julia service
    echo "Creating wrapper script $out/bin/ml-engine-julia"
    cat > $out/bin/ml-engine-julia << EOF
    #!${stdenv.shell}
    # Set environment variables for Julia runtime
    export JULIA_PROJECT="$out/lib/julia"
    export JULIA_DEPOT_PATH="$out/lib/julia/.julia_depot_runtime" # Use a runtime depot path
    # Optional: Point to Mojo library
    # export LD_LIBRARY_PATH="$out/lib/julia/mojo:\$LD_LIBRARY_PATH"

    # Ensure runtime depot exists
    mkdir -p \$JULIA_DEPOT_PATH

    # Execute Julia application
    exec ${julia-bin}/bin/julia --project="\$JULIA_PROJECT" --threads=auto \$JULIA_PROJECT/src/MLEngine.jl "\$@"
    EOF
    chmod +x $out/bin/ml-engine-julia

    runHook postInstall
  '';

  # Don't fixup command line paths in the wrapper script itself
  dontFixup = true;

  # Pass environment variables needed by the wrapper script at runtime
  passthru = {
    inherit SSL_CERT_FILE;
    # Add other runtime env vars if needed
  };


  meta = with lib; {
    description = "Julia ML Engine for Trading Bot";
    homepage = "https://example.com/trading-bot";
    license = licenses.mit; # Choose your license
    platforms = platforms.linux;
    maintainers = [ maintainers.your_github_username ];
  };
}
