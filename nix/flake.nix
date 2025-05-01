# nix/flake.nix
# Central Nix Flake for the trading-bot-monorepo
{
  description = "Development environment for the Polyglot Trading Bot";

  # Declare external dependencies (primarily nixpkgs)
  inputs = {
    # Use a specific, stable Nixpkgs revision for reproducibility
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-23.11"; # Or nixos-unstable, or a specific commit hash

    # For Rust development using Oxalica's overlay (recommended for better tooling)
    rust-overlay.url = "github:oxalica/rust-overlay";
    rust-overlay.inputs.nixpkgs.follows = "nixpkgs";

    # For fetching flake utilities like flake-utils
    flake-utils.url = "github:numtide/flake-utils";

    # For Coq Platform (provides Coq + standard libraries + OCaml for extraction)
    # coq-nixpkgs.url = "github:coq-community/coq-nixpkgs-overlay";
    # coq-nixpkgs.inputs.nixpkgs.follows = "nixpkgs";
    # Note: Coq integration can be complex. Start simple first. Let's use nixpkgs' Coq for now.
  };

  # Define the outputs of this flake (packages, dev shells, etc.)
  outputs = { self, nixpkgs, rust-overlay, flake-utils, ... }@inputs:
    # Use flake-utils to simplify defining outputs for multiple systems (x86_64-linux, aarch64-linux, x86_64-darwin)
    flake-utils.lib.eachDefaultSystem (system:
      let
        # Import nixpkgs for the specific system
        pkgs = import nixpkgs {
          inherit system;
          # Apply overlays if needed (e.g., rust-overlay)
          overlays = [ rust-overlay.overlays.default /* coq-nixpkgs.overlay */ ];
          # Allow packages with non-free licenses (e.g., CUDA)
          config.allowUnfree = true;
        };

        # ----- Common Dependencies -----
        commonDeps = with pkgs; [
          git
          openssl.dev # Often needed for network libs
          pkg-config
          protobuf # Protocol Buffers compiler
          grpc # gRPC library (might need specific language bindings below)
          zeromq # Alternative messaging library
          nats-server # If using NATS message bus
          # kafka # If using Kafka
        ];

        # ----- Rust Dependencies -----
        rustToolchain = pkgs.rust-bin.stable.latest.default.override {
          # Add components like clippy, rustfmt, rust-src
          extensions = [ "rust-src" "clippy" "rustfmt" ];
          # Add targets if cross-compiling (e.g., "aarch64-unknown-linux-gnu")
          # targets = ["..."];
        };
        rustDeps = with pkgs; [
          rustToolchain
          # System libraries needed by Rust crates
          openssl.dev
          # protobuf # For prost-build
          # grpc # For tonic-build
          pkg-config
        ];

        # ----- C++ Dependencies -----
        cppDeps = with pkgs; [
          gcc # Or clangStdenv.cc
          cmake
          ninja
          gdb
          pkg-config
          boost
          eigen
          # Intel MKL/IPP might require manual fetching or specific Nix setup if not in nixpkgs easily
          # ta-lib # May need to package this if not in nixpkgs
          # --- GPU Specific ---
          # cudaPackages.cudatoolkit # If using NVIDIA GPU
          # cudaPackages.cudnn # If needed for ML
          # opencl-headers ocl-icd # If using OpenCL
          # --- gRPC/Protobuf ---
          protobuf
          grpc
          protobuf-cpp # C++ specific protobuf runtime
        ];

        # ----- Julia Dependencies -----
        juliaDeps = with pkgs; [
          julia-bin # Pre-built Julia binary (often preferred)
          # --- GPU Specific ---
          # cudaPackages.cudatoolkit # For CUDA.jl
          # --- Python Interop ---
          python3 # For PyCall.jl
          # Add python packages needed by Julia here if necessary
        ];

        # ----- Mojo Dependencies -----
        # Mojo installation via Nix is currently non-standard (as of early 2025).
        # This might involve fetching binaries or using experimental packages.
        # Placeholder - requires specific Mojo SDK Nix packaging effort.
        mojoDeps = with pkgs; [
           python3 # Mojo often interacts with Python
           # Placeholder for actual Mojo SDK package when available
        ];

        # ----- OCaml Dependencies -----
        ocamlDeps = with pkgs; [
          ocamlPackages.ocaml
          ocamlPackages.opam # For managing OCaml libraries if needed outside Nix
          ocamlPackages.dune_3 # Build system
          ocamlPackages.menhir # Parser generator
          ocamlPackages.ocamllex # Lexer generator
          ocamlPackages.core # Jane Street's standard library extension
          ocamlPackages.ppx_deriving # For code generation
          # Add other OCaml packages: ocamlPackages.grpc, ocamlPackages.protobuf, etc.
        ];

        # ----- Lisp/Scheme Dependencies -----
        lispDeps = with pkgs; [
          # Choose one or both:
          sbcl # Steel Bank Common Lisp
          racket # Racket Scheme
        ];

        # ----- F# / .NET Dependencies -----
        fsharpDeps = with pkgs; [
          dotnet-sdk_8 # .NET 8 SDK (includes F#)
        ];

        # ----- Go Dependencies -----
        goDeps = with pkgs; [
          go
          gotools # Go development tools
          gopls # Go language server
          protobuf # For Go protobuf generator
          grpc # For Go gRPC generator
        ];

        # ----- Node.js / TypeScript Dependencies -----
        nodeDeps = with pkgs; [
          nodejs-20_x # Use a specific LTS version
          nodePackages.pnpm # Or yarn, npm
          # nodePackages.typescript # Global typescript if needed
          # nodePackages_latest.svelte-language-server # For IDE support
        ];

        # ----- Coq Dependencies -----
        coqDeps = with pkgs; [
           coq # The Coq proof assistant
           # OCaml is needed by Coq and for extraction
           ocamlPackages.ocaml
           ocamlPackages.findlib
           # Add Coq libraries if needed (e.g., coqPackages.mathcomp)
        ];

      in
      {
        # ----- Development Shells -----
        # Define a dev shell for each component/service
        devShells = {
          # Shell for the Rust Data Ingestion service
          data-ingestion-rust = pkgs.mkShell {
            name = "data-ingestion-rust-env";
            inputsFrom = []; # Avoid inheriting from parent shells if nested
            packages = commonDeps ++ rustDeps;
            # Environment variables needed for Rust build scripts (e.g., finding openssl)
            OPENSSL_DIR = "${pkgs.openssl.dev}";
            OPENSSL_LIB_DIR = "${pkgs.openssl.out}/lib";
            OPENSSL_INCLUDE_DIR = "${pkgs.openssl.dev}/include";
            # For finding protobuf during build
            PROTOC = "${pkgs.protobuf}/bin/protoc";
            PROTOC_INCLUDE = "${pkgs.protobuf}/include";

            # Hook to automatically run rustup commands if needed (e.g., ensure components)
            # shellHook = ''
            #   echo "Entered Rust dev shell."
            #   # rustup component add clippy rustfmt
            # '';
          };

          # Shell for the C++ Indicator Engine
          indicator-engine-cpp = pkgs.mkShell {
            name = "indicator-engine-cpp-env";
            inputsFrom = [];
            packages = commonDeps ++ cppDeps;
            # Environment variables for CMake, CUDA, etc. if needed
            # CUDA_PATH = "${pkgs.cudaPackages.cudatoolkit}";
          };

          # Shell for the ML Engine (Julia/Mojo)
          ml-engine-julia-mojo = pkgs.mkShell {
            name = "ml-engine-julia-mojo-env";
            inputsFrom = [];
            packages = commonDeps ++ juliaDeps ++ mojoDeps;
             # Env vars for Julia package directory, Python path, etc.
             # JULIA_DEPOT_PATH = ...
          };

          # Shell for the Strategy DSL (OCaml)
          strategy-dsl-ocaml = pkgs.mkShell {
            name = "strategy-dsl-ocaml-env";
            inputsFrom = [];
            packages = commonDeps ++ ocamlDeps;
          };

          # Shell for the Strategy Generator (Lisp/Scheme)
          strategy-gen-lisp = pkgs.mkShell {
            name = "strategy-gen-lisp-env";
            inputsFrom = [];
            packages = commonDeps ++ lispDeps;
          };

          # Shell for the Backtester (F#)
          backtester-fsharp = pkgs.mkShell {
            name = "backtester-fsharp-env";
            inputsFrom = [];
            packages = commonDeps ++ fsharpDeps;
          };

          # Shell for the Infrastructure (Go)
          infrastructure-go = pkgs.mkShell {
            name = "infrastructure-go-env";
            inputsFrom = [];
            packages = commonDeps ++ goDeps;
            # GOPATH, GOBIN setup if needed
          };

          # Shell for Risk Management (Rust + Coq)
          risk-management-rust-coq = pkgs.mkShell {
            name = "risk-management-rust-coq-env";
            inputsFrom = [];
            packages = commonDeps ++ rustDeps ++ coqDeps;
             # Env vars similar to data-ingestion-rust
             OPENSSL_DIR = "${pkgs.openssl.dev}";
             OPENSSL_LIB_DIR = "${pkgs.openssl.out}/lib";
             OPENSSL_INCLUDE_DIR = "${pkgs.openssl.dev}/include";
             PROTOC = "${pkgs.protobuf}/bin/protoc";
             PROTOC_INCLUDE = "${pkgs.protobuf}/include";
          };

          # Shell for the UI (SvelteKit/TypeScript)
          ui-sveltekit-typescript = pkgs.mkShell {
            name = "ui-sveltekit-typescript-env";
            inputsFrom = [];
            packages = commonDeps ++ nodeDeps;
          };

          # Default shell provides common tools if needed at the root
          default = pkgs.mkShell {
            name = "monorepo-default-env";
            packages = commonDeps ++ [ pkgs.git pkgs.docker-compose ];
          };
        };

        # ----- Packages -----
        # Define buildable packages for each component if needed (e.g., for deployment)
        packages = {
          # Example: Package the Rust data ingestion binary
          data-ingestion-rust = pkgs.callPackage ../data-ingestion-rust/package.nix {
             # Pass dependencies explicitly if the package.nix needs them
             # rustPlatform = pkgs.rustPlatform;
             # fetchgit = pkgs.fetchgit;
             # lib = pkgs.lib;
          };

          # Add packages for other buildable components here...

          default = self.packages.${system}.data-ingestion-rust; # Example default package
        };

        # ----- Formatter -----
        # Define a formatter check that can be run with `nix fmt`
        formatter = pkgs.alejandra; # Or nixpkgs-fmt, depends on preference

      });
}
```
**Note:** This `flake.nix` assumes a structure where build logic for individual packages might reside in separate `.nix` files within their respective component directories (like `../data-ingestion-rust/package.nix`). This keeps the central flake cleaner. You would need to create those files as well. For development shells (`nix develop`), this central flake is sufficie