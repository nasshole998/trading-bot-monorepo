# This shell.nix provides a reproducible development environment for the OCaml component.
# To enter the environment, navigate to this directory in your terminal and run `nix-shell`.
# If using Nix Flakes, you might use `nix develop .#strategy-dsl-ocaml` from the monorepo root
# if you define this component in the root flake.nix.

{ pkgs ? import <nixpkgs> {} }:

# Use opam2nix to manage OCaml dependencies defined in dune-project
let
  # This requires opam2nix to be installed and configured elsewhere,
  # typically running `opam2nix generate` in the project root or opam directory.
  # The generated `opam-packages.nix` or similar file lists the OCaml dependencies
  # and their Nix build instructions.
  opamPackages = import ./opam-packages.nix { inherit pkgs; }; # Assuming opam2nix output

  # Find the ocaml-protoc and ocaml-protoc-plugin binaries
  # Adjust attribute names if they differ in your opam2nix output
  ocamlProtoc = opamPackages.ocaml-protoc.build;
  ocamlProtocPlugin = opamPackages.ocaml-protoc-plugin.build;

in
pkgs.mkShell {
  # Build inputs needed for compilation and development
  buildInputs = [
    # OCaml compiler toolchain (includes ocamlc, ocamlopt, etc.)
    pkgs.ocaml

    # Build tool
    opamPackages.dune

    # Lexer and Parser generators
    opamPackages.menhir
    opamPackages.ocamlllex

    # Protobuf/gRPC tools
    pkgs.protobuf # Includes protoc
    ocamlProtoc # The ocaml-protoc binary
    ocamlProtocPlugin # The ocaml-protoc-plugin binary

    # All OCaml dependencies (including transitive ones) from opam2nix
    opamPackages.strategy_dsl.buildInputs # Get build dependencies defined in dune-project
  ];

  # Define environment variables if required by Dune or other tools
  # Example: Set the OCAML_VERSION environment variable if needed by some scripts
  # OCAML_VERSION = "${pkgs.ocaml.version}";

  # Define a shell hook to run commands when entering the shell
  shellHook = ''
    echo "Entering OCaml Strategy DSL development shell."
    echo "OCaml version: $(ocaml -version)"
    echo "Dune version: $(dune --version)"
    echo "Menhir version: $(menhir --version)"
    echo "Ocamlllex version: $(ocamlllex -version)"
    echo "Protoc version: $(protoc --version)"
    # echo "Ocaml-protoc path: ${ocamlProtoc}"
    # echo "Ocaml-protoc-plugin path: ${ocamlProtocPlugin}"

    echo ""
    echo "To build the project: dune build"
    echo "To run the executable: dune exec ./src/main.exe -- --config config/settings.yml" # Example with config
    echo "Ensure your opam2nix definitions in opam-packages.nix are up-to-date if dependencies change."
    echo "Remember to run 'opam2nix generate' in the directory containing your .opam files whenever you change dependencies in dune-project."
  '';