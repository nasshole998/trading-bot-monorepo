# trading-bot-monorepo/ui-sveltekit-typescript/frontend/shell.nix
# Development environment for the SvelteKit frontend.
# Enter with `nix-shell` or `nix develop .#ui-sveltekit-typescript-frontend` from monorepo root.

{ pkgs ? import <nixpkgs> {} }:

pkgs.mkShell {
  # Build inputs needed for SvelteKit development
  buildInputs = [
    pkgs.nodejs_20 # Specify Node.js version 20
    pkgs.npm # npm package manager
    # Or pkgs.yarn or pkgs.pnpm if preferred
  ];

  # Define shell hook
  shellHook = ''
    echo "Entering SvelteKit Frontend development shell."
    echo "Node.js version: $(node --version)"
    echo "npm version: $(npm --version)"
    echo ""
    echo "To install dependencies: npm install"
    echo "To run development server: npm run dev"
    echo "To build for production: npm run build"
    echo "Ensure BFF is running for graphql-codegen and data fetching."
    echo "To generate GraphQL code (requires BFF running on 8080): npm run graphql-codegen"
    echo ""
  '';
}