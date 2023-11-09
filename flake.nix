{
  description = "A Haskell package for a comonadic zipper backed by Sequence instead of a list.";
  
  inputs = {
    haskellNix.url = "github:input-output-hk/haskell.nix";
    nixpkgs.follows = "haskellNix/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils, haskellNix }:
    flake-utils.lib.eachSystem [ "x86_64-linux" "x86_64-darwin" "aarch64-darwin" ] (system:
    let
      overlays = [ haskellNix.overlay
        (final: prev: {
          seq-zipper = 
            final.haskell-nix.project' {
              src = ./.;
              compiler-nix-name = "ghc947";
              shell.tools = {
                cabal = {};
                hlint = {};
                haskell-language-server = {};
              };
              shell.buildInputs = with pkgs; [
                # Optional: Only needed to make use of ./justfile
                just

                # Optional: Only needed to make use of ./dev/cabal-gen-docs.sh
                coreutils
              ];
            };
        })
      ];
      pkgs = import nixpkgs { inherit system overlays; inherit (haskellNix) config; };
      flake = pkgs.seq-zipper.flake {
      };
    in flake // {
      packages.default = flake.packages."seq-zipper:lib:seq-zipper";
    });

  nixConfig = {
    extra-substituters = ["https://cache.iog.io"];
    extra-trusted-public-keys = ["hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="];
    allow-import-from-derivation = "true";
  };
}
