{
  description = "Data frames for tabular data.";

  nixConfig = {
    substituters = [
        "https://haskell-language-server.cachix.org"
    ];
    trusted-public-keys = [
      "haskell-language-server.cachix.org-1:juFfHrwkOxqIOZShtC4YC1uT1bBcq2RSvC7OMKx0Nz8="
    ];
  };

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    hls.url = "github:haskell/haskell-language-server";

    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };
  };

  outputs = { self, nixpkgs, hls, flake-utils, ... }@inputs:
    flake-utils.lib.eachSystem ["x86_64-linux"] (system: let

      compiler = "8107";
      pkgs = import nixpkgs {
        inherit system;
        config = { allowUnfree = true; allowBroken = true; };
      };
      hspkgs = (pkgs.haskell.packages."ghc${compiler}").override {
        overrides = self: super: {
          statestack = pkgs.haskell.lib.doJailbreak super.statestack;
          svg-builder = pkgs.haskell.lib.doJailbreak super.svg-builder;
          size-based = pkgs.haskell.lib.doJailbreak super.size-based;
          monoid-extras = pkgs.haskell.lib.doJailbreak super.monoid-extras;
          active = pkgs.haskell.lib.doJailbreak super.active;
          dual-tree = pkgs.haskell.lib.doJailbreak super.dual-tree;
          diagrams-core = pkgs.haskell.lib.doJailbreak super.diagrams-core;
          diagrams-lib = pkgs.haskell.lib.doJailbreak super.diagrams-lib;
          diagrams-postscript = pkgs.haskell.lib.doJailbreak super.diagrams-postscript;
          SVGFonts = pkgs.haskell.lib.doJailbreak super.SVGFonts;
          diagrams-svg = pkgs.haskell.lib.doJailbreak super.diagrams-svg;
          diagrams-rasterific = pkgs.haskell.lib.doJailbreak super.diagrams-rasterific;
        };
      };
      drv = hspkgs.callPackage ./default.nix {};
      ghc = hspkgs.ghc.withHoogle (ps: drv.passthru.getBuildInputs.haskellBuildInputs);

  in {

    devShell = pkgs.mkShell {
      buildInputs = [
        ghc
        hls.packages.${system}."haskell-language-server-${compiler}"
        hspkgs.cabal-install
        # pkgs.llvmPackages_7.llvm
        pkgs.llvmPackages_latest.llvm
      ];
  # shellHook = ''
  #   source <(grep '^export NIX_' ${ghc}/bin/ghc)
  #   source <(echo 'export HIE_HOOGLE_DATABASE='$(grep -F -- '--database' ${ghc}/bin/hoogle | sed 's/.* --database \(.*\.hoo\).*/\1/'))
  # '';
    };
  });
}
