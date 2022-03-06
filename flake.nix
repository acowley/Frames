{
  description = "Data frames for tabular data.";

  nixConfig = {
    extra-substituters = [
        "https://haskell-language-server.cachix.org"
    ];
    extra-trusted-public-keys = [
      "haskell-language-server.cachix.org-1:juFfHrwkOxqIOZShtC4YC1uT1bBcq2RSvC7OMKx0Nz8="
    ];
  };

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    hls.url = "github:haskell/haskell-language-server";
  };

  outputs = { self, nixpkgs, hls, flake-utils}:

    flake-utils.lib.eachDefaultSystem (system: let

      pkgs = import nixpkgs {
        inherit system;
        overlays = [ self.overlay ];
      };

      compilerVersionFromHsPkgs = hsPkgs:
        pkgs.lib.replaceStrings [ "." ] [ "" ] hsPkgs.ghc.version;

      hspkgs810 = pkgs.haskell.packages."ghc8107".override {
        overrides = pkgs.frameHaskellOverlay-8107;
      };
      hspkgs92 = pkgs.haskell.packages."ghc921".override {
        overrides = pkgs.frameHaskellOverlay-921;
      };

      mkPackage = hspkgs:
          hspkgs.developPackage {
            root =  pkgs.lib.cleanSource ./.;
            name = "Frames";
            returnShellEnv = false;
            withHoogle = true;
          };

      mkShell = hspkgs:
        let
          compilerVersion = compilerVersionFromHsPkgs hspkgs;
          myModifier = drv:
            pkgs.haskell.lib.addBuildTools drv (with hspkgs; [
              cabal-install
              ghcid
              hls.packages.${system}."haskell-language-server-${compilerVersion}"
              hasktags
            ]);
        in
        (myModifier (mkPackage hspkgs)).envFunc {};

      mkSimpleShell = compilerVersion:
        let
          compiler = pkgs.haskell.compiler."ghc${compilerVersion}";
        in
          pkgs.mkShell {
            buildInputs = [
              pkgs.haskell.compiler."ghc${compilerVersion}"
              pkgs.haskell.packages."ghc${compilerVersion}".cabal-install
              pkgs.llvmPackages_latest.llvm
            ] ++
            pkgs.lib.optional (compilerVersion != "921")
              hls.packages.${system}."haskell-language-server-${compilerVersion}";
          };
  in {
    packages = {
      Frames-8107 = mkPackage hspkgs810;
      Frames-921 = mkPackage hspkgs92;
    };

    devShell = mkSimpleShell "921";

    devShells = {
      Frames-8107 = mkShell hspkgs810;
      Frames-921 = mkShell hspkgs92;
    };
  }) // {

    overlay = final: prev: {
      frameHaskellOverlay-921 = hfinal: hprev: (
        (final.frameHaskellOverlay-8107 hfinal hprev) // (
        let doJailbreak = prev.haskell.lib.doJailbreak;
            overrideSrc = prev.haskell.lib.overrideSrc;
            dontHaddock = prev.haskell.lib.dontHaddock;
            dontCheck = prev.haskell.lib.dontCheck;
        in {
          # Temporary fixes for breakage with ghc-9.2.1
          attoparsec = dontCheck hprev.attoparsec;
          base-compat-batteries = dontCheck hprev.base-compat-batteries;
          basement = dontHaddock hprev.basement;
          blaze-builder = dontCheck hprev.blaze-builder;
          blaze-markup = dontCheck hprev.blaze-markup;
          case-insensitive = dontCheck hprev.case-insensitive;
          cassava = dontCheck hprev.cassava;
          conduit-extra = dontCheck hprev.conduit-extra;
          criterion = dontCheck hprev.criterion;
          cryptonite = dontHaddock hprev.cryptonite;
          fast-logger = dontCheck hprev.fast-logger;
          htoml = dontCheck hprev.htoml;
          lens-family-core = dontHaddock hprev.lens-family-core;
          ListLike = dontCheck hprev.ListLike;
          microlens = dontHaddock hprev.microlens;
          microstache = dontCheck hprev.microstache;
          readable = dontHaddock (doJailbreak hprev.readable);
          QuickCheck = dontCheck hprev.QuickCheck;
          operational = dontHaddock hprev.operational;
          optparse-applicative = dontCheck hprev.optparse-applicative;
          generic-deriving = dontHaddock hprev.generic-deriving;
          streaming-commons = dontCheck hprev.streaming-commons;
          utf8-string = dontCheck hprev.utf8-string;
          word8 = dontCheck hprev.word8;
        }));

      frameHaskellOverlay-8107 = hfinal: hprev:
        let doJailbreak = prev.haskell.lib.doJailbreak;
            overrideSrc = prev.haskell.lib.overrideSrc;
            dontHaddock = prev.haskell.lib.dontHaddock;
            dontCheck = prev.haskell.lib.dontCheck;
        in {
          statestack = doJailbreak hprev.statestack;
          svg-builder = doJailbreak hprev.svg-builder;
          # see https://github.com/JonasDuregard/sized-functors/pull/10
          # https://github.com/
          size-based = doJailbreak (overrideSrc hprev.size-based {
            version = "unstable-2022-01-20";
            src = final.fetchzip {
              url = "https://github.com/byorgey/sized-functors/archive/master.tar.gz";
              sha256 = "sha256-pVJbEGF4/lvXmWIypwkMQBYygOx3TQwLJbMpfdYovdY=";
            };
          });
          monoid-extras = doJailbreak hprev.monoid-extras;
          active = doJailbreak hprev.active;
          dual-tree = doJailbreak hprev.dual-tree;
          diagrams-core = doJailbreak hprev.diagrams-core;
          diagrams-lib = doJailbreak hprev.diagrams-lib;
          diagrams-postscript = doJailbreak hprev.diagrams-postscript;
          SVGFonts = doJailbreak hprev.SVGFonts;
          diagrams-svg = doJailbreak hprev.diagrams-svg;
          diagrams-rasterific = doJailbreak hprev.diagrams-rasterific;
          Chart = doJailbreak hprev.Chart;
          linear = hprev.callHackage "linear" "1.21.8" {};
          vinyl = overrideSrc hprev.vinyl {
            version = "0.14.1";
            src = prev.fetchFromGitHub {
              owner = "VinylRecords";
              repo = "Vinyl";
              rev = "892d597f9dd8e96c0853269ab78141ae2e03aa2c";
              hash = "sha256-ONw+8D1r4xX9+KgYOFpTNhk+pCsNZW8DbbAzOheSkS0=";
            };
          };
          readable = doJailbreak hprev.readable;
        };
    };
  };
}
