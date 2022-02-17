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

      compiler = "8107";
      # compiler = "921";
      pkgs = import nixpkgs {
        inherit system;
        overlays = [ self.overlay ];
        config = { allowUnfree = true; allowBroken = true; };
      };

      hspkgs = (pkgs.haskell.packages."ghc${compiler}").override {
        overrides = pkgs.frameHaskellOverlay;
      };

      drv = hspkgs.callPackage ./default.nix {};
      ghc = hspkgs.ghc.withHoogle (ps: drv.passthru.getBuildInputs.haskellBuildInputs);

      # modifier used in haskellPackages.developPackage
      myModifier = drv:
        pkgs.haskell.lib.addBuildTools drv (with hspkgs; [
          cabal-install
          ghcid
          hls.packages.${system}."haskell-language-server-${compiler}"
          hasktags
        ]);

  in {
    packages = {
      Frames = hspkgs.developPackage {
        root =  pkgs.lib.cleanSource ./.;
        name = "Frames";
        returnShellEnv = false;
        withHoogle = true;
        overrides = pkgs.frameHaskellOverlay;
        modifier = myModifier;
      };
    };

    devShell = pkgs.mkShell {
      buildInputs = [
        ghc
        hls.packages.${system}."haskell-language-server-${compiler}"
        hspkgs.cabal-install
        pkgs.llvmPackages_latest.llvm
      ];
  # shellHook = ''
  #   source <(grep '^export NIX_' ${ghc}/bin/ghc)
  #   source <(echo 'export HIE_HOOGLE_DATABASE='$(grep -F -- '--database' ${ghc}/bin/hoogle | sed 's/.* --database \(.*\.hoo\).*/\1/'))
  # '';
    };
    overlay = final: prev: {
      frameHaskellOverlay = hfinal: hprev: with prev.haskell.lib; {
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
          src = prev.fetchFromGitHub {
            owner = "VinylRecords";
            repo = "Vinyl";
            rev = "892d597f9dd8e96c0853269ab78141ae2e03aa2c";
            hash = "sha256-ONw+8D1r4xX9+KgYOFpTNhk+pCsNZW8DbbAzOheSkS0=";
          };
        } // if compiler == "921" then {
          basement = dontHaddock hprev.basement;
          lens-family-core = dontHaddock super.lens-family-core;
          microlens = dontHaddock super.microlens;
          readable = dontHaddock (doJailbreak super.readable);
          QuickCheck = dontCheck super.QuickCheck;
          operational = dontHaddock super.operational;
          optparse-applicative = dontCheck super.optparse-applicative;
          generic-deriving = dontHaddock super.generic-deriving;
        } else {
          readable = doJailbreak hprev.readable;
        };
      };
    }
  })#  // {
  #   overlay = final: prev: {
  #     frameHaskellOverlay = hfinal: hprev: with prev.haskell.lib; {
  #       statestack = doJailbreak hprev.statestack;
  #       svg-builder = doJailbreak hprev.svg-builder;
  #       # see https://github.com/JonasDuregard/sized-functors/pull/10 
  #       # https://github.com/
  #       size-based = doJailbreak (overrideSrc hprev.size-based {
  #         version = "unstable-2022-01-20";
  #         src = final.fetchzip {
  #           url = "https://github.com/byorgey/sized-functors/archive/master.tar.gz";
  #           sha256 = "sha256-pVJbEGF4/lvXmWIypwkMQBYygOx3TQwLJbMpfdYovdY=";
  #         };
  #       });
  #       monoid-extras = doJailbreak hprev.monoid-extras;
  #       active = doJailbreak hprev.active;
  #       dual-tree = doJailbreak hprev.dual-tree;
  #       diagrams-core = doJailbreak hprev.diagrams-core;
  #       diagrams-lib = doJailbreak hprev.diagrams-lib;
  #       diagrams-postscript = doJailbreak hprev.diagrams-postscript;
  #       SVGFonts = doJailbreak hprev.SVGFonts;
  #       diagrams-svg = doJailbreak hprev.diagrams-svg;
  #       diagrams-rasterific = doJailbreak hprev.diagrams-rasterific;
  #       Chart = doJailbreak hprev.Chart;
  #       linear = hprev.callHackage "linear" "1.21.8" {};
  #       vinyl = overrideSrc hprev.vinyl {
  #         src = prev.fetchFromGitHub {
  #           owner = "VinylRecords";
  #           repo = "Vinyl";
  #           rev = "892d597f9dd8e96c0853269ab78141ae2e03aa2c";
  #           hash = "sha256-ONw+8D1r4xX9+KgYOFpTNhk+pCsNZW8DbbAzOheSkS0=";
  #         };
  #       } // if compiler == "921" then {
  #         basement = dontHaddock hprev.basement;
  #         lens-family-core = dontHaddock super.lens-family-core;
  #         microlens = dontHaddock super.microlens;
  #         readable = dontHaddock (doJailbreak super.readable);
  #         QuickCheck = dontCheck super.QuickCheck;
  #         operational = dontHaddock super.operational;
  #         optparse-applicative = dontCheck super.optparse-applicative;
  #         generic-deriving = dontHaddock super.generic-deriving;
  #       } else {
  #         readable = doJailbreak hprev.readable;
  #       };
  #     };
  #   };
  # }
  ;
}
