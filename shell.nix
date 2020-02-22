{ compiler ? "ghc865"
, withHoogle ? true
, sources ? import ./nix/sources.nix
}:
let
  pkgs = import sources.nixpkgs {};
  ghcide = (import (builtins.fetchTarball "https://github.com/hercules-ci/ghcide-nix/archive/caab5c37a80c4b706ebdc5b50ad610cc96d6b9e2.tar.gz") {}).ghcide-ghc865;
  packageSet = pkgs.haskell.packages.${compiler};
  overrideByVersion = if compiler == "ghc882"
                      then super: {
                        haskell-src = super.callHackage "haskell-src" "1.0.3.1" {};
                        statestack = super.callHackage "statestack" "0.3" {};
                        bytes = super.callHackage "bytes" "0.16" {};
                        list-t = pkgs.haskell.lib.dontCheck super.list-t;
                        Chart = pkgs.haskell.lib.doJailbreak super.Chart;
                        active = pkgs.haskell.lib.doJailbreak super.active;
                        diagrams-core = pkgs.haskell.lib.doJailbreak super.diagrams-core;
                        diagrams-lib = pkgs.haskell.lib.doJailbreak super.diagrams-lib;
                        diagrams-svg = pkgs.haskell.lib.doJailbreak super.diagrams-svg;
                        diagrams-postscript = pkgs.haskell.lib.doJailbreak super.diagrams-postscript;
                        diagrams-rasterific = pkgs.haskell.lib.doJailbreak super.diagrams-rasterific;
                        Chart-diagrams = pkgs.haskell.lib.doJailbreak super.Chart-diagrams;
                      } else (_: {});
  hspkgs = packageSet.override {
    overrides = self: super: {
      vinyl = pkgs.haskell.lib.dontBenchmark (super.callPackage ~/Projects/Vinyl {});
      cabal2nix = pkgs.haskell.lib.dontCheck super.cabal2nix;
    } // overrideByVersion super;
  };
  drv = hspkgs.callPackage ./default.nix {};
  ghc = hspkgs.ghc.withHoogle (ps: drv.passthru.getBuildInputs.haskellBuildInputs ++ [ps.cabal-install]);
in
pkgs.mkShell {
  buildInputs = [ ghcide
                  ghc
                  pkgs.cabal2nix
                  pkgs.llvmPackages_7.llvm
  ];
  shellHook = ''
    source <(grep '^export NIX_' ${ghc}/bin/ghc)
    source <(echo 'export HIE_HOOGLE_DATABASE='$(grep -F -- '--database' ${ghc}/bin/hoogle | sed 's/.* --database \(.*\.hoo\).*/\1/'))
  '';
}
