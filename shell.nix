{ compiler ? null
, withHoogle ? true
, sources ? import ./nix/sources.nix
}:
let
  pkgs = import sources.nixpkgs-chan {};
  overrideByVersion = if compiler == "ghc8101"
                      then self: super: { }
                      else self: super: { };

  hspkgs = (if isNull compiler 
            then pkgs.haskellPackages
            else pkgs.haskell.packages.${compiler}).override {
    overrides = self: super: {
      vinyl = pkgs.haskell.lib.dontBenchmark (super.callPackage ~/Projects/Vinyl {});
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
    } // overrideByVersion self super;
  };
  drv = hspkgs.callPackage ./default.nix {};
  ghc = hspkgs.ghc.withHoogle (ps: drv.passthru.getBuildInputs.haskellBuildInputs);
in
pkgs.mkShell {
  buildInputs = [ ghc
                  hspkgs.haskell-language-server
                  hspkgs.cabal-install
                  # pkgs.llvmPackages_7.llvm
                  pkgs.llvmPackages_latest.llvm
                ]
  # hspkgs.ghcide
  ;
  # shellHook = ''
  #   source <(grep '^export NIX_' ${ghc}/bin/ghc)
  #   source <(echo 'export HIE_HOOGLE_DATABASE='$(grep -F -- '--database' ${ghc}/bin/hoogle | sed 's/.* --database \(.*\.hoo\).*/\1/'))
  # '';
}
