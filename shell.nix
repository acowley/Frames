{ compiler ? "ghc822"
, withHoogle ? true
}:

let
  pkgs = import <nixpkgs> {};
  f = import ./default.nix;
packageSet = pkgs.haskell.packages.${compiler};
  hspkgs = (
    if withHoogle then
      packageSet.override {
        overrides = (self: super: {
      ghc = super.ghc // { withPackages = f: super.ghc.withHoogle (ps: f ps ++ [ps.intero ps.cabal-install ]); };
          vinyl = super.callPackage ~/Projects/Vinyl {};
          diagrams-core = super.callPackage ./diagrams-core.nix {};
          diagrams-lib = pkgs.haskell.lib.doJailbreak super.diagrams-lib;
          diagrams-svg = pkgs.haskell.lib.doJailbreak super.diagrams-svg;
          diagrams-rasterific = pkgs.haskell.lib.doJailbreak super.diagrams-rasterific;
          diagrams-postscript = pkgs.haskell.lib.doJailbreak super.diagrams-postscript;
          pipes-group = pkgs.haskell.lib.dontCheck super.pipes-group;
          intero = pkgs.haskell.lib.dontCheck (super.callPackage ~/src/intero {});
          ghcWithPackages = self.ghc.withPackages;
        });
      }
      else packageSet
  );
  drv = hspkgs.callPackage f {};
in
  if pkgs.lib.inNixShell then drv.env else drv
