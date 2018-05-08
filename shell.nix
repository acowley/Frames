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
      ghc = super.ghc // { withPackages = f: super.ghc.withHoogle (ps: f ps ++ [ps.intero ps.cabal-install]); };
          vinyl = super.callHackage "vinyl" "0.8.1.1" {};
          pipes-group = pkgs.haskell.lib.dontCheck super.pipes-group;
          diagrams-svg = pkgs.haskell.lib.doJailbreak super.diagrams-svg;
          diagrams-postscript = pkgs.haskell.lib.doJailbreak super.diagrams-postscript;
          diagrams-rasterific = pkgs.haskell.lib.doJailbreak super.diagrams-rasterific;
          Chart-diagrams = pkgs.haskell.lib.doJailbreak super.Chart-diagrams;
          intero = pkgs.haskell.lib.dontCheck (super.callPackage ~/src/intero {});
          ghcWithPackages = self.ghc.withPackages;
        });
      }
      else packageSet
  );
  drv = hspkgs.callPackage f {};
in
  if pkgs.lib.inNixShell then drv.env else drv
