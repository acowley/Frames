{ compiler ? "ghc843"
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
          # Chart = super.callHackage "Chart" "1.9" {};
          SVGFonts = super.callHackage "SVGFonts" "1.6.0.3" {};
          intero = pkgs.haskell.lib.dontCheck (super.callPackage ~/src/intero {});
          ghcWithPackages = self.ghc.withPackages;
        });
      }
      else packageSet
  );
  drv = hspkgs.callPackage f {};
in
  if pkgs.lib.inNixShell then drv.env else drv
