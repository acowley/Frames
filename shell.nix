{ compiler ? "ghc844"
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
          pipes-safe = super.callCabal2nix "pipes-safe" (pkgs.fetchFromGitHub {
            owner = "Gabriel439";
            repo = "Haskell-Pipes-Safe-Library";
            rev = "de71394ea29be8558575392e4ad66f98b3d8b5e5";
            sha256 = "0i2l36xwqskhm5kcdvmyfy2n7cf2i2qdsrap5nib825gq2mnv9z5";
          }) {};
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
