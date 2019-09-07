{ compiler ? "ghc865"
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
        ghc = super.ghc // { withPackages = f: super.ghc.withHoogle (ps: f ps ++ [ ps.cabal-install ]); };
          vinyl = pkgs.haskell.lib.dontBenchmark (super.callPackage ~/Projects/Vinyl {});
          # pipes-safe = super.callCabal2nix "pipes-safe" (pkgs.fetchFromGitHub {
          #   owner = "Gabriel439";
          #   repo = "Haskell-Pipes-Safe-Library";
          #   rev = "de71394ea29be8558575392e4ad66f98b3d8b5e5";
          #   sha256 = "0i2l36xwqskhm5kcdvmyfy2n7cf2i2qdsrap5nib825gq2mnv9z5";
          # }) {};
          # Chart = super.callHackage "Chart" "1.9" {};
          SVGFonts = pkgs.haskell.lib.doJailbreak (super.callHackage "SVGFonts" "1.6.0.3" {});
          # intero = pkgs.haskell.lib.dontCheck (super.callPackage ~/src/intero {});
          Chart = super.callCabal2nix "Chart" (pkgs.fetchFromGitHub {
            owner = "timbod7";
            repo = "haskell-chart";
            rev = "23e9739b80ecbb6fe70c4a7989714414f9f915c7";
            sha256 = "14irxdaa9vykf958izfsjdc2mdzm5fjrwbw7c53nfjm0vfg2qr46";
          } + "/chart") {};
          Chart-diagrams = pkgs.haskell.lib.doJailbreak (super.callCabal2nix "Chart" (pkgs.fetchFromGitHub {
            owner = "timbod7";
            repo = "haskell-chart";
            rev = "23e9739b80ecbb6fe70c4a7989714414f9f915c7";
            sha256 = "14irxdaa9vykf958izfsjdc2mdzm5fjrwbw7c53nfjm0vfg2qr46";
          } + "/chart-diagrams") {});
          diagrams-postscript = pkgs.haskell.lib.doJailbreak super.diagrams-postscript;
          ghcWithPackages = self.ghc.withPackages;
        });
      }
      else packageSet
  );
  drv = hspkgs.callPackage f {};
in
  if pkgs.lib.inNixShell then drv.env else drv
