{ compiler ? "ghc865"
, withHoogle ? true
}:

let
  pkgs = import <nixpkgs> {};
  all-hies = import (fetchTarball "https://github.com/infinisil/all-hies/tarball/master") {};
  hie = all-hies.selection { selector = p: { inherit (p) ghc865; };};
  f = import ./default.nix;
packageSet = pkgs.haskell.packages.${compiler};
  hspkgs = (
    if withHoogle then
      packageSet.override {
        overrides = (self: super: {
          ghc = super.ghc // { withPackages = f: super.ghc.withHoogle (ps: f ps ++ [ ]); };
          # Cabal = super.Cabal_2_4_1_0;
          vinyl = pkgs.haskell.lib.dontBenchmark (super.callPackage ~/Projects/Vinyl {});
          # pipes-safe = super.callCabal2nix "pipes-safe" (pkgs.fetchFromGitHub {
          #   owner = "Gabriel439";
          #   repo = "Haskell-Pipes-Safe-Library";
          #   rev = "de71394ea29be8558575392e4ad66f98b3d8b5e5";
          #   sha256 = "0i2l36xwqskhm5kcdvmyfy2n7cf2i2qdsrap5nib825gq2mnv9z5";
          # }) {};
          # Chart = super.callHackage "Chart" "1.9" {};
          cabal-install-2_4 = super.callHackage "cabal-install" "2.4.1.0" {
            Cabal = super.Cabal_2_4_1_0;
          };
          hackage-security = pkgs.haskell.lib.dontCheck (super.callHackage "hackage-security" "0.5.3.0" {
            Cabal = super.Cabal_2_4_1_0;
          });
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
  ghc = hspkgs.ghc.withHoogle (_: drv.passthru.getBuildInputs.haskellBuildInputs);
in
# if pkgs.lib.inNixShell then drv.env else drv
pkgs.mkShell {
  buildInputs = [ hie # hspkgs.cabal-install
                  hspkgs.cabal-install-2_4
                  ghc ];
  shellHook = ''
    export NIX_GHC='${ghc}/bin/ghc'
    export NIX_GHCPKG='${ghc}/bin/ghc-pkg'
    export NIX_GHC_DOCDIR='${drv.compiler.doc}/share/doc/ghc/html'
    export NIX_GHC_LIBDIR='${ghc}/lib/${drv.compiler.name}'
    export HIE_HOOGLE_DATABASE='${ghc}/share/doc/hoogle/default.hoo'
  '';
}
