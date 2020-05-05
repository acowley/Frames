{ compiler ? "ghc883"
, withHoogle ? true
, sources ? import ./nix/sources.nix
}:
let
  pkgs = import sources.nixpkgs {};
  overrideByVersion = if compiler == "ghc8101"
                      then self: super: 
                        with {inherit (pkgs.haskell.lib) doJailbreak dontCheck;}; {
                          list-t = dontCheck super.list-t;
                          active = doJailbreak super.active;
                          diagrams-core = dontCheck (doJailbreak super.diagrams-core);
                          diagrams-lib = dontCheck (doJailbreak super.diagrams-lib);
                          diagrams-rasterific = doJailbreak super.diagrams-rasterific;
                          diagrams-solve = doJailbreak super.diagrams-solve;
                          diagrams-svg = doJailbreak super.diagrams-svg;
                          diagrams-postscript = doJailbreak super.diagrams-postscript;
                          haskell-src = doJailbreak super.haskell-src;
                          monoid-extras = doJailbreak super.monoid-extras;
                          size-based = doJailbreak super.size-based;
                          statestack = doJailbreak super.statestack;
                          svg-builder = doJailbreak super.svg-builder;
                          dual-tree = doJailbreak super.dual-tree;
                        }
                      else # compiler == "ghc883"
                        self: super: {
                          list-t = pkgs.haskell.lib.dontCheck super.list-t;
                          hie-bios = pkgs.haskell.lib.dontCheck super.hie-bios;
                          ghcide = pkgs.haskell.lib.dontCheck (super.callCabal2nix "ghcide" sources.ghcide {
                            haskell-lsp-types = self.haskell-lsp-types_0_21;
                            haskell-lsp = self.haskell-lsp_0_21;
                            ghc-check = self.ghc-check_0_1_0_3;
                          });
                          ghc-check_0_1_0_3 = super.callHackage "ghc-check" "0.1.0.3" {};
                          haskell-lsp-types_0_21 = super.callCabal2nix "haskell-lsp-types" (sources.haskell-lsp + "/haskell-lsp-types") {};        
                          haskell-lsp_0_21 = super.callCabal2nix "haskell-lsp" sources.haskell-lsp {
                            haskell-lsp-types = self.haskell-lsp-types_0_21;
                          };
                      };

  hspkgs = pkgs.haskell.packages.${compiler}.override {
    overrides = self: super: {
      vinyl = pkgs.haskell.lib.dontBenchmark (super.callPackage ~/Projects/Vinyl {});
    } // overrideByVersion self super;
  };
  drv = hspkgs.callPackage ./default.nix {};
  ghc = hspkgs.ghc.withHoogle (ps: drv.passthru.getBuildInputs.haskellBuildInputs);
in
pkgs.mkShell {
  buildInputs = [ ghc
                  hspkgs.cabal-install
                  pkgs.llvmPackages_7.llvm
  ] ++ pkgs.lib.optional (compiler == "ghc883") hspkgs.ghcide;
  shellHook = ''
    source <(grep '^export NIX_' ${ghc}/bin/ghc)
    source <(echo 'export HIE_HOOGLE_DATABASE='$(grep -F -- '--database' ${ghc}/bin/hoogle | sed 's/.* --database \(.*\.hoo\).*/\1/'))
  '';
}
