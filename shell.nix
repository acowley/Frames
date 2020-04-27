{ compiler ? "ghc883"
, withHoogle ? true
, sources ? import ./nix/sources.nix
}:
let
  pkgs = import sources.nixpkgs {};
  overrideByVersion = if compiler == "ghc883"
                      then self: super: {
                        list-t = pkgs.haskell.lib.dontCheck super.list-t;
                        hie-bios = pkgs.haskell.lib.dontCheck super.hie-bios;
                        ghcide = pkgs.haskell.lib.dontCheck (super.callCabal2nix "ghcide" sources.ghcide {
                          haskell-lsp-types = self.haskell-lsp-types_0_21;
                          haskell-lsp = self.haskell-lsp_0_21;
                        });
                        haskell-lsp-types_0_21 = super.callCabal2nix "haskell-lsp-types" (sources.haskell-lsp + "/haskell-lsp-types") {};        
                        haskell-lsp_0_21 = super.callCabal2nix "haskell-lsp" sources.haskell-lsp {
                          haskell-lsp-types = self.haskell-lsp-types_0_21;
                        };
                      } else (_: _: {});

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
                  hspkgs.ghcide
                  hspkgs.cabal-install
                  pkgs.llvmPackages_7.llvm
  ];
  shellHook = ''
    source <(grep '^export NIX_' ${ghc}/bin/ghc)
    source <(echo 'export HIE_HOOGLE_DATABASE='$(grep -F -- '--database' ${ghc}/bin/hoogle | sed 's/.* --database \(.*\.hoo\).*/\1/'))
  '';
}
