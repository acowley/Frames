with (import <nixpkgs> {}).pkgs;
with (import <nixpkgs/pkgs/development/haskell-modules/lib.nix> { pkgs = import <nixpkgs> {}; });
let
    hsPkgs = haskellngPackages.override {
      overrides = self: super: {
        Rasterific = dontCheck (super.Rasterific);
        readable = overrideCabal (super.readable.overrideScope (self: super: { }))(drv: {
           version = "0.2.0.2";
           sha256 = "0dv1xr4y5azcr8xnhsl7i8ab56mkq7b89x55c2rg9kfakmgxiqcl";
           });
      };
    };
    pkg = hsPkgs.callPackage
            ({ mkDerivation, base, ghc-prim, pipes, readable, stdenv
             , template-haskell, text, transformers, vector, vinyl
             , cairo, diagrams, diagrams-rasterific, Chart, Chart-diagrams
             , lens, lens-family, foldl, list-t, http-client, statistics, zip-archive
             }:
             mkDerivation {
               pname = "Frames";
               version = "0.1.0.0";
               src = ./.;
               isLibrary = true;
               isExecutable = true;
               buildDepends = [
                 base ghc-prim pipes readable template-haskell text transformers
                 vector vinyl
                 cairo diagrams diagrams-rasterific Chart Chart-diagrams
                 lens lens-family foldl list-t http-client statistics zip-archive
               ];
               description = "Data frames For working with tabular data files";
               license = stdenv.lib.licenses.bsd3;
             }) {};
in
  pkg.env
