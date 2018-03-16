{ mkDerivation, base, contravariant, criterion, deepseq, directory
, discrimination, ghc-prim, hashable, hspec, htoml, HUnit, pipes
, pipes-bytestring, pipes-group, pipes-parse, pipes-safe
, pipes-text, pretty, primitive, readable, regex-applicative
, stdenv, template-haskell, temporary, text, transformers
, unordered-containers, vector, vinyl, th-utilities
, Chart, Chart-diagrams, diagrams-lib, diagrams-rasterific
, foldl, http-client, list-t, microlens, statistics, zip-archive
, llvmPackages
}:
mkDerivation {
  pname = "Frames";
  version = "0.4.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base contravariant deepseq discrimination ghc-prim hashable pipes
    pipes-bytestring pipes-group pipes-parse pipes-safe pipes-text
    primitive readable template-haskell text transformers vector vinyl
  ];
  testHaskellDepends = [
    base directory hspec htoml HUnit pipes pretty regex-applicative
    template-haskell temporary text unordered-containers vinyl
    Chart Chart-diagrams diagrams-lib diagrams-rasterific
    foldl http-client list-t microlens statistics zip-archive
  ];
  doBenchmark = true;
  benchmarkHaskellDepends = [
    base criterion pipes transformers llvmPackages.llvm th-utilities
  ];
  description = "Data frames For working with tabular data files";
  license = stdenv.lib.licenses.bsd3;
}
