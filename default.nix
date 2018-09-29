{ mkDerivation, base, contravariant, criterion, deepseq, directory
, discrimination, ghc-prim, hashable, hspec, htoml, HUnit, pipes
, pipes-bytestring, pipes-group, pipes-parse, pipes-safe
, pretty, primitive, readable, regex-applicative
, stdenv, template-haskell, temporary, text, transformers
, unordered-containers, vector, vector-th-unbox, vinyl
, Chart, Chart-diagrams, diagrams-lib, diagrams-rasterific
, foldl, http-client, list-t, microlens, statistics, zip-archive
, llvmPackages, attoparsec
}:
mkDerivation {
  pname = "Frames";
  version = "0.5.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base contravariant deepseq discrimination ghc-prim hashable pipes
    pipes-bytestring pipes-group pipes-parse pipes-safe text vinyl
    primitive readable template-haskell transformers vector vector-th-unbox
  ];
  testHaskellDepends = [
    base directory hspec htoml HUnit pipes pretty regex-applicative
    template-haskell temporary text unordered-containers vinyl
    Chart Chart-diagrams diagrams-lib diagrams-rasterific attoparsec
    foldl http-client list-t microlens statistics zip-archive
  ];
  doBenchmark = true;
  benchmarkHaskellDepends = [
    base criterion pipes transformers llvmPackages.llvm
  ];
  description = "Data frames For working with tabular data files";
  license = stdenv.lib.licenses.bsd3;
}
