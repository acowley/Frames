{ mkDerivation, attoparsec, base, bytestring, Chart, Chart-diagrams
, containers, contravariant, criterion, deepseq, diagrams-lib
, diagrams-rasterific, directory, discrimination, foldl, ghc-prim
, hashable, hspec, htoml, http-client, http-client-tls, HUnit, lens
, list-t, microlens, pipes, pipes-bytestring, pipes-group
, pipes-parse, pipes-safe, pretty, primitive, readable
, regex-applicative, statistics, stdenv, template-haskell, temporary
, text, transformers, unordered-containers, vector, vector-th-unbox
, vinyl, zip-archive
}:
mkDerivation {
  pname = "Frames";
  version = "0.7.1";
  src = ./.;
  configureFlags = [ "-fdemos" ];
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base bytestring containers contravariant deepseq discrimination
    ghc-prim hashable pipes pipes-bytestring pipes-group pipes-parse
    pipes-safe primitive readable template-haskell text transformers
    vector vector-th-unbox vinyl
  ];
  executableHaskellDepends = [
    base bytestring Chart Chart-diagrams containers diagrams-lib
    diagrams-rasterific directory foldl ghc-prim http-client http-client-tls
    list-t microlens pipes pipes-safe readable statistics template-haskell
    text transformers vector vinyl zip-archive
  ];
  testHaskellDepends = [
    attoparsec base directory foldl hspec htoml HUnit lens pipes pretty
    regex-applicative template-haskell temporary text
    unordered-containers vinyl
  ];
  benchmarkHaskellDepends = [ base criterion pipes transformers ];
  doBenchmark = true;
  description = "Data frames For working with tabular data files";
  license = stdenv.lib.licenses.bsd3;
}
