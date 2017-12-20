with (import <nixpkgs> {});

haskell.lib.buildStackProject {
  name = "Frames";
  buildInputs = [ ncurses zlib llvm_39 ];
  ghc = haskell.compiler.ghc822;
}
