{
  description = "Data frames for tabular data.";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";

    flake-utils.url = "github:numtide/flake-utils";

    hls.url = "github:haskell/haskell-language-server";
  };

  outputs = { self, nixpkgs, flake-utils, ... }@inputs:
    flake-utils.lib.eachSystem ["x86_64-linux"] (system: let

      compiler = "ghc8104";
      pkgs = import nixpkgs {
        inherit system;
        config = { allowUnfree = true; allowBroken = true; };
      };
  in {

    devShell = import ./shell.nix { inherit pkgs; inherit compiler; };

  });
}
