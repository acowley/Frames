{
  description = "Data frames for tabular data.";

  inputs = {
    flake-utils.url = "github:numtide/flake-utils";
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let pkg-name = "Frames";
          pkgs = import nixpkgs {
            inherit system;
            config = { allowBroken = true; };
          };
          haskell = pkgs.haskellPackages;
          haskell-overlay = final: prev: 
            let overrideSrc = pkgs.haskell.lib.overrideSrc;
            in {
              ${pkg-name} = hspkgs.callCabal2nix pkg-name ./. {};
              # Add here any package overrides you may need
            };
          hspkgs = haskell.override {
            overrides = haskell-overlay;
          };
      in {
        packages = pkgs;
        apps.init = pkgs.writeShellApplication {
          name = "cabal-init";
          runtimeInputs = [hspkgs.ghc hspkgs.cabal-install];
          text = ''
            cabal init -p ${pkg-name}
          '';
        };
        inherit haskell-overlay;
        defaultPackage = hspkgs.${pkg-name};
        devShell = hspkgs.shellFor {
          packages = p: [p.${pkg-name}];
          root = ./.;
          withHoogle = true;
          buildInputs = with hspkgs; [
            haskell-language-server
            cabal-install
          ];
        };
      }
    );
}
