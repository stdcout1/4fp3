# This file is intended for NixOS users.
#
# If you are not running NixOS, then this file is not relevant,
# and feel free to ignore it!
{
  inputs = {
    nixpkgs = {
      url = "github:nixos/nixpkgs/nixos-25.05";
    };

    flake-parts = {
      url = "github:hercules-ci/flake-parts";
    };
  };

  outputs = inputs: inputs.flake-parts.lib.mkFlake { inputs = inputs; } {
    systems = [ "x86_64-linux" "aarch64-linux" "aarch64-darwin" "x86_64-darwin" ];
    perSystem = { system, pkgs, ... }: let
      hlib = pkgs.haskell.lib.compose;
      hpkgs = pkgs.haskell.packages.ghc984;
      a3 = hpkgs.developPackage {
        root = ./.;
      };
    in {
      packages = {
        default = a3;
      };
      devShells = {
        default = hpkgs.shellFor {
          packages = h: [a3];
          nativeBuildInputs = [
            pkgs.cabal-install
            pkgs.haskell-language-server
          ];
        };
      };
    };
  };
}
