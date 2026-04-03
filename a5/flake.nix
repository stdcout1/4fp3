# This file is intended for NixOS users.
#
# If you are not running NixOS, then this file is not relevant,
# and feel free to ignore it!
{
  inputs = {
    nixpkgs = {
      url = "github:NixOS/nixpkgs/nixos-25.11";
    };

    flake-parts = {
      url = "github:hercules-ci/flake-parts";
    };
  };

  outputs = inputs: inputs.flake-parts.lib.mkFlake { inputs = inputs; } {
    systems = [ "x86_64-linux" "aarch64-linux" "aarch64-darwin" "x86_64-darwin" ];
    perSystem = { system, pkgs, ... }: let
      hlib = pkgs.haskell.lib.compose;
      hpkgs = pkgs.haskell.packages.ghc912;
      a5 = hpkgs.developPackage {
        root = ./.;
      };
    in {
      packages = {
        default = a5;
      };
      devShells = {
        default = hpkgs.shellFor {
          packages = h: [a5];
          nativeBuildInputs = [
            pkgs.cabal-install
            hpkgs.haskell-language-server
          ];
        };
      };
    };
  };
}
