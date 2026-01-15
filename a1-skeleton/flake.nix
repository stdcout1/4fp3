# This file is intended for NixOS users.
#
# If you are not running NixOS, then this file is not relevant,
# and feel free to ignore it!
{
  inputs = {
    haskell-nix = {
      url = "github:input-output-hk/haskell.nix";
    };

    nixpkgs = {
      follows = "haskell-nix/nixpkgs-2505"; # This is known to work on x86_64 darwin.
    };

    systems = {
      url = "github:nix-systems/default";
    };
  };

  outputs = { nixpkgs, systems, haskell-nix, ... }:
    let
      eachSystem = cont: nixpkgs.lib.genAttrs (import systems) (system:
        let
          overlays = [
            haskell-nix.overlay (final: prev: {
              a1 =
                final.haskell-nix.cabalProject {
                  src = ./.;
                  compiler-nix-name = "ghc984";
                };
            })
          ];
        in cont rec {
          inherit system;
          pkgs = import nixpkgs { inherit system overlays; inherit (haskell-nix) config; };
          flake = pkgs.panbench.flake {};
        });
    in {
      packages = eachSystem({flake, ...}: {
        default = flake.packages."a1";
      });
      devShells = eachSystem({pkgs, ...}:
        {
          default = pkgs.a1.shellFor {
            tools = {
              cabal = "3.14.2.0";
              haskell-language-server = "latest";
            };
            packages = hp: [
              hp.a1
            ];
            exactDeps = true;
          };
        }
      );
    };
}
