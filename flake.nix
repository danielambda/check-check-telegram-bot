{
  description = "Check-Check backend flake";
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    systems.url = "github:nix-systems/default";
    flake-utils = {
      url = "github:numtide/flake-utils";
      inputs.systems.follows = "systems";
    };
  };

  outputs = { nixpkgs, flake-utils, ... }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        hPkgs = pkgs.haskell.packages.ghc984;

        packages = [
          pkgs.nixd

          hPkgs.ghc
          hPkgs.ghcid
          hPkgs.haskell-language-server
          pkgs.stack

          pkgs.zlib
        ];

        smart-primitives-src = pkgs.fetchFromGitHub {
          owner = "danielambda";
          repo = "smart-primitives";
          rev = "03193ff51a339bccaa3250f40b9d2fa032782824";
          sha256 = "1kg4y228qqj5685sygnydfpd3mkrrm97l50rzkp7bwg0w0gda2lv";
        };

        check-check-backend-contracts-src = pkgs.fetchFromGitHub {
          owner = "danielambda";
          repo = "check-check-backend-contracts";
          rev = "d00cdc3b2d85621a5afae21c81b6dbcb5bec32e5";
          sha256 = "1kgjxknwgkx8ymfffbz6sjj0r5iajg089f68dfs1dz6hbsvmshj9";
        };

        smart-primitives = hPkgs.callCabal2nix "smart-primitives" smart-primitives-src {};
        check-check-backend-contracts = hPkgs.callCabal2nix "check-check-backend-contracts" check-check-backend-contracts-src {
          inherit smart-primitives;
        };

        telegram-bot = hPkgs.callCabal2nix "telegram-bot" ./. {
          inherit smart-primitives check-check-backend-contracts;
        };

        telegram-bot-image = pkgs.dockerTools.buildLayeredImage {
          name = "check-check-telegram-bot";
          tag = "latest";

          contents = [telegram-bot pkgs.cacert];

          config.Entrypoint = ["${telegram-bot}/bin/check-check-telegram-bot"];
        };
      in {
        devShell = pkgs.mkShell {
          inherit packages;

          LD_LIBRARY_PATH = pkgs.lib.makeLibraryPath packages;
        };

        packages.telegram-bot-image = telegram-bot-image;
      }
    );
}
