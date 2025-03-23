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

        # telegram-bot = hPkgs.callCabal2nix "telegram-bot" ./telegram-bot {
        #   inherit smart-primitives contracts;
        # };
        #
        # telegram-bot-image = pkgs.dockerTools.buildLayeredImage {
        #   name = "check-check-telegram-bot";
        #   tag = "latest";
        #
        #   contents = [telegram-bot pkgs.cacert];
        #
        #   config.Entrypoint = ["${telegram-bot}/bin/telegram-bot"];
        # };
      in {
        devShell = pkgs.mkShell {
          inherit packages;

          LD_LIBRARY_PATH = pkgs.lib.makeLibraryPath packages;
        };

        # packages.telegram-bot-image = telegram-bot-image;
      }
    );
}
