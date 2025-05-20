{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    flake-parts.url = "github:hercules-ci/flake-parts";
    haskell-flake.url = "github:srid/haskell-flake";

    smart-primitives.url              = "github:danielambda/smart-primitives";
    check-check-backend-contracts.url = "github:danielambda/check-check-backend-contracts";
    telegram-bot-fsafe.url            = "github:danielambda/telegram-bot-fsafe";
    telegram-bot-message-dsl.url            = "github:danielambda/telegram-bot-message-dsl";
  };

  outputs = inputs@{ nixpkgs, flake-parts, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      systems = nixpkgs.lib.systems.flakeExposed;
      imports = [inputs.haskell-flake.flakeModule];
      perSystem = { self', config, pkgs, ... }:
        let
          telegram-bot = self'.packages.check-check-telegram-bot;
          telegram-bot-image = pkgs.dockerTools.buildLayeredImage {
            name = "check-check-telegram-bot";
            tag = "latest";

            contents = [telegram-bot pkgs.cacert];

            config.Entrypoint = ["${telegram-bot}/bin/check-check-telegram-bot"];
          };
      in {
        haskellProjects.default = {
          autoWire = ["packages" "apps"];
          packages = {
            smart-primitives.source = inputs.smart-primitives;
            check-check-backend-contracts.source = inputs.check-check-backend-contracts;
            telegram-bot-fsafe.source = inputs.telegram-bot-fsafe;
            telegram-bot-message-dsl.source = inputs.telegram-bot-message-dsl;
          };
        };

        devShells.default = pkgs.mkShell {
          inputsFrom = [config.haskellProjects.default.outputs.devShell];
          packages = [pkgs.nixd];

          shellHook = ''
            set -a
            source ./.env
            set +a
          '';
        };

        packages = {
          default = telegram-bot;
          inherit telegram-bot-image;
        };
      };
    };
}
