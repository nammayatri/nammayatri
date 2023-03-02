{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    flake-parts.url = "github:hercules-ci/flake-parts";
    flake-parts.inputs.nixpkgs-lib.follows = "nixpkgs";
    haskell-flake.url = "github:srid/haskell-flake";
    treefmt-nix.url = "github:numtide/treefmt-nix";
    treefmt-nix.inputs.nixpkgs.follows = "nixpkgs";
    cachix-push.url = "github:juspay/cachix-push";

    flake-root.url = "github:srid/flake-root";
    # Dependencies
    euler-hs.url = "github:juspay/euler-hs";
    euler-hs.flake = false;

    common.url = "github:nammayatri/common";
    beckn-shared-kernel.url = "github:nammayatri/shared-kernel/ghc810"; # https://github.com/nammayatri/shared-kernel/pull/12
    beckn-shared-kernel.flake = false;
    beckn-gateway.url = "github:nammayatri/beckn-gateway";
    beckn-gateway.flake = false;

    passetto-hs.url = "github:juspay/passetto/bb92cf1dd9699662d2a7bb96cd6a6aed6f20e8ff";
    passetto-hs.flake = false;
    sequelize.url = "github:juspay/haskell-sequelize/3abc8fe10edde3fd1c9a776ede81d057dc590341";
    sequelize.flake = false;
    beam-mysql.url = "github:juspay/beam-mysql/4c876ea2eae60bf3402d6f5c1ecb60a386fe3ace";
    beam-mysql.flake = false;
    # Different rev, because we need: https://github.com/juspay/beam/pull/12
    # Also, https://github.com/juspay/beam/pull/14
    beam.url = "github:srid/beam/ghc810";
    beam.flake = false;
    mysql-haskell.url = "github:juspay/mysql-haskell/788022d65538db422b02ecc0be138b862d2e5cee"; # https://github.com/winterland1989/mysql-haskell/pull/38
    mysql-haskell.flake = false;
    hedis.url = "github:juspay/hedis/46ea0ea78e6d8d1a2b1a66e6f08078a37864ad80";
    hedis.flake = false;
  };
  outputs = inputs@{ self, nixpkgs, flake-parts, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      systems = nixpkgs.lib.systems.flakeExposed;
      imports = [
        inputs.cachix-push.flakeModule
        inputs.common.flakeModule
        ./Backend
      ];
      perSystem = { config, self', system, pkgs, lib, ... }: {
        # Remove this after fixing
        # https://github.com/nammayatri/nammayatri/issues/13
        _module.args.pkgs = import nixpkgs {
          inherit system;
          config.allowUnfree = true;
        };

        cachix-push = {
          cacheName = "nammayatri";
          packages = [ "all" ];
        };

        packages.default = self'.packages.rider-app;

        # The default package is a dummy one, that builds all (other) packages.
        # Useful for CI run.
        packages.all = pkgs.runCommand "packages-combined"
          {
            packagesss =
              builtins.attrValues
                (lib.filterAttrs (k: _: k != "all" && k != "dockerImage")
                  # TODO: Use 'outputs' from https://github.com/srid/haskell-flake/issues/74#issuecomment-1424309168
                  self'.packages);
          } ''
          echo $packagesss > $out
        '';
      };
    };
}
