{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    common.url = "github:nammayatri/common";
    flake-parts.follows = "common/flake-parts";

    # TODO: Move to common repo?
    mission-control.url = "github:Platonic-Systems/mission-control";
    process-compose-flake.url = "github:Platonic-Systems/process-compose-flake";
    pre-commit-hooks-nix.url = "github:cachix/pre-commit-hooks.nix";
    treefmt-nix.url = "github:juspay/treefmt-nix/dhall-format"; # https://github.com/numtide/treefmt-nix/pull/56
    common.inputs.treefmt-nix.follows = "treefmt-nix";

    shared-kernel.url = "github:nammayatri/shared-kernel";
    shared-kernel.inputs.nixpkgs.follows = "nixpkgs";
    beckn-gateway.url = "github:nammayatri/beckn-gateway";
    beckn-gateway.inputs.shared-kernel.follows = "shared-kernel";
  };
  outputs = inputs@{ nixpkgs, flake-parts, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      systems = nixpkgs.lib.systems.flakeExposed;
      imports = [
        inputs.common.flakeModules.default
        inputs.mission-control.flakeModule
        inputs.process-compose-flake.flakeModule
        inputs.pre-commit-hooks-nix.flakeModule
        ./Backend/default.nix
        ./Frontend/default.nix
      ];
      perSystem = { self', pkgs, ... }: {
        packages.default = self'.packages.nammayatri;

        # TODO: Move these to common repo.
        pre-commit = {
          check.enable = true;
          settings.hooks = {
            treefmt.enable = true;
            nil.enable = true;
            hpack.enable = true;
          };
        };

        treefmt.config = {
          programs.dhall.enable = true;
        };
      };
    };
}
