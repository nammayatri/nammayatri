{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    common.url = "github:nammayatri/common";
    flake-parts.follows = "common/flake-parts";

    shared-kernel.url = "github:nammayatri/shared-kernel/nixify";
    shared-kernel.inputs.nixpkgs.follows = "nixpkgs";
    beckn-gateway.url = "github:nammayatri/beckn-gateway/nixify";
    beckn-gateway.inputs.shared-kernel.follows = "shared-kernel";
  };
  outputs = inputs@{ self, nixpkgs, flake-parts, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      systems = nixpkgs.lib.systems.flakeExposed;
      imports = [
        inputs.common.flakeModules.default
        ./Backend
      ];
      perSystem = { config, self', system, pkgs, lib, ... }: {
        cachix-push.packages = [ "all" ];

        packages.default =
          pkgs.haskell.lib.justStaticExecutables
            self'.packages.rider-app;
      };
    };
}
