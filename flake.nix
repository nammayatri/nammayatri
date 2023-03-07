{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    common.url = "github:nammayatri/common";
    flake-parts.follows = "common/flake-parts";

    shared-kernel.url = "github:nammayatri/shared-kernel/ghc810--nixify";
    shared-kernel.inputs.nixpkgs.follows = "nixpkgs";
    beckn-gateway.url = "github:nammayatri/beckn-gateway";
    beckn-gateway.flake = false;
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

        packages.default = self'.packages.rider-app;

        # A dummy package to force build of all local Haskell packages. 
        # Useful in CI.
        packages.all = pkgs.runCommand "packages-combined"
          {
            all = builtins.attrValues config.haskellProjects.default.outputs.localPackages;
          } '' echo $all > $out '';
      };
    };
}
