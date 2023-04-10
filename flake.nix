{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    systems.url = "github:nix-systems/default";
    common.url = "github:nammayatri/common";
    flake-parts.follows = "common/flake-parts";

    # Backend inputs
    shared-kernel.url = "github:nammayatri/shared-kernel";
    shared-kernel.inputs.nixpkgs.follows = "nixpkgs";
    beckn-gateway.url = "github:nammayatri/beckn-gateway/ca94cd38adbc4e8e6e65f0d83610edadca5a279b";
    beckn-gateway.inputs.shared-kernel.follows = "shared-kernel";
  };
  outputs = inputs:
    inputs.flake-parts.lib.mkFlake { inherit inputs; } {
      systems = import inputs.systems;
      imports = [
        inputs.common.flakeModules.default
        ./Backend/default.nix
        ./Frontend/default.nix
      ];
      perSystem = { config, self', pkgs, ... }: {
        packages.default = self'.packages.nammayatri;

        devShells.default = pkgs.mkShell {
          inputsFrom = [
            # Note: shellHook is evaluated from reverse order!
            config.mission-control.devShell
            config.pre-commit.devShell
            config.flake-root.devShell
            config.haskellProjects.default.outputs.devShell
          ];
        };
      };
    };
}
