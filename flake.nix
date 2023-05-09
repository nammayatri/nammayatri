{
  inputs = {
    common.url = "github:nammayatri/common";
    nixpkgs.follows = "common/nixpkgs";
    flake-parts.follows = "common/flake-parts";
    systems.url = "github:nix-systems/default";

    # Backend inputs
    shared-kernel.url = "github:nammayatri/shared-kernel";
    shared-kernel.inputs.nixpkgs.follows = "nixpkgs";
    beckn-gateway.url = "github:nammayatri/beckn-gateway";
    beckn-gateway.inputs.shared-kernel.follows = "shared-kernel";

    # Frontend inputs
    purifix.url = "github:purifix/purifix";
    dream2nix.url = "github:nix-community/dream2nix";
    proc-flake.url = "github:srid/proc-flake";
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
          # cf. https://haskell.flake.page/devshell#composing-devshells
          inputsFrom = [
            config.mission-control.devShell
            config.pre-commit.devShell
            config.flake-root.devShell
            config.haskellProjects.default.outputs.devShell
          ];
        };
      };
    };
}
