{
  inputs = {
    common.url = "github:nammayatri/common";

    # Backend inputs
    shared-kernel.url = "github:nammayatri/shared-kernel";
    shared-kernel.inputs.common.follows = "common";
    beckn-gateway.url = "github:nammayatri/beckn-gateway";
    beckn-gateway.inputs.common.follows = "common";
    beckn-gateway.inputs.shared-kernel.follows = "shared-kernel";

    easy-purescript-nix.url = "github:justinwoo/easy-purescript-nix";
    easy-purescript-nix.flake = false;

    # Extended backend inputs
    # ny-example.url = "path:/home/srid/code/ny/ny-example";
    ny-example.url = "git+ssh://git@github.com/nammayatri/ny-example.git";
    ny-example.flake = false;
  };
  outputs = inputs:
    inputs.common.lib.mkFlake { inherit inputs; } {
      imports = [
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
            config.haskellProjects.default.outputs.devShell
            config.flake-root.devShell
          ];
        };
      };
    };
}
