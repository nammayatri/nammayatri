{ inputs, ... }:
{
  imports = [
  ];
  perSystem = { config, self', system, pkgs, lib, ... }:
    let easy-ps = import inputs.easy-purescript-nix { inherit pkgs; };
    in {
      pre-commit.settings.imports = [
        ./nix/pre-commit.nix
      ];
      treefmt.config = {
        # Suppress autoformatting of frontend dhall files.
        settings.formatter.dhall.excludes = [
          "Frontend/packages.dhall"
        ];
      };
      devShells = lib.optionalAttrs (system != "aarch64-linux") {
        frontend = pkgs.mkShell {
          name = "ny-frontend";
          meta.description = "Frontend development environment for nammayatri";
          inputsFrom = [
            config.pre-commit.devShell
          ];
          packages = [
            easy-ps.purs-0_15_4
            easy-ps.spago
            easy-ps.psa
            easy-ps.purty
            pkgs.dhall
            pkgs.nodejs-14_x
          ];
        };
      };
    };
}
