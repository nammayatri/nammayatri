{ inputs, ... }:
{
  imports = [
  ];
  perSystem = { config, self', pkgs, lib, ... }:
      let easy-ps = import inputs.easy-purescript-nix { inherit pkgs; };
      in {
        treefmt.config = {
          # Suppress autoformatting of frontend dhall files.
          settings.formatter.dhall.excludes = [
            "Frontend/packages.dhall"
          ];
        };
        devShells.frontend = pkgs.mkShell {
          name = "ps-dev-shell";
          packages = [
              easy-ps.purs-0_15_4
              easy-ps.spago
              easy-ps.psa
              pkgs.dhall
              pkgs.nodejs-14_x
          ];
        };
    };
}
