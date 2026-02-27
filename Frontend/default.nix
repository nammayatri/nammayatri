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
    };
}
