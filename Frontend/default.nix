_:
{
  perSystem = { config, self', system, pkgs, lib, ... }:
    {
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
