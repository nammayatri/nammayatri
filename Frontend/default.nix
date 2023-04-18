{ inputs, ... }:
{
  imports = [
  ];
  perSystem = { config, self', pkgs, lib, system, ... }:
    let
      pkgsWithPurifix = import inputs.nixpkgs {
        inherit system;
        overlays = [ inputs.purifix.overlay ];
      };
      localPackages = pkgsWithPurifix.purifix {
        src = ./.;
      };
    in
    {
      treefmt.config = {
        # Suppress autoformatting of frontend dhall files.
        settings.formatter.dhall.excludes = [
          "Frontend/packages.dhall"
        ];
      };
      devShells.frontend = pkgs.mkShell {
        name = "ps-dev-shell";
        inputsFrom = [
          config.mission-control.devShell
          config.pre-commit.devShell
          localPackages.ui-customer.develop
        ];
        packages = [
          pkgs.dhall
          pkgs.nodejs-14_x
        ];
      };
      packages = {
        ui-customer = localPackages.ui-customer;
        ui-driver = localPackages.ui-driver;
        ui-common = localPackages.beckn-common;
      };
    };
}
