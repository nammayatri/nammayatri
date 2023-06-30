{ inputs, ... }:
{
  imports = [
    ./node
    ./nix/watch.nix
    ./nix/bundle.nix
  ];
  perSystem = { config, self', lib, system, ... }:
    let
      pkgs = import inputs.nixpkgs {
        inherit system;
        overlays = [
          inputs.purifix.overlay
        ];
      };

      localPackages = pkgs.purifix {
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
      devShells = lib.optionalAttrs (system != "aarch64-linux") {
        frontend = pkgs.mkShell {
          name = "ps-dev-shell";
          inputsFrom = [
            config.pre-commit.devShell
          ];
          packages = [
            easy-ps.purs-0_15_4
            easy-ps.spago
            easy-ps.psa
            pkgs.dhall
            pkgs.nodejs-14_x
          ];
        };
      };
    };

      devShells.frontend = pkgs.mkShell {
        name = "ps-dev-shell";
        inputsFrom = [
          config.mission-control.devShell
          config.pre-commit.devShell
          self'.packages.ui-common.develop
          self'.devShells.node
        ];
        packages = [
          pkgs.dhall
        ];
      };

      packages = {
        inherit (localPackages) ui-customer ui-driver ui-common;
        android-customer-bundle = self'.packages.ui-customer-android-bundle-js;
        android-driver-bundle = self'.packages.ui-driver-android-bundle-js;
      };
    };
}
