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
