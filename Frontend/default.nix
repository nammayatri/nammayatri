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
      nodeDeps = (import ./nix/node-composition.nix {
        inherit pkgs;
        nodejs = pkgs.nodejs-14_x;
      }).nodeDependencies;
      webpack = pkgs.writeShellApplication {
        name = "webpack-wrapped";
        runtimeInputs = [ nodeDeps ];
        text = ''
          ln -sf ${nodeDeps}/lib/node_modules node_modules
          trap 'unlink node_modules' EXIT
          webpack "$@"
        '';
      };
      webpack-dev-server = pkgs.writeShellApplication {
        name = "webpack-dev-server-wrapped";
        runtimeInputs = [ nodeDeps ];
        text = ''
          ln -sf ${nodeDeps}/lib/node_modules node_modules
          trap 'unlink node_modules' EXIT
          webpack-dev-server "$@"
        '';
      };
      run-all = pkgs.writeShellApplication {
        name = "run-all";
        text = ''
          trap 'kill 0' EXIT
          for program in "''${@}"; do
            sh -c "$program" &
          done
          wait >/dev/null
        '';
      };
      purifix-watch =
        pkgs.writeShellApplication {
          name = "purifix-watch";
          runtimeInputs = [
            pkgs.watchexec
            webpack
            pkgs.nodejs-14_x
          ] ++ localPackages.beckn-common.develop.buildInputs;
          text = ''
            SRC_FOLDERS=()
            SRC_GLOBS=()
            for SRC_FOLDER in "$@"; do
              if [ -d "$SRC_FOLDER" ]; then
                SRC_FOLDERS+=(-w "$SRC_FOLDER")
                SRC_GLOBS+=("$SRC_FOLDER/src/**/*.purs")
              fi
            done
            watchexec "''${SRC_FOLDERS[@]}" -rn -f "**/*.purs" -- purifix "''${SRC_GLOBS[@]}"
          '';
        };
      watch-customer = pkgs.writeShellApplication {
        name = "watch-customer";
        runtimeInputs = [ run-all purifix-watch webpack-dev-server ];
        text = ''
          if [ -d ./ui-customer ]; then
            cd ./ui-customer
          fi
          if [ "$(basename "$PWD")" != "ui-customer" ]; then
            echo "Not in ui-customer directory...exiting"
            exit 1
          fi
          run-all "purifix-watch . ../ui-common" "webpack-dev-server-wrapped --watch-files-reset --progress --config webpack.android.js --watch-files ./output/Main/index.js ./index.js"
        '';
      };
    in
    {
      treefmt.config = {
        # Suppress autoformatting of frontend dhall files.
        settings.formatter.dhall.excludes = [
          "Frontend/packages.dhall"
        ];
      };
      apps = {
        webpack = {
          type = "app";
          program = "${webpack}/bin/webpack-wrapped";
        };
        webpack-dev-server = {
          type = "app";
          program = "${webpack-dev-server}/bin/webpack-dev-server-wrapped";
        };
        purifix-watch = {
          type = "app";
          program = purifix-watch;
        };
        watch-customer = {
          type = "app";
          program = watch-customer;
        };
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
          purifix-watch
        ];
      };
      packages = {
        inherit (localPackages) ui-customer ui-driver;
        ui-common = localPackages.beckn-common;
      };
    };
}
