{ inputs, ... }:
{
  imports = [
  ];
  perSystem = { config, self', lib, system, ... }:
    let
      pkgs = import inputs.nixpkgs {
        inherit system;
        overlays = [
          inputs.purifix.overlay
          (final: prev: {
            nodejs = final.nodejs-14_x;
          })
        ];
      };
      localPackages = pkgs.purifix {
        src = ./.;
      };
      nodeDeps = (import ./nix/node-composition.nix {
        inherit pkgs;
        inherit (pkgs) nodejs;
      }).nodeDependencies;
      webpack-dev-server = pkgs.writeShellApplication {
        name = "webpack-dev-server-wrapped";
        runtimeInputs = [ nodeDeps ];
        text = ''
          TEMP_DIR=$(mktemp -d)
          function cleanup {
            trap - INT TERM EXIT
            unlink "node_modules"
            if [[ -e "$TEMP_DIR/node_modules" ]]; then
               mv "$TEMP_DIR/node_modules" "node_modules"
            fi
          }
          trap 'cleanup' INT TERM EXIT
          if [[ -e "node_modules" ]]; then
            mv "node_modules" "$TEMP_DIR/"
          fi
          ln -s "${nodeDeps}/lib/node_modules" "node_modules"
          webpack-dev-server "$@"
        '';
      };
      run-all = pkgs.writeShellApplication {
        name = "run-all";
        text = ''
          trap 'kill 0' EXIT
          for program in "''${@}"; do
            ($program) &
          done
          wait >/dev/null
        '';
      };
      purifix-watch =
        pkgs.writeShellApplication {
          name = "purifix-watch";
          runtimeInputs = [
            pkgs.watchexec
            pkgs.nodejs-14_x
          ] ++ localPackages.beckn-common.develop.buildInputs;
          text = ''
            SRC_FOLDERS=()
            SRC_GLOBS=()
            for SRC_FOLDER in "$@"; do
              if [ -d "$SRC_FOLDER/src" ]; then
                SRC_FOLDERS+=( -w "$SRC_FOLDER/src" )
                SRC_GLOBS+=( "\"$SRC_FOLDER/src/**/*.purs\"" )
              fi
            done
            watchexec "''${SRC_FOLDERS[@]}" -r -f "**/*.purs" -- "echo 'Rebuilding...'; purifix ''${SRC_GLOBS[*]}; echo 'Finished rebuilding'"
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

      make-bundle = { mode ? "production", env ? "prod", target, platform }:
        let
          webpack-config = "webpack.${platform}.js";
          dist-folder =
            if mode != "development" then
              "./dist/${platform}"
            else
              "./dist";
        in
        pkgs.stdenv.mkDerivation {
          name = "${target}-${platform}-${env}-${mode}-index-bundle-js";
          phases = [ "buildPhase" "installPhase" ];
          nativeBuildInputs = [ pkgs.nodejs ];
          buildPhase = ''
            ln -s ${nodeDeps}/lib/node_modules node_modules
            cp -r -L ${localPackages.${target}}/output output
            cp ${ ./${target}/index.js } index.js
            cp ${ ./${target}/package.json } package.json
            cp ${ ./${target}/webpack.config.js } webpack.config.js
            cp ${ ./${target}/${webpack-config} } ${webpack-config}
            node_modules/.bin/webpack --env ${env} --mode=${mode} --progress --config ${webpack-config}
          '';
          installPhase = ''
            mv ${dist-folder}/index_bundle.js $out
          '';
        };

      bundle-options =
        lib.attrsets.cartesianProductOfSets {
          mode = [ "production" "development" ];
          env = [ "master" "sandbox" "prod" ];
          target = builtins.attrNames localPackages;
          platform = [ "android" "ios" ];
        };

      bundles =
        builtins.listToAttrs (map
          (args: {
            name = "${args.target}-${args.platform}-${args.env}-${args.mode}-js";
            value = make-bundle args;
          })
          bundle-options);
    in
    {
      treefmt.config = {
        # Suppress autoformatting of frontend dhall files.
        settings.formatter.dhall.excludes = [
          "Frontend/packages.dhall"
        ];
      };
      apps = {
        webpack-dev-server = {
          type = "app";
          program = webpack-dev-server;
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
          localPackages.beckn-common.develop
        ];
        packages = [
          pkgs.dhall
          pkgs.nodejs
          purifix-watch
        ];
      };
      packages = {
        inherit (localPackages) ui-customer ui-driver;
        ui-common = localPackages.beckn-common;
        android-customer-bundle = bundles.ui-customer-android-prod-production-js;
        android-driver-bundle = bundles.ui-driver-android-prod-production-js;
      } // bundles;
    };
}
