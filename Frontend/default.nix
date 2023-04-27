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

      make-temporary-link = { src, dest, function-name ? "_" }:
        let
          cleanup-function-name = "${function-name}_cleanup";
          temp-dir = "${function-name}_TEMP_DIR";
        in
        ''
          function ${function-name}() {
            ${temp-dir}=$(mktemp -d)
            function ${cleanup-function-name} {
              trap - INT TERM EXIT
              unlink "${dest}"
              if [[ -e "''$${temp-dir}/${dest}" ]]; then
                 mv "''$${temp-dir}/${dest}" "${dest}"
              fi
              unset -f ${temp-dir}
              unset -f ${cleanup-function-name}
            }
            trap '${cleanup-function-name}' INT TERM EXIT
            if [[ -e "${dest}" ]]; then
              mv "${dest}" "''$${temp-dir}/"
            fi
            ln -s "${src}" "${dest}"
          }
          ${function-name}
          unset -f ${function-name}
        '';

      webpack-dev-server = pkgs.writeShellApplication {
        name = "webpack-dev-server-wrapped";
        runtimeInputs = [ nodeDeps ];
        text = ''
          ${make-temporary-link {
              src = "${nodeDeps}/lib/node_modules";
              dest = "node_modules";
            }}
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

      ui-customer-android-bundle-js =
        pkgs.stdenv.mkDerivation {
          name = "bundle-ui-customer-android";
          phases = [ "buildPhase" "installPhase" ];
          nativeBuildInputs = [ pkgs.nodejs ];
          buildPhase = ''
            ln -s ${nodeDeps}/lib/node_modules node_modules
            cp -r -L ${localPackages.ui-customer}/output output
            cp ${./ui-customer/index.js} index.js
            cp ${./ui-customer/package.json} package.json
            cp ${./ui-customer/webpack.config.js} webpack.config.js
            cp ${./ui-customer/webpack.android.js} webpack.android.js
            node_modules/.bin/webpack --env prod --mode=production --progress --config webpack.android.js
          '';
          installPhase = ''
            mv dist/android/index_bundle.js $out
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
          localPackages.ui-customer.develop
        ];
        packages = [
          pkgs.dhall
          pkgs.nodejs
          purifix-watch
        ];
      };
      packages = {
        inherit (localPackages) ui-customer ui-driver;
        inherit ui-customer-android-bundle-js;
        ui-common = localPackages.beckn-common;
      };
    };
}
