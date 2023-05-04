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
            npmlock2nix = final.callPackage inputs.npmlock2nix { };
          })
        ];
      };

      localPackages = pkgs.purifix {
        src = ./.;
      };

      nodePackages = inputs.dream2nix.lib.makeFlakeOutputs {
        systems = [ system ];
        source = ./node;
        projects = {
          "atlas-ui" = {
            name = "atlas-ui";
            subsystem = "nodejs";
            translator = "package-lock";
            builder = "strict-builder";
            subsystemInfo = {
              nodejs = 14;
            };
          };
        };
      };
      nodeDependencies = nodePackages.packages.${system}.atlas-ui.lib;

      webpack-dev-server = pkgs.writeShellApplication {
        name = "webpack-dev-server-wrapped";
        runtimeInputs = [ pkgs.nodejs ];
        text = ''
          export NODE_PATH="${nodeDependencies}/node_modules"
          PIPE="$1"
          shift
          tail -n1 -f "$PIPE" | node ${./watch.js} "$@"
        '';
      };

      run-all = pkgs.writeShellApplication {
        name = "run-all";
        text = ''
          trap 'kill 0' EXIT
          for program in "''${@}"; do
            ($program) &
          done
          wait -n >/dev/null
        '';
      };

      purifix-watch =
        pkgs.writeShellApplication {
          name = "purifix-watch";
          runtimeInputs = [
            pkgs.watchexec
          ] ++ localPackages.beckn-common.develop.buildInputs;
          text = ''
            PIPE=$1
            shift
            SRC_FOLDERS=()
            SRC_GLOBS=()
            for SRC_FOLDER in "$@"; do
              if [ -d "$SRC_FOLDER/src" ]; then
                SRC_FOLDERS+=( -w "$SRC_FOLDER/src" )
                SRC_GLOBS+=( "\"$SRC_FOLDER/src/**/*.purs\"" )
              fi
            done
            watchexec "''${SRC_FOLDERS[@]}" -r -f "**/*.purs" -- \
              "echo 'suspend' >$PIPE;purifix ''${SRC_GLOBS[*]} && echo 'ready' >$PIPE"
          '';
        };

      start-app-devserver = { target, env, platform }: pkgs.writeShellApplication rec {
        name = "watch-${target}-${platform}-${env}";
        runtimeInputs = [ run-all purifix-watch webpack-dev-server ];
        text = ''
          if [ -d ./${target} ]; then
            cd ./${target}
          fi
          if [ "$(basename "$PWD")" != "${target}" ]; then
            echo "Not in ${target} directory...exiting"
            exit 1
          fi
          PIPE=/tmp/${name}
          mkfifo $PIPE
          trap 'rm -f $PIPE' EXIT
          CONFIG_FILE=$(realpath ./webpack.${platform}.js)
          run-all "purifix-watch $PIPE . ../ui-common" \
                  "webpack-dev-server-wrapped $PIPE --env ${env} --config $CONFIG_FILE"
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
            ln -s ${nodeDependencies}/node_modules node_modules
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
        customer-start-android-master = {
          type = "app";
          program = start-app-devserver {
            target = "ui-customer";
            platform = "android";
            env = "master";
          };
        };
        driver-start-android-master = {
          type = "app";
          program = start-app-devserver {
            target = "ui-driver";
            platform = "android";
            env = "master";
          };
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
