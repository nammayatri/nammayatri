{ inputs, ... }:
{
  imports = [
    inputs.proc-flake.flakeModule
  ];

  perSystem = { config, self', lib, system, ... }:
    let
      nodejs = pkgs.nodejs-14_x;

      pkgs = import inputs.nixpkgs {
        inherit system;
        overlays = [
          inputs.purifix.overlay
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

      # Wrapper for typescript language server to make it work with vscode/neovim by default
      # Ensure that typescript is installed and the language server knows where tsserver lives
      # The --tsserver-path argument being hardcoded instead of sent in the initialization
      # both simplifies the process and ensures that everyone is using the same tsserver when
      # the editor is running in the development shell
      typescript-language-server = pkgs.symlinkJoin {
        name = "typescript-language-server";
        nativeBuildInputs = [ pkgs.makeWrapper ];
        paths = [
          pkgs.nodePackages.typescript
          pkgs.nodePackages.typescript-language-server
        ];
        postBuild = ''
          wrapProgram $out/bin/typescript-language-server \
            --add-flags "--tsserver-path $out/bin/tsserver"
        '';
      };

      webpack-dev-server = pkgs.writeShellApplication {
        name = "webpack-dev-server-wrapped";
        runtimeInputs = [ nodejs ];
        text = ''
          export NODE_PATH="${nodeDependencies}/node_modules"
          PIPE="$1"
          shift
          tail -n1 -f "$PIPE" | node ${./node/watch.js} "$@"
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
        text = ''
          FLAKE_ROOT=''$(${lib.getExe config.flake-root.package})
          cd "$FLAKE_ROOT"
          export ENV=${env}
          export TARGET=./Frontend/${target}
          export CONFIG_FILE
          CONFIG_FILE=$(realpath "$TARGET/webpack.${platform}.js")

          export PIPE=/tmp/${name}
          mkfifo $PIPE
          trap 'rm -f $PIPE' EXIT

          ${lib.getExe config.proc.groups.frontend-dev.package}
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
          nativeBuildInputs = [ nodejs ];
          buildPhase = ''
            ln -s ${nodeDependencies}/node_modules node_modules
            cp -r -L ${localPackages.${target}}/output output
            cp ${ ./${target}/index.js } index.js
            cp ${ ./${target}/package.json } package.json
            cp ${ ./${target}/webpack.config.js } webpack.config.js
            cp ${ ./${target}/${webpack-config} } ${webpack-config}
            node node_modules/.bin/webpack --env ${env} --mode=${mode} --progress --config ${webpack-config}
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
      proc.groups.frontend-dev.processes = {
        webpack-watch.command = "${webpack-dev-server}/bin/webpack-dev-server-wrapped $PIPE --env $ENV --config $CONFIG_FILE --entry $TARGET/index.js";
        purifix-watch.command = "cd \"$TARGET\";${purifix-watch}/bin/purifix-watch $PIPE . ../ui-common";
      };


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
          purifix-watch
          nodejs
          typescript-language-server
        ];
        shellHook = ''
          export NODE_PATH="${nodeDependencies}/node_modules"
        '';
      };

      packages = {
        inherit (localPackages) ui-customer ui-driver;
        ui-common = localPackages.beckn-common;
        android-customer-bundle = bundles.ui-customer-android-prod-production-js;
        android-driver-bundle = bundles.ui-driver-android-prod-production-js;
      } // bundles;
    };
}
