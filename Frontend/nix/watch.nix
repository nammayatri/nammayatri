{ inputs, ... }:
{
  imports = [
    ../node
  ];

  perSystem = { config, self', lib, pkgs, system, ... }:
    let
      webpack-dev-server = pkgs.writeShellApplication {
        name = "webpack-dev-server-wrapped";
        runtimeInputs = [ config.nammayatri.nodejs ];
        text = ''
          export NODE_PATH="${config.nammayatri.nodeDependencies}/node_modules"
          PIPE="$1"
          shift
          tail -n1 -f "$PIPE" | node ${../node/watch.js} "$@"
        '';
      };

      purifix-watch =
        pkgs.writeShellApplication {
          name = "purifix-watch";
          runtimeInputs = [
            pkgs.watchexec
          ] ++ config.packages.ui-common.develop.buildInputs;
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

      make-watch-processes = { target, env, platform }:
        let
          pipe = "/tmp/watch-${target}-${platform}-${env}";
        in
        {
          processes = {
            make-pipe = {
              command = "mkfifo ${pipe}";
              is_daemon = true;
              shutdown.command = "rm -f ${pipe}";
              availability.restart = "exit_on_failure";
            };

            purifix-watch = {
              command = ''
                cd ./Frontend/${target}
                ${purifix-watch}/bin/purifix-watch ${pipe} . ../ui-common
              '';
              depends_on.make-pipe.condition = "process_started";
            };

            webpack-watch = {
              command = ''
                ${webpack-dev-server}/bin/webpack-dev-server-wrapped \
                    ${pipe} \
                    --env ${env} \
                    --config $(realpath ./Frontend/${target}/webpack.${platform}.js) \
                    --entry ./Frontend/${target}/index.js
              '';
              depends_on.make-pipe.condition = "process_started";
            };
          };
        };

      process-options =
        lib.attrsets.cartesianProductOfSets {
          env = [ "master" "sandbox" "prod" ];
          target = [ "ui-customer" "ui-driver" ];
          platform = [ "android" ]; # TODO: support iOS
        };

      process-name = { target, platform, env }: "${target}-start-${platform}-${env}";

      process-configs =
        builtins.listToAttrs (map
          (args: {
            name = process-name args;
            value = make-watch-processes args;
          })
          process-options);

      mission-control-scripts =
        builtins.listToAttrs (map
          (args: rec {
            name = process-name args;
            value = {
              category = "Frontend Watch";
              description = "Start the dev server for ${args.target} on ${args.platform} with env set to ${args.env}";
              exec = "${lib.getExe self'.packages.${name}} -t=false";
            };
          })
          process-options);
    in
    {
      process-compose.configs = process-configs;
      mission-control.scripts = mission-control-scripts;
    };
}
