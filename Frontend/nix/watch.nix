{ ... }:
{
  imports = [
    ../node
  ];

  perSystem = { config, self', lib, pkgs, system, ... }:
    let
      webpack-dev-server = pkgs.writeShellApplication {
        name = "webpack-dev-server-wrapped";
        runtimeInputs = [ self'.packages.nodejs ];
        text = ''
          export NODE_PATH="${self'.packages.nodeDependencies}/node_modules"
          PIPE="$1"
          shift
          tail -n1 -f "$PIPE" | node ${./watch.js} "$@"
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
          port = 7812;
          tui = false;
          settings.processes = {
            make-pipe = {
              command = "mkfifo ${pipe}";
              is_daemon = true;
              shutdown.command = "echo 'Shutting down'; rm -f ${pipe}";
              availability.restart = "exit_on_failure";
              readiness_probe = {
                exec.command = "[ -e ${pipe} ]";
                period_seconds = 1;
              };
            };

            purifix-watch = {
              command = ''
                cd ./Frontend/${target}
                ${purifix-watch}/bin/purifix-watch ${pipe} . ../ui-common
              '';
              depends_on.make-pipe.condition = "process_healthy";
            };

            webpack-watch = {
              command = ''
                ${webpack-dev-server}/bin/webpack-dev-server-wrapped \
                    ${pipe} \
                    --env ${env}   \
                    --config "$(realpath ./Frontend/${target}/webpack.${platform}.js)" \
                    --entry ./Frontend/${target}/index.js
              '';
              depends_on.make-pipe.condition = "process_healthy";
            };
          };
        };

      build-configs =
        let
          options = lib.attrsets.cartesianProductOfSets {
            env = [ "master" "sandbox" "prod" ];
            target = [ "ui-customer" "ui-driver" ];
            platform = [ "android" ]; # TODO: support iOS
          };
        in
        builtins.listToAttrs (map
          (args: {
            name = "${args.target}-start-${args.platform}-${args.env}";
            value = args;
          })
          options);

    in
    {
      process-compose = builtins.mapAttrs (_: make-watch-processes) build-configs;

      mission-control.scripts = builtins.mapAttrs
        (name: args:
          {
            category = "Frontend Watch";
            description = "Start the dev server for ${args.target} on ${args.platform} with env set to ${args.env}";
            exec = "${lib.getExe config.process-compose.${name}.outputs.package}";
          }
        )
        build-configs;
    };
}
