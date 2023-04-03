# Scripts available in nix develop shell. Type `,` in nix develop shell to run
# these.
#
# We use https://github.com/Platonic-Systems/mission-control
{ ... }:
{
  perSystem = { config, self', pkgs, lib, ... }: {
    mission-control.scripts =
      let
        backendScripts = lib.mapAttrs' (n: v:
          lib.nameValuePair "backend-${n}" (v // { category = "Backend"; }));
        dockerComposeScript = { description, args }: {
          inherit description;
          exec = "${lib.getExe self'.packages.run-docker-compose} ${args}";
        };
      in
      backendScripts {
        ghcid = {
          description = "Compile the given local package using ghcid.";
          cdToProjectRoot = false;
          exec = ''
            if [[ "$(pwd)" != "''${FLAKE_ROOT}/Backend" ]]; then
              echo "Please run this script from ./Backend directory"
              exit 1
            fi
            set -x
            ghcid -c "cabal repl $1"
          '';
        };

        hoogle = {
          description = "Run Hoogle server for Haskell packages.";
          exec = ''
            echo "#### Hoogle running at: http://localhost:8090"
            hoogle serve --local --port 8090
          '';
        };

        hpack = {
          description = "Run hpack to generate cabal files.";
          exec = ''
            set -x
            time ${pkgs.findutils}/bin/find ./Backend -name package.yaml -exec hpack {} \;
          '';
        };

        run-mobility-stack = {
          description = ''
            Run the nammayatri backend components via "cabal run".
          '';
          exec = ''
            set -x
            cd ./Backend
            nix run .#run-mobility-stack-dev
          '';
        };

        run-svc = dockerComposeScript {
          description = ''
            Setup and run DB, redis and passetto instances in docker containers
          '';
          args = "up -d --remove-orphans";
        };

        run-monitoring = dockerComposeScript {
          description = ''
            Run monitoring stack - Prometheus and grafana in docker containers
          '';
          args = "--profile monitoring up -d";
        };

        run-pgadmin = dockerComposeScript {
          description = ''
            Run pgadmin stack - Pgadmin in a docker container
          '';
          args = "--profile pgadmin up -d";
        };

        stop-all-containers = dockerComposeScript {
          description = ''
            Stop all docker containers
          '';
          args = "down --remove-orphans";
        };

        new-service = {
          description = ''
            Create a new Haskell package locally
          '';
          exec = ''
            cd ./Backend
            echo 'Enter the name of a new service (in kebab case):'
            read -r name
            cp -r ./app/example-service ./app/"''${name}"
            echo "''${name}" | sed -i "s/example-service/''${name}/g" ./app/"''${name}"/package.yaml
            rm ./app/"''${name}"/example-service.cabal
            ${lib.getExe pkgs.tree} ./app/"''${name}"
          '';
        };
      };
  };
}
