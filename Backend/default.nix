{ inputs, ... }:
{
  imports = [
    ./nix/docker.nix
  ];
  perSystem = { config, self', pkgs, lib, ... }: {
    haskellProjects.default = {
      projectRoot = ./.;
      imports = [
        inputs.beckn-gateway.haskellFlakeProjectModules.output
      ];
      devShell = {
        tools = hp: {
          docker-compose = pkgs.docker-compose;
        };
        # TODO: Upstream mkShellArgs as an option in mission-control
        mkShellArgs = {
          nativeBuildInputs = [ config.mission-control.wrapper ];
          shellHook = ''
            ${config.pre-commit.installationScript}
            ${config.mission-control.banner}
          '';
        };
      };

      # Some tests fail under Nix. We shoud probably run them in CI directly.
      overrides = self: super:
        with pkgs.haskell.lib.compose;
        lib.mapAttrs (k: v: lib.pipe super.${k} v) {
          # location-updates-tests: Network.Socket.connect: <socket: 6>: does not exist (Connection refused)
          location-updates = [ dontCheck ];
          # tries to find dhall files from wrong CWD
          beckn-test = [ dontCheck ];
        };
    };

    mission-control.scripts =
      let
        backendScripts = lib.mapAttrs' (n: v:
          lib.nameValuePair "backend-${n}" (v // { category = "Backend"; }));
        dockerComposeScript = { description, args }: {
          inherit description;
          exec = ''
            set -x
            docker-compose -f ./Backend/dev/docker-compose.yml ${args}
          '';
        };
      in
      backendScripts {
        ghcid = {
          description = "Compile the given local package using ghcid.";
          exec = ''
            set +x
            cd ./Backend # TODO: https://github.com/Platonic-Systems/mission-control/issues/27
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

        run-mobility-stack = {
          description = ''
            Run the nammayatri backend components.
          '';
          exec = ''
            set -x
            cd ./Backend
            # Note: It is important not to reference self.packages, as that will
            # slow down the launch of the devshell by having it build the whole
            # backend.
            nix run .#run-nammayatri-dev
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

    process-compose.configs =
      let
        localPackagesStatic = lib.mapAttrs
          (_: p: pkgs.haskell.lib.justStaticExecutables p)
          (config.haskellProjects.default.outputs.localPackages);

        components = {
          beckn-gateway = {
            nixExe = lib.getExe config.haskellProjects.default.outputs.finalPackages.beckn-gateway;
            cabalExe = null;
          };
          mock-registry = {
            nixExe = lib.getExe config.haskellProjects.default.outputs.finalPackages.mock-registry;
            cabalExe = null;
          };
          # TODO: haskell-flake does not have a way to parse executables, so we must specify them manually.
          # See https://github.com/srid/haskell-flake/issues/36
          scheduler-example-app-exe = {
            nixExe = "${localPackagesStatic.scheduler-example}/bin/scheduler-example-app-exe";
            cabalExe = "scheduler-example:exe:scheduler-example-app-exe";
          };
          scheduler-example-scheduler-exe = {
            nixExe = "${localPackagesStatic.scheduler-example}/bin/scheduler-example-scheduler-exe";
            cabalExe = "scheduler-example:exe:scheduler-example-scheduler-exe";
          };
        } // lib.listToAttrs (builtins.map
          (p: lib.nameValuePair p.pname {
            nixExe = lib.getExe p;
            cabalExe = p.pname;
          })
          (with localPackagesStatic; [
            driver-offer-allocator
            driver-tracking-healthcheck
            dynamic-offer-driver-app
            image-api-helper
            kafka-consumers
            mock-fcm
            mock-idfy
            mock-sms
            provider-dashboard
            public-transport-rider-platform
            public-transport-search-consumer
            rider-app
            rider-dashboard
            search-result-aggregator
            static-offer-driver-app
            static-offer-driver-app-allocator
            static-offer-driver-app-scheduler
          ]));
        getNixExe = _: v: { command = "set -x; ${v.nixExe}"; };
        getDevExe = n: v: if v.cabalExe == null then getNixExe n v else { command = "set -x; cabal run ${v.cabalExe}"; };
      in
      {
        run-nammayatri.processes =
          lib.mapAttrs getNixExe components;
        run-nammayatri-dev.processes =
          lib.mapAttrs getDevExe components;
      };

    packages =
      let
        # Local haskell packages containing only the binaries.
        exes = lib.mapAttrs
          (_: p: pkgs.haskell.lib.justStaticExecutables p)
          (config.haskellProjects.default.outputs.localPackages);
      in
      {
        # The final nammayatri package containing the various executables.
        nammayatri = pkgs.symlinkJoin {
          name = "nammayatri-exes";
          paths = lib.attrValues exes;
        };
      };
  };
}
