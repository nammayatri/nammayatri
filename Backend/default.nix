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
          dhall = pkgs.dhall;
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
            nix run .#run-nammayatri
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

    process-compose.configs = {
      run-nammayatri.processes =
        let
          localPackagesStatic = lib.mapAttrs
            (_: p: pkgs.haskell.lib.justStaticExecutables p)
            (config.haskellProjects.default.outputs.localPackages);
          exes = with localPackagesStatic; [
            static-offer-driver-app-allocator
            rider-app
            config.haskellProjects.default.outputs.finalPackages.beckn-gateway
            static-offer-driver-app
            driver-tracking-healthcheck
            mock-fcm
            config.haskellProjects.default.outputs.finalPackages.mock-registry
            mock-sms
            mock-idfy
            public-transport-rider-platform
            public-transport-search-consumer
            search-result-aggregator
            static-offer-driver-app-scheduler
            scheduler-example # Provides two executables
            dynamic-offer-driver-app
            rider-dashboard
            provider-dashboard
            image-api-helper
            driver-offer-allocator
            kafka-consumers
          ];
        in
        builtins.listToAttrs
          (lib.concatMap
            (exe:
              # TODO: Upstream executable detection in haskell-flake
              if exe.pname == "scheduler-example"
              then [
                (lib.nameValuePair (exe.pname + "-app-exe") {
                  command = "${exe}/bin/${exe.pname}-app-exe";
                })
                (lib.nameValuePair (exe.pname + "-scheduler-exe") {
                  command = "${exe}/bin/${exe.pname}-scheduler-exe";
                })
              ]
              else [
                (lib.nameValuePair exe.pname {
                  command = lib.getExe exe;
                })
              ])
            exes);
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
