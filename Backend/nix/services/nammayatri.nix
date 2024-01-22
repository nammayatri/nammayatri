# process-compose module for running the nammayatri stack
ny:
{ config, pkgs, lib, ... }:
let
  inherit (lib) types;
  pcLib = import ny.inputs.common.inputs.process-compose-flake.lib { inherit lib; };
  # Which Haskell package contains the given cabal executable?
  cabalTargetForExe = lib.listToAttrs (lib.flatten (lib.mapAttrsToList
    (name: info: map (exe: lib.nameValuePair exe "${name}:exe:${exe}") (lib.attrNames info.exes))
    ny.config.haskellProjects.default.outputs.packages));
  accumulateProcessEnv = envVar: processes:
    lib.filter (x: x != null)
      (builtins.map
        (p:
          pcLib.lookupEnv envVar (p.environment or null))
        (lib.attrValues processes));
in
{
  options = {
    services.nammayatri = lib.mkOption {
      type = types.submodule {
        options = {
          enable = lib.mkEnableOption "Enable nammayatri stack";
          useCabal = lib.mkEnableOption "Use cabal instead of Nix";
        };
      };
    };
  };

  imports = [
    ny.inputs.services-flake.processComposeModules.default
    ny.inputs.passetto.processComposeModules.default
    ./postgres-with-replica.nix
  ];

  config =
    let
      inherit (ny) inputs self' inputs';
      cfg = config.services.nammayatri;
      # The cabal executables we want to run as part of the nammayatri service
      # group.
      cabalExecutables = [
        "driver-offer-allocator-exe"
        "dynamic-offer-driver-app-exe"
        "dynamic-offer-driver-drainer-exe"
        "rider-app-drainer-exe"
        "image-api-helper-exe"
        "kafka-consumers-exe"
        "mock-fcm-exe"
        "mock-google-exe"
        "mock-idfy-exe"
        "mock-sms-exe"
        "provider-dashboard-exe"
        "producer-exe"
        "public-transport-rider-platform-exe"
        "public-transport-search-consumer-exe"
        "rider-app-exe"
        "rider-dashboard-exe"
        "search-result-aggregator-exe"
        "special-zone-exe"
      ];

      haskellProcessFor = name:
        if cfg.useCabal
        then {
          command = "set -x; pwd; cabal run ${cabalTargetForExe.${name}}";
          environment.CABAL_TARGET = cabalTargetForExe.${name};
        }
        else {
          command = "set -x; pwd; ${ny.config.apps.${name}.program}";
        };

      haskellProcesses.processes =
        lib.listToAttrs (builtins.map
          (name: {
            inherit name;
            value = {
              imports = [ (haskellProcessFor name) ];
              log_location = "${name}.log";
              working_dir = "Backend";
              depends_on."nammayatri-init".condition = "process_completed_successfully";
            };
          })
          cabalExecutables);

    in
    {
      settings = {
        # Local Haskell processes
        imports = [ haskellProcesses ];

        processes = {
          # Things to do before local Haskell processes are started
          nammayatri-init = {
            working_dir = "Backend";
            depends_on = {
              # Compile Haskell code
              "cabal-build".condition = "process_completed_successfully";
              # Services
              "db-primary".condition = "process_healthy";
              "kafka".condition = "process_healthy";
              "redis".condition = "process_healthy";
              "redis-cluster".condition = "process_healthy";
              "nginx".condition = "process_healthy";
              "osrm-server".condition = "process_healthy";
              "passetto-server".condition = "process_healthy";
            };
            command = pkgs.writeShellApplication {
              name = "run-mobility-stack-init";
              runtimeInputs = with pkgs; [
                redis
              ];
              text = ''
                set -x
                pwd
                rm -f ./*.log # Clean up the log files
                redis-cli -p 30001 -c XGROUP CREATE Available_Jobs myGroup  0 MKSTREAM # TODO: remove this once cluster funtions from euler are fixed
                redis-cli XGROUP CREATE Available_Jobs myGroup 0 MKSTREAM
              '';
            };
          };

          # Run 'cabal build' for all local Haskel processes
          cabal-build = {
            working_dir = "Backend";
            disabled = !cfg.useCabal;
            command = pkgs.writeShellApplication {
              name = "cabal-build";
              text = ''
                set -x
                cabal build ${builtins.concatStringsSep " " (accumulateProcessEnv "CABAL_TARGET" config.settings.processes)}
              '';
            };
          };

          # Processes from other repos in nammayatri GitHub org
          beckn-gateway = {
            command = ny.config.haskellProjects.default.outputs.finalPackages.beckn-gateway;
            working_dir = "Backend";
          };
          mock-registry = {
            command = ny.config.haskellProjects.default.outputs.finalPackages.mock-registry;
            working_dir = "Backend";
          };
          location-tracking-service = {
            command = ny.inputs.location-tracking-service.packages.${pkgs.system}.default;
            working_dir = "Backend";
          };

          kafka-consumers-exe = {
            environment = {
              CONSUMER_TYPE = "AVAILABILITY_TIME";
            };
          };
        };
      };

      # External services
      services = {
        postgres-with-replica.db-primary = {
          enable = true;
          extraMasterDBSettings = {
            extensions = extensions: [
              extensions.postgis
            ];
            initialDatabases = [
              {
                name = "atlas_dev";
                schemas = [
                  ../../dev/sql-seed/pre-init.sql
                  ../../dev/sql-seed/rider-app-seed.sql
                  ../../dev/local-testing-data/rider-app.sql
                  ../../dev/sql-seed/public-transport-rider-platform-seed.sql
                  ../../dev/local-testing-data/public-transport-rider-platform.sql
                  ../../dev/sql-seed/mock-registry-seed.sql
                  ../../dev/local-testing-data/mock-registry.sql
                  ../../dev/sql-seed/dynamic-offer-driver-app-seed.sql
                  ../../dev/local-testing-data/dynamic-offer-driver-app.sql
                  ../../dev/sql-seed/rider-dashboard-seed.sql
                  ../../dev/local-testing-data/rider-dashboard.sql
                  ../../dev/sql-seed/provider-dashboard-seed.sql
                  ../../dev/local-testing-data/provider-dashboard.sql
                  ../../dev/sql-seed/special-zone-seed.sql
                  ../../dev/local-testing-data/special-zone.sql
                ];
              }
            ];
            initialScript.before = ''
              CREATE USER repl_user replication;
              CREATE USER atlas WITH PASSWORD 'atlas';
            '';
            port = 5434;
          };
          extraReplicaDBSettings.port = 5435;
        };

        redis."redis".enable = true;

        redis-cluster."cluster1".enable = true;

        zookeeper."zookeeper".enable = true;

        apache-kafka."kafka" = {
          enable = true;
          port = 29092;
          settings = {
            # Since the available brokers are only 1
            "offsets.topic.replication.factor" = 1;
            "zookeeper.connect" = [ "localhost:2181" ];
          };
        };


        nginx."nginx".enable = true;
      };
      # kafka should start only after zookeeper is healthy
      settings.processes.kafka.depends_on."zookeeper".condition = "process_healthy";

      services.passetto = {
        enable = true;
        port = 8021;
        extraDbSettings.port = 5422;
        # FIXME: https://github.com/juspay/passetto/issues/2
        package = lib.getBin
          (if pkgs.stdenv.isDarwin
          then inputs.passetto.packages.x86_64-darwin.passetto-service
          else inputs'.passetto.packages.passetto-service);
      };

      settings.processes.osrm-server = {
        command = self'.packages.osrm-server;
      };
    };
}
