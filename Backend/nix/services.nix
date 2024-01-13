{ inputs, ... }:
{
  config = {
    perSystem = { inputs', self', pkgs, lib, ... }: {
      process-compose."services" = { config, ... }: {
        imports = [
          inputs.services-flake.processComposeModules.default
          inputs.passetto.processComposeModules.default
          ./postgres-with-replica.nix
        ];

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
                    ../dev/sql-seed/pre-init.sql
                    ../dev/sql-seed/rider-app-seed.sql
                    ../dev/local-testing-data/rider-app.sql
                    ../dev/sql-seed/public-transport-rider-platform-seed.sql
                    ../dev/local-testing-data/public-transport-rider-platform.sql
                    ../dev/sql-seed/mock-registry-seed.sql
                    ../dev/local-testing-data/mock-registry.sql
                    ../dev/sql-seed/scheduler-example-seed.sql
                    ../dev/sql-seed/dynamic-offer-driver-app-seed.sql
                    ../dev/local-testing-data/dynamic-offer-driver-app.sql
                    ../dev/sql-seed/rider-dashboard-seed.sql
                    ../dev/local-testing-data/rider-dashboard.sql
                    ../dev/sql-seed/provider-dashboard-seed.sql
                    ../dev/local-testing-data/provider-dashboard.sql
                    ../dev/sql-seed/special-zone-seed.sql
                    ../dev/local-testing-data/special-zone.sql
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
            port = 29092;
            enable = true;
          };

          nginx."nginx".enable = true;
        };

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
    };
  };
}
