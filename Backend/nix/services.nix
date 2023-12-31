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

        services.postgres-with-replica.db-primary = {
          enable = true;
          master.extraInitialDumps = [
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
          master.extraInitialScript = ''
            CREATE USER atlas WITH PASSWORD 'atlas';
          '';
          master.port = 5432;
          replica.port = 5435;
        };

        services.postgres-with-replica.location-db = {
          enable = true;
          master.extraInitialDumps = [
            ../dev/sql-seed/driver-location-seed.sql
            ../dev/local-testing-data/person-location.sql
          ];
          master.port = 5454;
          replica.port = 5456;
        };

        services.redis."redis".enable = true;

        services.redis-cluster."cluster1".enable = true;

        services.zookeeper."zookeeper".enable = true;

        services.apache-kafka."kafka".enable = true;

        services.nginx."nginx".enable = true;

        services.passetto = {
          enable = true;
          initialDumps = [ ../dev/sql-seed/passetto-seed.sql ];
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
