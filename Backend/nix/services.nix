{ inputs, ... }:
{
  config = {
    perSystem = { inputs', self', pkgs, lib, ... }: {
      process-compose."services" = { config, ... }:
        {
          imports = [
            inputs.services-flake.processComposeModules.default
            inputs.passetto.processComposeModules.default
          ];

          services.postgres.db-primary = {
            extensions = extensions: [
              extensions.postgis
            ];
            initialDumps = [
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
            port = 5432;
            enable = true;
            initialScript.before = ''
              CREATE USER atlas WITH PASSWORD 'atlas';
            '';
          };

          # TODO: db-replica should depend on db-primary, maybe we can upstream this to provide a "dependsOn" option?
          services.postgres.db-replica = {
            enable = true;
            port = 5435;
          };

          services.postgres.location-db = {
            initialDumps = [
              ../dev/sql-seed/pre-init.sql
              ../dev/sql-seed/driver-location-seed.sql
              ../dev/local-testing-data/person-location.sql
            ];
            extensions = extensions: [
              extensions.postgis
            ];
            port = 5454;
            enable = true;
          };

          services.postgres.location-db-replica = {
            enable = true;
            port = 5456;
          };

          services.redis."redis".enable = true;

          services.passetto = {
            enable = true;
            initialDumps = [ ../dev/sql-seed/passetto-seed.sql ];
            package = lib.getBin inputs'.passetto.packages.passetto-service;
          };
      };
    };
  };
}
