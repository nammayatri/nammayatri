{ inputs, ... }:
{
  config = {
    perSystem = { inputs', self', pkgs, lib, ... }: {
      process-compose."services" = { config, ... }:
        let
          # Run `pg_basebackup` to create a replica of the database with the name `master-db-name`
          # See here about `pg_basebackup`: https://www.postgresql.org/docs/current/app-pgbasebackup.html
          pg-backup-script = master-db-name: replica-db-name: port: pkgs.writeShellApplication {
            name = "start-pg-basebackup";
            runtimeInputs = with pkgs; [ config.services.postgres."${master-db-name}".package coreutils ];
            text = ''
              MASTER_DB_ABS_PATH=$(readlink -f ${config.services.postgres."${master-db-name}".dataDir})
              REPLICA_DB_ABS_PATH=$(readlink -f ${config.services.postgres."${replica-db-name}".dataDir})
              pg_basebackup -h "$MASTER_DB_ABS_PATH" -U repl_user --checkpoint=fast -D "$REPLICA_DB_ABS_PATH"  -R --slot=some_name  -C --port=${builtins.toString port}
            '';
          };
        in
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
            listen_addresses = "*";
            hbaConf = [
              { type = "host"; database = "all"; user = "repl_user"; address = "127.0.0.1/32"; method = "trust"; }
            ];
            initialScript.before = ''
              CREATE USER atlas WITH PASSWORD 'atlas';
              
              CREATE USER repl_user replication;
            '';
          };

          settings.processes.pg-basebackup-primary-db = {
            command = pg-backup-script "db-primary" "db-replica" 5432;
            depends_on."db-primary".condition = "process_healthy";
          };

          services.postgres.db-replica = {
            depends_on."pg-base-backup-primary-db".condition = "process_completed_successfully";
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
            listen_addresses = "*";
            hbaConf = [
              { type = "host"; database = "all"; user = "repl_user"; address = "127.0.0.1/32"; method = "trust"; }
            ];
            initialScript.before = '' 
              CREATE USER repl_user replication;
            '';
          };

          settings.processes.pg-basebackup-location-db = {
            command = pg-backup-script "location-db" "location-db-replica" 5454;
            depends_on."location-db".condition = "process_healthy";
          };

          services.postgres.location-db-replica = {
            depends_on."pg-basebackup-location-db".condition = "process_completed_successfully";
            enable = true;
            port = 5456;
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
