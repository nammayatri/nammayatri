{ inputs, ... }:
{
  config = {
    perSystem = { inputs', self', pkgs, lib, ... }: {
      process-compose."services" = { config, ... }:
        let
          startScript = masterDb: replicaDb: port: pkgs.writeShellApplication {
            name = "start-pgbasebackup";
            runtimeInputs = with pkgs; [ config.services.postgres."${masterDb}".package coreutils ];
            text = ''
              masterDbPath=$(readlink -f data/${masterDb})
              replicaDbPath=$(readlink -f data/${replicaDb})
              pg_basebackup -h "$masterDbPath" -U repl_user --checkpoint=fast -D "$replicaDbPath"  -R --slot=some_name  -C --port=${port} 
            '';
          };
          openStreetDataFile = inputs.osrm-pbf;
          # NOTE: This *should* match the flake input.
          openStreetDataFileName = "southern-zone-231218";
          osrm-data =
            pkgs.runCommandNoCC "osrm-data"
              { buildInputs = [ pkgs.osrm-backend ]; }
              ''
                mkdir $out && cd $out
                cp ${openStreetDataFile} ${openStreetDataFileName}.osm.pbf
                osrm-extract -p ${pkgs.osrm-backend}/share/osrm/profiles/car.lua ${openStreetDataFileName}.osm.pbf
                osrm-partition ${openStreetDataFileName}.osrm
                osrm-customize ${openStreetDataFileName}.osrm
              '';

          osrm-server = pkgs.writeShellApplication {
            name = "osrm-server";
            runtimeInputs = [ pkgs.osrm-backend ];
            text = ''
              set -x
              osrm-routed --algorithm mld \
                ${osrm-data}/${openStreetDataFileName}.osrm
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

          settings.processes.pgBaseBackupForPrimaryDb = {
            command = "${startScript "db-primary" "db-replica" "5432"}/bin/start-pgbasebackup";
            depends_on."db-primary".condition = "process_healthy";
          };

          services.postgres.db-replica = {
            depends_on."pgBaseBackupForPrimaryDb".condition = "process_completed_successfully";
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

          settings.processes.pgBaseBackupForLocationDb = {
            command = "${startScript "location-db" "location-db-replica" "5454"}/bin/start-pgbasebackup";
            depends_on."location-db".condition = "process_healthy";
          };

          services.postgres.location-db-replica = {
            depends_on."pgBaseBackupForLocationDb".condition = "process_completed_successfully";
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
            package = lib.getBin inputs'.passetto.packages.passetto-service;
          };

          settings.processes.osrm-server = {
            command = "${lib.getExe osrm-server}";
          };
        };
    };
  };
}
