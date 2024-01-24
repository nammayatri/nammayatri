{ inputs, inputs', self' }:
{ lib, pkgs, ... }:
let
  ports = import ./ports.nix;
in
{
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
        port = ports.db-primary;
      };
      extraReplicaDBSettings = {
        listen_addresses = "127.0.0.1";
        port = ports.db-primary-replica;
      };
    };

    redis."redis" = {
      enable = true;
      port = ports.redis;
    };

    redis-cluster."cluster1" = {
      enable = true;
      nodes = {
        n1 = { port = ports.redis-cluster-n1; };
        n2 = { port = ports.redis-cluster-n2; };
        n3 = { port = ports.redis-cluster-n3; };
        n4 = { port = ports.redis-cluster-n4; };
        n5 = { port = ports.redis-cluster-n5; };
        n6 = { port = ports.redis-cluster-n6; };
      };
    };

    zookeeper."zookeeper" = {
      enable = true;
      port = ports.zookeeper;
    };

    apache-kafka."kafka" = {
      enable = true;
      port = ports.kafka;
      settings = {
        # Since the available brokers are only 1
        "offsets.topic.replication.factor" = 1;
        "zookeeper.connect" = [ "localhost:${builtins.toString ports.zookeeper}" ];
      };
    };


    nginx."nginx" = {
      enable = true;
      port = ports.nginx;
    };
  };
  # kafka should start only after zookeeper is healthy
  settings.processes.kafka.depends_on."zookeeper".condition = "process_healthy";

  services.passetto = {
    enable = true;
    port = ports.passetto-service;
    extraDbSettings.port = ports.passetto-db;
    # FIXME: https://github.com/juspay/passetto/issues/2
    package = lib.getBin
      (if pkgs.stdenv.isDarwin
      then inputs.passetto.packages.x86_64-darwin.passetto-service
      else inputs'.passetto.packages.passetto-service);
  };

  settings.processes.osrm-server = {
    command = self'.packages.osrm-server;
  };
}
