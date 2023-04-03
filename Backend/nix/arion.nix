# NOTE: This is a candidate for a flake-parts module.
{ ... }:

let
  dockerCompose = { pkgs, ... }: {
    project.name = "nammayatri-svc";
    docker-compose.raw.volumes = {
      prometheus-volume = null;
      pgadmin-data = null;
    };
    services = {
      db-primary.service = {
        image = "bitnami/postgresql:12.3.0";
        container_name = "atlas-db-primary";
        ports = [ "5432:5432" ];
        volumes = [
          "${../dev/sql-seed/pre-init.sql}:/docker-entrypoint-initdb.d/0-pre-init.sql:Z"
          "${../dev/sql-seed/rider-app-seed.sql}:/docker-entrypoint-initdb.d/1-rider-app-seed.sql:Z"
          "${../dev/local-testing-data/rider-app.sql}:/docker-entrypoint-initdb.d/2-rider-app-testing-data.sql:Z"
          "${../dev/sql-seed/static-offer-driver-app-seed.sql}:/docker-entrypoint-initdb.d/1-static-offer-driver-app-seed.sql:Z"
          "${../dev/local-testing-data/static-offer-driver-app.sql}:/docker-entrypoint-initdb.d/2-static-offer-driver-app-testing-data.sql:Z"
          "${../dev/sql-seed/public-transport-rider-platform-seed.sql}:/docker-entrypoint-initdb.d/1-public-transport-rider-platform-seed.sql:Z"
          "${../dev/local-testing-data/public-transport-rider-platform.sql}:/docker-entrypoint-initdb.d/2-public-transport-rider-platform-testing-data.sql:Z"
          "${../dev/sql-seed/mock-registry-seed.sql}:/docker-entrypoint-initdb.d/1-mock-registry-seed.sql:Z"
          "${../dev/local-testing-data/mock-registry.sql}:/docker-entrypoint-initdb.d/2-mock-registry-testing-data.sql:Z"
          "${../dev/sql-seed/scheduler-example-seed.sql}:/docker-entrypoint-initdb.d/1-scheduler-example-seed.sql:Z"
          "${../dev/sql-seed/dynamic-offer-driver-app-seed.sql}:/docker-entrypoint-initdb.d/1-dynamic-offer-driver-app-seed.sql:Z"
          "${../dev/local-testing-data/dynamic-offer-driver-app.sql}:/docker-entrypoint-initdb.d/2-dynamic-offer-driver-app-testing-data.sql:Z"
          "${../dev/sql-seed/rider-dashboard-seed.sql}:/docker-entrypoint-initdb.d/1-rider-dashboard-seed.sql:Z"
          "${../dev/local-testing-data/rider-dashboard.sql}:/docker-entrypoint-initdb.d/2-rider-dashboard-testing-data.sql:Z"
          "${../dev/sql-seed/provider-dashboard-seed.sql}:/docker-entrypoint-initdb.d/1-provider-dashboard-seed.sql:Z"
          "${../dev/local-testing-data/provider-dashboard.sql}:/docker-entrypoint-initdb.d/2-provider-dashboard-testing-data.sql:Z"
        ];
        environment = {
          BITNAMI_DEBUG = "true";
          POSTGRESQL_INITSCRIPTS_USERNAME = "postgres";
          POSTGRESQL_INITSCRIPTS_PASSWORD = "root";
          POSTGRESQL_REPLICATION_MODE = "master";
          POSTGRESQL_REPLICATION_USER = "repl_user";
          POSTGRESQL_REPLICATION_PASSWORD = "repl_password";
          POSTGRESQL_USERNAME = "atlas";
          POSTGRESQL_PASSWORD = "atlas";
          POSTGRESQL_DATABASE = "atlas_dev";
          POSTGRESQL_POSTGRES_PASSWORD = "root";
        };
      };

      db-replica.service = {
        image = "bitnami/postgresql:12.3.0";
        container_name = "atlas-dev-replica";
        ports = [ "5433:5432" ];
        depends_on = [ "db-primary" ];
        environment = {
          BITNAMI_DEBUG = "true";
          POSTGRESQL_REPLICATION_MODE = "slave";
          POSTGRESQL_REPLICATION_USER = "repl_user";
          POSTGRESQL_REPLICATION_PASSWORD = "repl_password";
          POSTGRESQL_MASTER_HOST = "db-primary";
          POSTGRESQL_PASSWORD = "atlas";
          POSTGRESQL_MASTER_PORT_NUMBER = "5432";
        };
      };

      redis.service = {
        image = "redis:5";
        ports = [ "6379:6379" ];
      };

      passetto-db.service = {
        image = "postgres:12.3";
        container_name = "passetto-enc-db";
        ports = [ "5422:5432" ];
        volumes = [
          "${../dev/sql-seed/passetto-seed.sql}:/docker-entrypoint-initdb.d/create_schema.sql:Z"
        ];
        environment = {
          POSTGRES_DB = "passetto";
          POSTGRES_USER = "passetto";
          POSTGRES_PASSWORD = "passetto";
          POSTGRES_HOST_AUTH_METHOD = "scram-sha-256";
          POSTGRES_INITDB_ARGS = "--auth=scram-sha-256";
        };
      };

      passetto-server.service = {
        image = "juspayin/passetto-hs:0b18530";
        container_name = "passetto-enc-server";
        ports = [ "8021:8012" ];
        environment = {
          PASSETTO_PG_BACKEND_CONN_STRING = "postgresql://passetto:passetto@passetto-enc-db:5432/passetto";
        };
        command = "demo";
      };

      prometheus.service = {
        image = "prom/prometheus:v2.27.1";
        container_name = "beckn-prom";
        ports = [ "9090:9090" ];
        volumes = [
          "prometheus-volume:/prometheus"
          "${../dev/prometheus/config.yml}:/etc/prometheus/config.yml"
        ];
        command = "--config.file=/etc/prometheus/config.yml";
        profiles = [ "monitoring" ];
      };

      grafana.service = {
        image = "grafana/grafana:7.5.9";
        container_name = "beckn-grafana";
        depends_on = [ "prometheus" ];
        environment = {
          GF_SECURITY_ADMIN_USER = "admin";
          GF_SECURITY_ADMIN_PASSWORD = "beckn";
        };
        ports = [ "3000:3000" ];
        profiles = [ "monitoring" ];
        volumes = [
          "${../dev/grafana/provisioning}:/etc/grafana/provisioning"
          "${../dev/grafana/config.ini}:/etc/grafana/config.ini"
          "${../dev/grafana/dashboards}:/var/lib/grafana/dashboards"
        ];
      };

      zookeeper.service = {
        image = "confluentinc/cp-zookeeper:latest";
        ports = [ "22181:2181" ];
        environment = {
          ZOOKEEPER_CLIENT_PORT = "2181";
          ZOOKEEPER_TICK_TIME = "2000";
        };
      };

      kafka.service = {
        image = "confluentinc/cp-kafka:latest";
        depends_on = [ "zookeeper" ];
        ports = [ "29092:29092" ];
        environment = {
          KAFKA_ADVERTISED_LISTENERS = "PLAINTEXT://kafka:9092,PLAINTEXT_HOST://localhost:29092";
          KAFKA_BROKER_ID = "1";
          KAFKA_INTER_BROKER_LISTENER_NAME = "PLAINTEXT";
          KAFKA_LISTENER_SECURITY_PROTOCOL_MAP = "PLAINTEXT:PLAINTEXT,PLAINTEXT_HOST:PLAINTEXT";
          KAFKA_OFFSETS_TOPIC_REPLICATION_FACTOR = "1";
          KAFKA_ZOOKEEPER_CONNECT = "zookeeper:2181";
        };
      };

      nginx.service = {
        image = "nginx:stable";
        container_name = "beckn-nginx";
        ports = [ "8080:80" ];
        volumes = [
          "${../dev/nginx/nginx.conf}:/etc/nginx/nginx.conf"
        ];
      };

      pg-admin.service = {
        image = "dpage/pgadmin4";
        ports = [ "9201:80" ];
        profiles = [ "pgadmin" ];
        environment = {
          PGADMIN_CONFIG_SERVER_MODE = "False";
          PGADMIN_DEFAULT_EMAIL = "root@localhost.localdomain";
          PGADMIN_DEFAULT_PASSWORD = "secret";
          PGADMIN_DISABLE_POSTFIX = "true";
        };
        volumes = [
          "pgadmin-data:/var/lib/pgadmin"
          "${../dev/pgadmin/servers.json}:/pgadmin4/servers.json"
        ];
      };
    };
  };
in
{
  perSystem = { inputs', pkgs, lib, ... }: {
    packages = {
      run-docker-compose =
        let
          dcYaml = pkgs.arion.passthru.build {
            modules = [
              dockerCompose
            ];
          };
        in
        pkgs.writeShellApplication {
          name = "run-docker-compose";
          text = ''
            ARG1="''${1:-up}"
            shift 1 || true
            set -x
            ${lib.getExe pkgs.arion} \
              --prebuilt-file ${dcYaml} \
              "$ARG1" "$@"
          '';
        };
    };
  };
}

