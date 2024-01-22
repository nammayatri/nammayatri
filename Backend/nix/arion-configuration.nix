_:

let
  arionProjectConfiguration = { pkgs, ... }: {
    project.name = "nammayatri-svc";
    # Note: This attr-set might be not needed now
    #       after latest arion version update.
    #       see docker-compose.volumes attr-set.
    docker-compose.raw.volumes = {
      prometheus-volume = null;
      pgadmin-data = null;
    };
    docker-compose.volumes = {
      prometheus-volume = null;
      pgadmin-data = null;
    };
    services = {

      prometheus.service = {
        image = "prom/prometheus:v2.27.1";
        container_name = "beckn-prom";
        ports = [ "9090:9090" ];
        volumes = [
          "prometheus-volume:/prometheus"
          "${../dev/prometheus/config.yml}:/etc/prometheus/config.yml"
        ];
        command = "--config.file=/etc/prometheus/config.yml";
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
        volumes = [
          "${../dev/grafana/provisioning}:/etc/grafana/provisioning"
          "${../dev/grafana/config.ini}:/etc/grafana/config.ini"
          "${../dev/grafana/dashboards}:/var/lib/grafana/dashboards"
        ];
      };

      pg-admin.service = {
        image = "dpage/pgadmin4";
        ports = [ "9201:80" ];
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
    inherit arionProjectConfiguration;

    mission-control.scripts =
      let
        arionScript = { description, args }: {
          inherit description;
          category = "Backend - Docker";
          exec = ''
            set -x
            nix run .#arion -- ${args} "$@"
          '';
        };
      in
      {
        run-monitoring = arionScript {
          description = ''
            Run monitoring stack - Prometheus and grafana in docker containers
          '';
          args = "up --remove-orphans prometheus grafana";
        };

        run-pgadmin = arionScript {
          description = ''
            Run pgadmin stack - Pgadmin in a docker container
          '';
          args = "up --remove-orphans pg-admin";
        };

        stop-all-containers = arionScript {
          description = ''
            Stop all docker containers
          '';
          args = "down --remove-orphans";
        };
      };

  };
}

