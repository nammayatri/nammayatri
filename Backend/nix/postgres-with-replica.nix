{ config, pkgs, lib, ... }:
let
  inherit (lib) types;
in
{
  options = {
    services.postgres-with-replica = lib.mkOption {
      type = types.attrsOf (types.submodule {
        options = {
          enable = lib.mkEnableOption "Enable postgres-with-replica service";
          package = lib.mkPackageOption pkgs "postgresql" { };
          master = lib.mkOption {
            type = types.submodule {
              options = {
                port = lib.mkOption {
                  type = types.int;
                  default = 5432;
                  description = "Port of the master postgresql service";
                };
                extraInitialDumps = lib.mkOption {
                  type = types.listOf types.path;
                  default = [ ];
                  description = "Extra initial dumps to append to the master";
                };
                extraInitialScript = lib.mkOption {
                  type = types.str;
                  default = "";
                  description = "Extra initial script to append to the master's `initialScript.before`";
                };
              };
            };
          };
          replica = lib.mkOption {
            type = types.submodule {
              options = {
                port = lib.mkOption {
                  type = types.int;
                  default = 5433;
                  description = "Port of the replica postgresql service";
                };
              };
            };
          };
        };
      });
    };
  };
  config =
    let
      enabled-services = lib.filterAttrs (_: cfg: cfg.enable) config.services.postgres-with-replica;
      pg-basebackup-process = name: cfg:
        lib.nameValuePair "pg-basebackup-${name}" {
          # Run `pg_basebackup` to create a replica of the `${name}` postgres database
          # See here about `pg_basebackup`: https://www.postgresql.org/docs/current/app-pgbasebackup.html
          command = pkgs.writeShellApplication {
            name = "start-pg-basebackup";
            runtimeInputs = with pkgs; [ cfg.package coreutils ];
            text = ''
              MASTER_DATA_DIR=$(readlink -f ${config.services.postgres.${name}.dataDir})
              REPLICA_DATA_DIR=$(readlink -f ${config.services.postgres."${name}-replica".dataDir})
              pg_basebackup -h "$MASTER_DATA_DIR" -U repl_user --checkpoint=fast -D "$REPLICA_DATA_DIR"  -R --slot=some_name  -C --port=${builtins.toString cfg.master.port}
            '';
          };
          depends_on."${name}".condition = "process_healthy";
        };
    in
    {
      settings.processes = lib.mapAttrs' pg-basebackup-process enabled-services;
      services.postgres = lib.concatMapAttrs
        (name: cfg: {
          # The master database
          "${name}" = {
            enable = true;
            package = cfg.package;
            extensions = extensions: [
              extensions.postgis
            ];
            listen_addresses = "*";
            hbaConf = [
              { type = "host"; database = "all"; user = "repl_user"; address = "127.0.0.1/32"; method = "trust"; }
            ];
            port = cfg.master.port;
            initialScript.before = ''
              CREATE USER repl_user replication;
            '' + cfg.master.extraInitialScript;
            initialDumps = [
              ../dev/sql-seed/pre-init.sql
            ] ++ cfg.master.extraInitialDumps;
          };
          # The replica database
          "${name}-replica" = {
            enable = true;
            package = cfg.package;
            depends_on."pg-basebackup-${name}".condition = "process_completed_successfully";
            port = cfg.replica.port;
          };
        })
        enabled-services;
    };
}
