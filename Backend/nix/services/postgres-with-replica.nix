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
          extraMasterDBSettings = lib.mkOption {
            type = lib.types.deferredModule;
            default = { };
            description = ''
              Extra master postgres database settings.
            '';
          };
          extraReplicaDBSettings = lib.mkOption {
            type = lib.types.deferredModule;
            default = { };
            description = ''
              Extra replica postgres database settings.
            '';
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
          command = with config.services; pkgs.writeShellApplication {
            name = "start-pg-basebackup";
            # Question: we would expect master and replica to be using same `package`?
            runtimeInputs = [ postgres."${name}".package pkgs.coreutils ];
            text = ''
              MASTER_DATA_DIR=$(readlink -f ${postgres.${name}.dataDir})
              REPLICA_DATA_DIR=$(readlink -f ${postgres."${name}-replica".dataDir})
              if [ ! -d "$REPLICA_DATA_DIR" ]; then
                pg_basebackup -h "$MASTER_DATA_DIR" -U repl_user --checkpoint=fast -D "$REPLICA_DATA_DIR"  -R --slot=some_name  -C --port=${builtins.toString postgres.${name}.port}
              else
                echo "Replica datadir already exists, not doing anything."
              fi
            '';
          };
          depends_on."${name}".condition = "process_healthy";
        };
      replica-settings = name: cfg:
        lib.nameValuePair "${name}-replica-init" {
          depends_on."pg-basebackup-${name}".condition = "process_completed_successfully";

        };
    in
    {
      settings.processes =
        lib.mapAttrs' pg-basebackup-process enabled-services //
        lib.mapAttrs' replica-settings enabled-services;
      services.postgres = lib.concatMapAttrs
        (name: cfg: {
          # The master database
          "${name}" = {
            imports = [ cfg.extraMasterDBSettings ];
            enable = true;
            listen_addresses = "*";
            hbaConf = [
              { type = "host"; database = "all"; user = "repl_user"; address = "127.0.0.1/32"; method = "trust"; }
            ];
          };
          # The replica database
          "${name}-replica" = {
            imports = [ cfg.extraReplicaDBSettings ];
            enable = true;
          };
        })
        enabled-services;
    };
}
