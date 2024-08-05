ny:
{ config, lib, pkgs, ... }:
{
  options = {
    services.cac = {
      enable = lib.mkEnableOption "context-aware-config";
    };
  };
  config =
    let
      dbName = "config";
      dbSuperUser = "postgres";
    in
    lib.mkIf config.services.cac.enable {
      services.postgres."cac-db" = {
        enable = true;
        initialScript.before = ''
          CREATE USER ${dbSuperUser} SUPERUSER;
        '';
        initialDatabases = [{ name = dbName; }];
      };
      settings.processes = {
        context-aware-config = {
          command = ny.inputs.context-aware-config.packages.${pkgs.system}.superposition;
          availability = {
            restart = "on_failure";
            backoff_seconds = 2;
            max_restarts = 5;
          };
          environment =
            let
              pgcfg = config.services.postgres."cac-db";
            in
            {
              DATABASE_URL = pgcfg.connectionURI { inherit dbName; };
              RUST_LOG = "debug";
              AWS_ACCESS_KEY_ID = "test";
              AWS_SECRET_ACCESS_KEY = "test";
              AWS_SESSION_TOKEN = "test";
              AWS_REGION = "ap-south-1";
              DB_USER = dbSuperUser;
              DB_HOST = "${pgcfg.listen_addresses}:${toString pgcfg.port}";
              DB_NAME = dbName;
              APP_ENV = "DEV";
              AWS_REGION_ENDPOINT = "http://localhost:4566";
              ALLOW_SAME_KEYS_OVERLAPPING_CTX = "true";
              ALLOW_DIFF_KEYS_OVERLAPPING_CTX = "true";
              ALLOW_SAME_KEYS_NON_OVERLAPPING_CTX = "true";
              CAC_HOST = "http://localhost:8080";
              API_HOSTNAME = "http://localhost:8080";
              CONTEXT_AWARE_CONFIG_VERSION = "v0.1.0";
              HOSTNAME = "<application_name>-<deployment_id>-<replicaset>-<pod>";
              MJOS_ALLOWED_ORIGINS = "https://potato.in,https://onion.in,http://localhost:8080";
              ACTIX_KEEP_ALIVE = "120";
              MAX_DB_CONNECTION_POOL_SIZE = "3";
              ENABLE_TENANT_AND_SCOPE = "true";
              TENANTS = "dev,test";
              TENANT_MIDDLEWARE_EXCLUSION_LIST = "/health,/assets/favicon.ico,/pkg/frontend.js,/pkg,/pkg/frontend_bg.wasm,/pkg/tailwind.css,/pkg/style.css,/assets,/admin,/";
              SERVICE_PREFIX = "";
              SERVICE_NAME = "CAC";
            };
        };
      };
    };
}
