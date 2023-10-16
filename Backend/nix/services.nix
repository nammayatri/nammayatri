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
        services.postgres.db-primary.enable = true;

        # TODO: db-replica should depend on db-primary, maybe we can upstream this to provide a "dependsOn" option?
        services.postgres.db-replica = {
          enable = true;
          port = 5435;
        };

        services.postgres.location-db = {
          enable = true;
          port = 5454;
        };

        services.postgres.location-db-replica = {
          enable = true;
          port = 5456;
        };

        services.redis."redis".enable = true;

        services.passetto = {
          enable = true;
          package = lib.getBin inputs'.passetto.packages.passetto-service;
          pgweb.enable = true;
        };
      };
    };
  };
}
