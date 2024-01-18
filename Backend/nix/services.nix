{ inputs, ... }:
{
  config = {
    perSystem = { inputs', self', pkgs, lib, ... }: {
      process-compose."services" = { config, ... }: {
        imports = [
          inputs.services-flake.processComposeModules.default
          inputs.passetto.processComposeModules.default
          ./services/postgres-with-replica.nix
          (import ./services/external-services.nix {inherit self' inputs inputs'; })
        ];
      };
    };
  };
}
