# Add a process-compose based package for running the entire backend stack.
{ inputs, ... }:
{
  perSystem = perSystem@{ inputs', self', pkgs, lib, ... }: {
    process-compose =
      let
        common = { config, ... }: {
          imports = [
            (import ./services/nammayatri.nix { inherit (perSystem) config; inherit inputs; })
          ];
          services.nammayatri.enable = true;
        };

        external = {
          settings.processes = {
            beckn-gateway.command = perSystem.config.haskellProjects.default.outputs.finalPackages.beckn-gateway;
            mock-registry.command = perSystem.config.haskellProjects.default.outputs.finalPackages.mock-registry;
            location-tracking-service.command = inputs'.location-tracking-service.packages.default;
          };
        };
      in
      {
        run-mobility-stack-nix = {
          imports = [
            common
            external
          ];
          services.nammayatri.useCabal = false;
        };

        run-mobility-stack-dev = {
          imports = [
            common
            external
          ];
          services.nammayatri.useCabal = true;
        };
      };
  };
}
