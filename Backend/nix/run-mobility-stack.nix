# Add a process-compose based package for running the entire backend stack.
{ inputs, ... }:
{
  perSystem = perSystem@{ inputs', self', pkgs, lib, ... }: {
    process-compose =
      let
        local = { config, ... }: {
          imports = [
            (import ./services/nammayatri.nix { inherit (perSystem) config; inherit inputs; })
          ];
          apiServer = false;
          services.nammayatri.enable = true;
        };

        external = {
          settings.processes = {
            beckn-gateway = {
              command = perSystem.config.haskellProjects.default.outputs.finalPackages.beckn-gateway;
              working_dir = "Backend";
            };
            mock-registry = {
              command = perSystem.config.haskellProjects.default.outputs.finalPackages.mock-registry;
              working_dir = "Backend";
            };
            location-tracking-service = {
              command = inputs'.location-tracking-service.packages.default;
              working_dir = "Backend";
            };
          };
        };
      in
      {
        run-mobility-stack-nix = {
          imports = [
            local
            external
          ];
          services.nammayatri.useCabal = false;
        };

        run-mobility-stack-dev = {
          imports = [
            local
            external
          ];
          services.nammayatri.useCabal = true;
        };
      };
  };
}
