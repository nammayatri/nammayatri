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
          apiServer = false;
          services.nammayatri.enable = true;
        };
      in
      {
        run-mobility-stack-nix = {
          imports = [
            common
          ];
          services.nammayatri.useCabal = false;
        };

        run-mobility-stack-dev = {
          imports = [
            common
          ];
          services.nammayatri.useCabal = true;
        };
      };
  };
}
