# Add a process-compose based package for running the entire backend stack.
_:
{
  perSystem = { inputs', config, self', pkgs, lib, ... }:
    let
      externalProcesses = {
        beckn-gateway.command = lib.getExe config.haskellProjects.default.outputs.finalPackages.beckn-gateway;
        mock-registry.command = lib.getExe config.haskellProjects.default.outputs.finalPackages.mock-registry;
        location-tracking-service.command = lib.getExe inputs'.location-tracking-service.packages.default;
      };
    in
    {
      process-compose =
        let
          common = {
            imports = [
              (import ./services/nammayatri.nix config)
            ];
            initCommand = ''
              set -x
              cd ./Backend  # These processes expect $PWD to be backend, for reading dhall configs
              rm -f ./*.log # Clean up the log files
              ${pkgs.redis}/bin/redis-cli -p 30001 -c XGROUP CREATE Available_Jobs myGroup  0 MKSTREAM # TODO: remove this once cluster funtions from euler are fixed
              ${pkgs.redis}/bin/redis-cli XGROUP CREATE Available_Jobs myGroup 0 MKSTREAM
            '';
          };
        in
        {
          run-mobility-stack-nix = {
            imports = [
              common
            ];

            services.nammayatri = {
              enable = true;
              useCabal = false;
            };
          };

          run-mobility-stack-dev = {
            imports = [
              common
            ];
            services.nammayatri = {
              enable = true;
              useCabal = true;
            };
          };
        };
    };
}
