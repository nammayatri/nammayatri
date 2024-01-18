# Add a process-compose based package for running the entire backend stack.
{ inputs, ... }:
{
  perSystem = perSystem@{ inputs', self', pkgs, lib, ... }: {
    process-compose =
      let
        pcLib = import inputs.process-compose-flake.lib { inherit lib; };
        common = { config, ... }: {
          imports = [
            (import ./services/nammayatri.nix perSystem.config)
          ];
          services.nammayatri.enable = true;
          initCommand =
            let
              # The cabal target of all Haskell processes.
              cabalTargets =
                lib.filter (x: x != null)
                  (builtins.map
                    (p:
                      pcLib.lookupEnv "CABAL_TARGET" (p.environment or null))
                    (lib.attrValues config.settings.processes));
            in
            ''
              set -x
              cd ./Backend  # These processes expect $PWD to be backend, for reading dhall configs
              rm -f ./*.log # Clean up the log files

              ${if config.services.nammayatri.useCabal then ''
                  cabal build ${builtins.concatStringsSep " " (builtins.trace cabalTargets cabalTargets)}
                '' else ""}

              #${pkgs.redis}/bin/redis-cli -p 30001 -c XGROUP CREATE Available_Jobs myGroup  0 MKSTREAM # TODO: remove this once cluster funtions from euler are fixed
              #${pkgs.redis}/bin/redis-cli XGROUP CREATE Available_Jobs myGroup 0 MKSTREAM
            '';
        };

        external = {
          settings.processes = {
            beckn-gateway.command = lib.getExe perSystem.config.haskellProjects.default.outputs.finalPackages.beckn-gateway;
            mock-registry.command = lib.getExe perSystem.config.haskellProjects.default.outputs.finalPackages.mock-registry;
            location-tracking-service.command = lib.getExe inputs'.location-tracking-service.packages.default;
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
