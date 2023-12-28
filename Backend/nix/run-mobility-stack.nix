# Add a process-compose based package for running the entire backend stack.
_:
{
  perSystem = perSystem@{ inputs', config, self', pkgs, lib, ... }:
    let
      # Top-level common process-compose configuration
      commonConfig = { config, ... }: {
        # process-compose Swagger API is served here.
        port = 7812;
      };
      # process-compose configuration for each process
      perProcessConfig = { name, ... }: {
        log_location = "${name}.log";
      };
      # import the given module in each of the attrset values.
      addModule = x: lib.mapAttrs (_: v: { imports = [ x v ]; });

      # process-compose configs for running a Haskell executable via either 'nix run' or 'cabal run'.
      runnerConfigs = {
        nix = { name, ... }: {
          command = self'.apps.${name}.program;
        };
        cabal = { config, name, ... }:
          let
            cabalTargetForExe = lib.listToAttrs (lib.flatten (lib.mapAttrsToList
              (name: info: map (exe: lib.nameValuePair exe "${name}:exe:${exe}") (lib.attrNames info.exes))
              perSystem.config.haskellProjects.default.outputs.packages));
          in
          {
            options = {
              cabalTarget = lib.mkOption {
                type = lib.types.str;
                default = cabalTargetForExe.${name};
                description = "The cabal target to run";
                internal = true;
                readOnly = true;
              };
            };
            config = {
              command = "set -x; cabal run ${config.cabalTarget}";
            };
          };
      };

      externalProcesses = {
        beckn-gateway.command = lib.getExe config.haskellProjects.default.outputs.finalPackages.beckn-gateway;
        mock-registry.command = lib.getExe config.haskellProjects.default.outputs.finalPackages.mock-registry;
        location-tracking-service.command = lib.getExe inputs'.location-tracking-service.packages.default;
      };

      mobilityStackProcesses = m: addModule m {
        # The keys of this attrset correspond to the cabal executable names
        driver-offer-allocator-exe = { };
        driver-tracking-healthcheck-exe = { };
        dynamic-offer-driver-app-exe = { };
        dynamic-offer-driver-drainer-exe = { };
        rider-app-drainer-exe = { };
        image-api-helper-exe = { };
        kafka-consumers-exe = {
          environment = {
            CONSUMER_TYPE = "AVAILABILITY_TIME";
          };
        };
        mock-fcm-exe = { };
        mock-google-exe = { };
        mock-idfy-exe = { };
        mock-sms-exe = { };
        provider-dashboard-exe = { };
        producer-exe = { };
        public-transport-rider-platform-exe = { };
        public-transport-search-consumer-exe = { };
        rider-app-exe = { };
        rider-dashboard-exe = { };
        scheduler-example-app-exe = { };
        # scheduler-example-scheduler-exe = { };
        search-result-aggregator-exe = { };
        special-zone-exe = { };
      };

    in
    {
      process-compose = addModule commonConfig {
        run-mobility-stack-nix = {
          settings.processes = addModule perProcessConfig
            (externalProcesses // mobilityStackProcesses runnerConfigs.nix);
        };
        run-mobility-stack-dev = {
          settings.processes = addModule perProcessConfig
            (externalProcesses // mobilityStackProcesses runnerConfigs.cabal);
        };
      };
    };
}
