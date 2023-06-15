# Add a process-compose based package for running the entire backend stack.
_:
{
  perSystem = { config, self', pkgs, lib, ... }:
    let
      withLogFiles = lib.mapAttrs (name: proc:
        proc // { log_location = "${name}.log"; });
      cabalTargetForExe = lib.listToAttrs (lib.flatten (lib.mapAttrsToList
        (name: info: map (exe: lib.nameValuePair exe "${name}:exe:${exe}") (lib.attrNames info.exes))
        config.haskellProjects.default.outputs.packages));
      exeGetters = {
        nix = exe: self'.apps.${exe}.program;
        cabal = exe: "set -x; cabal run ${cabalTargetForExe.${exe}}";
      };
      buildConfig = getExe:
        withLogFiles {
          # External services
          beckn-gateway.command = lib.getExe config.haskellProjects.default.outputs.finalPackages.beckn-gateway;
          mock-registry.command = lib.getExe config.haskellProjects.default.outputs.finalPackages.mock-registry;
          # Local services
          driver-offer-allocator-exe.command = getExe "driver-offer-allocator-exe";
          driver-tracking-healthcheck-exe.command = getExe "driver-tracking-healthcheck-exe";
          dynamic-offer-driver-app-exe.command = getExe "dynamic-offer-driver-app-exe";
          image-api-helper-exe.command = getExe "image-api-helper-exe";
          kafka-consumers-exe = {
            command = getExe "kafka-consumers-exe";
            environment = [
              "CONSUMER_TYPE=AVAILABILITY_TIME"
            ];
          };
          mock-fcm-exe.command = getExe "mock-fcm-exe";
          mock-google-exe.command = getExe "mock-google-exe";
          mock-idfy-exe.command = getExe "mock-idfy-exe";
          mock-sms-exe.command = getExe "mock-sms-exe";
          provider-dashboard-exe.command = getExe "provider-dashboard-exe";
          public-transport-rider-platform-exe.command = getExe "public-transport-rider-platform-exe";
          public-transport-search-consumer-exe.command = getExe "public-transport-search-consumer-exe";
          rider-app-exe.command = getExe "rider-app-exe";
          rider-dashboard-exe.command = getExe "rider-dashboard-exe";
          scheduler-example-app-exe.command = getExe "scheduler-example-app-exe";
          scheduler-example-scheduler-exe.command = getExe "scheduler-example-scheduler-exe";
          search-result-aggregator-exe.command = getExe "search-result-aggregator-exe";
          special-zone-exe.command = self'.apps.special-zone-exe.program;
        };
    in
    {
      process-compose = {
        run-mobility-stack-nix = {
          port = 7812; # process-compose Swagger API is served here.
          settings.processes = buildConfig exeGetters.nix;
        };
        run-mobility-stack-dev = {
          port = 7812; # process-compose Swagger API is served here.
          settings.processes = buildConfig exeGetters.cabal;
        };
      };
    };
}
