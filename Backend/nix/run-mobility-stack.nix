# Add a process-compose based package for running the entire backend stack.
_:
{
  perSystem = { config, self', pkgs, lib, ... }: {
    process-compose = {
      port = 7812; # process-compose Swagger API is served here.
      configs = {
        run-mobility-stack.processes = {
          # External services
          beckn-gateway.command = lib.getExe config.haskellProjects.default.outputs.finalPackages.beckn-gateway;
          mock-registry.command = lib.getExe config.haskellProjects.default.outputs.finalPackages.mock-registry;
          # Local services
          allocation-service-exe.command = self'.apps.allocation-service-exe.program;
          driver-offer-allocator-exe.command = self'.apps.driver-offer-allocator-exe.program;
          driver-tracking-healthcheck-exe.command = self'.apps.driver-tracking-healthcheck-exe.program;
          dynamic-offer-driver-app-exe.command = self'.apps.dynamic-offer-driver-app-exe.program;
          image-api-helper-exe.command = self'.apps.image-api-helper-exe.program;
          kafka-consumers-exe.command = self'.apps.kafka-consumers-exe.program;
          mock-fcm-exe.command = self'.apps.mock-fcm-exe.program;
          mock-google-exe.command = self'.apps.mock-google-exe.program;
          mock-idfy-exe.command = self'.apps.mock-idfy-exe.program;
          mock-sms-exe.command = self'.apps.mock-sms-exe.program;
          provider-dashboard-exe.command = self'.apps.provider-dashboard-exe.program;
          public-transport-rider-platform-exe.command = self'.apps.public-transport-rider-platform-exe.program;
          public-transport-search-consumer-exe.command = self'.apps.public-transport-search-consumer-exe.program;
          rider-app-exe.command = self'.apps.rider-app-exe.program;
          rider-dashboard-exe.command = self'.apps.rider-dashboard-exe.program;
          scheduler-example-app-exe.command = self'.apps.scheduler-example-app-exe.program;
          scheduler-example-scheduler-exe.command = self'.apps.scheduler-example-scheduler-exe.program;
          search-result-aggregator-exe.command = self'.apps.search-result-aggregator-exe.program;
          static-offer-driver-app-exe.command = self'.apps.static-offer-driver-app-exe.program;
          transporter-scheduler-exe.command = self'.apps.transporter-scheduler-exe.program;
        };
      };
    };
  };
}
