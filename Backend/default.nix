{ self, inputs, ... }:
{
  imports = [
    inputs.haskell-flake.flakeModule
    inputs.flake-root.flakeModule
    inputs.treefmt-nix.flakeModule
    ./nix/docker.nix
  ];
  perSystem = { config, self', system, pkgs, lib, ... }: {
    haskellProjects.default = {
      imports = [
        ./nix/haskell-overrides.nix
      ];
      # beckn is not upgraded to 9.2 yet (could take weeks per Hemant)
      # we can't use 884, because that's broken in nixpkgs. So 8.10.
      basePackages = pkgs.haskell.packages.ghc8107;
      devShell = {
        tools = hp: {
          dhall = pkgs.dhall;
        };
      };
      overrides = self: super: with pkgs.haskell.lib; {
        # FIXME: location-updates-tests: Network.Socket.connect: <socket: 6>: does not exist (Connection refused)
        location-updates = dontCheck super.location-updates;
        # FIXME: tries to find dhall files from wrong CWD
        beckn-test = dontCheck super.beckn-test;
      };
      packages = {
        beckn-services.root = ./lib/beckn-services;
        beckn-spec.root = ./lib/beckn-spec;
        location-updates.root = ./lib/location-updates;
        scheduler.root = ./lib/scheduler;

        mock-google.root = ./app/mocks/google;
        mock-idfy.root = ./app/mocks/idfy;
        mock-rider-platform.root = ./app/mocks/rider-platform;
        mock-sms.root = ./app/mocks/sms;
        mock-fcm.root = ./app/mocks/fcm;
        mock-public-transport-provider-platform.root = ./app/mocks/public-transport-provider-platform;

        search-result-aggregator.root = ./app/rider-platform/rider-app/search-result-aggregator;
        rider-app.root = ./app/rider-platform/rider-app/Main;
        public-transport-search-consumer.root = ./app/rider-platform/public-transport-rider-platform/search-consumer;
        public-transport-rider-platform.root = ./app/rider-platform/public-transport-rider-platform/Main;

        image-api-helper.root = ./app/utils/image-api-helper;
        route-extractor.root = ./app/utils/route-extractor;

        scheduler-example.root = ./app/scheduler-example;

        provider-dashboard.root = ./app/dashboard/provider-dashboard;
        dashboard-helper-api.root = ./app/dashboard/CommonAPIs;
        lib-dashboard.root = ./app/dashboard/Lib;
        rider-dashboard.root = ./app/dashboard/rider-dashboard;

        beckn-cli.root = ./app/beckn-cli;
        example-service.root = ./app/example-service;
        kafka-consumers.root = ./app/kafka-consumers;

        static-offer-driver-app-scheduler.root = ./app/provider-platform/static-offer-driver-app/Scheduler;
        static-offer-driver-app-allocator.root = ./app/provider-platform/static-offer-driver-app/Allocator;
        static-offer-driver-app.root = ./app/provider-platform/static-offer-driver-app/Main;
        driver-tracking-healthcheck.root = ./app/provider-platform/driver-tracking-health-check;
        driver-offer-allocator.root = ./app/provider-platform/dynamic-offer-driver-app/Allocator;
        dynamic-offer-driver-app.root = ./app/provider-platform/dynamic-offer-driver-app/Main;

        beckn-test.root = ./test;
      };
    };
  };
}
