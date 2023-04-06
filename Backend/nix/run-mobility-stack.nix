# Add process-compose based packages for running the entire backend stack.
#
# - .run-mobility-stack (Uses full nix build)
# - .run-mobility-stack-dev (Uses `cabal run`)
#
# The latter is called from `script.nix`
{ ... }:
{
  perSystem = { config, self', pkgs, lib, ... }: {
    process-compose = {
      port = 7812; # process-compose Swagger API is served here.
      configs =
        let
          components = {
            osrm-server = {
              nixExe = lib.getExe self'.packages.osrm-server;
            };

            # External Haskell packages cannot be run via `cabal run`.
            beckn-gateway = {
              nixExe = lib.getExe config.haskellProjects.default.outputs.finalPackages.beckn-gateway;
            };
            mock-registry = {
              nixExe = lib.getExe config.haskellProjects.default.outputs.finalPackages.mock-registry;
            };

            # TODO: Refactor this after https://github.com/srid/haskell-flake/pull/137
            scheduler-example-app-exe = {
              nixExe = config.haskellProjects.default.outputs.localApps.scheduler-example-app-exe.program;
              cabalExe = "scheduler-example:exe:scheduler-example-app-exe";
            };
            scheduler-example-scheduler-exe = {
              nixExe = config.haskellProjects.default.outputs.localApps.scheduler-example-scheduler-exe.program;
              cabalExe = "scheduler-example:exe:scheduler-example-scheduler-exe";
            };

          } // lib.listToAttrs (builtins.map
            (p: lib.nameValuePair p.pname {
              nixExe = lib.getExe p;
              cabalExe = p.pname;
            })
            (with config.localPackagesStatic; [
              driver-offer-allocator
              driver-tracking-healthcheck
              dynamic-offer-driver-app
              image-api-helper
              kafka-consumers
              mock-fcm
              mock-idfy
              mock-sms
              provider-dashboard
              public-transport-rider-platform
              public-transport-search-consumer
              rider-app
              rider-dashboard
              search-result-aggregator
              static-offer-driver-app
              static-offer-driver-app-allocator
              static-offer-driver-app-scheduler
            ]));

          getNixExe = _: v: { command = "set -x; ${v.nixExe}"; };
          # Like getNixExe, but use `cabal run` for fast iteration. But fall
          # back to nix build if it is a non-local package.
          getDevExe = n: v: if v.cabalExe or null == null then getNixExe n v else { command = "set -x; cabal run ${v.cabalExe}"; };
        in
        {
          run-mobility-stack.processes =
            lib.mapAttrs getNixExe components;
          run-mobility-stack-dev.processes =
            lib.mapAttrs getDevExe components;
        };
    };

    apps.run-mobility-stack-dev.program = pkgs.writeShellApplication {
      name = "run-mobility-stack-dev";
      text = ''
        set -x
        cd "''${FLAKE_ROOT}"/Backend
        # Build the entire stack first, before we launch the executables using 'cabal run'
        cabal build all
        ${lib.getExe self'.packages.run-mobility-stack-dev}
      '';
    };
  };
}
