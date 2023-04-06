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
          backendExecutablePackages =
            with config.haskellProjects.default.outputs.packages; [
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
              scheduler-example
              search-result-aggregator
              static-offer-driver-app
              static-offer-driver-app-allocator
              static-offer-driver-app-scheduler
            ];
          backendComponents =
            lib.listToAttrs
              (lib.concatMap
                (p: lib.attrValues (lib.mapAttrs
                  (name: exe: lib.nameValuePair name {
                    exec = exe.program;
                    cabalExe = "${p.package.pname}:exe:${name}";
                  })
                  p.exes))
                backendExecutablePackages);
          externalComponents = {
            beckn-gateway.exec = lib.getExe config.haskellProjects.default.outputs.finalPackages.beckn-gateway;
            mock-registry.exec = lib.getExe config.haskellProjects.default.outputs.finalPackages.mock-registry;
          };
          components = externalComponents // backendComponents;

          getNixExe = _: v: { command = "set -x; ${v.exec}"; };
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
