# Process-compose packages for the nammayatri stack.
#
# Four top-level configs, each importing the shared nammayatri service module
# (Backend/nix/services/nammayatri.nix) with a different `profile`:
#
#   run-mobility-stack-nix    → full stack, nix-built executables.
#   run-mobility-stack-dev    → backend + test-context-api + mock-server
#                                (no test-dashboard / test-local-api), cabal-built,
#                                on the fixed ports from ports.nix (no caddy).
#   run-mobility-stack-dev-on-available-ports
#                             → same processes as -dev, but on ports resolved at
#                                startup, fronted by caddy. For a shared dev-box,
#                                where several developers' stacks coexist.
#   run-mobility-stack-full   → full stack, cabal-built (legacy one-shot).
#   run-local-test-dashboard  → only test-local-api + test-dashboard (port 7083 / 7070).
#
# Each maps to a `, run-<name>` mission-control entry (see Backend/nix/scripts.nix).
{ inputs, ... }:
{
  perSystem = perSystem@{ inputs', self', pkgs, lib, ... }: {
    process-compose =
      let
        commonFor = profile: { config, ... }: {
          imports = [
            (import ./services/nammayatri.nix { inherit (perSystem) config self' inputs'; inherit inputs; })
          ];
          apiServer = false;
          services.nammayatri.enable = true;
          services.nammayatri.profile = profile;
        };
      in
      {
        run-mobility-stack-nix = {
          imports = [ (commonFor "full") ];
          services.nammayatri.useCabal = false;
        };

        # Fixed ports straight out of ports.nix. Caddy is off: its only job is to
        # give a shared dev-box one stable origin in front of per-developer
        # ports, and it needs a generated data/Caddyfile that this profile
        # deliberately never writes. useCaddy=false (rather than disabling the
        # process here) also drops the per-process "wait for caddy" gate the
        # backend profile adds — without that, every service would wait forever.
        run-mobility-stack-dev = {
          imports = [ (commonFor "backend") ];
          services.nammayatri.useCabal = true;
          services.nammayatri.useCaddy = false;
        };

        # Same processes, but the ports come from the devbox registry slice the
        # `, run-mobility-stack-dev-on-available-ports` preflight writes, and
        # caddy fronts them.
        run-mobility-stack-dev-on-available-ports = {
          imports = [ (commonFor "backend") ];
          services.nammayatri.useCabal = true;
        };

        run-mobility-stack-full = {
          imports = [ (commonFor "full") ];
          services.nammayatri.useCabal = true;
          services.nammayatri.useCaddy = false;
        };

        run-local-test-dashboard = {
          imports = [ (commonFor "testDashboard") ];
        };
      };
  };
}
