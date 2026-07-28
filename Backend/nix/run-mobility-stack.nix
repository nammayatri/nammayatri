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

        # Fixed ports straight out of ports.nix, fronted by caddy on a single
        # origin (:9090) so local URLs match the dev-box. The caddy process
        # self-generates data/Caddyfile from these fixed ports when none exists
        # (see caddy-reverse-proxy in nammayatri.nix), so it becomes healthy and
        # the backend profile's per-process "wait for caddy" gate is satisfied
        # instead of deadlocking.
        run-mobility-stack-dev = {
          imports = [ (commonFor "backend") ];
          services.nammayatri.useCabal = true;
          services.nammayatri.useCaddy = true;
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
          services.nammayatri.useCaddy = true;
        };

        run-local-test-dashboard = {
          imports = [ (commonFor "testDashboard") ];
        };
      };
  };
}
