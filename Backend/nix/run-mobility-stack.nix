# Process-compose packages for the nammayatri stack.
#
# Four top-level configs, each importing the shared nammayatri service module
# (Backend/nix/services/nammayatri.nix) with a different `profile`:
#
#   run-mobility-stack-nix    → full stack, nix-built executables.
#   run-mobility-stack-dev    → backend + test-context-api + mock-server
#                                (no test-dashboard / test-local-api), cabal-built.
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

        run-mobility-stack-dev = {
          imports = [ (commonFor "backend") ];
          services.nammayatri.useCabal = true;
        };

        run-mobility-stack-full = {
          imports = [ (commonFor "full") ];
          services.nammayatri.useCabal = true;
          settings.processes."caddy-reverse-proxy" = {
            disabled = lib.mkForce true;
            depends_on = lib.mkForce { };
            command = lib.mkForce ":";
          };
        };

        run-local-test-dashboard = {
          imports = [ (commonFor "testDashboard") ];
        };
      };
  };
}
