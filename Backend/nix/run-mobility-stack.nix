# Add a process-compose based package for running the entire backend stack.
{ inputs, ... }:
{
  perSystem = perSystem@{ inputs', self', pkgs, lib, ... }: {
    process-compose =
      let
        common = { config, ... }: {
          imports = [
            (import ./services/nammayatri.nix { inherit (perSystem) config self' inputs'; inherit inputs; })
          ];
          apiServer = false;
          services.nammayatri.enable = true;
        };

        # control-center frontend: clone-or-pull from GitHub, npm install, npm run dev.
        # working_dir is `Backend/` (set by the `common` module in nammayatri.nix), so
        # `../data/control-center` resolves to <repo-root>/data/control-center — same
        # convention metabase / redis-commander use for runtime state. Waits on the
        # dashboards it points at so VITE_BAP_URL / VITE_BPP_URL are live before Vite
        # starts proxying.
        controlCenterProcess = { name, ... }: {
          working_dir = "Backend";
          namespace = "test";
          log_location = "${name}.log";
          command = pkgs.writeShellApplication {
            name = "control-center";
            # Modern Node from nixpkgs-unstable. Vite (in control-center)
            # requires Node >=20.19 or >=22.12. The pinned `common/nixpkgs`
            # only ships up to 20.8, which Vite rejects with "TypeError:
            # crypto.hash is not a function". Pulling a newer Node from
            # nixpkgs-unstable scopes the upgrade to just this process.
            runtimeInputs = [ pkgs.git inputs'.nixpkgs-unstable.legacyPackages.nodejs_22 ];
            text = ''
              set -euo pipefail
              CC_PARENT="$(cd .. && pwd)/data"
              mkdir -p "$CC_PARENT"
              CC_DIR="$CC_PARENT/control-center"
              if [ ! -d "$CC_DIR/.git" ]; then
                echo "control-center: cloning into $CC_DIR"
                git clone https://github.com/nammayatri/control-center "$CC_DIR"
              else
                echo "control-center: pulling latest in $CC_DIR"
                git -C "$CC_DIR" pull --ff-only || echo "control-center: pull failed, continuing with existing checkout"
              fi
              cd "$CC_DIR"
              npm install
              export VITE_BPP_URL=http://localhost:8018
              export VITE_BAP_URL=http://localhost:8017
              exec npm run dev
            '';
          };
          depends_on = {
            "rider-dashboard-exe".condition = "process_healthy";
            "provider-dashboard-exe".condition = "process_healthy";
          };
          availability = {
            restart = "on_failure";
            backoff_seconds = 20;
            max_restarts = 30;
          };
          shutdown.signal = 9;
        };
      in
      {
        run-mobility-stack-nix = {
          imports = [
            common
          ];
          services.nammayatri.useCabal = false;
        };

        run-mobility-stack-dev = {
          imports = [
            common
          ];
          services.nammayatri.useCabal = true;
        };

        run-mobility-full-stack-dev = {
          imports = [
            common
          ];
          services.nammayatri.useCabal = true;
          settings.processes.control-center = controlCenterProcess;
        };
      };
  };
}
