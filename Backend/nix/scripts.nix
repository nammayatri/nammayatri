# Common backend scripts available in devshell.
#
# We use https://github.com/Platonic-Systems/mission-control
_:
{
  perSystem = { config, self', pkgs, lib, ... }: {
    mission-control.scripts = {
      ghcid = {
        category = "Backend";
        description = "Compile the given local package using ghcid.";
        cdToProjectRoot = false;
        exec = ''
          if [[ "$(pwd)" != "''${FLAKE_ROOT}/Backend" ]]; then
            echo "Please run this script from ./Backend directory"
            exit 1
          fi
          set -x
          ${lib.getExe pkgs.ghcid} -c "cabal repl $1"
        '';
      };

      docs = {
        category = "Backend";
        description = "Run Hoogle server for Haskell packages.";
        exec = ''
          echo "#### Hoogle running at: http://localhost:8090"
          hoogle serve --local --port 8090
        '';
      };

      hpack = {
        category = "Backend";
        description = "Run hpack to generate cabal files.";
        exec = ''
          set -x
          time ${pkgs.findutils}/bin/find ./Backend -name package.yaml -exec ${lib.getExe pkgs.hpack} {} \;
        '';
      };

      run-generator = {
        category = "Backend";
        description = "Run run-generate to generate code.";
        exec = ''
          set -x
          cd ./Backend  # These processes expect $PWD to be backend, for reading dhall configs
          cabal run alchemist-generator-exe
          cd ..
          treefmt
          hpack
        '';
      };

      run-mobility-stack-nix = {
        category = "Backend";
        description = ''
          Run the nammayatri backend components via Nix.

          NOTE: This is slower, due to doing full nix build.
        '';
        exec = ''
          set -x
          cd ./Backend  # These processes expect $PWD to be backend, for reading dhall configs
          rm -f ./*.log # Clean up the log files
          nix run .#run-mobility-stack-nix -- "$@"
        '';
      };

      run-mobility-stack-dev = {
        category = "Backend";
        description = ''
          Run the nammayatri backend components via "cabal run".
        '';
        exec =
          let
            cabalTargets =
              lib.pipe config.process-compose.run-mobility-stack-dev.settings.processes [
                (lib.mapAttrsToList
                  (_: lib.attrByPath [ "cabalTarget" ] null))
                (lib.filter (v: v != null))
              ];
          in
          ''
            set -x
            cd ./Backend  # These processes expect $PWD to be backend, for reading dhall configs
            rm -f ./*.log # Clean up the log files
            cabal build ${lib.concatStringsSep " " cabalTargets}
            nix run .#run-mobility-stack-dev -- "$@"
          '';
      };

      backend-new-service = {
        category = "Backend";
        description = ''
          Create a new Haskell package locally
        '';
        exec = ''
          cd ./Backend
          echo 'Enter the name of a new service (in kebab case):'
          read -r name
          cp -r ./app/example-service ./app/"''${name}"
          echo "''${name}" | sed -i "s/example-service/''${name}/g" ./app/"''${name}"/package.yaml
          rm ./app/"''${name}"/example-service.cabal
          ${lib.getExe pkgs.tree} ./app/"''${name}"
        '';
      };
    };
  };
}
