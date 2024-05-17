# Common backend scripts available in devshell.
#
# We use https://github.com/Platonic-Systems/mission-control
_:
{
  debug = true;
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
          skip_update=false
          for arg in "$@"; do
            if [[ "$arg" == "--skip-update" ]]; then
              skip_update=true
              break
            fi
          done
          current_commit_hash=$( ${pkgs.jq}/bin/jq -r '.nodes."namma-dsl".locked.rev' "''${FLAKE_ROOT}/flake.lock" || true)
          latest_commit_hash=$(curl -s "https://api.github.com/repos/nammayatri/namma-dsl/commits/main" | jq -r '.sha' || true)
          if [[ -z $latest_commit_hash ]];
          then
            echo -e "\033[33mNot able to get status of Namma-DSL"
          else
            if [[ "$current_commit_hash" != "$latest_commit_hash" ]]; then
                echo -e "\033[33mNamma-DSL in not up to date !!\nCurrent commit hash: $current_commit_hash\nLatest commit hash: $latest_commit_hash"
                if [[ $skip_update == false ]]; then
                    echo -e "\033[33mUpdating Namma-DSL to latest commit";
                    nix flake lock --update-input namma-dsl;
                    echo -e "\033[32mNamma-DSL updated to latest commit\nPlease run nix develop again to use the updated version"
                    echo -e "\033[00m";
                    exit 0
                fi
            else
                echo -e "\033[32mNamma-DSL is up to date";
            fi
          fi
          echo -e "\033[00m";
          set -x
          cd "''${FLAKE_ROOT}/Backend"
          cabal run alchemist-generator-exe -- "$@"
          cd ..
          treefmt --verbose
          hpack

          # Automatically applies Redundant bracket hint
          applyHintArg=false
          allArg=false
          for arg in "$@"; do
            if [[ "$arg" == "--apply-hint" ]];
            then applyHintArg=true
            elif [[ "$arg" == "--all" ]];
            then allArg=true
            fi
          done

          function applyHint {
          while read -r fileName; do
          prefix="''${FLAKE_ROOT}/"
          if "$2" || (git diff --name-only | grep "''${fileName/#$prefix}")
          then
              # recursive because sometimes we have hints inside of other hints
              for i in {1..3}
              do
                  if hlint --only "Redundant bracket" "$fileName" | grep "No hints"
                  then
                      echo "No hints found: loop $i; file: $fileName"
                      break
                  else
                      # FIXME avoid creating extra file
                      echo "Applying redundant bracket hints automatically: loop $i; file: $fileName"
                      hlint --refactor --only "Redundant bracket" "$fileName" > "$fileName".tmp
                      cp "$fileName".tmp "$fileName"
                      rm "$fileName".tmp
                  fi
              done
          fi
          done <<< "$(find "$1" -iname "*.hs")"
          echo "Redundant bracket hints applied: $1; allArgs: $2"
          }

          if "$applyHintArg"
          then
            applyHint "''${FLAKE_ROOT}/Backend/app/rider-platform/rider-app/Main/src-read-only" "$allArg"
            applyHint "''${FLAKE_ROOT}/Backend/app/provider-platform/dynamic-offer-driver-app/Main/src-read-only" "$allArg"
            applyHint "''${FLAKE_ROOT}/Backend/lib/payment/src-read-only" "$allArg"
          else
            echo "No hints applied"
          fi
        '';
      };

      run-mobility-stack-nix = {
        category = "Backend";
        description = ''
          Run the nammayatri backend components via Nix.

          NOTE: This is slower, due to doing full nix build.
        '';
        exec = ''
          nix run .#run-mobility-stack-nix -- "$@"
        '';
      };

      run-mobility-stack-dev = {
        category = "Backend";
        description = ''
          Run the nammayatri backend components via "cabal run".
        '';
        exec = ''
          nix run .#run-mobility-stack-dev -- "$@"
        '';
      };

      kill-svc-ports = {
        category = "Backend";
        description = ''
          Free up ports by killing all the external-services.
        '';
        exec =
          let
            ports = import ./services/ports.nix;
          in
          lib.concatMapStrings
            (port: ''
              # Get the process ID using lsof for the specified port
              set +e
              pid=$(${lib.getExe pkgs.lsof} -ti:${builtins.toString port})
              set -e

              # Check if lsof returned any process ID
              if [ -n "$pid" ]; then
                echo "Sending SIGKILL to processes running on ${builtins.toString port}:"
                echo "$pid"
                echo "$pid" | ${pkgs.findutils}/bin/xargs kill -9
              else
                echo "No processes found on port ${builtins.toString port}"
              fi
            '')
            (lib.attrValues ports);
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

      run-load-test-dev = {
        category = "Backend";
        description = ''
          Run load tests
        '';
        exec = ''
          set -x
          nix run .#load-test-dev -- -p=0 "$@"
        '';
      };

      run-integration-tests = {
        category = "Backend";
        description = ''
          Run integration tests using newman cli
        '';
        exec = ''
          set -x
          cd "''${FLAKE_ROOT}/Backend/newman-tests"
          ./run-tests.sh
        '';
      };
    };
  };
}
