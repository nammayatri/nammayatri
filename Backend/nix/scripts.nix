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

      latest-docs = {
        category = "Backend";
        description = "Replica of https://hoogle.haskell.org/ to run locally.";
        exec = ''
          re_generate=false
          for arg in "$@"; do
            if [[ "$arg" == "--generate" ]]; then
              re_generate=true
              break
            fi
          done
          set -x
          if [ ! -d "''${FLAKE_ROOT}/Backend/hoogle-db" ] || [[ $re_generate == true ]]; then
            hoogle generate --download --database="''${FLAKE_ROOT}/Backend/hoogle-db/.hoogle"
          fi
          echo "#### Hoogle running at: http://localhost:8091"
          hoogle serve --port 8091 --database="''${FLAKE_ROOT}/Backend/hoogle-db/.hoogle"
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
          pathArg=""
          for arg in "$@"; do
            argName=$(echo "$arg" | cut -d "=" -f 1)
            argValue=$(echo "$arg" | cut -d "=" -f 2)
            if [[ "$arg" == "--apply-hint" ]];
            then applyHintArg=true
            elif [[ "$arg" == "--all" ]];
            then allArg=true
            elif [[ "$argName" == "--path" ]];
            then
              if [[ "$applyHintArg" == false ]]
              then
                echo "path arg works only with applyHint arg"
                exit 1
              else pathArg=$argValue
              fi
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
            if  [[ "$pathArg" == "" ]]
            then
              applyHint "''${FLAKE_ROOT}/Backend/app/rider-platform/rider-app/Main/src-read-only" "$allArg"
              applyHint "''${FLAKE_ROOT}/Backend/app/provider-platform/dynamic-offer-driver-app/Main/src-read-only" "$allArg"
              applyHint "''${FLAKE_ROOT}/Backend/lib/yudhishthira/src-read-only" "$allArg"
              applyHint "''${FLAKE_ROOT}/Backend/lib/payment/src-read-only" "$allArg"
              applyHint "''${FLAKE_ROOT}/Backend/lib/shared-services/src-read-only" "$allArg"
              applyHint "''${FLAKE_ROOT}/Backend/app/dashboard/provider-dashboard/src-read-only" "$allArg"
              applyHint "''${FLAKE_ROOT}/Backend/app/dashboard/rider-dashboard/src-read-only" "$allArg"
              applyHint "''${FLAKE_ROOT}/Backend/app/dashboard/CommonAPIs/src-read-only" "$allArg"
              applyHint "''${FLAKE_ROOT}/Backend/app/safety-dashboard/src-read-only" "$allArg"
            else
              applyHint "''${FLAKE_ROOT}/''${pathArg}" true
            fi
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
              pid=$(${pkgs.lsof}/bin/lsof -ti:${builtins.toString port})
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
          ${pkgs.tree}/bin/tree ./app/"''${name}"
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

      apply-hlint-staged = {
        category = "Backend";
        description = ''
          Apply hlint hints about redundant paranthesis to the staged files.
        '';
        exec = ''
          set -x
          for word in $(git diff --staged --name-only | grep .hs| grep .hs)
          do
            echo "''$word"
            hlint --refactor --only="Redundant bracket" --refactor-options="--inplace" "''$word"
          done
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
