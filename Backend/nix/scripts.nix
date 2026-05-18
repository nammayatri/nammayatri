# Common backend scripts available in devshell.
#
# We use https://github.com/Platonic-Systems/mission-control
_:
{
  debug = true;
  perSystem = { config, self', pkgs, lib, ... }:
    let
      # Shell snippet that SIGKILLs any process holding one of the service ports.
      # Shared between the standalone `kill-svc-ports` script and the
      # pre-flight inside `run-mobility-stack-dev`/`run-mobility-stack-nix`.
      killPortsSnippet = portList:
        lib.concatMapStrings
          (port: ''
            set +e
            pid=$(${pkgs.lsof}/bin/lsof -ti:${builtins.toString port})
            set -e
            if [ -n "$pid" ]; then
              echo "Sending SIGKILL to processes running on ${builtins.toString port}:"
              echo "$pid"
              echo "$pid" | ${pkgs.findutils}/bin/xargs kill -9
            else
              echo "No processes found on port ${builtins.toString port}"
            fi
          '')
          portList;
      killSvcPortsScript =
        let ports = import ./services/ports.nix; in
        killPortsSnippet (lib.attrValues ports);
      resolvePorts = import ./services/resolve-ports.nix { inherit pkgs lib; };
      # Ports owned by , run-local-test-dashboard (test-dashboard UI + test-local-api
      # + config-sync-server).
      localTestDashboardPorts = [ 7070 7083 8090 ];
      killLocalTestDashboardPortsScript = killPortsSnippet localTestDashboardPorts;
    in
    {
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
            # Skip update check if using local path (current_commit_hash will be null)
            if [[ "$current_commit_hash" == "null" || -z "$current_commit_hash" ]];
            then
              echo -e "\033[32mUsing local Namma-DSL path, skipping update check"
            else
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
                applyHint "''${FLAKE_ROOT}/Backend/lib/finance-kernel/src-read-only" "$allArg"
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
            Run the nammayatri backend components via Nix (This is slower, due to doing full nix build).
          '';
          exec = ''
            # Bump soft stack to the hard max. `nix run` spawns a fresh shell
            # that doesn't inherit the devshell's shellHook, so set it here too.
            # Required by process-compose / some Haskell exes that want >= 60 MB stack.
            _hard=$(ulimit -Hs 2>/dev/null || true)
            if [ -n "$_hard" ] && [ "$_hard" != "unlimited" ]; then
              ulimit -s "$_hard" 2>/dev/null || true
              ulimit -n "$_hard" 2>/dev/null || true
            fi
            # -S NAME forces stable sort by process name (no re-ordering on status change)
            nix run .#run-mobility-stack-nix -- -S NAME "$@"
          '';
        };

        run-mobility-stack-dev = {
          category = "Backend";
          description = "Run the nammayatri backend + test-context-api + mock-server (no test-dashboard).";
          exec = ''
            export DEV=1
            # Free up ports from previous run FIRST — so resolve-ports.sh
            # sees them as free and can reuse the same ports.
            echo "── Pre-flight: freeing stale service ports ──"
            RESOLVED_FILE="''${FLAKE_ROOT}/data/ports-resolved.nix"
            # Collect ports to kill from ports-resolved.nix or devbox-registry.json.
            PORTS_TO_KILL=""
            if [[ -f "$RESOLVED_FILE" ]]; then
              while IFS= read -r line; do
                if [[ "$line" =~ ^[[:space:]]*[a-zA-Z][a-zA-Z0-9_-]*[[:space:]]*=[[:space:]]*([0-9]+)[[:space:]]*\;  ]]; then
                  PORTS_TO_KILL="$PORTS_TO_KILL ''${BASH_REMATCH[1]}"
                fi
              done < "$RESOLVED_FILE"
            else
              # Fallback: read ports from devbox-registry.json if ports-resolved.nix
              # doesn't exist yet (e.g. first re-deploy after a crash).
              REGISTRY="/tmp/devbox-registry.json"
              if [[ -f "$REGISTRY" ]]; then
                # Derive dev name from FLAKE_ROOT: /tmp/<devname>/nammayatri → <devname>
                DEV_NAME=$(echo "''${FLAKE_ROOT}" | ${pkgs.gnused}/bin/sed -n 's|^/tmp/\([^/]*\)/nammayatri.*|\1|p')
                if [[ -n "$DEV_NAME" ]]; then
                  echo "ports-resolved.nix not found, falling back to devbox-registry.json (dev: $DEV_NAME)"
                  PORTS_TO_KILL=$(${pkgs.jq}/bin/jq -r ".users[\"$DEV_NAME\"].ports // {} | to_entries[].value" "$REGISTRY" 2>/dev/null || true)
                fi
              fi
            fi
            # Kill only processes whose cwd is under our FLAKE_ROOT.
            for port in $PORTS_TO_KILL; do
              set +e
              pids=$(${pkgs.lsof}/bin/lsof -ti:"$port")
              set -e
              for p in $pids; do
                proc_cwd=$(readlink /proc/"$p"/cwd 2>/dev/null || true)
                case "$proc_cwd" in
                  "''${FLAKE_ROOT}"*)
                    echo "Sending SIGKILL to pid $p on port $port (cwd: $proc_cwd)"
                    kill -9 "$p" 2>/dev/null || true
                    ;;
                  *)
                    echo "Port $port pid $p belongs to $proc_cwd, not ours — skipping"
                    ;;
                esac
              done
            done
            # Resolve ports.nix with available free ports so multiple
            # backend instances on the same devbox don't collide.
            echo "── Pre-flight: resolving free ports ──"
            ${lib.getExe resolvePorts}
            if [[ ! -f "$RESOLVED_FILE" ]]; then
              echo "ERROR: resolve-ports did not produce $RESOLVED_FILE" >&2
              exit 1
            fi
            # Write resolved ports to devbox-registry.json so other tools
            # (dashboard, other developers) can discover our ports.
            REGISTRY="/tmp/devbox-registry.json"
            DEV_NAME=$(echo "''${FLAKE_ROOT}" | ${pkgs.gnused}/bin/sed -n 's|^/tmp/\([^/]*\)/nammayatri.*|\1|p')
            if [[ -n "$DEV_NAME" ]]; then
              echo "── Pre-flight: updating devbox-registry.json (dev: $DEV_NAME) ──"
              # Parse ports-resolved.nix into a JSON object: {"name": port, ...}
              PORTS_JSON="{"
              first=true
              while IFS= read -r line; do
                if [[ "$line" =~ ^[[:space:]]*([a-zA-Z][a-zA-Z0-9_-]*)[[:space:]]*=[[:space:]]*([0-9]+)[[:space:]]*\;  ]]; then
                  name="''${BASH_REMATCH[1]}"
                  port="''${BASH_REMATCH[2]}"
                  if [[ "$first" == true ]]; then first=false; else PORTS_JSON+=","; fi
                  PORTS_JSON+="\"$name\":$port"
                fi
              done < "$RESOLVED_FILE"
              PORTS_JSON+="}"
              # Extract caddy port
              CADDY_PORT=$(echo "$PORTS_JSON" | ${pkgs.jq}/bin/jq -r '.["caddy-reverse-proxy"] // empty' 2>/dev/null || true)
              # Read existing registry or create empty one
              if [[ -f "$REGISTRY" ]]; then
                REG=$(cat "$REGISTRY")
              else
                REG='{"users":{}}'
              fi
              # Update this developer's entry with dir, ports, and caddyPort
              REG=$(echo "$REG" | ${pkgs.jq}/bin/jq \
                --arg dev "$DEV_NAME" \
                --arg dir "''${FLAKE_ROOT}" \
                --argjson ports "$PORTS_JSON" \
                --argjson caddy "''${CADDY_PORT:-null}" \
                '.users[$dev] = {dir: $dir, ports: $ports, caddyPort: $caddy}')
              echo "$REG" > "$REGISTRY"
            fi
            # Generate Caddyfile from resolved ports for single-entry-point access.
            # Service list lives in Backend/nix/services/caddyfile.nix; we
            # evaluate it via nix-instantiate so the source-of-truth is Nix,
            # not a shell array. Falls back to the base ports.nix if the
            # resolved overlay isn't on disk yet.
            echo "── Pre-flight: generating Caddyfile ──"
            CADDY_PORTS_FILE="$RESOLVED_FILE"
            if [[ ! -f "$CADDY_PORTS_FILE" ]]; then
              CADDY_PORTS_FILE="''${FLAKE_ROOT}/Backend/nix/services/ports.nix"
            fi
            mkdir -p "''${FLAKE_ROOT}/data"
            nix-instantiate --eval --raw --impure --expr \
              "import ''${FLAKE_ROOT}/Backend/nix/services/caddyfile.nix { ports = import $CADDY_PORTS_FILE; }" \
              > "''${FLAKE_ROOT}/data/Caddyfile"
            if [[ ! -s "''${FLAKE_ROOT}/data/Caddyfile" ]]; then
              echo "ERROR: Caddyfile generation produced an empty file" >&2
              exit 1
            fi
            echo "Generated Caddyfile at ''${FLAKE_ROOT}/data/Caddyfile"
            # Bump soft stack to the hard max. `nix run` spawns a fresh shell
            # that doesn't inherit the devshell's shellHook, so set it here too.
            # Required by process-compose / some Haskell exes that want >= 60 MB stack.
            _hard=$(ulimit -Hs 2>/dev/null || true)
            if [ -n "$_hard" ] && [ "$_hard" != "unlimited" ]; then
              ulimit -s "$_hard" 2>/dev/null || true
              ulimit -n "$_hard" 2>/dev/null || true
            fi
            # Tell nammayatri.nix where to find the resolved ports file.
            # --impure lets builtins.getEnv read this at Nix evaluation time.
            export PORTS_RESOLVED_FILE="$RESOLVED_FILE"
            # -S NAME forces stable sort by process name (no re-ordering on status change)
            nix run --impure .#run-mobility-stack-dev -- -S NAME "$@"
          '';
        };

        run-mobility-stack-full = {
          category = "Backend";
          description = "Run the FULL nammayatri stack in one terminal: backend + test-context-api + mock-server + test-local-api + test-dashboard.";
          exec = ''
            echo "── Pre-flight: freeing service ports ──"
            ${killSvcPortsScript}
            echo "── Pre-flight: freeing dashboard ports ──"
            ${killLocalTestDashboardPortsScript}
            _hard=$(ulimit -Hs 2>/dev/null || true)
            if [ -n "$_hard" ] && [ "$_hard" != "unlimited" ]; then
              ulimit -s "$_hard" 2>/dev/null || true
              ulimit -n "$_hard" 2>/dev/null || true
            fi
            nix run .#run-mobility-stack-full -- -S NAME "$@"
          '';
        };

        run-local-test-dashboard = {
          category = "Backend";
          description = "Run only the test-dashboard (port 7070, React) + test-local-api (port 7083).";
          exec = ''
            echo "── Pre-flight: freeing dashboard ports ──"
            ${killLocalTestDashboardPortsScript}
            _hard=$(ulimit -Hs 2>/dev/null || true)
            if [ -n "$_hard" ] && [ "$_hard" != "unlimited" ]; then
              ulimit -s "$_hard" 2>/dev/null || true
              ulimit -n "$_hard" 2>/dev/null || true
            fi
            nix run .#run-local-test-dashboard -- -S NAME "$@"
          '';
        };

        kill-svc-ports = {
          category = "Backend";
          description = ''
            Free up ports by killing all the external-services.
          '';
          exec = killSvcPortsScript;
        };

        clear-data = {
          category = "Backend";
          description = "Wipe runtime state under <repo-root>/data/ (preserves ny-react-native/ and control-center/); asks for confirmation.";
          exec = ''
            set -euo pipefail
            DATA_DIR="''${FLAKE_ROOT}/data"
            if [ ! -d "$DATA_DIR" ]; then
              echo "No $DATA_DIR — nothing to clear."
              exit 0
            fi

            KEEP_RE='^(ny-react-native|control-center)$'
            to_delete=()
            while IFS= read -r entry; do
              name=$(basename "$entry")
              if echo "$name" | ${pkgs.gnugrep}/bin/grep -qE "$KEEP_RE"; then
                continue
              fi
              to_delete+=("$entry")
            done < <(${pkgs.findutils}/bin/find "$DATA_DIR" -mindepth 1 -maxdepth 1)

            if [ ''${#to_delete[@]} -eq 0 ]; then
              echo "Nothing to clear under $DATA_DIR (only ny-react-native / control-center present)."
              exit 0
            fi

            echo "── Will delete the following under $DATA_DIR ──"
            for p in "''${to_delete[@]}"; do
              size=$(${pkgs.coreutils}/bin/du -sh "$p" 2>/dev/null | ${pkgs.coreutils}/bin/cut -f1 || echo "?")
              echo "  $size  $p"
            done
            echo "── Will KEEP ──"
            for k in ny-react-native control-center; do
              if [ -d "$DATA_DIR/$k" ]; then
                size=$(${pkgs.coreutils}/bin/du -sh "$DATA_DIR/$k" 2>/dev/null | ${pkgs.coreutils}/bin/cut -f1 || echo "?")
                echo "  $size  $DATA_DIR/$k"
              fi
            done
            echo ""
            printf "Proceed with deletion? [y/N] "
            read -r ans
            case "$ans" in
              y|Y|yes|YES) ;;
              *) echo "Aborted."; exit 0 ;;
            esac

            for p in "''${to_delete[@]}"; do
              echo "rm -rf $p"
              rm -rf -- "$p"
            done
            echo "Done."
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

        run-mobility-stack-test = {
          category = "Backend";
          description = "Start the full mobility stack, import config from master, run integration tests, then stop everything.";
          exec =
            let
              # All ports that run-mobility-stack-dev uses
              systemPorts = [
                5434
                5435 # postgres primary + replica
                6379 # redis
                30001
                30002
                30003
                30004
                30005
                30006 # redis cluster
                2181
                29092 # zookeeper + kafka
                8085 # nginx
                5422
                8079 # passetto db + service
                5001 # osrm
              ];
              servicePorts = [
                8013 # rider-app
                8016 # driver-proxy (external driver-app port)
                8116 # driver-app (internal, behind driver-proxy)
                8015 # beckn-gateway
                8018 # provider-dashboard
                8019 # mock-google
                8020 # mock-registry
                8080 # mock-server (python)
                8081 # location-tracking-service
                4343 # mock-sms
                4545 # mock-fcm
                6235 # mock-idfy
              ];
              allPorts = systemPorts ++ servicePorts;
              portCheckStr = builtins.concatStringsSep " " (map builtins.toString allPorts);
              servicePortStr = builtins.concatStringsSep " " (map builtins.toString servicePorts);
              serviceHealthPorts = builtins.concatStringsSep " " (map builtins.toString [ 8013 8016 ]);
            in
            ''
              set -euo pipefail

              FLAKE="''${FLAKE_ROOT}"
              STACK_PID=""
              TEST_EXIT=1

              cleanup() {
                echo ""
                echo "═══════════════════════════════════════"
                echo "  Stopping mobility stack..."
                echo "═══════════════════════════════════════"
                if [ -n "$STACK_PID" ]; then
                  kill "$STACK_PID" 2>/dev/null || true
                  wait "$STACK_PID" 2>/dev/null || true
                fi
                # Kill any remaining services on known ports
                for port in ${portCheckStr}; do
                  pid=$(${pkgs.lsof}/bin/lsof -ti:"$port" 2>/dev/null || true)
                  if [ -n "$pid" ]; then
                    echo "$pid" | xargs kill -9 2>/dev/null || true
                  fi
                done
                echo "  Done."
                exit "$TEST_EXIT"
              }
              trap cleanup EXIT INT TERM

              # ── Step 1: Check if ports are free, wait up to 5 mins ──
              echo ""
              echo "═══════════════════════════════════════"
              echo "  Step 1: Checking ports..."
              echo "═══════════════════════════════════════"

              MAX_WAIT=300
              INTERVAL=30
              WAITED=0

              while true; do
                BUSY=""
                for port in ${portCheckStr}; do
                  if ${pkgs.lsof}/bin/lsof -ti:"$port" >/dev/null 2>&1; then
                    BUSY="$BUSY $port"
                  fi
                done

                if [ -z "$BUSY" ]; then
                  echo "  All ports free."
                  break
                fi

                if [ "$WAITED" -ge "$MAX_WAIT" ]; then
                  echo "  ERROR: Ports still busy after ''${MAX_WAIT}s:$BUSY"
                  echo "  Run: , kill-svc-ports"
                  TEST_EXIT=1
                  exit 1
                fi

                echo "  Ports busy:$BUSY — waiting ''${INTERVAL}s ($WAITED/''${MAX_WAIT}s)..."
                sleep "$INTERVAL"
                WAITED=$((WAITED + INTERVAL))
              done

              # ── Step 2: Start the mobility stack in background ──
              echo ""
              echo "═══════════════════════════════════════"
              echo "  Step 2: Starting mobility stack..."
              echo "═══════════════════════════════════════"

              nix run .#run-mobility-stack-dev &
              STACK_PID=$!

              # ── Step 3: Wait for core services to be healthy ──
              echo ""
              echo "═══════════════════════════════════════"
              echo "  Step 3: Waiting for services..."
              echo "═══════════════════════════════════════"

              MAX_HEALTH_WAIT=600
              HEALTH_WAITED=0

              while true; do
                ALL_HEALTHY=true
                for port in ${serviceHealthPorts}; do
                  if ! curl -sf "http://localhost:$port" >/dev/null 2>&1; then
                    ALL_HEALTHY=false
                    break
                  fi
                done

                if [ "$ALL_HEALTHY" = true ]; then
                  echo "  All services healthy!"
                  break
                fi

                if [ "$HEALTH_WAITED" -ge "$MAX_HEALTH_WAIT" ]; then
                  echo "  ERROR: Services not healthy after ''${MAX_HEALTH_WAIT}s"
                  TEST_EXIT=1
                  exit 1
                fi

                echo "  Waiting for services... ($HEALTH_WAITED/''${MAX_HEALTH_WAIT}s)"
                sleep 15
                HEALTH_WAITED=$((HEALTH_WAITED + 15))
              done

              # ── Step 4: Import config from master ──
              # Passetto is already running as part of run-mobility-stack-dev (process-compose)
              echo ""
              echo "═══════════════════════════════════════"
              echo "  Step 4: Importing config from master..."
              echo "═══════════════════════════════════════"

              cd "$FLAKE/Backend/dev/config-sync"
              ${pkgs.python3.withPackages (ps: [ ps.psycopg2 ps.requests ])}/bin/python3 config_transfer.py import --from master --to local || {
                echo "  WARNING: Config import failed, continuing with existing data..."
              }

              # ── Step 5: Restart services to refresh in-memory config ──
              echo ""
              echo "═══════════════════════════════════════"
              echo "  Step 5: Restarting services..."
              echo "═══════════════════════════════════════"

              # Kill service processes one by one (infrastructure stays up)
              for port in ${servicePortStr}; do
                pid=$(${pkgs.lsof}/bin/lsof -ti:"$port" 2>/dev/null || true)
                if [ -n "$pid" ]; then
                  echo "  Stopping service on port $port (pid $pid)..."
                  echo "$pid" | xargs kill 2>/dev/null || true
                fi
              done
              sleep 2
              # Force-kill any stragglers
              for port in ${servicePortStr}; do
                pid=$(${pkgs.lsof}/bin/lsof -ti:"$port" 2>/dev/null || true)
                if [ -n "$pid" ]; then
                  echo "  Force-killing straggler on port $port..."
                  echo "$pid" | xargs kill -9 2>/dev/null || true
                fi
              done

              # Flush Redis caches so services load fresh config
              ${pkgs.redis}/bin/redis-cli FLUSHALL 2>/dev/null || true
              ${pkgs.redis}/bin/redis-cli -p 30001 -c FLUSHALL 2>/dev/null || true
              echo "  Redis flushed."

              # Stop the whole stack and restart (ensures all processes come back)
              if [ -n "$STACK_PID" ]; then
                kill "$STACK_PID" 2>/dev/null || true
                wait "$STACK_PID" 2>/dev/null || true
                STACK_PID=""
              fi
              # Clean up any remaining processes
              for port in ${portCheckStr}; do
                pid=$(${pkgs.lsof}/bin/lsof -ti:"$port" 2>/dev/null || true)
                if [ -n "$pid" ]; then
                  echo "$pid" | xargs kill -9 2>/dev/null || true
                fi
              done

              echo "  Restarting mobility stack..."
              nix run .#run-mobility-stack-dev &
              STACK_PID=$!

              # Wait for services to be healthy again
              HEALTH_WAITED=0
              while true; do
                ALL_HEALTHY=true
                for port in ${serviceHealthPorts}; do
                  if ! curl -sf "http://localhost:$port" >/dev/null 2>&1; then
                    ALL_HEALTHY=false
                    break
                  fi
                done

                if [ "$ALL_HEALTHY" = true ]; then
                  echo "  All services restarted and healthy!"
                  break
                fi

                if [ "$HEALTH_WAITED" -ge "$MAX_HEALTH_WAIT" ]; then
                  echo "  ERROR: Services not healthy after restart (''${MAX_HEALTH_WAIT}s)"
                  TEST_EXIT=1
                  exit 1
                fi

                echo "  Waiting for services after restart... ($HEALTH_WAITED/''${MAX_HEALTH_WAIT}s)"
                sleep 15
                HEALTH_WAITED=$((HEALTH_WAITED + 15))
              done

              # ── Step 6: Run integration tests ──
              echo ""
              echo "═══════════════════════════════════════"
              echo "  Step 6: Running integration tests..."
              echo "═══════════════════════════════════════"

              cd "$FLAKE/Backend/dev/integration-tests"
              if ./run-tests.sh "$@"; then
                TEST_EXIT=0
                echo ""
                echo "═══════════════════════════════════════"
                echo "  ALL TESTS PASSED"
                echo "═══════════════════════════════════════"
              else
                TEST_EXIT=$?
                echo ""
                echo "═══════════════════════════════════════"
                echo "  TESTS FAILED (exit code: $TEST_EXIT)"
                echo "═══════════════════════════════════════"
              fi
              # cleanup runs via trap
            '';
        };
      };
    };
}
