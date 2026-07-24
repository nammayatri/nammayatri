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
      # Nix's glibc resolves locale names ONLY through LOCALE_ARCHIVE. On a
      # non-NixOS host that variable points at the system archive
      # (/usr/lib/locale/locale-archive), which has no C.UTF-8 entry — glibc
      # >= 2.35 compiles that locale in instead of shipping it in the archive.
      # Nix still hands every child LC_ALL=C.UTF-8, so each service's shell
      # prints "setlocale: LC_ALL: cannot change locale (C.UTF-8)" at startup.
      # Pointing at a nix-built archive that *does* contain C.UTF-8 fixes it at
      # the source, for every process, without downgrading anyone to LC_ALL=C.
      # Lazily evaluated — only forced on Linux (see the isLinux guard below).
      localeArchive = "${pkgs.glibcLocalesUtf8 or pkgs.glibcLocales}/lib/locale/locale-archive";
      # Ports owned by , run-local-test-dashboard (test-dashboard UI + test-local-api
      # + config-sync-server).
      localTestDashboardPorts = [ 7070 7083 8090 ];
      killLocalTestDashboardPortsScript = killPortsSnippet localTestDashboardPorts;
      # Python interpreter with the deps that Backend/dev/config-sync needs
      # (see config-sync/requirements.txt). Used by the `import-config-data` script.
      configSyncPython = pkgs.python3.withPackages (ps: with ps; [
        boto3
        psycopg2
        requests
        rich
        python-dotenv
        websockets
      ]);
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

        run-cabal-build-devbox = {
          category = "Backend";
          description = "Auto-assign a dev-box worker and run cache-restore + cabal build all remotely, streaming output here. Flags: --reassign (pick a fresh box), --clean/--no-clean (skip the cabal-clean prompt).";
          cdToProjectRoot = false;
          exec = ''
            export PATH="${lib.makeBinPath (with pkgs; [ curl jq rsync openssh git coreutils ])}:$PATH"
            set -euo pipefail

            REPO_ROOT=$(git -C "''${FLAKE_ROOT}" rev-parse --show-toplevel)
            BASE_API="''${BASE_API:-http://34.100.155.111:8787}"
            # Same pin file the test dashboard's local-api writes — so , run-cabal-build-devbox
            # and the dashboard's Deploy/Start land on the SAME machine + /tmp/<id>/nammayatri.
            ID_FILE="$REPO_ROOT/.devbox-id.json"

            # ── flags ──
            REASSIGN=0; CLEAN_ARG=""
            for a in "$@"; do
              case "$a" in
                --reassign) REASSIGN=1 ;;
                --clean)    CLEAN_ARG=1 ;;
                --no-clean) CLEAN_ARG=0 ;;
              esac
            done

            # ── cabal clean? wipe dist-newstyle so cache-restore pulls a fresh
            #    CI cache; default No = build on the existing dist-newstyle. ──
            if [ -n "$CLEAN_ARG" ]; then
              CLEAN="$CLEAN_ARG"
            else
              printf 'Run cabal clean first (wipe dist-newstyle for a fresh cache-restore)? [y/N]: ' >&2
              read -r DO_CLEAN
              CLEAN=0
              case "$DO_CLEAN" in [yY]|[yY][eE][sS]) CLEAN=1 ;; esac
            fi

            # ── auto-assign dev-box: reuse the machine pinned in .devbox-id.json,
            #    else auto-pick the registered dev-box with the LOWEST RAM usage.
            #    The developer id is derived from $USER + machine (no prompt).
            #    --reassign, or a pinned machine that has left the fleet, forces a
            #    fresh pick (and a new id). Machine is pinned by NAME; its IP is
            #    re-resolved live every run, so DHCP changes don't break it. ──
            PIN_MACHINE=""
            if [ "$REASSIGN" -eq 0 ] && [ -f "$ID_FILE" ]; then
              PIN_MACHINE=$(jq -r '.machine // ""' "$ID_FILE" 2>/dev/null || true)
            fi

            STATUS=$(curl -fsS --max-time 5 "$BASE_API/api/status") \
              || { echo "base station API unreachable at $BASE_API" >&2; exit 1; }
            SELROW=$(printf '%s' "$STATUS" | jq -r --arg pin "$PIN_MACHINE" '
              [ .workers[]
                | select(.type=="dev-box")
                | { name:(.name//""), lip:(.localIp//""), aip:(.awsIp//""), user:(.username//""),
                    pct:( (.usage.ram // "") as $r
                          | if ($r|test("[0-9.]+%"))
                            then ($r|capture("(?<p>[0-9.]+)%").p|tonumber) else 100 end ) } ]
              | ( map(select(.name==$pin)) | .[0] ) as $pinned
              | ( $pinned // (sort_by(.pct) | .[0]) ) as $sel
              | if $sel==null then empty
                else [ $sel.name, $sel.lip, $sel.aip, $sel.user,
                       (if $pinned then "pin" else "auto" end) ] | join("|") end
            ')
            if [ -z "$SELROW" ]; then
              echo "No dev-box machines registered at $BASE_API/api/status" >&2; exit 1
            fi
            IFS='|' read -r NAME LOCAL_IP AWS_IP RUSER PICKMODE <<<"$SELROW"
            HOST="''${LOCAL_IP:-$AWS_IP}"
            PORT=22
            if [ -z "$HOST" ]; then echo "dev-box '$NAME' has no reachable IP" >&2; exit 1; fi

            # ── developer id: reuse the pinned id, else mint <user>-<machine>-<rand6> ──
            DEV_ID=""
            if [ "$PICKMODE" = "pin" ] && [ -f "$ID_FILE" ]; then
              DEV_ID=$(jq -r '.id // ""' "$ID_FILE" 2>/dev/null || true)
            fi
            if [ -z "''${DEV_ID:-}" ]; then
              LOCAL_USER=$(printf '%s' "''${USER:-dev}" | tr '[:upper:]' '[:lower:]' | tr -cd 'a-z0-9')
              [ -z "$LOCAL_USER" ] && LOCAL_USER=dev
              MSLUG=$(printf '%s' "$NAME" | tr '[:upper:]' '[:lower:]' | tr -c 'a-z0-9' '-' | tr -s '-')
              MSLUG="''${MSLUG#-}"; MSLUG="''${MSLUG%-}"; MSLUG="''${MSLUG:0:24}"
              RAND=$(head -c3 /dev/urandom | od -An -tx1 | tr -d ' \n')
              DEV_ID="$LOCAL_USER-$MSLUG-$RAND"
              jq -n --arg id "$DEV_ID" --arg m "$NAME" --arg su "$RUSER" \
                    --arg lu "$LOCAL_USER" --arg ca "$(date +%Y-%m-%dT%H:%M:%S)" \
                '{id:$id, machine:$m, sshUser:$su, localUser:$lu, createdAt:$ca}' > "$ID_FILE"
              echo "Auto-assigned dev-box: $NAME (lowest RAM usage) → id $DEV_ID [pinned in .devbox-id.json]" >&2
            else
              echo "Using pinned dev-box: $NAME → id $DEV_ID [.devbox-id.json]" >&2
            fi
            REMOTE_DIR="/tmp/$DEV_ID/nammayatri"
            echo "Target: $RUSER@$HOST:$REMOTE_DIR  port=$PORT" >&2

            # ── ssh key discovery; generate a key if the user has none ──
            SSH_KEY=""
            for k in "$HOME/.ssh/id_ed25519" "$HOME/.ssh/id_rsa" "$HOME/.ssh/id_ecdsa"; do
              if [ -f "$k" ]; then SSH_KEY="$k"; break; fi
            done
            SSH_OPTS=(-o StrictHostKeyChecking=accept-new -o ServerAliveInterval=15 -o GSSAPIAuthentication=no -o ConnectTimeout=10 -p "$PORT")
            if [ -n "$SSH_KEY" ]; then SSH_OPTS+=(-i "$SSH_KEY"); fi
            # key-only preflight; if it fails, generate a key (when absent) then copy it up.
            if ! ssh "''${SSH_OPTS[@]}" -o BatchMode=yes "$RUSER@$HOST" 'echo ok' >/dev/null 2>&1; then
              if [ -z "$SSH_KEY" ]; then
                echo "No SSH key found in ~/.ssh — generating an ed25519 key pair ..." >&2
                mkdir -p "$HOME/.ssh" && chmod 700 "$HOME/.ssh"
                ssh-keygen -t ed25519 -N "" -f "$HOME/.ssh/id_ed25519" -C "$DEV_ID@devbox" >&2
                SSH_KEY="$HOME/.ssh/id_ed25519"
                SSH_OPTS+=(-i "$SSH_KEY")
              fi
              echo "Authorizing your SSH key on $RUSER@$HOST (enter the $RUSER password when prompted) ..." >&2
              if ! ssh-copy-id -i "$SSH_KEY.pub" -p "$PORT" "$RUSER@$HOST"; then
                echo "ssh-copy-id failed. Run manually: ssh-copy-id -i $SSH_KEY.pub -p $PORT $RUSER@$HOST" >&2
                exit 1
              fi
              if ! ssh "''${SSH_OPTS[@]}" -o BatchMode=yes "$RUSER@$HOST" 'echo ok' >/dev/null 2>&1; then
                echo "SSH key still not authorized on $RUSER@$HOST after ssh-copy-id." >&2
                exit 1
              fi
              echo "SSH key authorized on $RUSER@$HOST." >&2
            fi

            # ── nearest 'minio-pushed'-tagged commit, from LOCAL git ──
            git -C "$REPO_ROOT" fetch --quiet origin 'refs/tags/minio-pushed/*:refs/tags/minio-pushed/*' 2>/dev/null || true
            git -C "$REPO_ROOT" fetch --quiet origin main 2>/dev/null || true
            BASE="HEAD"
            MB=$(git -C "$REPO_ROOT" merge-base origin/main HEAD 2>/dev/null || true)
            if [ -n "$MB" ]; then BASE="$MB"; fi
            CACHE_SHA=""
            while IFS= read -r sha; do
              if git -C "$REPO_ROOT" rev-parse -q --verify "refs/tags/minio-pushed/$sha" >/dev/null 2>&1; then
                CACHE_SHA="$sha"; break
              fi
            done < <(git -C "$REPO_ROOT" log --format=%H -n 30 "$BASE" 2>/dev/null)
            echo "cache commit: ''${CACHE_SHA:-<none — remote builds from scratch>}" >&2

            # ── (iii) rsync deploy (same excludes as the dashboard) ──
            RSYNC_SSH="ssh -o StrictHostKeyChecking=accept-new -o BatchMode=yes -o ConnectTimeout=10 -p $PORT"
            if [ -n "$SSH_KEY" ]; then RSYNC_SSH="$RSYNC_SSH -i $SSH_KEY"; fi
            echo "Deploying $REPO_ROOT -> $RUSER@$HOST:$REMOTE_DIR ..." >&2
            rsync -az --delete --info=progress2 \
              -e "$RSYNC_SSH" \
              --rsync-path "mkdir -p $REMOTE_DIR && rsync" \
              --exclude data --exclude node_modules --exclude dist-newstyle --exclude dist \
              --exclude .direnv --exclude _build --exclude result --exclude 'result-*' \
              --exclude .cabal-dir --exclude .git --exclude .hie --exclude hie \
              --exclude .nix-deps --exclude .ci-project-root --exclude .ci-cabal-dir \
              --exclude cabal.project.local \
              --exclude 'Frontend/android-native' --exclude 'Frontend/ios' \
              --exclude 'Frontend/build' --exclude 'Frontend/dist' \
              "$REPO_ROOT/" "$RUSER@$HOST:$REMOTE_DIR/"

            # ── (iv) minimal 1-commit repo + write Backend/.ci-cache-sha ──
            # git-ignore the devbox-only build artifacts BEFORE `git add -A`, so
            # `nix develop` copies only real source to the store (hie/ alone is
            # ~400MB of tiny files and stalls the copy otherwise).
            ssh "''${SSH_OPTS[@]}" -o BatchMode=yes "$RUSER@$HOST" \
              "cd $REMOTE_DIR && rm -rf .git && git init -q && printf '%s\n' 'dist-newstyle/' 'hie/' '.hie/' '.nix-deps/' '.cabal-dir/' '.ci-project-root' '.ci-cabal-dir' '.ci-cache-sha' > .git/info/exclude && git add -A && GIT_AUTHOR_NAME=deploy GIT_AUTHOR_EMAIL=deploy@deploy GIT_COMMITTER_NAME=deploy GIT_COMMITTER_EMAIL=deploy@deploy git commit -q -m deploy --allow-empty 2>/dev/null || true; mkdir -p Backend && printf '%s' '$CACHE_SHA' > Backend/.ci-cache-sha"

            # ── (v) cache-restore + cabal build all, streamed here via ssh -tt ──
            echo "== running cache-restore + cabal build all on $NAME ==" >&2
            ssh -tt "''${SSH_OPTS[@]}" "$RUSER@$HOST" \
              "bash $REMOTE_DIR/Backend/nix/devbox/remote-run.sh $REMOTE_DIR/Backend $CLEAN"
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
            # -S NAME forces stable sort by process name (no re-ordering on status change).
            # -d hides `disabled = true` processes (the ones this profile never
            # starts) so the list only shows what the profile actually runs;
            # they are still defined, so `process-compose process start <name>`
            # can bring one up on demand.
            nix run .#run-mobility-stack-nix -- -S NAME -d "$@"
          '';
        };

        run-mobility-stack-dev = {
          category = "Backend";
          description = "Run the nammayatri backend + test-context-api + mock-server (no test-dashboard).";
          exec = ''
            # errexit + pipefail for the whole preflight (matches sibling scripts).
            # nounset is intentionally omitted — this legacy block has guarded
            # optional refs (''${VAR:-}) but wasn't audited for a hard set -u.
            set -eo pipefail
            # NOTE: DEV is intentionally NOT exported. Only the location-tracking-service
            # reads it: with DEV=1 the LTS skips /internal/auth and uses the raw `token`
            # header as the driver id, which breaks real apps/emulators (they send a
            # registration token, so drivers never enter the geo pool and never get
            # nearbyRideRequest). Integration-test collections now send {{driver_token}}
            # to the LTS, so they work in authenticated (non-DEV) mode too.
            # Give nix's glibc a locale archive that actually contains the
            # C.UTF-8 it hands to every child (see `localeArchive` above).
            # process-compose passes its environment to each service, so this
            # one export silences the setlocale warning for the whole stack.
            ${lib.optionalString pkgs.stdenv.isLinux ''
              export LOCALE_ARCHIVE="${localeArchive}"
            ''}

            REGISTRY="/tmp/devbox-registry.json"
            DEVBOX_KEY=""
            if [[ -f "''${FLAKE_ROOT}/.devbox-id.json" ]]; then
              DEVBOX_KEY=$(${pkgs.jq}/bin/jq -r '.id // ""' "''${FLAKE_ROOT}/.devbox-id.json" 2>/dev/null || true)
            fi
            if [[ -z "$DEVBOX_KEY" ]]; then
              DEVBOX_KEY=$(echo "''${FLAKE_ROOT}" | ${pkgs.gnused}/bin/sed -n 's|^/tmp/\([^/]*\)/nammayatri.*|\1|p')
            fi
            if [[ -z "$DEVBOX_KEY" ]]; then
              DEVBOX_KEY="local-$(id -un 2>/dev/null || echo dev)"
            fi
            # Exported for: resolve-ports (self-exclusion), the Caddyfile eval,
            # and nammayatri.nix (module + test-context-api) — all key off these.
            DEVBOX_REGISTRY_FILE="$REGISTRY"
            export DEVBOX_KEY DEVBOX_REGISTRY_FILE

            # Kill this workspace's stale process-compose first (it has no port,
            # so the port kill below can't reach it, but it respawns children
            # onto the ports we free). cwd under FLAKE_ROOT scopes it to us.
            _pcs=$(${pkgs.procps}/bin/pgrep -x process-compose 2>/dev/null || true)
            _superseded=""
            for p in $_pcs; do
              cwd=$(readlink /proc/"$p"/cwd 2>/dev/null || true)
              case "$cwd" in
                "''${FLAKE_ROOT}"*)
                  echo "── Pre-flight: superseding stale stack (process-compose pid $p) ──"
                  kill -TERM "$p" 2>/dev/null || true
                  _superseded=1
                  ;;
              esac
            done
            [[ -n "$_superseded" ]] && sleep 3

            # Free up ports from the PREVIOUS run FIRST — so resolve-ports sees
            # them free and can reuse the same numbers. The previous run's ports
            # live in our registry slice (devbox-registry.json[KEY]).
            echo "── Pre-flight: freeing stale service ports ──"
            PORTS_TO_KILL=""
            if [[ -f "$REGISTRY" ]]; then
              # For redis-cluster-* ports also emit the cluster-bus port
              # (client-port + 10000, e.g. 30040 -> 40040): a stale node can
              # survive holding only that and block the next cluster startup.
              PORTS_TO_KILL=$(${pkgs.jq}/bin/jq -r --arg me "$DEVBOX_KEY" \
                '.users[$me].ports // {} | to_entries[] | if (.key | startswith("redis-cluster")) then "\(.value)\n\(.value + 10000)" else "\(.value)" end' \
                "$REGISTRY" 2>/dev/null || true)
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

            # ── Acquire an exclusive lock so concurrent startups on the same
            #    devbox serialise their resolve-ports + registry-write (prevents
            #    two developers resolving to the same ports). fd-based: released
            #    automatically if the process exits on error or signal. ──
            LOCKFILE="/tmp/devbox-ports.lock"
            exec 9>"$LOCKFILE"
            ${pkgs.util-linux}/bin/flock 9
            echo "── Pre-flight: acquired devbox port lock ──"

            # ── Resolve free ports. resolve-ports prints the resolved map as
            #    JSON on stdout (diagnostics go to stderr). ──
            echo "── Pre-flight: resolving free ports ──"
            PORTS_JSON=$(${lib.getExe resolvePorts})
            if ! echo "$PORTS_JSON" | ${pkgs.jq}/bin/jq -e 'type == "object" and length > 0' >/dev/null 2>&1; then
              echo "ERROR: resolve-ports did not produce a valid ports JSON object" >&2
              exit 1
            fi

            # ── Record our slice in the registry so nammayatri.nix, the
            #    Caddyfile, the dashboard and other developers can read it.
            #    ATOMIC (temp + rename): a concurrent reader — our own Nix eval,
            #    or another dev's preflight — never sees a half-written file.
            #    We touch only .users[KEY]; every other entry is preserved. ──
            echo "── Pre-flight: updating devbox-registry.json (key: $DEVBOX_KEY) ──"
            CADDY_PORT=$(echo "$PORTS_JSON" | ${pkgs.jq}/bin/jq -r '.["caddy-reverse-proxy"] // empty' 2>/dev/null || true)
            if [[ -f "$REGISTRY" ]]; then REG=$(cat "$REGISTRY"); else REG='{"users":{}}'; fi
            REG=$(echo "$REG" | ${pkgs.jq}/bin/jq \
              --arg dev "$DEVBOX_KEY" \
              --arg dir "''${FLAKE_ROOT}" \
              --argjson ports "$PORTS_JSON" \
              --argjson caddy "''${CADDY_PORT:-null}" \
              '.users[$dev] = {dir: $dir, ports: $ports, caddyPort: $caddy}')
            # temp + rename = atomic; set -eo pipefail aborts if either fails.
            printf '%s\n' "$REG" > "$REGISTRY.tmp.$$"
            mv -f "$REGISTRY.tmp.$$" "$REGISTRY"

            # ── Release the devbox port lock now that the registry is written. ──
            ${pkgs.util-linux}/bin/flock -u 9
            exec 9>&-
            echo "── Pre-flight: released devbox port lock ──"

            # ── Generate the Caddyfile from our registry slice (single entry
            #    point). Service list is the source of truth in caddyfile.nix.
            #    The expression is single-quoted so the key never enters the Nix
            #    source text: it reads FLAKE_ROOT / DEVBOX_REGISTRY_FILE /
            #    DEVBOX_KEY via getEnv (same, injection-proof mechanism as
            #    nammayatri.nix) and falls back to base ports.nix if the registry
            #    file is missing. ──
            echo "── Pre-flight: generating Caddyfile ──"
            mkdir -p "''${FLAKE_ROOT}/data"
            # shellcheck disable=SC2016  # single quotes intentional: the expr is Nix, not shell
            nix eval --raw --impure --expr '
              let root = builtins.getEnv "FLAKE_ROOT";
                  base = import (/. + (root + "/Backend/nix/services/ports.nix"));
                  registryPath = builtins.getEnv "DEVBOX_REGISTRY_FILE";
                  devKey = builtins.getEnv "DEVBOX_KEY";
                  ports =
                    if registryPath != "" && devKey != "" && builtins.pathExists (/. + registryPath)
                    then (builtins.fromJSON (builtins.readFile (/. + registryPath))).users.''${devKey}.ports or base
                    else base;
              in import (/. + (root + "/Backend/nix/services/caddyfile.nix")) { inherit ports; }
            ' > "''${FLAKE_ROOT}/data/Caddyfile"
            if [[ ! -s "''${FLAKE_ROOT}/data/Caddyfile" ]]; then
              echo "ERROR: Caddyfile generation produced an empty file" >&2
              exit 1
            fi
            echo "Generated Caddyfile at ''${FLAKE_ROOT}/data/Caddyfile"

            # ── Publish our slice at data/devbox-ports.json — the single source
            #    of truth for anything outside this machine. The local
            #    test-dashboard SSHes in and cats this one file (host + workspace
            #    dir come from .devbox-id.json); test-context-api prefers it over
            #    the registry. data/ is never rsynced, so a redeploy can't clobber
            #    the stack host's copy, and every preflight rewrites it.
            #    caddyRoutes is scraped from the freshly generated Caddyfile,
            #    keeping caddyfile.nix the only place the exposed-service list is
            #    maintained. ──
            PORTS_PUBLISH="''${FLAKE_ROOT}/data/devbox-ports.json"
            CADDY_ROUTES=$(${pkgs.gnused}/bin/sed -n 's|^[[:space:]]*handle_path /\([^/]*\)/\*.*|\1|p' \
              "''${FLAKE_ROOT}/data/Caddyfile" | ${pkgs.jq}/bin/jq -R -s -c 'split("\n") | map(select(length > 0))')
            ${pkgs.jq}/bin/jq -n \
              --arg dev "$DEVBOX_KEY" \
              --arg dir "''${FLAKE_ROOT}" \
              --argjson ports "$PORTS_JSON" \
              --argjson caddy "''${CADDY_PORT:-null}" \
              --argjson routes "''${CADDY_ROUTES:-[]}" \
              '{devKey: $dev, dir: $dir, caddyPort: $caddy, caddyRoutes: $routes, ports: $ports}' \
              > "$PORTS_PUBLISH.tmp.$$"
            mv -f "$PORTS_PUBLISH.tmp.$$" "$PORTS_PUBLISH"
            echo "Published resolved ports at $PORTS_PUBLISH"

            # Bump soft stack to the hard max. `nix run` spawns a fresh shell
            # that doesn't inherit the devshell's shellHook, so set it here too.
            # Required by process-compose / some Haskell exes that want >= 60 MB stack.
            _hard=$(ulimit -Hs 2>/dev/null || true)
            if [ -n "$_hard" ] && [ "$_hard" != "unlimited" ]; then
              ulimit -s "$_hard" 2>/dev/null || true
              ulimit -n "$_hard" 2>/dev/null || true
            fi
            # -S NAME forces stable sort by process name (no re-ordering on status change).
            # -d hides `disabled = true` processes (the ones this profile never
            # starts) so the list only shows what the profile actually runs;
            # they are still defined, so `process-compose process start <name>`
            # can bring one up on demand.
            nix run --impure .#run-mobility-stack-dev -- -S NAME -d "$@"
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
            nix run .#run-mobility-stack-full -- -S NAME -d "$@"
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
            nix run .#run-local-test-dashboard -- -S NAME -d "$@"
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

        import-config-data = {
          category = "Backend";
          description = "Import config data from a remote env into the local DB (config-sync). Source env defaults to master; override with CONFIG_SYNC_FROM=prod|prod_international. Extra args pass through (e.g. --dry-run, --force-fetch).";
          exec = ''
            set -euo pipefail
            FROM_ENV="''${CONFIG_SYNC_FROM:-master}"
            cd "''${FLAKE_ROOT}/Backend/dev/config-sync"
            # The connection/patch config files are gitignored; seed them from
            # the checked-in templates on first run.
            [ -f assets/environments.json ] || cp assets/environments.json.example assets/environments.json
            [ -f assets/patches.json ] || cp assets/patches.json.example assets/patches.json
            echo "── Importing config data: $FROM_ENV → local ──"
            ${configSyncPython}/bin/python3 config_transfer.py import --from "$FROM_ENV" --to local --fetch \
              --fetch-url "https://backend-ny-config-sync.s3.ap-south-1.amazonaws.com/''${FROM_ENV}_to_local/v2" "$@"
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
