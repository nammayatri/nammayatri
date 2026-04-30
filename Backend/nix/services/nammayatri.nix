# process-compose module for running the nammayatri stack
ny:
{ config, pkgs, lib, ... }:
let
  inherit (lib) types;
  pcLib = import ny.inputs.common.inputs.process-compose-flake.lib { inherit lib; };
  # Which Haskell package contains the given cabal executable?
  cabalTargetForExe = lib.listToAttrs (lib.flatten (lib.mapAttrsToList
    (name: info: map (exe: lib.nameValuePair exe "${name}:exe:${exe}") (lib.attrNames info.exes))
    ny.config.haskellProjects.default.outputs.packages));
  accumulateProcessEnv = envVar: processes:
    lib.filter (x: x != null)
      (builtins.map
        (p:
          pcLib.lookupEnv envVar (p.environment or null))
        (lib.attrValues processes));
in
{
  options = {
    services.nammayatri = lib.mkOption {
      type = types.submodule {
        options = {
          enable = lib.mkEnableOption "Enable nammayatri stack";
          useCabal = lib.mkEnableOption "Use cabal instead of Nix";
        };
      };
    };
  };

  imports = [
    ny.inputs.services-flake.processComposeModules.default
    ny.inputs.passetto.processComposeModules.default
    ./postgres-with-replica.nix
  ];

  config =
    let
      inherit (ny) inputs self' inputs';
      cfg = config.services.nammayatri;

      # Metabase plugin JARs — deterministic, store-backed.
      #
      # Metabase OSS bundles the Postgres, MySQL, SQLite, and H2 drivers, which is
      # all atlas_dev needs. Without any extras, this evaluates to an empty
      # directory and Metabase falls back to its built-ins. To add e.g. Snowflake
      # later, append an entry below — `nix-prefetch-url <url>` gives the sha256:
      #
      #   { name = "snowflake"; version = "1.0.0"; url = "https://github.com/.../snowflake.metabase-driver.jar"; sha256 = "..."; }
      #
      # Pinned URLs + hashes mean no first-run downloads and no cache leakage
      # into the repo (which is what `Backend/plugins/` was before).
      metabaseDrivers = [
        # (none — Postgres is built-in)
      ];
      metabasePluginsDir = pkgs.linkFarm "metabase-plugins" (map
        (d: {
          name = "${d.name}.metabase-driver.jar";
          path = pkgs.fetchurl { inherit (d) url sha256; };
        })
        metabaseDrivers);

      # Metabase declarative config — applied on every startup via MB_CONFIG_FILE_PATH.
      # Bootstraps the admin user and registers atlas_dev as a Postgres data source so
      # the UI is usable on first launch without manual setup wizard clicks.
      # Docs: https://www.metabase.com/docs/latest/configuring-metabase/config-file
      metabaseConfigFile = pkgs.writeText "metabase-config.yml" ''
        version: 1
        config:
          users:
            - first_name: Admin
              last_name: Dev
              password: metabase123
              email: admin@nammayatri.local
          databases:
            - name: atlas_dev
              engine: postgres
              details:
                host: localhost
                port: 5434
                user: atlas_superuser
                password: ""
                dbname: atlas_dev
                ssl: false
      '';

      # Skip the /setup wizard entirely. config_file (above) creates the admin
      # user, but doesn't mark the instance "set up", so Metabase still shows
      # the wizard on a fresh metabase.db. MB_USER_DEFAULTS fills + submits
      # the setup form programmatically on first launch.
      # Docs: https://www.metabase.com/docs/latest/configuring-metabase/environment-variables#mb_user_defaults
      metabaseUserDefaults = builtins.toJSON {
        token = "ny-local-dev-setup-token";
        user = {
          first_name = "Admin";
          last_name = "Dev";
          email = "admin@nammayatri.local";
          password = "metabase123";
          site_name = "Nammayatri";
        };
      };

      common = { name, ... }: {
        working_dir = "Backend";
        namespace = "ny";
        log_location = "${name}.log";
      };

      # The cabal executables we want to run as part of the nammayatri service
      # group.
      cabalExecutables = [
        "rider-app-exe"
        "dynamic-offer-driver-app-exe"
        "rider-dashboard-exe"
        "provider-dashboard-exe"
        "rider-app-drainer-exe"
        "dynamic-offer-driver-drainer-exe"
        "driver-offer-allocator-exe"
        "rider-app-scheduler-exe"
        "image-api-helper-exe"
        "mock-fcm-exe"
        "mock-google-exe"
        "mock-idfy-exe"
        "mock-sms-exe"
        "producer-exe"
        "search-result-aggregator-exe"
        "kafka-consumers-exe"
        "unified-dashboard-exe"
      ];

      haskellProcessFor = name:
        if cfg.useCabal
        then {
          command = "set -x; pwd; cabal run ${cabalTargetForExe.${name}}";
          environment.CABAL_TARGET = cabalTargetForExe.${name};
        }
        else {
          command = "set -x; pwd; ${ny.config.haskellProjects.default.outputs.apps.${name}.program}";
        };

      # Run every cabal executable strictly sequentially. Each one waits for
      # the previous entry in `cabalExecutables` so only one process at a
      # time is in its early-startup window. This avoids the
      # `stripConcurrentlyForLocal` race in mobility-core where multiple
      # services concurrently `removePathForcibly` the shared
      # `/tmp/.../ny-migrations` staging dir and one fails with ENOTEMPTY.
      #
      # Chain rules:
      #   - First exe in the list depends only on init + mock-registry.
      #   - Every subsequent exe adds `<prev>.<cond>` where:
      #       * cond = `process_healthy` if `<prev>` has a readiness probe
      #         (the BAP/BPP servers — rider-app-exe, dynamic-offer-driver-app-exe).
      #         These have to be fully up before the next service starts so
      #         downstream services hit a real, working API.
      #       * cond = `process_started` otherwise (services without probes).
      #         `process_started` fires post-fork, by which time the previous
      #         service is already past its migration window.
      probeBackedExecutables = [
        "rider-app-exe"
        "dynamic-offer-driver-app-exe"
      ];
      prevChainCondition = prev:
        if builtins.elem prev probeBackedExecutables
        then "process_healthy"
        else "process_started";

      haskellProcesses.processes =
        lib.listToAttrs (lib.imap0
          (idx: name: {
            inherit name;
            value = {
              imports = [
                common
                (haskellProcessFor name)
              ];
              depends_on =
                {
                  "nammayatri-init".condition = "process_completed_successfully";
                  "mock-registry".condition = "process_healthy";
                } // (
                  if idx == 0 then { }
                  else
                    let prev = builtins.elemAt cabalExecutables (idx - 1); in
                    { ${prev}.condition = prevChainCondition prev; }
                );
              shutdown.signal = 9;
            };
          })
          cabalExecutables);

    in
    {
      settings = {

        # Local Haskell processes
        imports = [ haskellProcesses ];

        processes = {
          # Rider producer: same binary as producer-exe, different env vars
          rider-producer-exe = {
            imports = [
              common
              (haskellProcessFor "producer-exe")
            ];
            environment.PRODUCER_TYPE = "Rider";
            environment.GET_MY_SCHEMA = "atlas_app";
            depends_on = {
              "nammayatri-init".condition = "process_completed_successfully";
              "rider-app-exe".condition = "process_healthy";
              "dynamic-offer-driver-app-exe".condition = "process_healthy";
              "mock-registry".condition = "process_healthy";
            };
            shutdown.signal = 9;
          };

          # Things to do before local Haskell processes are started
          nammayatri-init = {
            imports = [ common ];
            depends_on = {
              # Services
              "db-primary".condition = "process_healthy";
              "kafka".condition = "process_healthy";
              "redis".condition = "process_healthy";
              # "redis-cluster".condition = "process_healthy";
              "nginx".condition = "process_healthy";
              "osrm-server".condition = "process_started";
              "passetto-service".condition = "process_started";
            } // lib.optionalAttrs cfg.useCabal {
              # Compile Haskell code
              "cabal-build".condition = "process_completed_successfully";
            };
            command = pkgs.writeShellApplication {
              name = "run-mobility-stack-init";
              runtimeInputs = with pkgs; [
                redis
              ];
              text = ''
                set -x
                pwd
                rm -f ./*.log # Clean up the log files
                redis-cli -p 30001 -c XGROUP CREATE Available_Jobs_Rider myGroup_Rider  0 MKSTREAM # TODO: remove this once cluster funtions from euler are fixed
                redis-cli -p 30001 -c XGROUP CREATE Available_Jobs myGroup  0 MKSTREAM # TODO: remove this once cluster funtions from euler are fixed
                redis-cli -p 30001 -c XGROUP CREATE Available_Chakras myGroup_Chakras  0 MKSTREAM # TODO: remove this once cluster funtions from euler are fixed
                redis-cli XGROUP CREATE Available_Jobs_Rider myGroup_Rider 0 MKSTREAM
                redis-cli XGROUP CREATE Available_Jobs myGroup 0 MKSTREAM
                redis-cli XGROUP CREATE Available_Chakras myGroup_Chakras 0 MKSTREAM
              '';
            };
          };

          # Periodic log cleaner: truncates .log files exceeding 1GB
          # Covers process-compose logs (./*.log) and dhall/app logs (/tmp/*.log)
          log-cleaner = {
            imports = [ common ];
            command = pkgs.writeShellApplication {
              name = "log-cleaner";
              runtimeInputs = with pkgs; [ coreutils findutils ];
              text = ''
                while true; do
                  find . -maxdepth 1 -name '*.log' -size +1G -exec truncate -s 0 {} \;
                  find /tmp -maxdepth 1 -name '*.log' -size +1G -exec truncate -s 0 {} \;
                  sleep 300
                done
              '';
            };
            shutdown.signal = 9;
          };

          # Run 'cabal build' for all local Haskel processes
          cabal-build = {
            imports = [ common ];
            disabled = !cfg.useCabal;
            command = pkgs.writeShellApplication {
              name = "cabal-build";
              text = ''
                set -x
                cabal build ${builtins.concatStringsSep " " (accumulateProcessEnv "CABAL_TARGET" config.settings.processes)}
              '';
            };
          };

          # Processes from other repos in nammayatri GitHub org
          beckn-gateway = {
            imports = [ common ];
            command = ny.config.haskellProjects.default.outputs.finalPackages.beckn-gateway;
          };
          mock-registry = {
            imports = [ common ];
            command = ny.config.haskellProjects.default.outputs.finalPackages.mock-registry;
            depends_on."nammayatri-init".condition = "process_completed_successfully";
            readiness_probe = {
              http_get = {
                host = "127.0.0.1";
                port = 8020;
                path = "/";
              };
            };
            availability = {
              restart = "always";
              backoff_seconds = 20;
              max_restarts = 50;
            };
          };
          location-tracking-service = {
            imports = [ common ];
            command = ny.inputs.location-tracking-service.packages.${pkgs.system}.default;
            environment = {
              DEV = "true";
            };
            availability = {
              restart = "on_failure";
              backoff_seconds = 20;
              max_restarts = 50;
            };
          };
          osrm-server = {
            imports = [ common ];
            command = self'.packages.osrm-server;
          };

          kafka-consumers-exe = {
            environment = {
              CONSUMER_TYPE = "LOCATION_UPDATE";
            };
          };

          # Test tools — dashboard, mock servers, context API
          # Unified mock server for Juspay, Stripe, PayTM, Acko, SOS, WhatsApp, CMRL, CRIS, etc.
          mock-server = {
            imports = [ common ];
            command = "${pkgs.python3.withPackages (ps: [ ps.pynacl ps.psycopg2 ])}/bin/python3 dev/mock-servers/server.py --port 8080";
            namespace = lib.mkForce "test";
            depends_on."nammayatri-init".condition = "process_completed_successfully";
            availability = {
              restart = "on_failure";
              backoff_seconds = 20;
              max_restarts = 30;
            };
          };
          test-context-api = {
            imports = [ common ];
            # Hosts the config-sync trigger (replaces the old `config-sync` process), so it needs
            # the same python deps as config_transfer.py: psycopg2, requests, python-dotenv, rich, websockets.
            command = "${pkgs.python3.withPackages (ps: with ps; [ psycopg2 requests python-dotenv rich websockets ])}/bin/python3 dev/test-tool/context-api/server.py --port 7082";
            namespace = lib.mkForce "test";
            # Start once infra is up. The startup config-sync runs in a background thread,
            # so we DON'T block on dashboards — if a dashboard is still settling, the API
            # is still serving and the config-sync will succeed against the DB regardless.
            # The post-sync restart step targets rider-app-exe / dynamic-offer-driver-app-exe /
            # mock-registry, which we wait on so those PIDs exist when we try to kill them.
            depends_on = {
              "db-primary".condition = "process_healthy";
              "redis".condition = "process_healthy";
              "kafka".condition = "process_healthy";
              "passetto-service".condition = "process_started";
              "rider-app-exe".condition = "process_healthy";
              "rider-dashboard-exe".condition = "process_healthy";
              "provider-dashboard-exe".condition = "process_healthy";
              "dynamic-offer-driver-app-exe".condition = "process_healthy";
              "mock-registry".condition = "process_healthy";
            };
            availability = {
              restart = "on_failure";
              backoff_seconds = 20;
              max_restarts = 30;
            };
          };
          test-dashboard = {
            imports = [ common ];
            command = pkgs.writeShellApplication {
              name = "test-dashboard";
              runtimeInputs = [ pkgs.nodejs ];
              text = ''
                cd dev/test-tool/dashboard
                npm install
                npm run build
                npx serve -s build -l 7070 --no-clipboard
              '';
            };
            namespace = lib.mkForce "test";
            depends_on."rider-app-exe".condition = "process_healthy";
            availability = {
              restart = "on_failure";
              backoff_seconds = 20;
              max_restarts = 30;
            };
          };
          metabase = {
            imports = [ common ];
            command = pkgs.writeShellApplication {
              name = "metabase";
              runtimeInputs = [ pkgs.metabase ];
              text = ''
                set -x
                # Working_dir for this service is `Backend/` (set in `common`).
                # `../data/metabase` resolves to <repo-root>/data/metabase,
                # the same convention postgres/kafka/passetto use.
                MB_WORK="$(mkdir -p ../data/metabase && cd ../data/metabase && pwd)"
                MB_PLUGINS="$MB_WORK/plugins"
                mkdir -p "$MB_PLUGINS"
                # Seed the writable plugins dir from our pinned nix-store
                # linkFarm. Metabase needs MB_PLUGINS_DIR to be writable
                # because it extracts its built-in drivers there on first
                # launch; the read-only store path alone won't work.
                # `cp -fL` deref's the linkFarm symlinks so the JARs land
                # as real files. With an empty driver list, the loop is
                # a no-op and only built-in drivers are used.
                for jar in ${metabasePluginsDir}/*.jar; do
                  [ -e "$jar" ] || break
                  cp -fL "$jar" "$MB_PLUGINS/"
                done
                export MB_DB_FILE="$MB_WORK/metabase.db"
                export MB_JETTY_HOST=0.0.0.0
                export MB_JETTY_PORT=3001
                export MB_CONFIG_FILE_PATH=${metabaseConfigFile}
                # Auto-complete the /setup wizard on first launch — see comment
                # next to `metabaseUserDefaults` in nammayatri.nix.
                export MB_USER_DEFAULTS=${lib.escapeShellArg metabaseUserDefaults}
                export MB_PLUGINS_DIR="$MB_PLUGINS"
                # Pin CWD to the data dir so any stray writes (e.g. the H2
                # sample DB) stay there, not in the repo's Backend/ dir.
                cd "$MB_WORK"
                exec metabase
              '';
            };
            namespace = lib.mkForce "tools";
            depends_on."db-primary".condition = "process_healthy";
            readiness_probe = {
              # Metabase exposes /api/health as soon as the DB is migrated
              # and Jetty is bound. metabase-setup waits on this.
              http_get = {
                host = "127.0.0.1";
                port = 3001;
                path = "/api/health";
              };
              initial_delay_seconds = 10;
              period_seconds = 5;
              failure_threshold = 60;
              timeout_seconds = 3;
            };
            availability = {
              restart = "on_failure";
              backoff_seconds = 50;
              max_restarts = 30;
            };
          };

          metabase-setup = {
            imports = [ common ];
            command = pkgs.writeShellApplication {
              name = "metabase-setup";
              runtimeInputs = [ pkgs.curl pkgs.jq ];
              text = ''
                set -uo pipefail
                MB="http://127.0.0.1:3001"
                EMAIL="admin@nammayatri.local"
                PASS="metabase123"

                # 1. Try to log in. Success => setup-token clears automatically.
                login_body=$(jq -n --arg u "$EMAIL" --arg p "$PASS" \
                  '{username: $u, password: $p}')
                login_code=$(curl -sS -o /tmp/mb-login.json -w "%{http_code}" \
                  -X POST "$MB/api/session" \
                  -H 'Content-Type: application/json' \
                  -d "$login_body" || echo 000)
                if [ "$login_code" = "200" ]; then
                  echo "metabase: admin login OK — setup token cleared."
                  exit 0
                fi
                echo "metabase: login returned $login_code, falling back to /api/setup"

                # 2. Fall back to /api/setup with the current setup-token.
                token=$(curl -fsS "$MB/api/session/properties" \
                  | jq -r '."setup-token" // empty')
                if [ -z "$token" ]; then
                  # No token => already set up by something else; don't fail.
                  echo "metabase: no setup-token — assuming already set up."
                  exit 0
                fi
                payload=$(jq -n --arg token "$token" --arg email "$EMAIL" --arg pass "$PASS" '{
                  token: $token,
                  user: {
                    first_name: "Admin",
                    last_name: "Dev",
                    email: $email,
                    password: $pass,
                    site_name: "Nammayatri"
                  },
                  prefs: {
                    site_name: "Nammayatri",
                    site_locale: "en",
                    allow_tracking: false
                  },
                  database: {
                    engine: "postgres",
                    name: "atlas_dev",
                    details: {
                      host: "localhost",
                      port: 5434,
                      user: "atlas_superuser",
                      password: "",
                      dbname: "atlas_dev",
                      ssl: false
                    }
                  }
                }')
                setup_code=$(curl -sS -o /tmp/mb-setup.json -w "%{http_code}" \
                  -X POST "$MB/api/setup" \
                  -H 'Content-Type: application/json' \
                  -d "$payload" || echo 000)
                if [ "$setup_code" = "200" ]; then
                  echo "metabase: setup complete via /api/setup."
                  exit 0
                fi
                echo "metabase: /api/setup returned $setup_code (response: $(cat /tmp/mb-setup.json 2>/dev/null || echo '?'))"
                # Best-effort: exit 0 so the dev stack stays green even if setup
                # is in some odd partial state. Worst case: visit /setup once.
                exit 0
              '';
            };
            namespace = lib.mkForce "tools";
            depends_on."metabase".condition = "process_healthy";
            availability = {
              restart = "no";
              max_restarts = 5;
            };
          };

          redis-commander = {
            imports = [ common ];
            command = pkgs.writeShellApplication {
              name = "redis-commander";
              runtimeInputs = [ pkgs.nodejs ];
              text = ''
                set -x  # debug output
                # Same convention as metabase: persist under <repo-root>/data/
                # (gitignored) instead of ~/.cache. working_dir is `Backend/`.
                RC_WORK="$(mkdir -p ../data/redis-commander && cd ../data/redis-commander && pwd)"
                RC_PKG="$RC_WORK/node_modules/redis-commander"

                # Install only if not already present
                if [ ! -d "$RC_PKG" ]; then
                  mkdir -p "$RC_WORK"
                  cd "$RC_WORK"
                  npm init -y
                  npm install redis-commander@0.9.0
                fi

                # Always rewrite default.json with our connections (idempotent).
                # redis-commander reads TOP-LEVEL config.connections — not config.redis.connections!
                node <<NODE_SCRIPT
                const fs = require('fs');
                const p = '$RC_PKG/config/default.json';
                const c = JSON.parse(fs.readFileSync(p, 'utf8'));
                c.connections = [
                  { label: 'standalone', host: '127.0.0.1', port: 6379, dbIndex: 0 },
                  { label: 'cluster',    host: '127.0.0.1', port: 30001, dbIndex: 0, isCluster: true, clusterNoTlsValidation: true }
                ];
                c.server.address = '0.0.0.0';
                c.server.port = 8431;
                fs.writeFileSync(p, JSON.stringify(c, null, 2));
                console.log('Patched default.json top-level connections:', c.connections.length);
                NODE_SCRIPT

                cd "$RC_PKG"
                exec node bin/redis-commander.js --noauth
              '';
            };
            namespace = lib.mkForce "tools";
            depends_on = {
              "redis".condition = "process_healthy";
              "cluster1-cluster-create".condition = "process_completed_successfully";
            };
            availability = {
              restart = "on_failure";
              backoff_seconds = 50;
              max_restarts = 50;
            };
          };

          dynamic-offer-driver-app-exe = {
            readiness_probe = {
              http_get = {
                host = "127.0.0.1";
                port = 8016;
                path = "/ui";
              };
            };
            liveness_probe = {
              http_get = {
                host = "127.0.0.1";
                port = 8016;
                path = "/ui";
              };
              initial_delay_seconds = 60;
              period_seconds = 30;
              failure_threshold = 5;
              timeout_seconds = 5;
            };
            availability = {
              restart = "always";
              backoff_seconds = 20;
              max_restarts = 50;
            };
          };

          rider-app-exe = {
            readiness_probe = {
              http_get = {
                host = "127.0.0.1";
                port = 8013;
                path = "/v2";
              };
            };
            liveness_probe = {
              http_get = {
                host = "127.0.0.1";
                port = 8013;
                path = "/v2";
              };
              initial_delay_seconds = 60;
              period_seconds = 30;
              failure_threshold = 5;
              timeout_seconds = 5;
            };
            availability = {
              restart = "always";
              backoff_seconds = 20;
              max_restarts = 50;
            };
          };

          rider-dashboard-exe = {
            readiness_probe = {
              http_get = {
                host = "127.0.0.1";
                port = 8017;
                path = "/";
              };
              initial_delay_seconds = 15;
              period_seconds = 5;
              failure_threshold = 30;
              timeout_seconds = 3;
            };
            availability = {
              restart = "on_failure";
              backoff_seconds = 30;
              max_restarts = 50;
            };
          };

          provider-dashboard-exe = {
            readiness_probe = {
              http_get = {
                host = "127.0.0.1";
                port = 8018;
                path = "/";
              };
              initial_delay_seconds = 15;
              period_seconds = 5;
              failure_threshold = 30;
              timeout_seconds = 3;
            };
            availability = {
              restart = "on_failure";
              backoff_seconds = 30;
              max_restarts = 50;
            };
          };
        };
      };

      # External services
      services = {
        postgres-with-replica.db-primary = {
          enable = true;
          extraMasterDBSettings = { name, ... }: {
            # Unix socket length is supposed to be under 108 chars, see: https://linux.die.net/man/7/unix
            socketDir = "$HOME/NY/socket/${name}";
            extensions = extensions: [
              extensions.postgis
            ];
            initialDatabases = [
              {
                name = "atlas_dev";
                # Schema-only initialization. local-testing-data/*.sql is NOT loaded here
                # because dev/ddl-migrations and feature-migrations need to run on empty tables
                # (so SET NOT NULL / FK validations pass). After dev/ddl-migrations + feature-migrations
                # complete, test-context-api applies dev/local-testing-data/*.sql.
                schemas = [
                  ../../dev/sql-seed/pre-init.sql
                  ../../dev/sql-seed/rider-app-seed.sql
                  ../../dev/sql-seed/public-transport-rider-platform-seed.sql
                  ../../dev/sql-seed/mock-registry-seed.sql
                  ../../dev/sql-seed/dynamic-offer-driver-app-seed.sql
                  ../../dev/sql-seed/rider-dashboard-seed.sql
                  ../../dev/sql-seed/provider-dashboard-seed.sql
                  ../../dev/sql-seed/unified-dashboard-seed.sql
                  ../../dev/sql-seed/safety-dashboard-seed.sql
                  ../../dev/sql-seed/special-zone-seed.sql
                  ../../dev/sql-seed/kaal-chakra-seed.sql
                ];
              }
            ];
            initialScript.before = ''
              CREATE USER repl_user replication;
              CREATE USER atlas WITH PASSWORD 'atlas';
            '';
            port = 5434;
          };
          extraReplicaDBSettings = { name, ... }: {
            socketDir = "$HOME/NY/socket/${name}";
            port = 5435;
          };
        };

        redis."redis".enable = true;

        redis-cluster."cluster1".enable = true;

        zookeeper."zookeeper".enable = true;

        apache-kafka."kafka" = {
          enable = true;
          port = 29092;
          settings = {
            # Since the available brokers are only 1
            "offsets.topic.replication.factor" = 1;
            "zookeeper.connect" = [ "localhost:2181" ];
          };
        };


        nginx."nginx" = {
          enable = true;
          port = 8085;
        };
      };
      # kafka should start only after zookeeper is healthy
      settings.processes.kafka.depends_on."zookeeper".condition = "process_healthy";

      services.passetto = {
        enable = true;
        port = 8079;
        extraDbSettings = { name, ... }: {
          port = 5422;
          socketDir = "$HOME/NY/socket/${name}";
        };
        package = inputs'.passetto.packages.passetto-service;
      };

      # Override passetto-service startup: skip passetto-init (keys are seeded via passetto-seed.sql).
      # The upstream process-compose.nix runs `passetto-init $password $keys` on every boot,
      # which unconditionally appends keys — causing pool growth across restarts.
      settings.processes.passetto-service.command = lib.mkForce (pkgs.writeShellApplication {
        name = "passetto-service";
        runtimeInputs = [ inputs'.passetto.packages.passetto-service ];
        text = ''
          set -x
          MASTER_PASSWORD=1 passetto-server
        '';
      });

      services.clickhouse."clickhouse-db" = {
        enable = true;
        port = 9000;
        extraConfig.http_port = 8123;
        initialDatabases = [
          {
            name = "atlas_kafka";
            schemas = [
              ../../dev/clickhouse/sql-seed/atlas-kafka-seed.sql
              ../../dev/clickhouse/local-testing-data/atlas-kafka.sql
            ];
          }
          {
            name = "atlas_driver_offer_bpp";
            schemas = [
              ../../dev/clickhouse/sql-seed/atlas-driver-offer-bpp-seed.sql
              ../../dev/clickhouse/local-testing-data/atlas-driver-offer-bpp.sql
            ];
          }
          {
            name = "app_monitor";
            schemas = [
              ../../dev/clickhouse/sql-seed/app-monitor-seed.sql
            ];
          }
        ];
      };
    };
}
