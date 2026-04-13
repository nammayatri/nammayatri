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

      common = { name, ... }: {
        working_dir = "Backend";
        namespace = "ny";
        log_location = "${name}.log";
      };

      # The cabal executables we want to run as part of the nammayatri service
      # group.
      cabalExecutables = [
        "driver-offer-allocator-exe"
        "dynamic-offer-driver-app-exe"
        "dynamic-offer-driver-drainer-exe"
        "rider-app-drainer-exe"
        "rider-app-scheduler-exe"
        "image-api-helper-exe"
        "mock-fcm-exe"
        "mock-google-exe"
        "mock-idfy-exe"
        "mock-sms-exe"
        "provider-dashboard-exe"
        "producer-exe"
        "rider-app-exe"
        "rider-dashboard-exe"
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

      # Primary services that must be healthy before other services start
      primaryExecutables = [
        "rider-app-exe"
        "dynamic-offer-driver-app-exe"
      ];

      secondaryExecutables = builtins.filter
        (name: !builtins.elem name primaryExecutables)
        cabalExecutables;

      haskellProcesses.processes =
        # Primary services: only depend on nammayatri-init
        lib.listToAttrs (builtins.map
          (name: {
            inherit name;
            value = {
              imports = [
                common
                (haskellProcessFor name)
              ];
              depends_on."nammayatri-init".condition = "process_completed_successfully";
              shutdown.signal = 9;
            };
          })
          primaryExecutables)
        //
        # Secondary services: depend on nammayatri-init + primary services healthy
        lib.listToAttrs (builtins.map
          (name: {
            inherit name;
            value = {
              imports = [
                common
                (haskellProcessFor name)
              ];
              depends_on = {
                "nammayatri-init".condition = "process_completed_successfully";
                "rider-app-exe".condition = "process_healthy";
                "dynamic-offer-driver-app-exe".condition = "process_healthy";
              };
              shutdown.signal = 9;
            };
          })
          secondaryExecutables);

    in
    {
      settings = {
        # Local Haskell processes
        imports = [ haskellProcesses ];

        processes = {
          # Sync config from master to local DB after infra is ready
          config-sync = {
            imports = [ common ];
            depends_on = {
              "db-primary".condition = "process_healthy";
              "redis".condition = "process_healthy";
              "kafka".condition = "process_healthy";
              "passetto-service".condition = "process_started";
              "rider-app-exe".condition = "process_healthy";
              "dynamic-offer-driver-app-exe".condition = "process_healthy";
            };
            command = pkgs.writeShellApplication {
              name = "config-sync";
              runtimeInputs = [
                (pkgs.python3.withPackages (ps: with ps; [
                  psycopg2
                  requests
                  python-dotenv
                  rich
                  websockets
                ]))
              ];
              text = ''
                set -x
                cd dev/config-sync
                if [ -d "assets/data/prod_to_local" ] && [ "$(ls -A assets/data/prod_to_local 2>/dev/null)" ]; then
                  echo "Found prod_to_local data, importing from prod"
                  DEV=true python3 config_transfer.py import --from prod --to local
                else
                  echo "Using master_to_local data"
                  DEV=true python3 config_transfer.py import --from master --to local
                fi
              '';
            };
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
            availability = {
              restart = "on_failure";
              backoff_seconds = 2;
              max_restarts = 5;
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
              backoff_seconds = 2;
              max_restarts = 5;
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
              backoff_seconds = 2;
              max_restarts = 3;
            };
          };
          test-context-api = {
            imports = [ common ];
            command = "${pkgs.python3.withPackages (ps: [ ps.psycopg2 ])}/bin/python3 dev/test-tool/context-api/server.py --port 7082";
            namespace = lib.mkForce "test";
            depends_on."db-primary".condition = "process_healthy";
            availability = {
              restart = "on_failure";
              backoff_seconds = 2;
              max_restarts = 3;
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
              backoff_seconds = 2;
              max_restarts = 3;
            };
          };

          # ── Dev tools: DB explorer + Redis explorer ──
          pgweb = {
            imports = [ common ];
            command = "${pkgs.pgweb}/bin/pgweb --bind 0.0.0.0 --listen 8432 --host localhost --port 5434 --user atlas_superuser --db atlas_dev --ssl disable --skip-open";
            namespace = lib.mkForce "tools";
            depends_on."db-primary".condition = "process_healthy";
            availability = {
              restart = "on_failure";
              backoff_seconds = 2;
              max_restarts = 3;
            };
          };
          redis-commander = {
            imports = [ common ];
            command = pkgs.writeShellApplication {
              name = "redis-commander";
              runtimeInputs = [ pkgs.nodejs ];
              text = ''
                set -x  # debug output
                RC_WORK="$HOME/.cache/redis-commander-ny"
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
              backoff_seconds = 5;
              max_restarts = 5;
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
            availability = {
              restart = "on_failure";
              backoff_seconds = 2;
              max_restarts = 5;
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
            availability = {
              restart = "on_failure";
              backoff_seconds = 2;
              max_restarts = 5;
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
                schemas = [
                  ../../dev/sql-seed/pre-init.sql
                  ../../dev/sql-seed/rider-app-seed.sql
                  ../../dev/sql-seed/public-transport-rider-platform-seed.sql
                  ../../dev/local-testing-data/public-transport-rider-platform.sql
                  ../../dev/sql-seed/mock-registry-seed.sql
                  ../../dev/local-testing-data/mock-registry.sql
                  ../../dev/sql-seed/dynamic-offer-driver-app-seed.sql
                  ../../dev/sql-seed/rider-dashboard-seed.sql
                  ../../dev/local-testing-data/rider-dashboard.sql
                  ../../dev/sql-seed/provider-dashboard-seed.sql
                  ../../dev/local-testing-data/provider-dashboard.sql
                  ../../dev/sql-seed/unified-dashboard-seed.sql
                  ../../dev/sql-seed/safety-dashboard-seed.sql
                  ../../dev/local-testing-data/safety-dashboard.sql
                  ../../dev/sql-seed/special-zone-seed.sql
                  ../../dev/local-testing-data/special-zone.sql
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
