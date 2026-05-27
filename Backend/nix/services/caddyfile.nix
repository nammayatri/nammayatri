# Renders a Caddyfile from `ports.nix` (or the resolved overlay generated
# by resolve-ports.sh). Pure builtins — no pkgs / no lib — so it can be
# evaluated cheaply via `nix-instantiate --eval --raw`.
#
# Usage from a shell hook, AFTER resolve-ports.sh has written
# data/ports-resolved.nix:
#
#   nix-instantiate --eval --raw --impure --expr \
#     "import $FLAKE_ROOT/Backend/nix/services/caddyfile.nix { \
#        ports = import \"$RESOLVED_FILE\"; }" > data/Caddyfile
#
# The Caddyfile provides a single reverse-proxy entry point so developers on
# the devbox (via Tailscale) only need to know one port to reach any backend
# service. Each exposed service is reachable at /<service-name>/* and the
# path prefix is stripped before forwarding (handle_path).
{ ports }:
let
  # Services exposed via the reverse proxy. Path is always /<service-name>;
  # the port is looked up from the same `ports` attrset by key. Keep this
  # list to HTTP-facing services only — skip DBs, Redis, Kafka, metrics
  # endpoints, the internal driver-app port, gRPC ports, etc.
  exposedServices = [
    "rider-app"
    "dynamic-offer-driver-app"
    "rider-dashboard"
    "provider-dashboard"
    "beckn-gateway"
    "mock-registry"
    "location-tracking-service"
    "notification-service"
    "mock-server"
    "mock-fcm"
    "mock-sms"
    "mock-idfy"
    "mock-google"
    "test-context-api"
    "metabase"
    "victoria-metrics"
  ];

  caddyPort = ports.caddy-reverse-proxy or (throw "caddy-reverse-proxy port not defined in ports.nix");

  routeBlock = svc:
    if ports ? ${svc}
    then ''
      	handle_path /${svc}/* {
      		reverse_proxy 127.0.0.1:${toString ports.${svc}}
      	}
    ''
    else
    # builtins.trace prints to stderr at eval time, mirroring the old
    # `echo "  WARN: skipping $svc — not in ports file" >&2`.
      builtins.trace "build-caddyfile: skipping ${svc} — not in ports file" "";

  routes = builtins.concatStringsSep "" (map routeBlock exposedServices);
in
''
  {
  	auto_https off
  	admin off
  }

  http://:${toString caddyPort} {
  	bind 0.0.0.0

  	handle /__caddy_health {
  		respond "ok" 200
  	}

  ${routes}
  	log {
  		output stderr
  		format console
  	}
  }
''
