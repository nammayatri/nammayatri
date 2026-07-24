# Renders a Caddyfile from a `ports` attrset. Pure builtins — no pkgs / no lib
# — so it can be evaluated cheaply via `nix eval --raw`.
#
# The run-mobility-stack-dev preflight passes the resolved ports read from this
# checkout's devbox-registry.json slice:
#
#   nix eval --raw --impure --expr \
#     "import $FLAKE_ROOT/Backend/nix/services/caddyfile.nix { \
#        ports = (builtins.fromJSON (builtins.readFile \"$REGISTRY\")).users.\"$DEVBOX_KEY\".ports; }" \
#     > data/Caddyfile
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
    "db-manager-frontend"
    "db-manager-backend"
  ];

  caddyPort = ports.caddy-reverse-proxy or (throw "caddy-reverse-proxy port not defined in ports.nix");

  routeBlock = svc:
    if ports ? ${svc}
    then
      let
        addr = "127.0.0.1:${toString ports.${svc}}";
        proxy =
          if svc == "db-manager-backend"
          then "reverse_proxy ${addr} {\n\t\t\theader_up -Origin\n\t\t}"
          else "reverse_proxy ${addr}";
      in "\thandle_path /${svc}/* {\n\t\t${proxy}\n\t}\n"
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
