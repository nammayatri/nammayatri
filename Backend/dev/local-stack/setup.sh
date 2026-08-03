#!/usr/bin/env bash
# Bring up a working self-hosted Namma Yatri rider backend from scratch.
#
#   ./setup.sh          full setup (fetch tree, build, start, seed, verify)
#   ./setup.sh verify   just re-run the API checks against a running stack
#   ./setup.sh algeria  re-apply the Algeria service areas, then verify
#   ./setup.sh down     stop everything (keeps the database volume)
#   ./setup.sh clean    stop everything and delete the database volume
set -euo pipefail

cd "$(dirname "$0")"

# Pinned upstream commit. This is the last baseline that is *self-contained*:
# it seeds a real merchant (YATRI), so the stack works without Namma Yatri's
# internal config-sync data. Newer commits come up with an empty database.
UPSTREAM_REPO="https://github.com/nammayatri/nammayatri.git"
UPSTREAM_SHA="03a753113af1fdcddf3378d9dc2fc31170e385e4"
TREE_DIR="2023"

PG="docker exec ny-postgres psql -U postgres -d atlas_dev"

log() { printf '\n\033[1;36m==> %s\033[0m\n' "$*"; }
ok()  { printf '\033[1;32m    %s\033[0m\n' "$*"; }
die() { printf '\n\033[1;31mFAILED: %s\033[0m\n' "$*" >&2; exit 1; }

preflight() {
  command -v docker >/dev/null 2>&1 || die \
"docker CLI not found.

If you are inside WSL: enable Docker Desktop -> Settings -> Resources ->
WSL integration for this distro, then re-run."

  docker info >/dev/null 2>&1 || die "Docker daemon is not running. Start Docker Desktop and re-run."

  # Docker Desktop cannot bind-mount \\\\wsl.localhost paths from the Windows
  # side, and this compose file uses bind mounts. Fail early with a fix rather
  # than dying halfway through with a confusing volume error.
  case "$PWD" in
    //wsl.localhost/*|/\\\\wsl.localhost/*)
      die \
"Running from a \\\\wsl.localhost path with the Windows docker CLI.
Docker Desktop cannot bind-mount those, so the stack would fail.

Pick one:
  1. (recommended) Enable Docker Desktop -> Settings -> Resources ->
     WSL integration for your distro, then run this script from inside WSL.
  2. Copy this directory to a Windows path (e.g. C:/ny-local-stack) and run it there."
      ;;
  esac
}

fetch_tree() {
  # Probe for a file we actually need, not just .git — a failed fetch leaves a
  # .git behind and we'd otherwise carry on against an empty tree.
  local marker="$TREE_DIR/Backend/dhall-configs/dev/rider-app.dhall"
  if [ -f "$marker" ]; then
    ok "upstream tree already present ($TREE_DIR)"
    return
  fi
  log "Fetching pinned upstream tree ${UPSTREAM_SHA:0:7}"
  # Retried: GitHub connectivity is intermittent on some networks.
  local attempt
  for attempt in 1 2 3 4 5; do
    rm -rf "$TREE_DIR"; mkdir -p "$TREE_DIR"
    if ( cd "$TREE_DIR"
         git init -q .
         git remote add origin "$UPSTREAM_REPO"
         git fetch --depth 1 -q origin "$UPSTREAM_SHA"
         git checkout -q FETCH_HEAD ); then
      break
    fi
    [ "$attempt" = "5" ] && die "could not fetch $UPSTREAM_REPO after 5 attempts (network?)"
    echo "    fetch attempt $attempt failed; retrying in 10s..."
    sleep 10
  done
  [ -f "$marker" ] || die "upstream fetch did not produce $marker"
  # Only top-secret.dhall is gitignored upstream; create it from the template.
  cp -n "$TREE_DIR/Backend/dhall-configs/dev/secrets/top-secret-template.dhall" \
        "$TREE_DIR/Backend/dhall-configs/dev/secrets/top-secret.dhall" 2>/dev/null || true
  ok "checked out $(cd "$TREE_DIR" && git rev-parse --short HEAD)"
}

wait_for_pg() {
  log "Waiting for Postgres"
  for _ in $(seq 1 60); do
    if docker exec ny-postgres pg_isready -h 127.0.0.1 -U postgres -d atlas_dev >/dev/null 2>&1; then
      ok "ready"; return
    fi
    sleep 2
  done
  die "Postgres did not become ready"
}

seed_db() {
  log "Seeding base schema"
  # Only the base schema + extensions here. rider-app applies dev/migrations
  # itself at startup, so applying them here too would collide.
  docker cp "$TREE_DIR/Backend/dev/sql-seed/pre-init.sql"            ny-postgres:/tmp/pre-init.sql
  docker cp "$TREE_DIR/Backend/dev/sql-seed/rider-app-seed.sql"      ny-postgres:/tmp/rider-app-seed.sql

  $PG -q -f /tmp/pre-init.sql       >/dev/null 2>&1 || true
  $PG -q -f /tmp/rider-app-seed.sql >/dev/null 2>&1 || true
  ok "base schema applied"
}

seed_algeria() {
  # Must run *after* rider-app has migrated: atlas_app.geometry only exists
  # once its migrations have been applied.
  log "Applying Algeria service areas (Algiers, Oran, Annaba)"
  docker cp algeria-geofences.sql ny-postgres:/tmp/algeria-geofences.sql
  $PG -q -v ON_ERROR_STOP=1 -f /tmp/algeria-geofences.sql >/dev/null \
    || die "could not apply algeria-geofences.sql"
  # The merchant row (which carries the service-area restriction) is cached in
  # Redis, so the API would otherwise keep serving the previous service areas.
  docker exec ny-redis redis-cli FLUSHALL >/dev/null
  $PG -t -c "SELECT region || ' (' || ST_NPoints(geom) || ' pts)'
               FROM atlas_app.geometry
              WHERE region IN ('Algiers','Oran','Annaba') ORDER BY region;" \
    | sed 's/^ */    /' | grep -v '^ *$'
}

show_db_state() {
  # The merchant (YATRI) comes from rider-app-seed.sql: it seeds an
  # `organization` row, which migration 1014 (transform-org-to-whitelisted-provider)
  # converts into `merchant`. No separate merchant seed is needed.
  #
  # local-testing-data/rider-app.sql is deliberately NOT applied: it targets the
  # pre-migration schema (INSERT INTO atlas_app.organization, person.full_name)
  # and errors out against the migrated tables. It isn't needed either — the
  # OTP login flow creates the rider row on first sign-in.
  log "Database state"
  $PG -t -c "SELECT 'tables='   || (SELECT count(*) FROM information_schema.tables
                                    WHERE table_schema='atlas_app')
                 || '  merchant=' || (SELECT count(*) FROM atlas_app.merchant)
                 || '  person='   || (SELECT count(*) FROM atlas_app.person);" \
    | tr -d ' \r' | grep -v '^$' | sed 's/^/    /'
}

wait_for_api() {
  log "Waiting for rider-app on :8014"
  for _ in $(seq 1 60); do
    code=$(curl -s -o /dev/null -w '%{http_code}' --max-time 5 http://localhost:8014/openapi || true)
    [ "$code" = "200" ] && { ok "API is up"; return; }
    sleep 3
  done
  echo "--- rider-app logs ---"; docker logs ny-rider 2>&1 | tail -25
  die "rider-app did not come up"
}

verify() {
  log "Verifying the API end to end"
  local base="http://localhost:8014"

  # Status checks by http_code, not curl's exit status: /swagger is served with
  # Transfer-Encoding: chunked and Warp closes without a terminating chunk, so
  # curl reports exit 18 (partial transfer) even on a perfectly good 200.
  # `|| true` keeps set -e happy; the captured body is still the status code.
  local code
  code=$(curl -s -o /dev/null -w '%{http_code}' --max-time 15 "$base/swagger" 2>/dev/null) || true
  [ "$code" = "200" ] || die "/swagger not serving (HTTP ${code:-000})"
  ok "GET  /swagger                  200"
  code=$(curl -s -o /dev/null -w '%{http_code}' --max-time 15 "$base/openapi" 2>/dev/null) || true
  [ "$code" = "200" ] || die "/openapi not serving (HTTP ${code:-000})"
  ok "GET  /openapi                  200"

  local auth authid token svc
  auth=$(curl -s --max-time 20 -X POST "$base/v2/auth" \
      -H 'content-type: application/json' \
      -H 'x-bundle-version: 1.0.1' -H 'x-client-version: 1.0.0' \
      -d '{"mobileCountryCode":"+91","mobileNumber":"9999900001","merchantId":"YATRI"}')
  authid=$(printf '%s' "$auth" | sed -nE 's/.*"authId":"([^"]+)".*/\1/p')
  [ -n "$authid" ] || die "login failed: $auth"
  ok "POST /v2/auth                  200  authId=$authid"

  token=$(curl -s --max-time 20 -X POST "$base/v2/auth/$authid/verify" \
      -H 'content-type: application/json' \
      -d '{"otp":"7891","deviceToken":"setup-check"}' \
      | sed -nE 's/.*"token":"([^"]+)".*/\1/p')
  [ -n "$token" ] || die "OTP verification failed"
  ok "POST /v2/auth/{id}/verify      200  token=$token"

  # Positive: Algiers city centre must be inside the service area.
  svc=$(curl -s --max-time 20 -X POST "$base/v2/serviceability/origin" \
      -H 'content-type: application/json' -H "token: $token" \
      -d '{"location":{"lat":36.7538,"lon":3.0588}}')
  printf '%s' "$svc" | grep -q '"serviceable":true' || die "Algiers should be serviceable: $svc"
  ok "POST /v2/serviceability/origin 200  Algiers      serviceable=true"

  # Negative: proves the Algeria swap actually replaced the Indian service
  # areas rather than just adding to them. Without this, an always-true
  # serviceability endpoint would pass the check above.
  svc=$(curl -s --max-time 20 -X POST "$base/v2/serviceability/origin" \
      -H 'content-type: application/json' -H "token: $token" \
      -d '{"location":{"lat":12.9715987,"lon":77.5945627}}')
  printf '%s' "$svc" | grep -q '"serviceable":false' || die "Bangalore should NOT be serviceable: $svc"
  ok "POST /v2/serviceability/origin 200  Bangalore    serviceable=false"

  printf '\n\033[1;32m*** Backend is fully operational ***\033[0m\n\n'
}

case "${1:-up}" in
  down)   docker compose down; exit 0 ;;
  clean)  docker compose down -v; rm -rf "$TREE_DIR"; exit 0 ;;
  verify) verify; exit 0 ;;
  algeria) seed_algeria; verify; exit 0 ;;
esac

preflight
fetch_tree

# Building compiles librdkafka from source and needs Docker Hub, so reuse the
# image once it exists. REBUILD=1 ./setup.sh forces a fresh build.
if [ "${REBUILD:-0}" = "1" ] || ! docker image inspect ny-rider:patched >/dev/null 2>&1; then
  log "Building patched rider-app image (compiles librdkafka, first run is slow)"
  docker compose build rider-app
else
  ok "patched rider-app image already built (REBUILD=1 to force)"
fi
log "Starting infrastructure"
docker compose up -d postgres redis kafka passetto-db passetto
wait_for_pg
seed_db
log "Starting rider-app (it applies its own migrations) + proxy"
docker compose up -d rider-app proxy
wait_for_api
seed_algeria
verify
show_db_state
