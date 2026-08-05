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

# Which service areas the backend serves.
#   nationwide  the whole of Algeria, one national border   (default)
#   cities      Algiers, Oran and Annaba only
# Both sets of boundaries are always loaded; this only picks which apply, so
# switching costs one UPDATE:  COVERAGE=cities ./setup.sh algeria
COVERAGE="${COVERAGE:-nationwide}"

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

  # The image is built from these; COPY would otherwise fail with "no source
  # files were specified", which reads like a Dockerfile bug rather than a
  # missing download.
  local missing=""
  local exe
  for exe in rider-app-exe dynamic-offer-driver-app-exe \
             beckn-gateway-exe mock-registry-exe; do
    [ -f "bin/$exe" ] || missing="$missing $exe"
  done
  [ -z "$missing" ] || die \
"missing backend binaries in ./bin:$missing

These are built by the GitHub Actions workflow in this repo -- the published
image does not contain them. rider-app-exe and dynamic-offer-driver-app-exe
there still reject +213, and beckn-gateway-exe and mock-registry-exe are not
in it at all.

  1. Actions -> 'algeria: build backend' -> newest successful run
     (or push to the branch algeria/build-backend to start one)
  2. Download the artifact 'ny-backend-binaries-<run>'
  3. Unzip the four *-exe files into $PWD/bin/ and chmod +x them

See .github/scripts/algeria/README.md."
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

seed_registry() {
  log "Seeding the registry (schema + subscribers)"
  # mock-registry does not create its own schema: mock-registry.dhall points at
  # schema atlas_registry as user atlas_registry_user, and autoMigrate only runs
  # dev/migrations/mock-registry, which is empty upstream. So the schema, the
  # role and the subscriber table all come from sql-seed, and the subscriber
  # rows from local-testing-data.
  #
  # The fixture rows already match this deployment, which is why none of this is
  # hand-written: BAP JUSPAY.MOBILITY.APP.UAT.1 at :8013/cab/v1, BG JUSPAY.BG.1
  # at :8015/v1, and BPP JUSPAY.MOBILITY.PROVIDER.UAT.3 at
  # :8016/beckn/favorit0-0000-0000-0000-00000favorit -- which is exactly
  # atlas_driver_offer_bpp.merchant.subscriber_id for NAMMA_YATRI_PARTNER.
  docker cp "$TREE_DIR/Backend/dev/sql-seed/mock-registry-seed.sql" \
            ny-postgres:/tmp/registry-seed.sql
  docker cp "$TREE_DIR/Backend/dev/local-testing-data/mock-registry.sql" \
            ny-postgres:/tmp/registry-data.sql

  # Both are idempotent-by-tolerance rather than by design: re-running finds the
  # role and schema already there and says so. Failing on that would make the
  # script un-rerunnable, which is worse.
  $PG -q -f /tmp/registry-seed.sql >/dev/null 2>&1 || true
  $PG -q -f /tmp/registry-data.sql >/dev/null 2>&1 || true

  local n
  n=$($PG -At -c "SELECT count(*) FROM atlas_registry.subscriber;" 2>/dev/null) \
    || die "registry schema was not created -- check sql-seed/mock-registry-seed.sql"
  [ "${n:-0}" -gt 0 ] || die "registry has no subscribers; the gateway will find no BPP"

  $PG -t -c "SELECT '  ' || type || '  ' || subscriber_id || '  -> ' || subscriber_url
               FROM atlas_registry.subscriber
              WHERE subscriber_id IN ('JUSPAY.BG.1','JUSPAY.MOBILITY.APP.UAT.1')
                 OR subscriber_id = (SELECT subscriber_id
                                       FROM atlas_driver_offer_bpp.merchant
                                      WHERE short_id='NAMMA_YATRI_PARTNER')
              ORDER BY type;" 2>/dev/null | grep -v '^ *$' | sed 's/^ *//'
  ok "$n subscribers"
}

seed_driver_db() {
  log "Seeding the driver side (schema + drivers + fare policy)"
  # Ordering here is not interchangeable, and getting it wrong fails silently
  # in a way that is hard to read later:
  #
  #   1. sql-seed        creates atlas_driver_offer_bpp and 13 base tables,
  #                      including `organization`. It contains no data at all.
  #   2. local-testing-data  inserts the organizations, drivers, vehicles and
  #                      fare policies -- into `organization`.
  #   3. migrations      run when driver-app starts, and migration 0050
  #                      (rename-org-to-merchant) renames organization ->
  #                      merchant, carrying those rows across.
  #
  # So the data must be loaded BEFORE driver-app starts. Load it afterwards and
  # every insert fails, because `organization` no longer exists.
  #
  # This is also why the rider side does NOT load its equivalent test data:
  # rider-app-seed.sql already carries its merchant, and
  # local-testing-data/rider-app.sql targets the pre-migration rider schema.
  docker cp "$TREE_DIR/Backend/dev/sql-seed/dynamic-offer-driver-app-seed.sql" \
            ny-postgres:/tmp/driver-seed.sql
  docker cp "$TREE_DIR/Backend/dev/local-testing-data/dynamic-offer-driver-app.sql" \
            ny-postgres:/tmp/driver-data.sql

  $PG -q -f /tmp/driver-seed.sql >/dev/null 2>&1 || true
  $PG -q -f /tmp/driver-data.sql >/dev/null 2>&1 || true

  $PG -t -c "SELECT '  organizations=' || (SELECT count(*) FROM atlas_driver_offer_bpp.organization)
                 || '  drivers='       || (SELECT count(*) FROM atlas_driver_offer_bpp.person WHERE role='DRIVER')
                 || '  vehicles='      || (SELECT count(*) FROM atlas_driver_offer_bpp.vehicle)
                 || '  fare policies=' || (SELECT count(*) FROM atlas_driver_offer_bpp.fare_policy);" \
    2>/dev/null | tr -s ' ' | grep -v '^ *$' | sed 's/^ */  /' \
    || die "driver seed did not load"
}

seed_algeria() {
  # Must run *after* rider-app has migrated: atlas_app.geometry only exists
  # once its migrations have been applied.
  log "Applying Algeria service areas (coverage: $COVERAGE)"
  docker cp algeria-geofences.sql ny-postgres:/tmp/algeria-geofences.sql
  $PG -q -v ON_ERROR_STOP=1 -f /tmp/algeria-geofences.sql >/dev/null \
    || die "could not apply algeria-geofences.sql"

  # The geometry rows are always loaded; coverage is only which of them the
  # merchant is allowed to serve. That keeps switching to a single UPDATE.
  case "$COVERAGE" in
    nationwide) regions="ARRAY['Algeria']" ;;
    cities)     regions="ARRAY['Algiers', 'Oran', 'Annaba']" ;;
    *) die "COVERAGE must be 'nationwide' or 'cities', got '$COVERAGE'" ;;
  esac
  $PG -q -v ON_ERROR_STOP=1 -c "
    UPDATE atlas_app.merchant
       SET origin_restriction = $regions, destination_restriction = $regions;" >/dev/null \
    || die "could not set coverage"

  # The driver side has its OWN geofences, and getting this wrong does not look
  # like a geofence problem from the passenger side -- it looks like "search
  # returns a route but no price".
  #
  # atlas_driver_offer_bpp ships {Karnataka}, so the BPP answered every Algerian
  # /search the gateway forwarded with
  #     400 RIDE_NOT_SERVICEABLE  "not serviceable due to georestrictions"
  # which the BAP has nowhere to show. The passenger just sees no offers.
  #
  # Same rows, same coverage setting, copied across rather than re-imported so
  # the two sides cannot drift. Karnataka is left in place: it restricts
  # nothing once the merchant no longer references it.
  $PG -q -v ON_ERROR_STOP=1 -c "
    INSERT INTO atlas_driver_offer_bpp.geometry (id, region, geom)
    SELECT id, region, geom FROM atlas_app.geometry
     WHERE region IN ('Algeria', 'Algiers', 'Oran', 'Annaba')
    ON CONFLICT (id) DO NOTHING;
    UPDATE atlas_driver_offer_bpp.merchant
       SET origin_restriction = $regions, destination_restriction = $regions;" >/dev/null \
    || die "could not apply coverage to the driver side"

  # The merchant row (which carries the restriction) is cached in Redis, so the
  # API would otherwise keep serving the previous service areas. Both apps cache
  # it, so both have to be told.
  docker exec ny-redis redis-cli FLUSHALL >/dev/null
  docker restart ny-driver >/dev/null 2>&1 || true

  place_drivers_in_algiers

  $PG -t -c "SELECT '  serving: ' || array_to_string(origin_restriction, ', ')
               FROM atlas_app.merchant;" | sed 's/^ */  /' | grep -v '^ *$'
  $PG -t -c "SELECT region || ' (' || ST_NPoints(geom) || ' pts)'
               FROM atlas_app.geometry
              WHERE region IN ('Algeria','Algiers','Oran','Annaba') ORDER BY region;" \
    | sed 's/^ */    /' | grep -v '^ *$'

  export_geojson
}

# The seeded test drivers sit in Kochi, India. A search from Algiers therefore
# finds nobody, and "nobody nearby" is indistinguishable from "the connector is
# broken" at the API: both give you a route and no price.
place_drivers_in_algiers() {
  log "Placing the test drivers in Algiers"

  # THE COLUMN THAT MATTERS IS `point`, NOT lat/lon.
  #
  # driver_location carries lat, lon AND a PostGIS `point`. The driver-pool
  # query does its distance test on `point`; lat/lon are carried along for
  # display. Updating only lat/lon looks completely correct in psql and changes
  # nothing at all -- the pool stays empty and the search still returns no
  # price. Cost an hour.
  #
  # coordinates_calculated_at matters too: a driver whose position is older than
  # the freshness window is skipped.
  #
  # Spread is +/-0.005 degrees, roughly +/-550 m. transporter_config here is
  # min_radius 700 / max_radius 1500, so anything wider simply falls outside the
  # search and the pool is empty again.
  $PG -q -v ON_ERROR_STOP=1 -c "
    UPDATE atlas_driver_offer_bpp.driver_location dl
       SET lat = 36.7538 + (random() - 0.5) * 0.01,
           lon = 3.0588  + (random() - 0.5) * 0.01,
           coordinates_calculated_at = now(),
           updated_at = now()
      FROM atlas_driver_offer_bpp.person p
     WHERE p.id = dl.driver_id
       AND p.merchant_id = 'favorit0-0000-0000-0000-00000favorit';

    UPDATE atlas_driver_offer_bpp.driver_location
       SET point = ST_SetSRID(ST_Point(lon, lat), 4326)
     WHERE lat BETWEEN 36.7 AND 36.8;

    UPDATE atlas_driver_offer_bpp.driver_information di
       SET active = true, on_ride = false
      FROM atlas_driver_offer_bpp.person p
     WHERE p.id = di.driver_id
       AND p.merchant_id = 'favorit0-0000-0000-0000-00000favorit';" >/dev/null \
    || die "could not place the drivers"

  $PG -t -c "
    SELECT '  ' || v.variant || '  ' || round(ST_Distance(
             dl.point::geography,
             ST_SetSRID(ST_Point(3.0588, 36.7538), 4326)::geography)::numeric)
             || ' m from Algiers centre'
      FROM atlas_driver_offer_bpp.driver_location dl
      JOIN atlas_driver_offer_bpp.vehicle v ON v.driver_id = dl.driver_id
      JOIN atlas_driver_offer_bpp.driver_information di ON di.driver_id = dl.driver_id
     WHERE di.active AND dl.lat BETWEEN 36.7 AND 36.8
     ORDER BY 1;" | grep -v '^ *$' | sed 's/^ *//'
}

# The end-to-end check that actually matters: an Algerian number asks for a
# ride and gets a price back. That single assertion covers the whole chain --
# the +213 patch, OSRM routing, the gateway, the registry, the driver-side
# geofences and the driver pool. Every one of those failed at some point while
# this was being built, and all but the first are invisible from the passenger
# side: you get a route and no price, whichever link is broken.
verify_connector() {
  log "Verifying the BAP <-> BPP chain (search must come back with a price)"
  local base="http://localhost:8014" auth authid token sid res n

  auth=$(curl -s --max-time 20 -X POST "$base/v2/auth" \
    -H 'content-type: application/json' \
    -d '{"mobileCountryCode":"+213","mobileNumber":"0550123456","merchantId":"YATRI"}')
  authid=$(printf '%s' "$auth" | sed -nE 's/.*"authId":"([^"]+)".*/\1/p')
  [ -n "$authid" ] || die "+213 login rejected -- is this the patched rider-app-exe? $auth"
  ok "POST /v2/auth                  200  (+213 accepted)"

  token=$(curl -s --max-time 20 -X POST "$base/v2/auth/$authid/verify" \
    -H 'content-type: application/json' -d '{"otp":"7891","deviceToken":"setup-check"}' \
    | sed -nE 's/.*"token":"([^"]+)".*/\1/p')
  [ -n "$token" ] || die "OTP verification failed"

  sid=$(curl -s --max-time 40 -X POST "$base/v2/rideSearch" \
    -H 'content-type: application/json' -H "token: $token" \
    -d '{"fareProductType":"ONE_WAY","contents":{"origin":{"address":{"area":"pickup","city":"Algiers","country":"Algeria","state":"Alger","building":"1","areaCode":"16000","street":"-","door":"1"},"gps":{"lat":36.7538,"lon":3.0588}},"destination":{"address":{"area":"destination","city":"Algiers","country":"Algeria","state":"Alger","building":"1","areaCode":"16000","street":"-","door":"1"},"gps":{"lat":36.7169,"lon":3.1836}}}}' \
    | sed -nE 's/.*"searchId":"([^"]+)".*/\1/p')
  [ -n "$sid" ] || die "ride search returned no searchId"
  ok "POST /v2/rideSearch            200  searchId=$sid"

  # The offer arrives asynchronously: rider -> gateway -> driver-app -> on_search
  # -> gateway -> rider. A few seconds is normal.
  local i
  for i in $(seq 1 12); do
    sleep 5
    res=$(curl -s --max-time 20 "$base/v2/rideSearch/$sid/results" -H "token: $token")
    # The estimate id is "id" inside "estimates" -- there is no "estimateId"
    # field. Grepping for one silently reports success as failure.
    n=$(printf '%s' "$res" | grep -o '"estimatedFare"' | wc -l)
    if [ "$n" -gt 0 ]; then
      ok "GET  /v2/rideSearch/{}/results 200  $n estimate(s) after $((i * 5))s"
      printf '%s' "$res" \
        | grep -o '"vehicleVariant":"[A-Z_]*"\|"estimatedTotalFare":[0-9]*' \
        | paste - - 2>/dev/null | sed 's/^/    /' | head -6
      return
    fi
  done

  echo "--- gateway ---";    docker logs --tail 15 ny-beckn-gateway 2>&1 | tail -5
  echo "--- driver-app ---"; docker logs --tail 40 ny-driver 2>&1 | grep -o 'driver pool \[\]' | tail -1
  die "search returned a route but no price after 60s.

The usual causes, in the order they bit us:
  * driver-app says RIDE_NOT_SERVICEABLE -> atlas_driver_offer_bpp geofences
  * 'driver pool []'                     -> driver_location.point (not lat/lon!)
                                            or drivers outside max_radius (1500 m)
  * gateway cannot reach the BPP         -> atlas_registry.subscriber URL"
}

# Export the service areas the *database* actually holds, so the map can never
# drift from what the API enforces. Regenerated on every run; not committed.
export_geojson() {
  # Export exactly the regions the merchant is allowed to serve, read back from
  # the merchant row itself. The map then cannot show a different coverage from
  # the one the API enforces.
  $PG -At -c "
    SELECT json_build_object(
             'type','FeatureCollection',
             'features', json_agg(json_build_object(
               'type','Feature',
               'properties', json_build_object('region', region),
               'geometry', ST_AsGeoJSON(geom)::json)))
      FROM atlas_app.geometry
     WHERE region = ANY (SELECT unnest(origin_restriction) FROM atlas_app.merchant);" \
    | tr -d '\r' > demo-map/site/areas.geojson
  [ -s demo-map/site/areas.geojson ] || die "failed to export demo-map/site/areas.geojson"
  ok "map data exported ($(wc -c < demo-map/site/areas.geojson) bytes)"
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
  $PG -t -c "SELECT 'rider  tables=' || (SELECT count(*) FROM information_schema.tables
                                         WHERE table_schema='atlas_app')
                 || '  merchant='    || (SELECT count(*) FROM atlas_app.merchant)
                 || '  person='      || (SELECT count(*) FROM atlas_app.person);" \
    | tr -d ' \r' | grep -v '^$' | sed 's/^/    /'
  $PG -t -c "SELECT 'driver tables=' || (SELECT count(*) FROM information_schema.tables
                                         WHERE table_schema='atlas_driver_offer_bpp')
                 || '  merchant='    || (SELECT count(*) FROM atlas_driver_offer_bpp.merchant)
                 || '  drivers='     || (SELECT count(*) FROM atlas_driver_offer_bpp.person WHERE role='DRIVER');" \
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

wait_for_driver_api() {
  log "Waiting for driver-app on :8017"
  for _ in $(seq 1 60); do
    code=$(curl -s -o /dev/null -w '%{http_code}' --max-time 5 http://localhost:8017/openapi || true)
    [ "$code" = "200" ] && { ok "driver API is up"; return; }
    sleep 3
  done
  echo "--- driver-app logs ---"; docker logs ny-driver 2>&1 | tail -25
  die "driver-app did not come up"
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

  # Positive: Algiers city centre must be inside the service area under either
  # coverage setting.
  svc=$(curl -s --max-time 20 -X POST "$base/v2/serviceability/origin" \
      -H 'content-type: application/json' -H "token: $token" \
      -d '{"location":{"lat":36.7538,"lon":3.0588}}')
  printf '%s' "$svc" | grep -q '"serviceable":true' || die "Algiers should be serviceable: $svc"
  ok "POST /v2/serviceability/origin 200  Algiers      serviceable=true"

  # Coverage-dependent: Constantine is in Algeria but is not one of the three
  # cities, so it distinguishes the two settings rather than just passing.
  svc=$(curl -s --max-time 20 -X POST "$base/v2/serviceability/origin" \
      -H 'content-type: application/json' -H "token: $token" \
      -d '{"location":{"lat":36.3650,"lon":6.6147}}')
  if [ "$COVERAGE" = "nationwide" ]; then
    printf '%s' "$svc" | grep -q '"serviceable":true' \
      || die "nationwide: Constantine should be serviceable: $svc"
    ok "POST /v2/serviceability/origin 200  Constantine  serviceable=true"
  else
    printf '%s' "$svc" | grep -q '"serviceable":false' \
      || die "cities: Constantine should NOT be serviceable: $svc"
    ok "POST /v2/serviceability/origin 200  Constantine  serviceable=false"
  fi

  # Negative: a neighbouring country must be refused even nationwide. This is
  # what makes 'nationwide' meaningfully different from 'no geofence at all'.
  svc=$(curl -s --max-time 20 -X POST "$base/v2/serviceability/origin" \
      -H 'content-type: application/json' -H "token: $token" \
      -d '{"location":{"lat":36.8065,"lon":10.1815}}')
  printf '%s' "$svc" | grep -q '"serviceable":false' || die "Tunis should NOT be serviceable: $svc"
  ok "POST /v2/serviceability/origin 200  Tunis (TN)   serviceable=false"

  # Negative: proves the Algeria swap actually replaced the Indian service
  # areas rather than just adding to them. Without this, an always-true
  # serviceability endpoint would pass the check above.
  svc=$(curl -s --max-time 20 -X POST "$base/v2/serviceability/origin" \
      -H 'content-type: application/json' -H "token: $token" \
      -d '{"location":{"lat":12.9715987,"lon":77.5945627}}')
  printf '%s' "$svc" | grep -q '"serviceable":false' || die "Bangalore should NOT be serviceable: $svc"
  ok "POST /v2/serviceability/origin 200  Bangalore    serviceable=false"

  verify_driver

  printf '\n\033[1;32m*** Backend is fully operational ***\033[0m\n'
  printf '\033[1;36m    Service-area map: http://localhost:8025\033[0m\n\n'
}

verify_driver() {
  log "Verifying the driver side"
  local base="http://localhost:8017" code mid r authid token

  code=$(curl -s -o /dev/null -w '%{http_code}' --max-time 15 "$base/openapi" 2>/dev/null) || true
  [ "$code" = "200" ] || die "driver /openapi not serving (HTTP ${code:-000})"
  ok "GET  /openapi                  200"

  # Driver auth takes the merchant UUID, not the shortId the rider side uses.
  mid=$($PG -At -c "SELECT id FROM atlas_driver_offer_bpp.merchant WHERE short_id='NAMMA_YATRI_PARTNER';" \
        | tr -d ' \r')
  [ -n "$mid" ] || die "NAMMA_YATRI_PARTNER merchant missing — driver seed did not migrate"
  ok "merchant NAMMA_YATRI_PARTNER    $mid"

  # An unknown number is fine: auth calls createDriverWithDetails, so this
  # exercises registration and login in one go.
  r=$(curl -s --max-time 20 -X POST "$base/ui/auth" -H 'content-type: application/json' \
        -d "{\"mobileNumber\":\"9999901234\",\"mobileCountryCode\":\"+91\",\"merchantId\":\"$mid\"}")
  authid=$(printf '%s' "$r" | sed -nE 's/.*"authId":"([^"]+)".*/\1/p')
  [ -n "$authid" ] || die "driver login failed: $r"
  ok "POST /ui/auth                  200  authId=$authid"

  token=$(curl -s --max-time 20 -X POST "$base/ui/auth/$authid/verify" \
        -H 'content-type: application/json' \
        -d '{"otp":"7891","deviceToken":"setup-check"}' \
        | sed -nE 's/.*"token":"([^"]+)".*/\1/p')
  [ -n "$token" ] || die "driver OTP verification failed"
  ok "POST /ui/auth/{id}/verify      200  token=$token"
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
# Both services must be seeded before either starts: each applies its own
# migrations on startup, and the driver test data has to be in place first.
seed_driver_db
# Before the gateway starts: it resolves every BPP through the registry, and an
# empty registry means a search that reaches nobody.
seed_registry
log "Starting rider-app and driver-app (each applies its own migrations)"
docker compose up -d rider-app proxy map driver-app driver-proxy
log "Starting the BAP <-> BPP connector"
docker compose up -d mock-registry beckn-gateway
wait_for_api
wait_for_driver_api
seed_algeria
verify
verify_connector
show_db_state
