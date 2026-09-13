#!/usr/bin/env bash
#
# Step 4 of the two-country plan (2026-09-13): one map for Algeria and
# Mauritania — routes, tiles and place search.
#
#   bash maps-two-countries.sh all      # places, merge, osrm, tiles, switch, check
#   bash maps-two-countries.sh places   # add the Algerian places to the live index
#   bash maps-two-countries.sh build    # merge + osrm + tiles (nothing served changes)
#   bash maps-two-countries.sh switch   # serve the combined files, then check
#   bash maps-two-countries.sh check    # routes, tiles and search in both countries
#   bash maps-two-countries.sh rollback # back to the Mauritania-only files
#
# ── Built beside, then switched ────────────────────────────────────────────
# The combined graph and tiles are built under new names while the Mauritanian
# ones keep serving. Only `switch` changes what riders get, and a failed check
# after it rolls back on its own.
#
# ── MAP_COUNTRY now lives in .env ──────────────────────────────────────────
# It did not, until this script. The compose defaults it to `algeria`, and
# `mauritania` had been given inline on 2026-09-03 — so any later plain
# `docker compose up` would have silently put the Algeria-only map back.
#
set -uo pipefail
cd "$(dirname "$0")"

BOTH=algeria-mauritania
VOL=ny-osrm-data
PG="docker exec ny-postgres psql -U postgres -d atlas_dev"
say() { printf '\n== %s  (%s)\n' "$*" "$(date -u +%T)"; }
ok()  { printf '   ok   %s\n' "$*"; }
bad() { printf '   BAD  %s\n' "$*"; }

places() {
  say "places: add Algeria to the live index"
  if [ "$($PG -At -c "select count(*) from geo.place where place_id like 'n%' and lat > 30")" != "0" ]; then
    ok "Algerian places already present — skipped"
    return 0
  fi
  local csv=geocoder/places.algeria.csv
  [ -f "$csv" ] || { bad "$csv missing"; return 1; }
  local want="osm_type,osm_id,kind,class,subclass,name,name_fr,name_en,name_latin,display_name,alt_names,importance,lat,lon"
  local got; got=$(head -n 1 "$csv" | tr -d '\r')
  [ "$got" = "$want" ] || { bad "CSV header differs from place_raw: $got"; return 1; }

  mkdir -p backups && chmod 700 backups
  local bk="backups/geo-place-$(date -u +%Y%m%dT%H%M%SZ).sql"
  docker exec ny-postgres pg_dump -U postgres -d atlas_dev -t geo.place > "$bk" && [ -s "$bk" ] \
    || { bad "backup of geo.place failed — nothing appended"; return 1; }
  chmod 600 "$bk"; ok "backup $bk ($(du -h "$bk" | cut -f1))"

  $PG -v ON_ERROR_STOP=1 -q -c "drop table if exists geo.place_raw;
    create table geo.place_raw (
      osm_type text, osm_id bigint, kind text, class text, subclass text,
      name text, name_fr text, name_en text, name_latin text,
      display_name text, alt_names text, importance real,
      lat double precision, lon double precision);" || return 1
  docker exec -i ny-postgres psql -U postgres -d atlas_dev -v ON_ERROR_STOP=1 \
    -c "\copy geo.place_raw from stdin with (format csv, header true)" < "$csv" || return 1
  ok "$($PG -At -c 'select count(*) from geo.place_raw') raw rows loaded"

  docker cp geocoder/append-country.sql ny-postgres:/tmp/append-country.sql >/dev/null
  $PG -v ON_ERROR_STOP=1 -f /tmp/append-country.sql || { bad "append failed — rolled back"; return 1; }
  ok "appended"
}

merge() {
  say "merge the two extracts"
  if docker run --rm -v "$VOL:/data" alpine:latest test -f "/data/$BOTH-latest.osm.pbf"; then
    ok "$BOTH-latest.osm.pbf already there"; return 0
  fi
  for c in algeria mauritania; do
    docker run --rm -v "$VOL:/data" alpine:latest test -f "/data/$c-latest.osm.pbf" \
      || { bad "$c-latest.osm.pbf missing from $VOL"; return 1; }
  done
  docker run --rm -v "$VOL:/data" python:3.12-slim bash -c \
    "apt-get -qq update >/dev/null && apt-get -qq install -y --no-install-recommends osmium-tool >/dev/null \
     && osmium merge /data/algeria-latest.osm.pbf /data/mauritania-latest.osm.pbf \
          -o /data/$BOTH-latest.osm.pbf.part --output-format pbf --overwrite \
     && mv /data/$BOTH-latest.osm.pbf.part /data/$BOTH-latest.osm.pbf \
     && ls -lh /data/$BOTH-latest.osm.pbf" || { bad "osmium merge failed"; return 1; }
  ok "merged"
}

osrm()  { say "routing graph"; COUNTRY=$BOTH bash ./osrm-prepare.sh build  || { bad "osrm-prepare failed"; return 1; }; }
tiles() { say "map tiles";     COUNTRY=$BOTH bash ./tiles-prepare.sh build || { bad "tiles-prepare failed"; return 1; }; }

set_country() {
  if grep -q '^MAP_COUNTRY=' .env; then
    sed -i "s/^MAP_COUNTRY=.*/MAP_COUNTRY=$1/" .env
  else
    echo "MAP_COUNTRY=$1" >> .env
  fi
  MAP_COUNTRY=$1 docker compose up -d --no-build --no-deps --force-recreate osrm tiles 2>&1 | tail -n 4
  ok "serving MAP_COUNTRY=$1"
}

check() {
  say "check both countries"
  # Wait for OSRM to finish loading before judging it. The first run of this
  # check slept 8 s, hit "connection refused" on a graph ten times the size of
  # Mauritania's that was still loading, and rolled a good switch back.
  local i
  for i in $(seq 1 90); do
    curl -s -o /dev/null "http://127.0.0.1:5000/route/v1/driving/-15.9582,18.0858;-15.95,18.103?overview=false" && break
    sleep 2
  done
  ok "osrm answering after ~$((i * 2)) s"
  python3 - <<'PY'
import json, math, sys, urllib.request

def get(url, raw=False):
    with urllib.request.urlopen(url, timeout=20) as r:
        b = r.read()
        return b if raw else json.loads(b)

fails = 0
for name, a, b in [("Algiers  Centre -> Hussein Dey", (3.0588, 36.7538), (3.1750, 36.7050)),
                   ("Nouakchott  Tevragh Zeina -> Ksar", (-15.9582, 18.0858), (-15.9500, 18.1030))]:
    try:
        r = get(f"http://127.0.0.1:5000/route/v1/driving/{a[0]},{a[1]};{b[0]},{b[1]}?overview=false")
        d = r["routes"][0]["distance"] if r.get("code") == "Ok" else 0
    except Exception as e:
        d = 0; print("   route error", e)
    good = d > 1000
    fails += 0 if good else 1
    print(f"   {'ok ' if good else 'BAD'}  route {name}: {d/1000:.1f} km")

try:
    style = get("http://127.0.0.1:8035/styles/basic-preview/style.json")
    src = next(iter(style["sources"].values()))
    tj = get(src["url"].replace("https://api.movinapp.net/tiles", "http://127.0.0.1:8035")) if "url" in src else src
    tpl = tj["tiles"][0].replace("https://api.movinapp.net/tiles", "http://127.0.0.1:8035")
    def xy(lon, lat, z=14):
        n = 2 ** z
        return int((lon + 180) / 360 * n), int((1 - math.log(math.tan(math.radians(lat)) + 1 / math.cos(math.radians(lat))) / math.pi) / 2 * n)
    for name, (lon, lat) in [("Algiers", (3.0588, 36.7538)), ("Nouakchott", (-15.9582, 18.0858))]:
        x, y = xy(lon, lat)
        size = len(get(tpl.replace("{z}", "14").replace("{x}", str(x)).replace("{y}", str(y)), raw=True))
        good = size > 1000
        fails += 0 if good else 1
        print(f"   {'ok ' if good else 'BAD'}  tile z14 {name}: {size} bytes")
except Exception as e:
    fails += 1; print("   BAD  tiles:", e)
sys.exit(1 if fails else 0)
PY
  local rc=$?
  for q in "Didouche|36.7538|3.0588" "Hussein Dey|36.7538|3.0588" "تفرغ|18.0858|-15.9582" "Ksar|18.0858|-15.9582"; do
    IFS='|' read -r text lat lon <<< "$q"
    n=$($PG -At -c "select count(*) from geo.search('$text', $lat, $lon, 5)")
    first=$($PG -At -c "select display_name from geo.search('$text', $lat, $lon, 1)")
    [ "${n:-0}" -gt 0 ] && ok "search '$text': $n, first: $first" || { bad "search '$text': nothing"; rc=1; }
  done
  $PG -At -c "select '   index: ' || count(*) || ' places, ' || count(name_ar) || ' with Arabic' from geo.place"
  return $rc
}

switch() {
  say "switch routes and tiles to $BOTH"
  set_country "$BOTH"
  if check; then
    ok "both countries route, draw and search"
  else
    bad "check failed after the switch — rolling back to mauritania"
    set_country mauritania
    return 1
  fi
}

case "${1:-all}" in
  places)   places ;;
  build)    merge && osrm && tiles ;;
  switch)   switch ;;
  check)    check ;;
  rollback) set_country mauritania ;;
  all)      places && merge && osrm && tiles && switch ;;
  *) sed -n '2,20p' "$0"; exit 2 ;;
esac
rc=$?
say "finished with exit $rc"
exit $rc
