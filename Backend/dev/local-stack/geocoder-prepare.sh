#!/usr/bin/env bash
#
# Build the place-search index from one country's OSM extract.
#
#   ./geocoder-prepare.sh            # everything: extract, load, functions
#   ./geocoder-prepare.sh extract    # PBF -> geocoder/places.csv   (~4 min)
#   ./geocoder-prepare.sh load       # places.csv -> geo.place      (~1 min)
#   ./geocoder-prepare.sh functions  # reload geo.search / geo.reverse only
#   ./geocoder-prepare.sh check      # a few searches, to see it works
#
# `functions` is the one to use while tuning the ranking: it is instant, and
# rebuilding the table to change a weight would be absurd.
#
# Reuses the same .osm.pbf that already feeds OSRM and the tile server -- it
# lives in the ny-osrm-data volume, put there by osrm-prepare.sh. Nothing here
# downloads anything except the one-off python image.
set -euo pipefail

cd "$(dirname "$0")"

PG_CONTAINER=${PG_CONTAINER:-ny-postgres}
PG_USER=${PG_USER:-postgres}
PG_DB=${PG_DB:-atlas_dev}
OSRM_VOLUME=${OSRM_VOLUME:-ny-osrm-data}
# The country this stack serves. Everything below is derived from it, and it
# defaults to Algeria so that an unqualified run builds exactly what it built
# before. Mauritania: `COUNTRY=mauritania ./geocoder-prepare.sh`
COUNTRY=${COUNTRY:-algeria}
# Geofabrik files live under a continent. Wrong value, 404, and the error would
# otherwise be a curl failure against a URL nobody printed.
REGION=${REGION:-africa}
# Still overridable directly, which is how it was written.
PBF=${PBF:-$COUNTRY-latest.osm.pbf}
DIR="$(pwd)/geocoder"
CSV="$DIR/places.csv"

psql() { docker exec -i "$PG_CONTAINER" psql -U "$PG_USER" -d "$PG_DB" "$@"; }

step() { printf '\n\033[1m== %s\033[0m\n' "$*"; }

do_extract() {
  step "extract  $PBF -> geocoder/places.csv"
  # python:3.12-slim has no libexpat, which pyosmium's wheel links against --
  # the failure is an ImportError at `import osmium`, which reads like a bad
  # install rather than a missing system library.
  docker run --rm \
    -v "$OSRM_VOLUME":/pbf:ro \
    -v "$DIR":/work \
    python:3.12-slim \
    bash -c "apt-get -qq update >/dev/null \
          && apt-get -qq install -y --no-install-recommends libexpat1 >/dev/null \
          && pip install --quiet --no-cache-dir osmium==3.7.0 \
          && python /work/extract.py /pbf/$PBF /work/places.csv"
  ls -lh "$CSV"
}

do_load() {
  [ -f "$CSV" ] || { echo "no $CSV -- run './geocoder-prepare.sh extract' first" >&2; exit 1; }
  step "load  $(wc -l < "$CSV") lines -> geo.place_raw"

  psql -v ON_ERROR_STOP=1 <<'SQL'
create schema if not exists geo;
drop table if exists geo.place_raw;
create table geo.place_raw (
  osm_type text, osm_id bigint, kind text, class text, subclass text,
  name text, name_fr text, name_en text, name_latin text,
  display_name text, alt_names text, importance real,
  lat double precision, lon double precision
);
SQL

  # \copy runs client-side, so the file is read from this shell rather than
  # from inside the container -- no bind mount needed.
  psql -v ON_ERROR_STOP=1 -c "\copy geo.place_raw from stdin with (format csv, header true)" < "$CSV"

  step "build  geo.place"
  psql -v ON_ERROR_STOP=1 -f - < "$DIR/index.sql"
  do_functions
  do_summary
}

do_functions() {
  step "functions  geo.search, geo.reverse"
  psql -v ON_ERROR_STOP=1 -f - < "$DIR/search.sql"
}

do_summary() {
  step "what is in there"
  psql -At -F'  ' <<'SQL'
select kind, count(*), round(avg(importance)::numeric, 2)
  from geo.place group by kind order by count(*) desc;
select 'TOTAL', count(*), '' from geo.place;
SQL
}

do_check() {
  step "a few searches from Algiers centre (36.7538, 3.0588)"
  for q in "Didouche" "Bab Ezzouar" "aeroport" "Mairie" "pharmacie" "Rue Icosium" "Bab Ezouar"; do
    printf '\n\033[36m%s\033[0m\n' "$q"
    psql -At -F' | ' -c \
      "select display_name, coalesce(locality,'-'), kind, round(distance_m::numeric) || ' m', round(score::numeric,3)
         from geo.search('$q', 36.7538, 3.0588, 5);"
  done

  step "reverse: what is at 36.7538, 3.0588"
  psql -At -F' | ' -c "select * from geo.reverse(36.7538, 3.0588);"
}

case "${1:-all}" in
  extract)   do_extract ;;
  load)      do_load ;;
  functions) do_functions ;;
  check)     do_check ;;
  summary)   do_summary ;;
  all)       do_extract; do_load; do_check ;;
  *)         echo "usage: $0 [all|extract|load|functions|summary|check]" >&2; exit 2 ;;
esac
