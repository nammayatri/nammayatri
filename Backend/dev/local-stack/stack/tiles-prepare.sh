#!/usr/bin/env bash
# Build the Algeria map tiles -- the picture of the map, not the routing.
#
#   ./tiles-prepare.sh          build them (skips if already built)
#   ./tiles-prepare.sh rebuild  force a rebuild, e.g. for fresher map data
#   ./tiles-prepare.sh clean    delete the tiles
#
# Why this exists: OSRM gives us routes, but a route drawn on a blank screen is
# useless -- the app also needs streets, buildings, water and labels underneath
# it. Those come from "tiles". The alternatives were a foreign subscription
# (MapTiler, Mapbox, Google), which costs money per use and needs an
# international payment card. Decided 2026-08-06 to host them ourselves, same
# as we already do for routing.
#
# Reuses the Algeria extract that osrm-prepare.sh already downloaded into the
# ny-osrm-data volume, so this does not fetch another 285 MB of the same data.
#
# Output is MBTiles, not PMTiles. PMTiles is the nicer format -- a single file
# any static web server can serve -- but MapLibre on Android cannot read it
# without a translating server, so the format buys us nothing here and costs
# compatibility.
set -euo pipefail
cd "$(dirname "$0")"

PLANETILER_IMAGE="ghcr.io/onthegomap/planetiler:latest"
VOLUME="ny-tiles-data"
OSM_VOLUME="ny-osrm-data"
# The country this stack serves. Everything below is derived from it, and it
# defaults to Algeria so that an unqualified run builds exactly what it built
# before. Mauritania: `COUNTRY=mauritania ./tiles-prepare.sh`
COUNTRY=${COUNTRY:-algeria}
# Geofabrik files live under a continent. Wrong value, 404, and the error would
# otherwise be a curl failure against a URL nobody printed.
REGION=${REGION:-africa}

PBF="$COUNTRY-latest.osm.pbf"
OUT="$COUNTRY.mbtiles"
# Planetiler writes progressively, so a half-finished file looks like a
# finished one. Build under a different name and rename only on success.
BUILD="build/$OUT"

log() { printf '\n\033[1;36m==> %s\033[0m\n' "$*"; }
ok()  { printf '\033[1;32m    %s\033[0m\n' "$*"; }
die() { printf '\n\033[1;31mFAILED: %s\033[0m\n' "$*" >&2; exit 1; }

in_vol() { MSYS_NO_PATHCONV=1 docker run --rm -v "$VOLUME:/data" "$@"; }

case "${1:-build}" in
  clean)
    docker rm -f ny-tiles >/dev/null 2>&1 || true
    docker volume rm "$VOLUME" >/dev/null 2>&1 || true
    echo "cleaned"; exit 0 ;;
  rebuild) FORCE=1 ;;
  build)   FORCE=0 ;;
  *) die "usage: $0 [build|rebuild|clean]" ;;
esac

docker volume create "$VOLUME" >/dev/null

if [ "$FORCE" = "0" ] && in_vol alpine:latest test -f "/data/$OUT" 2>/dev/null; then
  ok "tiles already built (./tiles-prepare.sh rebuild to redo)"
  exit 0
fi

log "Checking the Algeria extract osrm-prepare.sh downloaded"
MSYS_NO_PATHCONV=1 docker run --rm -v "$OSM_VOLUME:/osm:ro" alpine:latest \
  test -f "/osm/$PBF" 2>/dev/null \
  || die "$PBF not found in the $OSM_VOLUME volume -- run ./osrm-prepare.sh first"
ok "$PBF present, reusing it"

log "Fetching Planetiler"
docker pull -q "$PLANETILER_IMAGE" >/dev/null || die "could not pull $PLANETILER_IMAGE"
ok "$PLANETILER_IMAGE"

log "Building vector tiles for Algeria (zoom 0-14)"
echo "    This also downloads land and water polygons the first time (~700 MB)."
echo "    Expect roughly 10-20 minutes."

# Planetiler spells its arguments with underscores (osm_path, not osm-path); the
# hyphenated forms are silently ignored, which produces a confusing run against
# the default Monaco extract instead of ours.
#
# --download fetches the two global datasets Planetiler needs for the low zoom
#   levels (Natural Earth, OSM water polygons); they are cached in the volume.
# --nodemap_type=sparsearray and --storage=mmap keep peak memory modest, which
#   matters on a 16 GB laptop.
MSYS_NO_PATHCONV=1 docker run --rm \
  -v "$VOLUME:/data" \
  -v "$OSM_VOLUME:/osm:ro" \
  -e JAVA_TOOL_OPTIONS="-Xmx4g" \
  "$PLANETILER_IMAGE" \
    --osm_path="/osm/$PBF" \
    --output="/data/$BUILD" \
    --download \
    --download_dir=/data/sources \
    --tmpdir=/data/tmp \
    --force \
    --nodemap_type=sparsearray \
    --storage=mmap \
  || die "planetiler failed"

log "Publishing"
in_vol alpine:latest sh -c "mv '/data/$BUILD' '/data/$OUT' && rm -rf /data/tmp" \
  || die "could not move the finished tiles into place"

SIZE=$(in_vol alpine:latest sh -c "du -h '/data/$OUT' | cut -f1" | tr -d '\r')
ok "$OUT built ($SIZE)"

cat <<EOF

Next:
  docker compose up -d tiles      # serve them on http://localhost:8035
EOF
