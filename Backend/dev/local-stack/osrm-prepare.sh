#!/usr/bin/env bash
# Build the OSRM routing graph for Algeria.
#
#   ./osrm-prepare.sh          build it (skips if already built)
#   ./osrm-prepare.sh rebuild  force a rebuild, e.g. for fresher map data
#   ./osrm-prepare.sh clean    delete the graph and the downloaded extract
#
# Why this exists: the backend needs distances and routes before a ride search
# can go anywhere, and the only providers it supports out of the box are
# Google, OSRM and MMI (India-only). OSRM is free, self-hosted, unlimited, and
# already present in merchant_service_config -- so it is a config change rather
# than a code change.
#
# Everything lives in the ny-osrm-data Docker volume rather than a bind mount:
# preprocessing is heavily I/O bound and the Windows filesystem makes it
# drastically slower.
set -euo pipefail
cd "$(dirname "$0")"

OSRM_IMAGE="ghcr.io/project-osrm/osrm-backend:latest"
VOLUME="ny-osrm-data"
# Geofabrik republishes daily. Algeria is ~285 MB.
EXTRACT_URL="https://download.geofabrik.de/africa/algeria-latest.osm.pbf"
PBF="algeria-latest.osm.pbf"
BASE="algeria-latest"

log() { printf '\n\033[1;36m==> %s\033[0m\n' "$*"; }
ok()  { printf '\033[1;32m    %s\033[0m\n' "$*"; }
die() { printf '\n\033[1;31mFAILED: %s\033[0m\n' "$*" >&2; exit 1; }

in_vol() { docker run --rm -v "$VOLUME:/data" "$@"; }

case "${1:-build}" in
  clean)
    docker rm -f ny-osrm >/dev/null 2>&1 || true
    docker volume rm "$VOLUME" >/dev/null 2>&1 || true
    echo "cleaned"; exit 0 ;;
  rebuild) FORCE=1 ;;
  build)   FORCE=0 ;;
  *) die "usage: $0 [build|rebuild|clean]" ;;
esac

docker volume create "$VOLUME" >/dev/null

# The .osrm.mldgr file is the last artefact osrm-customize writes, so its
# presence means the whole pipeline finished. Checking for the .osrm file
# instead would wrongly accept a half-finished extract.
if [ "$FORCE" = "0" ] && in_vol alpine:latest test -f "/data/$BASE.osrm.mldgr" 2>/dev/null; then
  ok "routing graph already built (./osrm-prepare.sh rebuild to redo)"
  exit 0
fi

log "Fetching OSRM"
docker pull -q "$OSRM_IMAGE" >/dev/null || die "could not pull $OSRM_IMAGE"
ok "$OSRM_IMAGE"

if in_vol alpine:latest test -f "/data/$PBF" 2>/dev/null; then
  ok "map extract already downloaded"
else
  log "Downloading Algeria map data (~285 MB)"
  # -C - resumes a partial file, so a dropped connection costs only the
  # remainder rather than starting over.
  for attempt in 1 2 3 4 5 6 7 8; do
    if in_vol alpine/curl:latest -fL --retry 3 --retry-delay 5 -C - \
         -o "/data/$PBF" "$EXTRACT_URL"; then
      break
    fi
    [ "$attempt" = "8" ] && die "could not download the map extract after 8 attempts"
    echo "    attempt $attempt failed; retrying in 20s"
    sleep 20
  done
  ok "downloaded $(in_vol alpine:latest du -h "/data/$PBF" | cut -f1)"
fi

# The three stages must run in order, and each reads what the previous wrote.
#   extract    parse the OSM data and apply the car profile
#   partition  split the graph into cells (multi-level Dijkstra)
#   customize  compute the cell weights
# MLD is used rather than contraction hierarchies because it is far quicker to
# prepare and re-customise, and the routing quality difference does not matter
# at our scale.
log "osrm-extract (the slow one -- parses the whole country)"
in_vol "$OSRM_IMAGE" osrm-extract -p /opt/car.lua "/data/$PBF" \
  || die "osrm-extract failed"
ok "extracted"

log "osrm-partition"
in_vol "$OSRM_IMAGE" osrm-partition "/data/$BASE.osrm" || die "osrm-partition failed"
ok "partitioned"

log "osrm-customize"
in_vol "$OSRM_IMAGE" osrm-customize "/data/$BASE.osrm" || die "osrm-customize failed"
ok "customised"

in_vol alpine:latest test -f "/data/$BASE.osrm.mldgr" \
  || die "pipeline finished but $BASE.osrm.mldgr is missing"

log "Done"
in_vol alpine:latest sh -c "du -sh /data | cut -f1" | sed 's/^/    graph size: /'
printf '\033[1;32m    Start it with: docker compose up -d osrm\033[0m\n\n'
