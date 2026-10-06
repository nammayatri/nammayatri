#!/usr/bin/env bash
#
# The map in Arabic (2026-09-14, approved by Moha). Gives the tile server a font
# that has Arabic letters, and a second style whose labels read `name:ar` first.
#
#   bash tiles-arabic.sh            # build, test on a side port, switch, verify
#   bash tiles-arabic.sh rollback   # back to the bundled --file mode
#
# Runs ON the VPS, from /opt/ny/local-stack.
#
# ── Why the font pack, and not a glyph build ──────────────────────────────────
# The image's bundled "Noto Sans Regular" has NO Arabic: its 1536-1791 glyph
# range is 32 bytes (measured 2026-09-14), so any Arabic label drew nothing.
# The OpenMapTiles font pack v2.0 ships the same family WITH it (96 kB for that
# range). Same font names, so the French style needs no edit at all.
#
# ── Why config mode ───────────────────────────────────────────────────────────
# `--file` serves one bundled style; a second style needs a config. The ids
# are kept — style `basic-preview`, data `v3` — so every URL today's APKs use
# answers exactly as before, and only `styles/movin-ar/…` is new.
#
# ── Order, and the way back ───────────────────────────────────────────────────
# Built beside the live server, then proven on a throwaway container on
# 127.0.0.1:8036 BEFORE the live one is touched. The compose file is edited in
# place (the deployed one is a superset of the repo's — never copy over it),
# with a backup, and a failed public check puts it all back on its own.
#
# NOTE: the mbtiles file name is written into tiles-config/config.json from
# MAP_COUNTRY. Change MAP_COUNTRY and this script must be run again.
set -uo pipefail
cd "$(dirname "$0")"
say() { printf '\n== %s  (%s)\n' "$*" "$(date -u +%T)"; }
ok()  { printf '   ok   %s\n' "$*"; }
bad() { printf '   BAD  %s\n' "$*"; }

DIR=tiles-config
PACK_URL=https://github.com/openmaptiles/fonts/releases/download/v2.0/v2.0.zip
PACK=/tmp/omt-fonts.zip
BUNDLED=/usr/src/app/node_modules/tileserver-gl-styles/styles/basic-preview/style.json
IMAGE=maptiler/tileserver-gl:latest
MAP=$(grep -E '^MAP_COUNTRY=' .env | cut -d= -f2); MAP=${MAP:-algeria}
PUBLIC=$(grep -E '^TILES_PUBLIC_URL=' .env | cut -d= -f2); PUBLIC=${PUBLIC:-https://api.movinapp.net/tiles/}
PUBLIC=${PUBLIC%/}

OLD_CMD='      --file /data/${MAP_COUNTRY:-algeria}.mbtiles'
NEW_CMD='      --config /config/config.json'
OLD_VOL='      - ny-tiles-data:/data
    restart: unless-stopped'
NEW_VOL='      - ny-tiles-data:/data
      - ./tiles-config:/config:ro
    restart: unless-stopped'

# Edit the tiles service of the compose file in place. $1 = to-config | to-file
edit_compose() {
  python3 - "$1" "$OLD_CMD" "$NEW_CMD" "$OLD_VOL" "$NEW_VOL" <<'PY'
import sys
way, old_cmd, new_cmd, old_vol, new_vol = sys.argv[1:]
p = 'docker-compose.yml'
s = open(p).read()
a, b = (old_cmd, new_cmd), (old_vol, new_vol)
if way == 'to-file':
    a, b = (new_cmd, old_cmd), (new_vol, old_vol)
for old, new in (a, b):
    n = s.count(old)
    if n != 1:
        print(f"   BAD  expected exactly one match, found {n}: {old.strip()[:60]}")
        sys.exit(1)
    s = s.replace(old, new)
open(p, 'w').write(s)
print(f"   ok   compose edited ({way})")
PY
}

wait_healthy() {  # $1 = base URL
  for i in $(seq 1 40); do
    curl -sf "$1/health" >/dev/null && return 0
    sleep 2
  done
  return 1
}

# Everything the app needs from the tile server. $1 = base URL. Non-zero on any miss.
check() {
  local b=$1 fail=0 code size
  for p in styles/basic-preview/style.json styles/movin-ar/style.json data/v3.json; do
    code=$(curl -s -o /tmp/tiles-check.out -w '%{http_code}' "$b/$p")
    if [ "$code" = 200 ]; then ok "$p"; else bad "$p -> $code"; fail=1; fi
  done
  # The Arabic style really reads name:ar, and its glyph URL is public.
  if curl -s "$b/styles/movin-ar/style.json" | grep -q 'name:ar'; then ok "movin-ar labels read name:ar"; else bad "movin-ar has no name:ar"; fail=1; fi
  size=$(curl -s -o /tmp/tiles-glyph.pbf -w '%{size_download}' "$b/fonts/Noto%20Sans%20Regular/1536-1791.pbf")
  if [ "${size:-0}" -gt 10000 ]; then ok "Arabic glyphs: $size bytes (was 32)"; else bad "Arabic glyphs: $size bytes"; fail=1; fi
  size=$(curl -s -o /tmp/tiles-glyph.pbf -w '%{size_download}' "$b/fonts/Noto%20Sans%20Regular/0-255.pbf")
  if [ "${size:-0}" -gt 10000 ]; then ok "Latin glyphs: $size bytes"; else bad "Latin glyphs: $size bytes"; fail=1; fi
  # Real tiles, one in each country: Nouakchott and Algiers, z12.
  for t in 12/1866/1838 12/2082/1597; do
    code=$(curl -s -o /tmp/tiles-tile.pbf -w '%{http_code}' "$b/data/v3/$t.pbf")
    if [ "$code" = 200 ]; then ok "tile $t ($(wc -c < /tmp/tiles-tile.pbf) bytes)"; else bad "tile $t -> $code"; fail=1; fi
  done
  return $fail
}

if [ "${1:-}" = "rollback" ]; then
  say "rollback: back to the bundled --file mode"
  edit_compose to-file || exit 1
  docker compose up -d tiles 2>&1 | tail -n 3
  wait_healthy "http://127.0.0.1:8035" && ok "tile server back" || bad "tile server not healthy"
  exit 0
fi

say "build $DIR beside the live server (map: $MAP)"
mkdir -p "$DIR/fonts" "$DIR/styles"
[ -s "$PACK" ] || curl -sL --max-time 300 -o "$PACK" "$PACK_URL"
python3 - "$PACK" "$DIR/fonts" <<'PY'
import sys, zipfile
z = zipfile.ZipFile(sys.argv[1]); z.extractall(sys.argv[2])
print(f"   ok   {len({n.split('/')[0] for n in z.namelist() if '/' in n})} font families unpacked")
PY
docker exec ny-tiles cat "$BUNDLED" > "$DIR/styles/basic-preview.json" 2>/dev/null \
  || docker run --rm --entrypoint cat "$IMAGE" "$BUNDLED" > "$DIR/styles/basic-preview.json"
python3 - "$DIR/styles" "$DIR/config.json" "$MAP" <<'PY'
import json, sys
styles, config, mapname = sys.argv[1:]
s = json.load(open(f"{styles}/basic-preview.json"))
ar = json.loads(json.dumps(s))
ar["name"] = "Movin — étiquettes en arabe"
changed = 0
for layer in ar["layers"]:
    lay = layer.get("layout", {})
    tf = lay.get("text-field")
    if tf == "{name}":
        lay["text-field"] = ["coalesce", ["get", "name:ar"], ["get", "name"]]; changed += 1
    elif tf == "{name:latin}":
        lay["text-field"] = ["coalesce", ["get", "name:ar"], ["get", "name:latin"], ["get", "name"]]; changed += 1
json.dump(ar, open(f"{styles}/movin-ar.json", "w"), ensure_ascii=False)
json.dump({
    "options": {
        "paths": {"root": "/config", "fonts": "fonts", "styles": "styles", "mbtiles": "/data"},
        "serveAllFonts": True,
    },
    "styles": {
        "basic-preview": {"style": "basic-preview.json"},
        "movin-ar": {"style": "movin-ar.json"},
    },
    "data": {"v3": {"mbtiles": f"{mapname}.mbtiles"}},
}, open(config, "w"), indent=2)
print(f"   ok   movin-ar: {changed} label layers read name:ar first; config for {mapname}.mbtiles")
PY
chmod -R a+rX "$DIR"

say "prove it on a throwaway container, 127.0.0.1:8036"
docker rm -f ny-tiles-test >/dev/null 2>&1
docker run -d --name ny-tiles-test -p 127.0.0.1:8036:8080 \
  -v ny-tiles-data:/data:ro -v "$PWD/$DIR:/config:ro" \
  "$IMAGE" --config /config/config.json --port 8080 --public_url http://127.0.0.1:8036/ >/dev/null
if wait_healthy "http://127.0.0.1:8036" && check "http://127.0.0.1:8036"; then
  ok "side test passed"
  docker rm -f ny-tiles-test >/dev/null
else
  bad "side test failed -- the live server was NOT touched"
  docker logs --tail 20 ny-tiles-test 2>&1 | sed 's/^/   /'
  docker rm -f ny-tiles-test >/dev/null
  exit 1
fi

say "switch the live server"
mkdir -p backups
cp docker-compose.yml "backups/docker-compose.yml.pre-tiles-arabic-$(date -u +%Y%m%dT%H%M%SZ)"
ok "compose backed up"
edit_compose to-config || exit 1
docker compose up -d tiles 2>&1 | tail -n 3

say "check through the public URL ($PUBLIC)"
if wait_healthy "http://127.0.0.1:8035" && check "$PUBLIC"; then
  ok "the map speaks Arabic -- movin-ar is live, basic-preview unchanged"
else
  bad "public check failed -- rolling back"
  edit_compose to-file
  docker compose up -d tiles 2>&1 | tail -n 3
  wait_healthy "http://127.0.0.1:8035" && ok "rolled back, tile server healthy" || bad "rolled back but NOT healthy"
  exit 1
fi
