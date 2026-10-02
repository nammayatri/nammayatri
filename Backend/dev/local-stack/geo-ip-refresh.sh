#!/usr/bin/env bash
#
# Rebuild maps-shim/ip-countries.json: which IP blocks belong to Algeria (DZ)
# and Mauritania (MR), from AfriNIC's own delegation file. For the sign-in
# screen's country detection (2026-09-14) — see maps-shim/geo.js.
#
#   bash geo-ip-refresh.sh      # then deploy maps-shim as usual
#
# ── Why AfriNIC and not a GeoIP database ──────────────────────────────────────
# AfriNIC is the registry that hands out African address space, and its
# delegation file is public, free and carries no licence terms. Two countries
# need ~70 rows of it; a commercial database would be a key, a licence and an
# attribution clause for the same answer. Its limit: an operator using space
# registered elsewhere (RIPE) is not in it — the phone asks GPS first for that
# reason, and the app falls back to the last country used.
set -euo pipefail
cd "$(dirname "$0")"
SRC=https://ftp.afrinic.net/pub/stats/afrinic/delegated-afrinic-extended-latest
TMP=$(mktemp)
curl -sf --max-time 120 -o "$TMP" "$SRC"
python3 - "$TMP" maps-shim/ip-countries.json <<'PY'
import json, sys, datetime
src, out = sys.argv[1], sys.argv[2]
v4, v6 = [], []
for line in open(src):
    p = line.strip().split('|')
    if len(p) < 7 or p[1] not in ('DZ', 'MR') or p[6] not in ('allocated', 'assigned'):
        continue
    if p[2] == 'ipv4':
        v4.append([p[1], p[3], int(p[4])])      # country, first address, count
    elif p[2] == 'ipv6':
        v6.append([p[1], p[3], int(p[4])])      # country, prefix, prefix length
json.dump({
    "source": "AfriNIC delegated-afrinic-extended-latest",
    "built": datetime.date.today().isoformat(),
    "v4": v4, "v6": v6,
}, open(out, 'w'), indent=0)
by = lambda rows, c: sum(1 for r in rows if r[0] == c)
print(f"ip-countries.json: DZ {by(v4,'DZ')} v4 + {by(v6,'DZ')} v6, MR {by(v4,'MR')} v4 + {by(v6,'MR')} v6")
PY
rm -f "$TMP"
