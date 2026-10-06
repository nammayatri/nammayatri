#!/usr/bin/env bash
# Install a Moosyl payment key on the VPS.  Never prints the key.
#
#     ./install-moosyl-key.sh '<key>'
#
# The key is an argument and not a line in this file, because this file is in
# git and the key moves real money.  It lands in /opt/ny/secrets/moosyl.env
# (root, 600), which is the only place it has ever lived -- there is nothing to
# change in the app, in docker-compose.yml or in the Haskell backend.  The app
# never holds it: the driver's phone talks to maps-shim, and maps-shim talks to
# Moosyl.
#
# ── Why --force-recreate and not restart ───────────────────────────────────
# env_file is read when the CONTAINER IS CREATED, not when it starts.  A plain
# `docker compose restart maps-shim` keeps the old key, and every check below
# would then pass or fail for a reason that has nothing to do with the key you
# just installed.  Same trap as install-moorsyl-key.sh (2026-09-20).
#
# ── What "live" means here, and how it is proved ───────────────────────────
# Test and live are the SAME base URL -- https://api.moosyl.com.  Nothing in
# the config distinguishes them; the key does, and the only way to know which
# one you are holding is to ask.  `GET /configuration` lists the payment
# methods with an `isTestingMode` flag each, so the last check is the one that
# matters: with a production key every method must come back false.  A key that
# authenticates is not the same as a key that takes money.
#
# A wrong key answers 404 "Invalid API key", never 401.
set -uo pipefail

KEY="${1:-}"
if [ -z "$KEY" ]; then
  echo "usage: $0 '<moosyl-key>'   (the key is never stored in this repo)" >&2
  exit 2
fi

ssh ny "bash -s -- '$KEY'" <<'REMOTE'
set -uo pipefail
KEY="$1"
F=/opt/ny/secrets/moosyl.env
stamp=$(date -u +%Y%m%dT%H%M%SZ)

mkdir -p /opt/ny/secrets

# Length only, never the key itself. Cut with sed and not `awk -F=`: a base64
# key ends in '=' padding, which awk would read as another separator and drop,
# reporting a length one or two short -- a number that looks like a truncated
# key and would send you looking for a quoting bug that is not there.
keylen () { sed -n 's/^MOOSYL_SECRET_KEY=//p' "$1" | head -1 | tr -d '\n' | wc -c; }

if [ -f "$F" ]; then
  cp -p "$F" "$F.before-$stamp"
  echo "backup   $F.before-$stamp  (old key length $(keylen "$F.before-$stamp"))"
else
  echo "backup   none -- $F did not exist"
fi

printf 'MOOSYL_SECRET_KEY=%s\n' "$KEY" > "$F"
chmod 600 "$F"; chown root:root "$F"
echo "written  new key length $(keylen "$F")"

cd /opt/ny/local-stack
echo
echo "== recreating maps-shim (env_file is read at create, not at start)"
docker compose up -d --force-recreate --no-deps maps-shim 2>&1 | tail -3
for i in $(seq 1 30); do curl -sf http://127.0.0.1:8030/healthz >/dev/null && break; sleep 1; done

echo
echo "== the shim holds it"
docker exec ny-maps-shim sh -c 'echo "   MOOSYL_SECRET_KEY length ${#MOOSYL_SECRET_KEY}"'
docker exec ny-maps-shim sh -c 'echo "   MOOSYL_BASE ${MOOSYL_BASE:-unset}"; echo "   PUBLIC_URL ${PUBLIC_URL:-unset}"'

echo
echo "== the key, asked from inside the container that will use it"
# node, not curl or wget: wallet.js already calls global fetch, so this is the
# same client on the same key -- nothing here can pass while the wallet fails.
docker exec ny-maps-shim node -e '
  fetch("https://api.moosyl.com/configuration",
        { headers: { Authorization: process.env.MOOSYL_SECRET_KEY } })
    .then(async r => { console.log(JSON.stringify({ status: r.status, body: await r.text() })); })
    .catch(e => console.log(JSON.stringify({ status: 0, body: String(e) })));
' > /tmp/moosyl-cfg.json 2>/dev/null

python3 - <<'PY'
import json, sys
try:
    outer = json.load(open('/tmp/moosyl-cfg.json'))
except Exception as e:
    print("   could not read the reply:", e); sys.exit(1)

status = outer.get("status")
print(f"   GET /configuration -> http={status}")
if status != 200:
    print("   FAIL  the key did not authenticate (404 = Invalid API key)")
    print("   body:", outer.get("body", "")[:300]); sys.exit(1)

try:
    doc = json.loads(outer["body"])
except Exception:
    print("   FAIL  reply was not JSON:", outer.get("body", "")[:300]); sys.exit(1)

# Walk the document rather than assuming its shape: what matters is every
# isTestingMode anywhere in it, and their docs have been wrong before.
flags = []
def walk(node, name=None):
    if isinstance(node, dict):
        if "isTestingMode" in node:
            flags.append((node.get("name") or node.get("method") or name or "?",
                          bool(node["isTestingMode"])))
        for k, v in node.items():
            walk(v, k)
    elif isinstance(node, list):
        for v in node:
            walk(v, name)
walk(doc)

if not flags:
    print("   WARN  no isTestingMode anywhere in the reply -- mode unproven")
    print("   body:", json.dumps(doc)[:400]); sys.exit(1)

for name, testing in flags:
    print(f"   {name:12} {'TESTING' if testing else 'LIVE'}")

testing = [n for n, t in flags if t]
if testing:
    print(f"   FAIL  still a TEST key -- {len(testing)} of {len(flags)} methods in testing mode")
    sys.exit(1)
print(f"   OK    production key: all {len(flags)} methods LIVE -- this moves real money")
PY
cfg_rc=$?
rm -f /tmp/moosyl-cfg.json

echo
echo "== the wallet reports itself configured"
# No driver token here, so this is the honest unauthenticated answer: 401 proves
# the route is up and reached the auth check.  `configured` per driver is what
# probe-wallet-screens.py asserts with a real token.
echo "   /wallet/status -> $(curl -s -o /dev/null -w '%{http_code}' http://127.0.0.1:8030/wallet/status)  (401 expected)"
echo "   /healthz       -> $(curl -s -o /dev/null -w '%{http_code}' http://127.0.0.1:8030/healthz)"
echo "   public api     -> $(curl -s -o /dev/null -w '%{http_code}' https://api.movinapp.net/healthz)"

echo
if [ $cfg_rc -eq 0 ]; then
  echo "DONE -- live key installed. Run ./probe-wallet-screens.py on the VPS next,"
  echo "and remember the first real top-up is real money."
else
  echo "STOPPED -- the key is written but did NOT prove itself live. Read the lines"
  echo "above. To go back:  cp $F.before-$stamp $F && cd /opt/ny/local-stack &&"
  echo "docker compose up -d --force-recreate --no-deps maps-shim"
  exit 1
fi
REMOTE
