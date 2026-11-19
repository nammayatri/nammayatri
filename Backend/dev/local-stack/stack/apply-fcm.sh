#!/usr/bin/env bash
#
# Point this backend's push notifications at a real Firebase project.
#
#     ./apply-fcm.sh /path/to/service-account.json
#
# ── Why this is only config ─────────────────────────────────────────────────
# Push was never missing. `Kernel.External.FCM.Flow` is compiled into the
# running binaries, nine message types exist, and the rider app has been
# collecting device tokens since the day it shipped. The only thing wrong was
# the key: upstream's placeholder ships with `project_id: jp-beckn-dev` and a
# private key that is literally `xxxxxxx`, which produces
#
#     [FCM] |> error while sending message to person with id … : "Bad RSA key!"
#
# on every single send. So this changes three columns and restarts two
# containers. No rebuild, no new service, nothing added to the compose file --
# the same trick that gave us maps and routing for nothing.
#
# ── BOTH sides, deliberately ────────────────────────────────────────────────
# The rider config lives on `atlas_app.merchant`; the driver config lives on
# `atlas_driver_offer_bpp.transporter_config`, under slightly different column
# names, and carries the *same* dead placeholder. One Firebase service account
# is scoped to the whole project rather than to one app, so the single key given
# here serves the passenger app today and the driver app the day it exists.
# Doing only half now would mean rediscovering the other half later.
#
# ── The secret ──────────────────────────────────────────────────────────────
# The JSON is read from the path you pass and is NEVER written into this
# repository. It lands base64-encoded in Postgres, which `backup.sh` already
# covers. Anyone holding it can send push as Movin DZ, so if it leaks the fix is
# Firebase console -> Service accounts -> generate a new key, then re-run this.
#
# ── Traps this is written around ────────────────────────────────────────────
#   * `docker exec` WITHOUT -i does not forward stdin at all. A heredoc piped to
#     psql that way runs with no input, does nothing, and exits 0 -- a
#     verification step written that way reported success having checked
#     nothing. SQL goes in as a FILE, via docker cp, every time.
#   * `base64` wraps at 76 columns by default. A wrapped value stored in a text
#     column decodes to broken JSON; -w 0 is not optional.
#   * A 200 from anything here proves nothing. Every write is read back.
set -euo pipefail

SA="${1:-}"
PG="${PG:-ny-postgres}"
DB="${DB:-atlas_dev}"

if [ -z "$SA" ] || [ ! -f "$SA" ]; then
  echo "usage: ./apply-fcm.sh /path/to/service-account.json" >&2
  echo >&2
  echo "Firebase console -> Project settings -> Service accounts" >&2
  echo "  -> Generate new private key" >&2
  exit 1
fi

# Fail here rather than halfway through, and name the project being installed so
# a wrong file is caught by eye before it reaches the database.
PROJECT=$(python3 - "$SA" <<'PY'
import json, sys
d = json.load(open(sys.argv[1]))
missing = [k for k in ("type", "project_id", "private_key", "client_email") if not d.get(k)]
if missing:
    sys.exit("not a service-account key -- missing: " + ", ".join(missing))
if d["type"] != "service_account":
    sys.exit("type is %r, expected service_account" % d["type"])
if "PRIVATE KEY" not in d["private_key"] or "xxxx" in d["private_key"]:
    sys.exit("private_key is a placeholder, not a real key")
print(d["project_id"])
PY
)

# The endpoint the binary posts to. It builds no path of its own -- there is no
# `projects` or `messages:send` string anywhere in either executable -- so the
# whole endpoint, project id included, has to be in this column.
URL="${FCM_URL:-https://fcm.googleapis.com/v1/projects/${PROJECT}/messages:send}"

echo "project : $PROJECT"
echo "endpoint: $URL"
echo

B64=$(base64 -w 0 "$SA")

SQL=$(mktemp /tmp/fcm-XXXXXX.sql)
trap 'rm -f "$SQL"' EXIT
cat > "$SQL" <<SQLEOF
\\set ON_ERROR_STOP on
begin;

update atlas_app.merchant
   set fcm_service_account = '${B64}',
       fcm_url             = '${URL}';

update atlas_driver_offer_bpp.transporter_config
   set fcm_service_account = '${B64}',
       fcm_url             = '${URL}';

commit;

-- Read back through a decode, so a wrapped or truncated value cannot pass.
select 'rider ' as side,
       fcm_url,
       (convert_from(decode(fcm_service_account,'base64'),'UTF8')::json->>'project_id') as project,
       (convert_from(decode(fcm_service_account,'base64'),'UTF8')::json->>'client_email') as account
  from atlas_app.merchant
union all
select 'driver',
       fcm_url,
       (convert_from(decode(fcm_service_account,'base64'),'UTF8')::json->>'project_id'),
       (convert_from(decode(fcm_service_account,'base64'),'UTF8')::json->>'client_email')
  from atlas_driver_offer_bpp.transporter_config;
SQLEOF

docker cp "$SQL" "$PG:/tmp/fcm.sql" >/dev/null
docker exec "$PG" psql -U postgres -d "$DB" -f /tmp/fcm.sql
docker exec "$PG" rm -f /tmp/fcm.sql

echo
echo "restarting the two services that read this at boot..."
docker restart ny-rider ny-driver >/dev/null
echo "done."
echo
echo "Next: send one notification and read the log."
echo "  docker logs ny-rider --since 5m 2>&1 | grep -i fcm"
echo
echo "  'Bad RSA key!'            -> the key did not take"
echo "  404                       -> \$URL is wrong; override with FCM_URL="
echo "  INVALID_ARGUMENT / 404 on"
echo "  the TOKEN, not the URL    -> config is CORRECT, the device token is fake"
