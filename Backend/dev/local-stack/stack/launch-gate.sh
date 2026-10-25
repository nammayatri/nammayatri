#!/usr/bin/env bash
# The go / no-go before the first real passenger. READ-ONLY: it changes nothing,
# prints no secret and no phone number. Run on the server, from this folder:
#
#   sudo ./launch-gate.sh
#
# Exit 0 only when no check FAILED. WARN means "look at it, then decide": a
# launch can go ahead with a warning that a person has read, never with a FAIL.
# The same checks, kept short, are what to run again the morning after launch.
#
# It checks what has gone wrong before, by name: the payment gateway still on
# its TEST address (CHARGILY_BASE decides test or live, and nothing else does),
# a sign-in bypass still open, the simulated fleet still answering ride requests
# and still present as accounts, a wallet balance that disagrees with its ledger,
# a certificate or a backup that quietly stopped. The Telegram bot watches most
# of these continuously; this is the one moment they are all asked together.
set -uo pipefail
HERE="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
cd "$HERE"

PG="${PG:-ny-postgres}"
DB="${DB:-atlas_dev}"
SECRETS="${SECRETS_DIR:-/opt/ny/secrets}"
CERT_DIR="${CERT_DIR:-$HERE/edge-certs/live}"
BACKUP_DIR="${BACKUP_DIR:-/var/backups/movin}"
MAX_BACKUP_H="${MAX_BACKUP_H:-26}"
MIN_CERT_DAYS="${MIN_CERT_DAYS:-20}"
MAX_DISK_PCT="${MAX_DISK_PCT:-80}"

FAILS=0 WARNS=0
pass() { printf '   \033[1;32mPASS\033[0m %s\n' "$*"; }
fail() { printf '   \033[1;31mFAIL\033[0m %s\n' "$*"; FAILS=$((FAILS + 1)); }
warn() { printf '   \033[1;33mWARN\033[0m %s\n' "$*"; WARNS=$((WARNS + 1)); }
say() { printf '\n\033[1m== %s\033[0m\n' "$*"; }
pg() { docker exec "$PG" psql -U postgres -d "$DB" -v ON_ERROR_STOP=1 -X -q -At "$@" 2>/dev/null; }
envof() { docker inspect "$1" -f '{{range .Config.Env}}{{println .}}{{end}}' 2>/dev/null; }

[ "$(id -u)" = 0 ] || { echo "run it with sudo" >&2; exit 2; }

say "the stack"
bad="$(docker ps -a --format '{{.Names}} {{.Status}}' | grep -E ' (Exited|Restarting|Dead)' || true)"
if [ -z "$bad" ]; then pass "$(docker ps -q | wc -l) containers running, none stopped or restarting"
else fail "containers not running: $(tr '\n' ';' <<<"$bad")"; fi
for u in "auth guard|http://127.0.0.1:8031/healthz" "maps shim|http://127.0.0.1:8030/healthz"; do
  code="$(curl -s -o /dev/null -w '%{http_code}' --max-time 8 "${u#*|}")"
  [ "$code" = 200 ] && pass "${u%%|*} healthz 200" || fail "${u%%|*} healthz answered $code"
done
code="$(curl -s -o /dev/null -w '%{http_code}' --max-time 10 https://api.movinapp.net/healthz)"
case "$code" in 000|5??) fail "the public API answered $code";; *) pass "the public API answers ($code)";; esac

say "nothing is still in test mode"
base="$(envof ny-maps-shim | sed -n 's/^CHARGILY_BASE=//p')"
key="$(envof ny-maps-shim | sed -n 's/^CHARGILY_SECRET_KEY=//p')"
if [ -z "$base" ]; then warn "no CHARGILY_BASE in the shim: Algerian top-ups would use the default (the TEST address)"
elif grep -q '/test/' <<<"$base"; then fail "Chargily is on its TEST address ($base): drivers in Algeria would pay with nothing"
else pass "Chargily is on its live address"; fi
case "$key" in
  live_*) pass "the Chargily key is a live key (not shown)";;
  test_*) fail "the Chargily key is a TEST key";;
  '')     fail "no Chargily key is set";;
  *)      warn "the Chargily key has an unexpected prefix";;
esac
if [ -s "$SECRETS/moosyl.env" ]; then pass "the Moosyl key file exists and is not empty (not shown)"
else fail "$SECRETS/moosyl.env is missing or empty: top-ups in Mauritania cannot be created"; fi
byp="$(envof ny-auth-guard | sed -n 's/^SMS_BYPASS=//p')"
[ -z "$byp" ] && pass "no number skips the SMS gateway in the compose file" || fail "SMS_BYPASS is not empty in the guard: someone signs in without a code"
if [ -f "$SECRETS/test-accounts.env" ] && grep -Eq '^SMS_BYPASS=.+' "$SECRETS/test-accounts.env"; then
  fail "$SECRETS/test-accounts.env still lists bypass numbers"
else pass "no bypass list on disk (a reviewer's number is added only for the review, then removed)"; fi
mode="$(envof ny-auth-guard | sed -n 's/^SMS_MODE=//p')"
[ "$mode" = verify ] && pass "SMS_MODE is verify (the live gateway)" || warn "SMS_MODE is '$mode', not verify"
if [ -s "$HERE/auth-guard/driver-codes.json" ] && [ "$(tr -d ' \n\t' < "$HERE/auth-guard/driver-codes.json")" != '{}' ]; then
  warn "auth-guard/driver-codes.json has personal drivers' codes: expected empty on the live server"
else pass "no personal driver codes"; fi

say "the simulated fleet and the test accounts are gone"
if systemctl is-active --quiet movin-fleet 2>/dev/null; then fail "movin-fleet is running: simulated cars answer real ride requests"
else pass "movin-fleet is not running"; fi
left="$(./launch-cleanup.sh 2>&1 | sed 's/\x1b\[[0-9;]*m//g')"
if grep -q 'nothing to do' <<<"$left"; then pass "none of the 12 cars or 2 test passengers can be found by their numbers"
else fail "launch-cleanup.sh still finds test accounts: run it (see docs/runbooks/launch.md, day 1)"; fi
online="$(pg -c "select count(*) from atlas_driver_offer_bpp.driver_location where updated_at > now() - interval '10 minutes'")"
info_online="${online:-?} driver position(s) fresher than 10 minutes"
if [ "${online:-0}" = 0 ]; then warn "$info_online: nobody online, so a search returns no car (true on a quiet morning, not on launch day)"
else pass "$info_online"; fi

say "the money is consistent"
drift="$(pg -c "select count(*) from movin.wallet_check where drift <> 0")"
[ "$drift" = 0 ] && pass "every wallet balance equals its ledger" || fail "${drift:-?} wallet(s) disagree with their ledger (movin.wallet_check)"
neg="$(pg -c "select count(*) from movin.wallet where balance < 0")"
[ "${neg:-0}" = 0 ] && pass "no negative wallet" || warn "$neg wallet(s) negative (a driver who finished a course on credit; the bot says who)"

say "what protects the data"
newest="$(ls -t "$BACKUP_DIR"/movin-*.tar.gz.gpg 2>/dev/null | head -1)"
if [ -z "$newest" ]; then fail "no backup in $BACKUP_DIR"
else
  age_h=$(( ( $(date +%s) - $(stat -c %Y "$newest") ) / 3600 ))
  [ "$age_h" -le "$MAX_BACKUP_H" ] && pass "newest backup is ${age_h} h old" || fail "newest backup is ${age_h} h old (limit $MAX_BACKUP_H)"
fi
res="$(systemctl show movin-backup.service -p Result --value 2>/dev/null)"
[ "$res" = success ] && pass "the last backup run succeeded" || warn "the backup service's last result is '$res'"
enc="$(pg -c "select mobile_number_encrypted from atlas_app.person where mobile_number_encrypted like '0.1.0|%' limit 1")"
if [ -n "$enc" ]; then
  out="$(curl -s --max-time 10 -X POST -H 'Content-Type: application/json' -d "{\"value\":\"$enc\"}" http://127.0.0.1:8021/decrypt)"
  grep -q '"value":"S\\"' <<<"$out" && pass "a stored phone number decrypts through passetto (not shown)" || fail "a stored phone number does not decrypt: the keys do not match the data"
else warn "no encrypted number to try"; fi
if [ -d "$CERT_DIR" ]; then
  for pem in "$CERT_DIR"/*/fullchain.pem; do
    [ -f "$pem" ] || continue
    end="$(openssl x509 -enddate -noout -in "$pem" | cut -d= -f2)"
    days=$(( ( $(date -d "$end" +%s) - $(date +%s) ) / 86400 ))
    name="$(basename "$(dirname "$pem")")"
    [ "$days" -ge "$MIN_CERT_DAYS" ] && pass "TLS $name: $days days left" || fail "TLS $name expires in $days days"
  done
else warn "no certificate folder at $CERT_DIR"; fi
used="$(df / | awk 'NR==2{gsub("%","",$5); print $5}')"
[ "$used" -lt "$MAX_DISK_PCT" ] && pass "disk ${used}% used" || fail "disk ${used}% used (limit $MAX_DISK_PCT)"

say "waiting for a person (information, not pass or fail)"
printf '       open deletion requests:   %s\n' "$(pg -c "select count(*) from movin.deletion_request where status = 'open'")"
printf '       drivers awaiting a decision: see the console, Validation\n'
printf '       open ride reports:         %s\n' "$(pg -c "select count(*) from movin.ride_report where status = 'open'" || echo '?')"

say "result"
if [ "$FAILS" = 0 ]; then
  printf '   \033[1;32mNO FAIL\033[0m  (%s warning(s))\n' "$WARNS"; exit 0
else
  printf '   \033[1;31m%s FAIL\033[0m  (%s warning(s)): do not launch until each is dealt with\n' "$FAILS" "$WARNS"; exit 1
fi
