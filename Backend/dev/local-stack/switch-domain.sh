#!/usr/bin/env bash
# Move the API from the sslip.io hostname onto a real domain.
#
#     ./switch-domain.sh api.movinapp.net       # on the VPS
#     ./switch-domain.sh --check api.movinapp.net
#
# ── What it does, and what it deliberately does not ─────────────────────────
# It gets a certificate for the new name, teaches nginx to answer on it, and
# checks that it works. It does NOT change `PUBLIC_URL` or the app's
# `API_BASE_URL`, because those two are the point of no return and are better
# done deliberately -- see the end of this script, which prints them.
#
# ── The old hostname keeps working, and that is not politeness ──────────────
# Chargily writes the callback address into each checkout **at the moment it is
# created**. A payment started before the switch still calls back to
# api.169-58-139-65.sslip.io. If that name stops answering, the driver pays and
# the webhook lands nowhere: money in, no month out, and nothing on any screen
# to say so. So the certificate is *expanded* to cover both names rather than
# replaced, and both stay in `server_name`. Retire the old one only after a
# fortnight with no checkouts referencing it -- movin.subscription_payment.event
# carries the URL each one was created with.
#
# ── Why it refuses to run against Cloudflare's proxy ────────────────────────
# Measured 2026-08-26: movinapp.net resolved to 104.21.75.212 / 172.67.182.57
# with `server: cloudflare` and answered 404 -- the record was created with the
# proxy on, so the name reaches Cloudflare and Cloudflare has no origin for it.
# HTTP-01 cannot complete through that, and it should not be worked around:
#
#   * Chargily's webhook would arrive through Cloudflare's bot filtering. That
#     filtering has already blocked one legitimate automated client of ours --
#     HTTP 403, error 1010, on Chargily's own Cloudflare-fronted domain. A
#     webhook silently dropped is a payment taken and never applied.
#   * The driver and rider apps would gain a dependency between them and us
#     that nobody here can debug.
#
# DNS only -- the grey cloud -- and the certificate is ours.
set -uo pipefail

HERE="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
OLD="api.169-58-139-65.sslip.io"
IP="169.58.139.65"
CONF="$HERE/edge/nginx.conf"
CHECK_ONLY=0

say() { printf '\n\033[1m== %s\033[0m\n' "$*"; }
ok()  { printf '   \033[1;32mok  \033[0m%s\n' "$*"; }
bad() { printf '   \033[1;31mBAD \033[0m%s\n' "$*"; }
die() { bad "$*"; exit 1; }

[ "${1:-}" = "--check" ] && { CHECK_ONLY=1; shift; }
NEW="${1:-api.movinapp.net}"

# ── 1. does the name actually reach this machine ────────────────────────────
say "DNS for $NEW"
# Two resolvers, because one of them lies. `getent ahostsv4` answered for
# movinapp.net and then, minutes later on the same box, reported it unresolvable
# while still answering for api.movinapp.net -- a resolver hiccup, not a DNS
# change. Refusing to switch on a flake is safe; *reporting* a flake as "the
# boss has not made the record" is how a false alarm gets sent to a client.
resolve() {
  python3 - "$1" <<'PYEOF' 2>/dev/null
import socket, sys
try:
    print(" ".join(sorted({a[4][0] for a in socket.getaddrinfo(sys.argv[1], None, socket.AF_INET)})))
except Exception:
    pass
PYEOF
}
GOT="$(resolve "$NEW")"
[ -z "$GOT" ] && GOT="$(getent ahostsv4 "$NEW" 2>/dev/null | awk '{print $1}' | sort -u | tr '\n' ' ')"
if [ -z "$GOT" ]; then
  die "$NEW does not resolve. Add an A record pointing at $IP."
fi
printf '   resolves to: %s\n' "$GOT"
if ! echo "$GOT" | grep -q "$IP"; then
  bad "$NEW does not point at $IP."
  case "$GOT" in
    104.21.*|172.67.*|104.16.*|172.64.*)
      echo
      echo "   Those are Cloudflare addresses. The record exists but the proxy"
      echo "   is on (orange cloud), so the name reaches Cloudflare and not us."
      echo "   Switch that record to DNS only -- grey cloud -- and re-run."
      echo "   See the header for why proxying this endpoint is a bad idea." ;;
  esac
  exit 1
fi
ok "points here"

# ── 2. can Let's Encrypt reach the challenge path ───────────────────────────
say "the ACME challenge path"
TOKEN="switch-domain-$(date +%s)"
echo "$TOKEN" > /opt/ny/local-stack/edge-webroot/.well-known/acme-challenge/"$TOKEN" 2>/dev/null \
  || { mkdir -p /opt/ny/local-stack/edge-webroot/.well-known/acme-challenge
       echo "$TOKEN" > /opt/ny/local-stack/edge-webroot/.well-known/acme-challenge/"$TOKEN"; }
FETCHED="$(curl -s --max-time 15 "http://$NEW/.well-known/acme-challenge/$TOKEN")"
rm -f /opt/ny/local-stack/edge-webroot/.well-known/acme-challenge/"$TOKEN"
[ "$FETCHED" = "$TOKEN" ] || die "http://$NEW/.well-known/... did not serve what we wrote (got '${FETCHED:0:60}'). Certbot would fail the same way."
ok "reachable over plain HTTP"

if [ "$CHECK_ONLY" = 1 ]; then
  say "check only"
  ok "everything needed for the switch is in place; re-run without --check"
  exit 0
fi

# ── 3. one certificate covering both names ─────────────────────────────────
#
# --expand keeps the certificate lineage, so the renewal timer that already
# works keeps working. A separate certificate would mean a second server block
# and two things to remember at renewal time.
say "certificate for $OLD + $NEW"
if docker exec ny-certbot certbot certificates 2>/dev/null | grep -q "$NEW"; then
  ok "already covers $NEW"
else
  docker exec ny-certbot certbot certonly \
    --webroot -w /var/www/certbot \
    --cert-name "$OLD" -d "$OLD" -d "$NEW" \
    --expand --non-interactive --agree-tos --keep-until-expiring \
    || die "certbot refused. Nothing has changed; read its output above."
  ok "issued"
fi

# ── 4. nginx answers on the new name ───────────────────────────────────────
say "nginx"
if grep -q "server_name $OLD $NEW;" "$CONF"; then
  ok "already listed"
else
  cp "$CONF" "$CONF.before-$NEW"
  # `cp` and not `mv`: nginx.conf is a single-file bind mount, and anything
  # that unlinks the inode leaves the container serving the old file while
  # `nginx -t` cheerfully passes.
  sed -i "s/^    server_name $OLD;$/    server_name $OLD $NEW;/" "$CONF"
  grep -q "server_name $OLD $NEW;" "$CONF" || die "could not edit server_name; $CONF is unchanged"
  ok "server_name extended (previous copy: $CONF.before-$NEW)"
fi

docker exec ny-edge nginx -t || die "nginx rejected the config; fix it before reloading"
docker exec ny-edge nginx -s reload
ok "reloaded"

# ── 5. prove it ────────────────────────────────────────────────────────────
say "does it work"
# `--resolve` pins the name to loopback rather than going out to the public IP
# and back. This box cannot reach its own public address -- the first version
# of this check went out to the internet and got 000 for *both* names,
# including the one that had been serving perfectly for weeks, which reads as
# "the switch just took the API down". It had not: verified from a laptop, both
# answered 200 throughout. A verification step that cries wolf on a live
# migration is worse than none, because the instinct it triggers is to roll back
# something that is working.
for host in "$NEW" "$OLD"; do
  code="$(curl -s -o /dev/null -w '%{http_code}' --max-time 20 \
          --resolve "$host:443:127.0.0.1" "https://$host/healthz")"
  if [ "$code" = "200" ]; then ok "https://$host/healthz -> 200"; else bad "https://$host/healthz -> $code"; fi
done
echo "   (checked against 127.0.0.1 with the right SNI; confirm from outside too)"

cat <<NEXT

   The two things this script does NOT do, on purpose:

   1. PUBLIC_URL in .env, which is what Chargily writes into new checkouts.
        sed -i 's|^PUBLIC_URL=.*|PUBLIC_URL=https://$NEW|' /opt/ny/local-stack/.env
        cd /opt/ny/local-stack && docker compose up -d --no-build maps-shim
      Old checkouts keep calling the old name, which is why it still answers.

   2. API_BASE_URL in the app (src/config.ts), which needs a new build and
      every phone updated. Do it while the only phones are ours.

NEXT
