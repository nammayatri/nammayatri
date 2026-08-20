#!/usr/bin/env bash
#
# Keep the seeded drivers visible to the dispatch pool.
#
# ── The problem this exists for ─────────────────────────────────────────────
# The pool only considers drivers whose recorded position is recent. Real
# drivers send one constantly; ours are rows in a table that nobody updates.
# So a stack that worked yesterday returns **zero estimates today, with no
# error anywhere** — empty arrays, HTTP 200, nothing in any log. Measured on
# 12 Aug: six drivers sitting within 600 m of the pickup, every one of them
# invisible, positions 1 day 21 hours old.
#
# That has now cost time twice (9 Aug, 12 Aug) and it looks exactly like broken
# dispatch, which is the worst thing about it.
#
# ── What this is, and what it is not ────────────────────────────────────────
# A timer that re-stamps the seeded positions every two minutes so a demo stack
# stays demonstrable. It is **not** a fix: the real fix is a driver app sending
# real positions, at which point this should be deleted rather than left to
# quietly keep fictional cars alive next to real ones.
#
# It only ever touches rows that `setup.sh drivers` already created.
#
#   ./drivers-keepalive.sh install     # timer, every 2 min
#   ./drivers-keepalive.sh status      # is it running, how fresh are they
#   ./drivers-keepalive.sh uninstall   # stop and remove
#
set -uo pipefail

HERE="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
UNIT=movin-drivers
INTERVAL=2min

say() { printf '\n\033[1m== %s\033[0m\n' "$*"; }
ok()  { printf '   \033[1;32mok  \033[0m%s\n' "$*"; }
bad() { printf '   \033[1;31mBAD \033[0m%s\n' "$*"; }

fresh_count() {
  docker exec ny-postgres psql -U postgres -d atlas_dev -At -c \
    "SELECT count(*) FILTER (WHERE updated_at > now() - interval '5 minutes')
          || ' of ' || count(*)
       FROM atlas_driver_offer_bpp.driver_location;" 2>/dev/null
}

case "${1:-status}" in
  install)
    say "installing $UNIT.timer (every $INTERVAL)"

    cat > "/etc/systemd/system/$UNIT.service" <<EOF
[Unit]
Description=Movin DZ — keep the seeded driver positions fresh
Documentation=file://$HERE/drivers-keepalive.sh

[Service]
Type=oneshot
WorkingDirectory=$HERE
# Only re-stamps rows setup.sh already created. Output to the journal so a
# silent failure here does not become a silent failure in the app.
ExecStart=$HERE/drivers-keepalive.sh refresh
EOF

    cat > "/etc/systemd/system/$UNIT.timer" <<EOF
[Unit]
Description=Movin DZ — driver freshness, every $INTERVAL

[Timer]
OnBootSec=1min
OnUnitActiveSec=$INTERVAL
AccuracySec=15s

[Install]
WantedBy=timers.target
EOF

    systemctl daemon-reload
    systemctl enable --now "$UNIT.timer" >/dev/null 2>&1
    ok "installed"
    systemctl start "$UNIT.service"
    sleep 2
    ok "fresh now: $(fresh_count)"
    ;;

  refresh)
    # Deliberately not `setup.sh drivers`: that re-places the drivers and prints
    # a banner every two minutes. This only moves the clock forward on rows that
    # already exist, so a driver moved by hand for a test stays where it was put.
    docker exec ny-postgres psql -U postgres -d atlas_dev -q -c \
      "UPDATE atlas_driver_offer_bpp.driver_location
          SET updated_at = now(), coordinates_calculated_at = now()
        WHERE updated_at < now() - interval '1 minute';" >/dev/null
    ;;

  status)
    say "timer"
    systemctl is-active "$UNIT.timer" >/dev/null 2>&1 \
      && ok "$UNIT.timer active" || bad "$UNIT.timer not running"
    systemctl list-timers "$UNIT.timer" --no-pager 2>/dev/null | sed -n '2p' | sed 's/^/   /'
    say "driver positions fresh in the last 5 minutes"
    echo "   $(fresh_count)"
    ;;

  uninstall)
    say "removing $UNIT"
    systemctl disable --now "$UNIT.timer" >/dev/null 2>&1
    rm -f "/etc/systemd/system/$UNIT.timer" "/etc/systemd/system/$UNIT.service"
    systemctl daemon-reload
    ok "removed — positions will go stale again, which is the honest default"
    ;;

  *)
    sed -n '2,30p' "$0" | sed 's/^# \{0,1\}//'
    ;;
esac
