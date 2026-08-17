#!/usr/bin/env bash
#
# Keep the simulated fleet answering, permanently.
#
# ── The failure this exists to stop ─────────────────────────────────────────
# `drivers-keepalive.sh` keeps the seeded driver *positions* fresh, and its
# timer has been running for days. That is enough for a search to succeed:
# dispatch finds drivers near the rider, estimates come back, and screen 10 even
# draws their cars on the map.
#
# It is **not** enough for anyone to get a ride. Positions are rows in a table;
# an offer requires a process that polls the driver API and answers. Without
# `simulate-driver.py daemon` running, a rider sees cars on the map, waits the
# full five minutes, and is told nobody responded — which reads exactly like
# broken dispatch and is not.
#
# That is what happened today. The simulator had only ever been started by hand,
# usually wrapped in `timeout`, so it always died a few hours later and left the
# stack in that half-working state.
#
#   ./fleet-service.sh install     # run the fleet, and keep it running
#   ./fleet-service.sh status      # is it up, and what has it done lately
#   ./fleet-service.sh uninstall   # stop and remove
#
# ── A demo prop, and it must stay obviously one ─────────────────────────────
# Exactly like the keepalive timer: this makes six invented drivers behave like
# a fleet so the passenger app can be finished and shown on one phone. **Delete
# it the day a real driver app exists.** Leaving it running alongside real
# drivers would have imaginary cars bidding against them.
#
set -uo pipefail

UNIT=movin-fleet
HERE="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

# Real time divided by this. 3 keeps a demo ride short enough to sit through
# while still looking like driving rather than teleporting.
SPEED="${SPEED:-3}"

say() { printf '\n\033[1m== %s\033[0m\n' "$*"; }
ok()  { printf '   \033[1;32mok  \033[0m%s\n' "$*"; }
bad() { printf '   \033[1;31mBAD \033[0m%s\n' "$*"; }

case "${1:-status}" in
  install)
    [ "$(id -u)" -eq 0 ] || { bad "run as root"; exit 1; }
    say "installing $UNIT.service (speed ${SPEED}x)"

    cat > "/etc/systemd/system/$UNIT.service" <<EOF
[Unit]
Description=Movin DZ — the simulated fleet, answering ride requests
Documentation=file://$HERE/simulate-driver.py
# The driver API is inside the stack, so there is nothing to answer before it.
After=docker.service network-online.target
Wants=docker.service

[Service]
Type=simple
WorkingDirectory=$HERE
ExecStart=$HERE/simulate-driver.py daemon --speed $SPEED

# The backend may not be accepting requests yet at boot, and a demo stack gets
# restarted often. Always come back rather than leaving the fleet silently
# absent -- absent looks identical to broken dispatch from the app.
Restart=always
RestartSec=10

# SIGTERM is handled by the script itself: it turns it into the interrupt its
# cleanup already knows, so a stop takes the drivers offline instead of leaving
# them online with positions that then go stale.
KillSignal=SIGTERM
TimeoutStopSec=30

StandardOutput=journal
StandardError=journal

[Install]
WantedBy=multi-user.target
EOF

    systemctl daemon-reload
    systemctl enable --now "$UNIT.service" >/dev/null 2>&1
    sleep 4
    if systemctl is-active --quiet "$UNIT.service"; then
      ok "running, and will restart on boot"
    else
      bad "it did not stay up — journalctl -u $UNIT -n 40"
      exit 1
    fi
    ;;

  status)
    say "$UNIT"
    systemctl is-active "$UNIT.service" >/dev/null 2>&1 \
      && ok "active since $(systemctl show -p ActiveEnterTimestamp --value "$UNIT.service")" \
      || bad "not running — a rider will see cars on the map and get no offers"
    echo
    journalctl -u "$UNIT.service" -n 12 --no-pager 2>/dev/null | tail -12
    ;;

  uninstall)
    [ "$(id -u)" -eq 0 ] || { bad "run as root"; exit 1; }
    say "removing $UNIT"
    systemctl disable --now "$UNIT.service" >/dev/null 2>&1
    rm -f "/etc/systemd/system/$UNIT.service"
    systemctl daemon-reload
    ok "gone; the drivers were taken offline on the way out"
    ;;

  *)
    bad "usage: $0 install|status|uninstall"
    exit 1
    ;;
esac
