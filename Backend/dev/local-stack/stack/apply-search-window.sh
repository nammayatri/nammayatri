#!/usr/bin/env bash
# How long a driver has to answer a ride request.
#
#     ./apply-search-window.sh          # set it to 60 seconds
#     ./apply-search-window.sh 30       # or to something else
#     ./apply-search-window.sh --show   # just read what it is now
#
# ── What this actually changes ──────────────────────────────────────────────
# One Dhall value, `singleBatchProcessTime`, in the config the driver app has
# bind-mounted. No rebuild, no image, no migration -- the same trick as the FCM
# key and `useFakeSms`: the thing that looks like it must be compiled in is a
# line in a file.
#
#     searchRequestValidTill = singleBatchProcessTime `addUTCTime` now
#       -- SendSearchRequestToDrivers/Handle/Internal.hs:101
#
# That single value is what the driver's countdown reads, and it shipped at
# **10 seconds**. Measured across all 164 requests in the database on 19 August:
# exactly 10, every time, no exception. The client drove the app on 20 August
# and said ten seconds is not enough to answer while driving, which is the
# correct verdict -- it is the time for two glances, not a decision.
#
# ── It is not only the driver's countdown, and that is the real cost ────────
# The same value paces the *batches*. `driverPoolBatchesCfg` sends the request
# to `driverBatchSize` drivers at a time for `maxNumberOfBatches` rounds, and
# waits one `singleBatchProcessTime` between rounds:
#
#     driverBatchSize     5      maxNumberOfBatches   3
#
#   at 10 s   batch 1 at 0 s, batch 2 at 10 s, batch 3 at 20 s -- all done in 30 s
#   at 60 s   batch 1 at 0 s, batch 2 at 60 s, batch 3 at 120 s -- done in 180 s
#
# So raising it buys the driver time and spends the *rider's*: if the first five
# drivers ignore the request, nobody else is even asked for a full minute. The
# rider's own search lives 300 s, so 3 x 60 still fits inside it with room --
# but 60 is the largest value that comfortably does. Past ~90 the third batch
# lands after the rider has given up.
#
# If the rider's wait turns out to be the bigger complaint, 30 is the middle
# setting: 30 s to answer, all three batches done inside 90 s.
#
# ── Why this is a script and not an edit ────────────────────────────────────
# `2023/` is fetched by `setup.sh` and is **gitignored**, so an edit made by
# hand on the server is undone the next time that tree is refreshed, silently,
# and the window goes back to 10 seconds with nothing to show why. Same reason
# `apply-tariff.sh` and `apply-fcm.sh` exist. Re-run this after any `setup.sh`
# that refetches the pinned tree.
set -euo pipefail

STACK_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
CONFIG="$STACK_DIR/2023/Backend/dhall-configs/dev/dynamic-offer-driver-app.dhall"
CONTAINER="${CONTAINER:-ny-driver}"

if [ ! -f "$CONFIG" ]; then
  echo "Not found: $CONFIG" >&2
  echo "The pinned tree is fetched by ./setup.sh -- run that first." >&2
  exit 1
fi

current() {
  grep -oE 'singleBatchProcessTime = \+[0-9]+' "$CONFIG" | grep -oE '[0-9]+' || true
}

NOW="$(current)"
if [ -z "$NOW" ]; then
  echo "singleBatchProcessTime is not in $CONFIG in the expected form." >&2
  echo "Expected a line containing: singleBatchProcessTime = +<seconds>" >&2
  exit 1
fi

if [ "${1:-}" = "--show" ]; then
  echo "singleBatchProcessTime = ${NOW}s"
  grep -nE 'driverBatchSize|maxNumberOfBatches|singleBatchProcessTime' "$CONFIG"
  exit 0
fi

WANT="${1:-60}"
case "$WANT" in
  ''|*[!0-9]*) echo "Seconds must be a whole number, got: $WANT" >&2; exit 1 ;;
esac

# 300 s is the rider's own search window. A batch schedule that runs past it is
# a driver being asked about a ride nobody is waiting for any more.
if [ "$WANT" -gt 100 ]; then
  echo "Refusing ${WANT}s: three batches would take $((WANT * 3))s, past the" >&2
  echo "rider's 300s search. Pick 100 or less." >&2
  exit 1
fi

if [ "$NOW" = "$WANT" ]; then
  echo "Already ${WANT}s. Nothing to do."
  exit 0
fi

# Edited in place. The bind mount here is on the *directory*, so replacing a
# file inside it is safe -- unlike the single-file nginx.conf mount, where
# unlinking the inode leaves the container serving the old content while
# `nginx -t` passes on the new.
cp -a "$CONFIG" "$CONFIG.bak"
sed -i -E "s/singleBatchProcessTime = \+[0-9]+/singleBatchProcessTime = +${WANT}/" "$CONFIG"

AFTER="$(current)"
if [ "$AFTER" != "$WANT" ]; then
  echo "Edit did not take -- restoring." >&2
  mv "$CONFIG.bak" "$CONFIG"
  exit 1
fi

echo "singleBatchProcessTime: ${NOW}s -> ${AFTER}s"
echo "three batches now take $((AFTER * 3))s of the rider's 300s search"

# The config is read once, at start. Nothing re-reads it.
echo "restarting $CONTAINER ..."
docker restart "$CONTAINER" >/dev/null

# A restarted driver-app re-applies its own migrations before it listens, so
# "the container is up" is not the same as "the API answers". Ask the API.
for _ in $(seq 1 30); do
  if curl -fsS --max-time 3 http://localhost:8017/openapi >/dev/null 2>&1; then
    echo "driver API is answering again"
    exit 0
  fi
  sleep 2
done

echo "Container restarted but the driver API did not answer within 60s." >&2
echo "Check: docker logs --tail 50 $CONTAINER" >&2
exit 1
