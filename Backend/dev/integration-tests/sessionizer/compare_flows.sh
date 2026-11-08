#!/usr/bin/env bash
# Runs the same ride flows on the unedited code (the origin/main merge-base, in a
# git worktree) and on this working tree, one suite at a time, then:
#   1. diffs what each suite wrote (Postgres rows, Redis keys, Kafka messages), and
#   2. checks this tree's driver_analytics_5m against Postgres and location-updates.
#
# Usage (from anywhere in the repo, with no stack running):
#   Backend/dev/integration-tests/sessionizer/compare_flows.sh                         # the three auto suites
#   NAMES="01-AutoRideFlow" .../compare_flows.sh                                       # one suite
#   SKIP_BASE=1 .../compare_flows.sh       # reuse the last base snapshots, rerun only this tree
#   RIDES=20 .../compare_flows.sh          # repeat the first suite on fresh databases: crashes and pop-ups
#
# One suite per mark because run-tests.sh flushes Redis before each suite, which
# drops writes the drainer has not reached yet. Any flow failure stops the run: a
# failed flow on both sides would otherwise diff as "the same".
#
# The base worktree (BASE_TREE, default ../nammayatri-base) gets its own data/ dir,
# so its first start initialises a fresh database, imported from the bundle this
# tree last synced from, and its first build is a full one. Both sides use this
# tree's run-tests.sh and collections, so the inputs are the same. Results go to
# Backend/dev/integration-tests/data/compare/. Exits 1 if anything differs or fails.
set -euo pipefail
# A shell pointed at a remote warehouse: the local ClickHouse setup would log in as
# that user and fail, leaving a fresh database without its tables.
unset CLICKHOUSE_HOST CLICKHOUSE_USER CLICKHOUSE_PASSWORD

REPO="$(git -C "$(dirname "$0")" rev-parse --show-toplevel)"
BASE_REV="${BASE_REV:-$(git -C "$REPO" merge-base HEAD origin/main)}"
BASE_TREE="${BASE_TREE:-$(cd "$REPO/.." && pwd)/nammayatri-base}"
NAMES="${NAMES:-01-AutoRideFlow 02-AutoUserCancellation 03-AutoDriverCancellation}"
HARNESS="$REPO/Backend/dev/integration-tests"
CONFIG_ENV="Backend/dev/config-sync/assets/environments.json"
OUT="$HARNESS/data/compare"
PY="uv run -q $HARNESS/sessionizer/compare_flows.py"
mkdir -p "$OUT"
FAILED=0
# The dev shell points CABAL_DIR at the tree it runs in, and a new one can't bootstrap
# its Hackage index with this cabal ("root.json does not have enough signatures").
# The store is keyed by content hash and dist-newstyle stays per tree, so share this one.
SHARED_CABAL_DIR="$REPO/Backend/.cabal-dir"
DATA_KEEP="$(cd "$REPO/.." && pwd)/nammayatri-data.keep"

say() { echo "$(date +%H:%M:%S)  $*"; }

wait_port() {  # port, seconds
  for _ in $(seq 1 "$2"); do nc -z -w1 127.0.0.1 "$1" >/dev/null 2>&1 && return 0; sleep 1; done
  return 1
}

apps_up() {
  local p
  for p in 8016 8013 8015 8020 8081 7082; do wait_port "$p" 900 || return 1; done
}

stack_down() {
  pkill -TERM -f "process-compose" 2>/dev/null || true
  for _ in $(seq 1 60); do
    if ! nc -z -w1 127.0.0.1 5434 >/dev/null 2>&1 && ! nc -z -w1 127.0.0.1 8016 >/dev/null 2>&1; then
      # ClickHouse can outlive process-compose, and the next stack would then use this one.
      sleep 5
      pkill -TERM -x clickhouse-server 2>/dev/null || true
      for _ in $(seq 1 15); do nc -z -w1 127.0.0.1 8123 >/dev/null 2>&1 || return 0; sleep 2; done
      say "ClickHouse still holds 8123; stop it by hand"; return 1
    fi
    sleep 2
  done
  say "stack still up after 2 min; stop it by hand"; return 1
}

stack_up() {  # tree, label
  say "[$2] starting the stack"
  (cd "$1" && nix develop .#backend -c bash -c "export CABAL_DIR='$SHARED_CABAL_DIR'; exec nix run .#run-mobility-stack-dev -- -t=false") >> "$OUT/$2-stack.log" 2>&1 &
  apps_up || { say "[$2] apps did not come up: $OUT/$2-stack.log"; stack_down; return 1; }
  sleep 60   # the consumers start after the apps
}

# RIDES mode needs both sides on a new database. This tree's data/ is moved aside, not
# deleted, and put back on exit; if that is ever interrupted it stays at $DATA_KEEP.
restore_data() {
  [ -d "$DATA_KEEP" ] || return 0
  stack_down || true
  rm -rf "$REPO/data"
  mv "$DATA_KEEP" "$REPO/data"
  say "restored $REPO/data"
}

# Each ride: the flow, a fixed drain, then its numbers; the same pace on both sides. A
# crash is logged and the stack restarted, so one doesn't end the count.
run_rides() {  # tree, label, flags
  local tree="$1" label="$2" flags="${3:-}" name="${NAMES%% *}" i rc
  : > "$OUT/rides-$label.jsonl"
  for i in $(seq 1 "$RIDES"); do
    $PY mark --out "$OUT/ride-pre.json"
    rc=0
    (cd "$REPO" && nix develop .#backend -c bash -c "cd '$HARNESS' && ./run-tests.sh auto NY_Bangalore $name") > "$OUT/$label-ride-$i.log" 2>&1 || rc=$?
    sleep 60
    if ! nc -z -w1 127.0.0.1 8116 >/dev/null 2>&1; then
      say "[$label] ride $i: DRIVER APP DOWN"
      echo "{\"ride\": $i, \"crash\": true}" >> "$OUT/crashes-$label.jsonl"
      $PY ride-stats --pre "$OUT/ride-pre.json" --ride "$i" --flow-rc "$rc" --out "$OUT/rides-$label.jsonl" || true
      stack_down; stack_up "$tree" "$label" || return 1
      continue
    fi
    $PY ride-stats --pre "$OUT/ride-pre.json" --ride "$i" --flow-rc "$rc" --out "$OUT/rides-$label.jsonl"
    [ "$rc" = 0 ] || say "[$label] ride $i: flow failed: $OUT/$label-ride-$i.log"
  done
  # One end-to-end check of the stream after all those rides.
  if [ "$label" = branch ]; then run_suite "$label" "$name" "$flags" || FAILED=1; fi
}

run_suite() {  # label, suite, extra mark flags
  local label="$1" name="$2" flags="${3:-}" tag="$1-$2"
  # truth compares whole 5-minute buckets with Postgres rows since the mark, so a driver
  # from the warm-up or the last suite must not share this suite's bucket.
  [ "$label" = branch ] && sleep $(( 300 - $(date +%s) % 300 + 65 ))
  $PY mark $flags --out "$OUT/$tag-pre.json"
  say "[$label] ./run-tests.sh auto NY_Bangalore $name"
  (cd "$REPO" && nix develop .#backend -c bash -c "cd '$HARNESS' && ./run-tests.sh auto NY_Bangalore $name") > "$OUT/$tag-flows.log" 2>&1 \
    || { say "[$label] FLOW FAILED: $OUT/$tag-flows.log"; return 1; }
  sleep 60   # let the drainers and consumers catch up; the next suite flushes Redis
  $PY snapshot --pre "$OUT/$tag-pre.json" --out "$OUT/$tag.json"
  if [ "$label" = branch ]; then
    $PY truth --pre "$OUT/$tag-pre.json" | grep -v 'waiting for the sessionizer flush' | tee "$OUT/truth-$name.txt" \
      || { say "[$label] truth check failed for $name"; FAILED=1; }
  fi
}

run_side() {  # tree, label, extra mark flags
  local tree="$1" label="$2" flags="${3:-}" name
  say "[$label] building $tree (cabal build all)"
  (cd "$tree" && nix develop .#backend -c bash -c "export CABAL_DIR='$SHARED_CABAL_DIR'; cd Backend && cabal build all") > "$OUT/$label-build.log" 2>&1 \
    || { say "[$label] build failed: $OUT/$label-build.log"; return 1; }
  stack_up "$tree" "$label" || return 1
  say "[$label] preparing config and registry"
  local rc=0
  $PY prepare --sync-metadata "$SYNC_METADATA" || rc=$?
  if [ "$rc" = 10 ]; then
    say "[$label] config imported; restarting the stack so every app boots with it"
    # New Kafka: the consumers must find the branch's topics when they start.
    [ -z "$flags" ] || $PY mark $flags --out "$OUT/$label-setup-pre.json"
    stack_down
    stack_up "$tree" "$label" || return 1
  elif [ "$rc" != 0 ]; then
    stack_down; return 1
  fi
  # The first ride after a start can outlast the flows' fixed waits (11 s from search to
  # estimate on a cold stack, against about 10). This one is unmeasured and may fail;
  # the next suite's Redis flush clears what it leaves behind.
  say "[$label] warm-up ride"
  (cd "$REPO" && nix develop .#backend -c bash -c "cd '$HARNESS' && ./run-tests.sh auto NY_Bangalore ${NAMES%% *}") > "$OUT/$label-warmup.log" 2>&1 || true
  if [ -n "${RIDES:-}" ]; then
    run_rides "$tree" "$label" "$flags" || { stack_down; return 1; }
    stack_down
    return 0
  fi
  for name in $NAMES; do
    run_suite "$label" "$name" "$flags" || { stack_down; return 1; }
  done
  stack_down
}

if nc -z -w1 127.0.0.1 5434 >/dev/null 2>&1 || nc -z -w1 127.0.0.1 8016 >/dev/null 2>&1; then
  say "a stack is already running; stop it first"; exit 1
fi
for p in 8081 8123; do
  if nc -z -w1 127.0.0.1 "$p" >/dev/null 2>&1; then
    say "something already holds port $p, which the stack needs:"; lsof -nP -iTCP:"$p" -sTCP:LISTEN; exit 1
  fi
done

SYNC_METADATA="$REPO/data/config-sync/metadata.json"
if [ -n "${RIDES:-}" ]; then
  [ ! -e "$DATA_KEEP" ] || { say "$DATA_KEEP exists from an earlier run; restore or remove it first"; exit 1; }
  cp -p "$SYNC_METADATA" "$OUT/sync-metadata.json"
  SYNC_METADATA="$OUT/sync-metadata.json"
  rm -f "$OUT"/crashes-*.jsonl
fi

if [ -z "${SKIP_BASE:-}" ]; then
  if [ ! -d "$BASE_TREE" ]; then
    say "creating worktree $BASE_TREE at $BASE_REV"
    git -C "$REPO" worktree add --detach "$BASE_TREE" "$BASE_REV"
  fi
  # The stack's cabal-build reads the shared CABAL_DIR's path in setup-config as a CI
  # cache from another root and takes a Linux-only route (unshare). Worktree only.
  sed -i '' 's/if \[ ! -f \.ci-project-root \] && \[ -d dist-newstyle\/build \]; then/if false; then/' "$BASE_TREE/Backend/nix/services/nammayatri.nix"
  [ -e "$BASE_TREE/$CONFIG_ENV" ] || cp -p "$REPO/$CONFIG_ENV" "$BASE_TREE/$CONFIG_ENV"   # git-ignored; config_transfer.py needs it
  [ -z "${RIDES:-}" ] || rm -rf "$BASE_TREE/data"
  run_side "$BASE_TREE" base
fi
if [ -n "${RIDES:-}" ]; then
  mv "$REPO/data" "$DATA_KEEP"
  trap restore_data EXIT
fi
run_side "$REPO" branch --sessionizer

if [ -n "${RIDES:-}" ]; then
  $PY ride-report "$OUT/rides-base.jsonl" "$OUT/rides-branch.jsonl" | tee "$OUT/ride-report.txt"
  for s in base branch; do say "$s driver-app crashes: $(cat "$OUT/crashes-$s.jsonl" 2>/dev/null | wc -l | tr -d ' ')"; done
  exit "$FAILED"
fi

: > "$OUT/diff.txt"
for name in $NAMES; do
  say "diff $name: base ($BASE_REV) vs this tree"
  { echo "=== $name"; $PY diff "$OUT/base-$name.json" "$OUT/branch-$name.json"; } | tee -a "$OUT/diff.txt" || FAILED=1
done
say "done: $OUT/diff.txt, $OUT/truth-*.txt"
exit "$FAILED"
