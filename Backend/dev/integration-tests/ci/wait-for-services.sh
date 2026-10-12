#!/usr/bin/env bash
# CI: wait for the mobility stack, but fail fast when a startup process crashes and name the app, error and commit.
#
# Usage (from the repo root, where process-compose writes <process>.log):
#   Backend/dev/integration-tests/ci/wait-for-services.sh <timeout-minutes> [label]
# Env: CONTEXT_API (default http://localhost:7082), STACK_PID_FILE (default stack.pid), LOG_DIR (default .),
#      GH_TOKEN + GITHUB_REPOSITORY + GITHUB_SHA (optional; culprit-commit lookup via the GitHub API).
set -uo pipefail

TIMEOUT_MIN=${1:?timeout in minutes}
LABEL=${2:-Wait for services}
CONTEXT_API=${CONTEXT_API:-http://localhost:7082}
STACK_PID_FILE=${STACK_PID_FILE:-stack.pid}
LOG_DIR=${LOG_DIR:-.}
SUMMARY=${GITHUB_STEP_SUMMARY:-/dev/null}
INTERVAL=10

# Processes whose failure blocks readiness, in startup order (so the root cause is reported first).
# Format: <log file>|<display name>|<migration dirs under Backend/dev, space separated>
WATCHED=(
  "$LOG_DIR/cabal-build.log|cabal-build|"
  "$LOG_DIR/nammayatri-init.log|nammayatri-init|"
  "$LOG_DIR/dashboard-migrations.log|dashboard-migrations|dashboard"
  "$LOG_DIR/rider-app-exe.log|rider-app|rider-app scheduler"
  "$LOG_DIR/dynamic-offer-driver-app-exe.log|dynamic-offer-driver-app|dynamic-offer-driver-app"
  "$LOG_DIR/mock-registry.log|mock-registry|mock-registry"
)

fsize() { [ -f "$1" ] && wc -c < "$1" | tr -d ' ' || echo 0; }

declare -A OFFSET
for w in "${WATCHED[@]}"; do
  f=${w%%|*}
  OFFSET[$f]=$(fsize "$f")
done

# process-compose writes JSON lines; print just the messages (NULs and ANSI colours removed).
messages() { tr -d '\0' | jq -R -r 'fromjson? | .message // empty' 2>/dev/null | sed $'s/\x1b\\[[0-9;]*m//g'; }
# Bytes written since the script started (from 0 if log-cleaner truncated the file), capped at the last 2 MB.
new_messages() {
  local f=$1 off=${OFFSET[$1]}; [ -f "$f" ] || return 0
  [ "$(fsize "$f")" -lt "$off" ] && off=0
  tail -c +$((off + 1)) "$f" | tail -c 2000000 | messages
}
last_messages() { [ -f "$1" ] && tail -c 400000 "$1" | messages | tail -n "${2:-40}"; }

# First crash signature in a block of messages, plus the ~15 lines after it. Normal shutdowns are ignored.
crash_text() {
  awk '
    { l = tolower($0) }
    !found && ( l ~ /^[a-z0-9_-]+-exe: / || l ~ /couldn.t migrate database/ || l ~ /expression doesn.t match annotation/ \
                || l ~ /^cabal: / || l ~ /failed to build/ || l ~ /couldn.t build appenv/ || l ~ /^sqlerror \{/ ) \
           && l !~ /killed|sigterm|terminated|exitsuccess|interrupt/ { found = 1 }
    found && n < 16 { print; n++ }
  '
}

# Last migration file that started but never finished.
failing_migration() { awk -F'\t' '/^Executing:/ { f = $2 } /^Executed:/ && $2 == f { f = "" } END { print f }'; }

commits_for() { # <path> <count>
  local path=$1 n=$2
  if [ -n "${GH_TOKEN:-}" ] && [ -n "${GITHUB_REPOSITORY:-}" ]; then
    curl -sf -H "Authorization: Bearer $GH_TOKEN" \
      "https://api.github.com/repos/$GITHUB_REPOSITORY/commits?path=$path&sha=${GITHUB_SHA:-HEAD}&per_page=$n" \
      | jq -r '.[] | "\(.sha[0:10]) · \(.commit.author.name) · \(.commit.author.date[0:10]) · \(.commit.message | split("\n")[0])"' && return
  fi
  git log -n "$n" --date=short --format='%h · %an · %ad · %s' -- "$path" 2>/dev/null
}

ready_json() { curl -sf --max-time 5 "$CONTEXT_API/api/services-ready" 2>/dev/null; }

status_table() {
  local js; js=$(ready_json)
  echo "| Service | Port | Status |"; echo "|---|---|---|"
  if [ -n "$js" ]; then
    echo "$js" | jq -r '.services[] | "| \(.name) | \(.port) | \(if .up then "✅ up" else "❌ down (\(.error // .status // "no response"))" end) |"'
  else
    echo "| test-context-api | 7082 | ❌ not responding |"
  fi
}

annotate() { # <title> <body>; GitHub annotations need %, CR and LF escaped
  local body=${2//'%'/'%25'}; body=${body//$'\r'/}; body=${body//$'\n'/'%0A'}
  echo "::error title=$1::$body"
}

fail() { # <title> <error text> <culprit text> <log file or empty>
  local title=$1 err=$2 culprit=$3 log=$4 elapsed=$(( SECONDS )) f
  annotate "$title" "$err${culprit:+$'\n'$culprit}"
  {
    echo "## ❌ $LABEL — failed after $((elapsed / 60))m $((elapsed % 60))s"
    echo; echo "**$title**"; echo
    status_table; echo
    echo '**Error:**'; echo '```'; echo "$err"; echo '```'
    [ -n "$culprit" ] && { echo; echo "$culprit"; }
    [ -n "$log" ] && { echo; echo "<details><summary>${log##*/} — last 40 lines</summary>"; echo; echo '```'; last_messages "$log" 40; echo '```'; echo '</details>'; }
  } >> "$SUMMARY"
  echo "::group::services-ready"; ready_json | jq . 2>/dev/null || echo "test-context-api not responding"; echo "::endgroup::"
  for w in "${WATCHED[@]}"; do
    f=${w%%|*}; [ -f "$f" ] || continue
    echo "::group::${f##*/} (last 40 lines)"; last_messages "$f" 40; echo "::endgroup::"
  done
  exit 1
}

echo "$LABEL: waiting up to ${TIMEOUT_MIN} min (poll every ${INTERVAL}s)"
deadline=$(( SECONDS + TIMEOUT_MIN * 60 ))
while [ "$SECONDS" -lt "$deadline" ]; do
  if ready_json | jq -e '.ready == true' >/dev/null 2>&1; then
    echo "✅ Services ready after $((SECONDS / 60))m $((SECONDS % 60))s"
    echo "✅ $LABEL: ready after $((SECONDS / 60))m $((SECONDS % 60))s" >> "$SUMMARY"
    exit 0
  fi

  if [ -f "$STACK_PID_FILE" ] && ! kill -0 "$(cat "$STACK_PID_FILE")" 2>/dev/null; then
    fail "Mobility stack exited" "The stack process ($(cat "$STACK_PID_FILE")) is no longer running." "" ""
  fi

  for w in "${WATCHED[@]}"; do
    IFS='|' read -r log name dirs <<< "$w"
    msgs=$(new_messages "$log")
    err=$(printf '%s\n' "$msgs" | crash_text)
    [ -z "$err" ] && continue

    culprit=""
    mig=$(printf '%s\n' "$msgs" | failing_migration)
    if [ -n "$mig" ]; then
      path=""
      for d in $dirs; do
        path=$(git ls-files "Backend/dev/*/$d/$mig" | head -1); [ -n "$path" ] && break
      done
      err="Migration $mig failed:"$'\n'"$err"
      [ -n "$path" ] && culprit="**Failing file:** \`$path\`"$'\n'"**Last changed by:** $(commits_for "$path" 1)"
    elif grep -q "Expression doesn.t match annotation" <<< "$err"; then
      culprit="**Dhall config doesn't match the Haskell type.** Recent commits to Backend/dhall-configs (suspects):"$'\n'"$(commits_for Backend/dhall-configs 3 | sed 's/^/- /')"
    else
      culprit="**Commit under test:** $(git log -1 --date=short --format='%h · %an · %ad · %s')"
    fi
    fail "$name crashed at startup" "$err" "$culprit" "$log"
  done

  echo "waiting for services... ($((SECONDS / 60))m $((SECONDS % 60))s)"
  sleep "$INTERVAL"
done

fail "Services not ready after ${TIMEOUT_MIN} min" "No crash detected, but these services never became ready (see table)." \
  "**Commit under test:** $(git log -1 --date=short --format='%h · %an · %ad · %s')" ""
