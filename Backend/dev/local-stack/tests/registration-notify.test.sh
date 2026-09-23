#!/usr/bin/env bash
#
# Does the queue notifier announce each driver exactly once, escalate once
# more when he has been left waiting, and then stop?
#
# `docker` and `curl` are stubbed on PATH, so this needs no database, no
# network and no Telegram bot: the stub prints the rows the real query would
# have returned, and records every message the script tries to send.
set -uo pipefail

HERE="$(cd "$(dirname "$0")" && pwd)"
SCRIPT="$HERE/../registration-notify.sh"
WORK="$(mktemp -d)"
trap 'rm -rf "$WORK"' EXIT

mkdir -p "$WORK/bin"
export PATH="$WORK/bin:$PATH"
export REGISTRATION_STATE="$WORK/state"
export TELEGRAM_BOT_TOKEN="test-token"
export TELEGRAM_CHAT_ID="42"
export PATIENCE_HOURS=4

cat > "$WORK/bin/docker" <<'STUB'
#!/usr/bin/env bash
cat "$ROWS_FILE"
STUB

cat > "$WORK/bin/curl" <<'STUB'
#!/usr/bin/env bash
# Record what would have been sent, then answer like Telegram does.
for ((i = 1; i <= $#; i++)); do
  if [ "${!i}" = "--data-urlencode" ]; then
    j=$((i + 1))
    case "${!j}" in text=*) printf '%s\n---\n' "${!j#text=}" >> "$SENT_FILE" ;; esac
  fi
done
printf '%s' "${FAKE_HTTP_CODE:-200}"
STUB

chmod +x "$WORK/bin/docker" "$WORK/bin/curl"
export ROWS_FILE="$WORK/rows" SENT_FILE="$WORK/sent"
: > "$SENT_FILE"

fails=()
ok() {
  if [ "$2" = "$3" ]; then echo "   PASS  $1"
  else echo "   **FAIL**  $1 (want $2, got $3)"; fails+=("$1"); fi
}
sent_count() { grep -c '^---$' "$SENT_FILE" 2>/dev/null; true; }

# ── 1. a fresh registration is announced once ─────────────────────────────
echo "1. One new driver, script run three times"
printf 'id-aaa\tYas Kara\t36664750\t0\t2\t1\n' > "$ROWS_FILE"
bash "$SCRIPT" >/dev/null 2>&1
bash "$SCRIPT" >/dev/null 2>&1
bash "$SCRIPT" >/dev/null 2>&1
ok "announced exactly once" 1 "$(sent_count)"
grep -q "nouvelle inscription" "$SENT_FILE" && echo "   PASS  it was the new-driver message" \
  || { echo "   **FAIL**  wrong message"; fails+=("new message"); }
grep -q "2 papier(s) reçu(s), véhicule déclaré" "$SENT_FILE" \
  && echo "   PASS  it says what he sent" \
  || { echo "   **FAIL**  papers line wrong"; fails+=("papers line"); }

# ── 2. still waiting, hours later: one more, then silence ─────────────────
echo
echo "2. Same driver, now 6 hours old, script run three times"
: > "$SENT_FILE"
printf 'id-aaa\tYas Kara\t36664750\t6\t2\t1\n' > "$ROWS_FILE"
bash "$SCRIPT" >/dev/null 2>&1
bash "$SCRIPT" >/dev/null 2>&1
bash "$SCRIPT" >/dev/null 2>&1
ok "escalated exactly once" 1 "$(sent_count)"
grep -q "toujours en attente" "$SENT_FILE" && echo "   PASS  it was the still-waiting message" \
  || { echo "   **FAIL**  wrong escalation message"; fails+=("escalation"); }

# ── 3. a Telegram failure must not count as delivered ─────────────────────
echo
echo "3. Telegram refuses: the driver must be retried, not forgotten"
: > "$SENT_FILE"
printf 'id-bbb\tSidi Ould\t22334455\t0\t0\t0\n' > "$ROWS_FILE"
FAKE_HTTP_CODE=500 bash "$SCRIPT" >/dev/null 2>&1
ok "nothing recorded as sent" 0 "$(grep -c '^id-bbb' "$REGISTRATION_STATE" 2>/dev/null; true)"
bash "$SCRIPT" >/dev/null 2>&1
ok "retried on the next run" 1 "$(grep -c '^id-bbb new$' "$REGISTRATION_STATE" 2>/dev/null; true)"
grep -q "aucun papier reçu" "$SENT_FILE" && echo "   PASS  says plainly that nothing arrived" \
  || { echo "   **FAIL**  empty-papers line wrong"; fails+=("no-papers line"); }

# ── 4. once validated, he leaves the state file ───────────────────────────
echo
echo "4. Queue empties: state is pruned so a later re-registration is seen"
: > "$ROWS_FILE"
bash "$SCRIPT" >/dev/null 2>&1
ok "state emptied" 0 "$(grep -c . "$REGISTRATION_STATE" 2>/dev/null; true)"

echo
if [ ${#fails[@]} -eq 0 ]; then echo "ALL PASSED"; else
  echo "FAILED: ${fails[*]}"; exit 1
fi
