#!/usr/bin/env bash
# stack/launch-gate.sh promises to be READ-ONLY, and a script run on the eve of a
# launch, by hand, as root, is exactly where that promise must be mechanical.
# This cannot run the gate (it needs the server); it proves what can be proved
# without one: the file parses and contains no verb that writes, prints no secret,
# and runs the clean-up only as a dry run.
#   bash tests/launch-gate.test.sh
set -uo pipefail
HERE="$(cd "$(dirname "$0")/.." && pwd)"
G="$HERE/stack/launch-gate.sh"
fails=0
pass() { printf '   ok    %s\n' "$*"; }
fail() { printf '   FAIL  %s\n' "$*"; fails=$((fails + 1)); }

bash -n "$G" && pass "launch-gate.sh parses" || fail "launch-gate.sh does not parse"

# Strip comments, then look for anything that changes state.
body="$(sed 's/#.*$//' "$G")"
if grep -Eiq '\b(insert|update|delete|drop|alter|truncate)\b[^|]*\b(into|from|table|set|schema)\b' <<<"$body"; then
  fail "SQL that writes appears in launch-gate.sh"; else pass "no SQL that writes"; fi
if grep -Eq '(^|[;&|( ])(rm|mv|cp|chmod|chown|kill|reboot)( |$)|systemctl +(stop|start|restart|disable|enable)|docker +(stop|start|restart|rm|kill|compose +(up|down|restart))|redis-cli.*(DEL|FLUSH|SET)' <<<"$body"; then
  fail "a command that changes the machine appears"; else pass "no command that changes the machine"; fi
if grep -Eq 'echo .*\$(key|enc|base)\b|printf .*\$(key|enc)\b' <<<"$body"; then
  fail "a secret or an encrypted number is printed"; else pass "no secret is printed"; fi
# It does run launch-cleanup.sh, and only without --apply.
grep -q 'launch-cleanup.sh' <<<"$body" && ! grep -q 'launch-cleanup.sh --apply' <<<"$body" \
  && pass "it runs the clean-up only as a dry run" || fail "it must run launch-cleanup.sh without --apply"

[ "$fails" = 0 ] || exit 1
