#!/usr/bin/env bash
# A whole release, rehearsed on a fake server: ops/release-remote.py against a
# copy of the layout the VPS had on 2026-10-06, docker and systemd stubbed.
#
#     bash tests/release.test.sh
#
# What it proves, because each one is a way a release can hurt the live box:
#   1. the plan of the first release is the one expected (SQL moves into db/,
#      nothing that runs changes, nothing restarts)
#   2. apply writes every shipped file, IN PLACE (the inode of a bind-mounted
#      file survives), and touches nothing it does not ship (.env, edge-web/,
#      bin/ stay byte for byte)
#   3. `status` sees no drift after a release, and sees a hand edit after one
#   4. a release refuses to overwrite a hand edit
#   5. `hashes` + ops/release-verify.py: the deployed tree checked against the
#      release from OUTSIDE -- and a hand edit hidden by rewriting the server's
#      own record is still caught
#   6. `rollback` puts the previous version back exactly
#   7. `tidy` archives the .bak copies and nothing else
# and, through 2 and 6, the systemd units in stack/systemd/: installed by the
# release, put back by rollback (phase 4: the nightly backup runs the shipped copy)
set -uo pipefail
HERE="$(cd "$(dirname "$0")/.." && pwd)"
ROOT="$(git -C "$HERE" rev-parse --show-toplevel)"
LS="Backend/dev/local-stack"
T="$(mktemp -d)"
trap 'rm -rf "$T"' EXIT
export MOVIN_RELEASE_TEST=1 MOVIN_STACK="$T/box" MOVIN_LEFTOVERS="$T/root-snapshots" MOVIN_UNITS="$T/units"
R="python3 $HERE/ops/release-remote.py"
fails=0
pass() { printf '   ok    %s\n' "$*"; }
fail() { printf '   FAIL  %s\n' "$*"; fails=$((fails + 1)); }
h() { sha256sum "$1" | cut -d' ' -f1; }
manifest() { (cd "$1" && find . -type f -printf '%P\n' | LC_ALL=C sort | while IFS= read -r f; do
  printf '%s  %s  %s\n' "$(sha256sum "$f" | cut -d' ' -f1)" "$(stat -c '%a' "$f")" "$f"; done); }

# ── the fake server: the old layout (commit c6562a1926) plus the box's own files
mkdir -p "$T/old"
git -C "$ROOT" archive c6562a1926 "$LS" | tar -x -C "$T/old"
cp -a "$T/old/$LS" "$T/box"
rm -rf "$T/box/tests" "$T/box/docs"            # never on the server
mkdir -p "$T/box/edge-web/site" "$T/box/bin"
echo 'SECRET=1' > "$T/box/.env";            ENV_H=$(h "$T/box/.env")
echo '<html>' > "$T/box/edge-web/site/index.html"; WEB_H=$(h "$T/box/edge-web/site/index.html")
echo 'manifest' > "$T/box/bin/MANIFEST.txt"
cp "$T/box/docker-compose.yml" "$T/box/docker-compose.yml.bak-20260831-123757"
NGINX_INODE=$(stat -c %i "$T/box/edge/nginx.conf")
# The unit as `backup.sh install` wrote it before phase 4: running /root's copy.
mkdir -p "$T/units"
printf '[Service]\nType=oneshot\nExecStart=/root/backup.sh\n' > "$T/units/movin-backup.service"
OLD_UNIT_H=$(h "$T/units/movin-backup.service")

# ── the release: stack/ as it is in the working tree
mkdir -p "$T/rel/stack"
# Only what git knows (committed or not yet), never ignored files: a real
# release takes stack/ from the commit, so a stray file on the laptop -- the
# auth-guard tests leave a trusted-phones.json behind -- must not count here.
(cd "$HERE/stack" && git ls-files -co --exclude-standard -z | xargs -0 -I{} cp --parents {} "$T/rel/stack/") 2>/dev/null \
  || { mkdir -p "$T/rel/stack" && (cd "$HERE/stack" && git ls-files -co --exclude-standard -z | xargs -0 -I{} cp --parents {} "$T/rel/stack/"); }
manifest "$T/rel/stack" > "$T/rel/MANIFEST"
manifest "$T/old/$LS" > "$T/rel/PREVIOUS"
echo '{"commit": "test0000000000", "branch": "test", "subject": "rehearsal", "by": "test"}' > "$T/rel/INFO.json"

echo "== 1. the plan"
PLAN="$($R plan "$T/rel" 2>&1)"
echo "$PLAN" | grep -qE "restart |reload |compose up|rebuild " && fail "the plan restarts something" || pass "nothing to restart"
echo "$PLAN" | grep -q "install systemd units" && pass "the backup units are installed" || fail "units not in the plan"
echo "$PLAN" | grep -q "no SQL to apply" && pass "no SQL applied (it only moved)" || fail "the plan applies SQL"
echo "$PLAN" | grep -q "new      db/algeria-tariff.sql" && pass "db/ arrives" || fail "db/ not in the plan"
echo "$PLAN" | grep -q "remove   algeria-tariff.sql" && pass "the old top-level copy goes" || fail "old SQL copy not removed"
echo "$PLAN" | grep -q "BAD" && fail "the plan reports a conflict" || pass "no conflict"

echo "== 2. apply"
$R apply "$T/rel" > "$T/apply.log" 2>&1 && pass "apply succeeded" || { fail "apply failed"; tail -20 "$T/apply.log"; }
bad=0; while read -r d m f; do [ "$(h "$T/box/$f" 2>/dev/null)" = "$d" ] || bad=$((bad+1)); done < "$T/rel/MANIFEST"
[ $bad -eq 0 ] && pass "every shipped file matches the release" || fail "$bad files do not match"
[ "$(stat -c %i "$T/box/edge/nginx.conf")" = "$NGINX_INODE" ] && pass "nginx.conf kept its inode (bind mount safe)" || fail "nginx.conf was replaced, not rewritten"
[ "$(h "$T/box/.env")" = "$ENV_H" ] && pass ".env untouched" || fail ".env changed"
[ "$(h "$T/box/edge-web/site/index.html")" = "$WEB_H" ] && pass "the website's build untouched" || fail "edge-web changed"
[ -f "$T/box/bin/MANIFEST.txt" ] && pass "bin/ untouched" || fail "bin/ touched"
[ ! -f "$T/box/algeria-tariff.sql" ] && [ -f "$T/box/db/algeria-tariff.sql" ] && pass "SQL now in db/ only" || fail "SQL layout wrong"
[ -f "$T/box/.shipped" ] && grep -q "test0000000000" "$T/box/.shipped" && pass ".shipped names the commit" || fail "no .shipped"
[ -f "$T/box.prev/RELEASE.json" ] && pass "previous version kept in .prev" || fail "no .prev"
cmp -s "$T/units/movin-backup.service" "$HERE/stack/systemd/movin-backup.service" \
  && pass "movin-backup.service installed as shipped" || fail "unit not installed"
grep -q "^ExecStart=/opt/ny/local-stack/backup.sh$" "$T/units/movin-backup.service" \
  && pass "and it runs the shipped backup.sh, not /root's" || fail "unit runs the wrong backup.sh"
[ -f "$T/units/movin-backup.timer" ] && pass "movin-backup.timer installed" || fail "timer not installed"

echo "== 3. status"
$R status 2>&1 | grep -q "exactly as released" && pass "no drift after a release" || fail "status sees drift"
echo "# hand edit" >> "$T/box/auth-guard/server.js"
$R status 2>&1 | grep -q "auth-guard/server.js differs" && pass "status sees a hand edit" || fail "status missed a hand edit"

echo "== 4. a release refuses to overwrite a hand edit"
$R apply "$T/rel" > "$T/apply2.log" 2>&1 && fail "it overwrote the hand edit" || pass "it stopped"
grep -q "auth-guard/server.js: different on the server" "$T/apply2.log" && pass "and named the file" || fail "did not name the file"
sed -i '$ d' "$T/box/auth-guard/server.js"     # undo the hand edit

echo "== 5. the deployed tree, checked from outside"
V="python3 $HERE/ops/release-verify.py"
$R hashes > "$T/hashes" 2>&1 && head -1 "$T/hashes" | grep -q "^commit test0000000000" \
  && pass "hashes names the claimed commit" || fail "hashes did not answer"
$V "$T/hashes" --manifest "$T/rel/MANIFEST" > "$T/v1.log" 2>&1 && pass "the release verifies" || { fail "a clean release did not verify"; cat "$T/v1.log"; }
echo "// hand edit" >> "$T/box/maps-shim/server.js"
$R hashes > "$T/hashes" 2>&1
$V "$T/hashes" --manifest "$T/rel/MANIFEST" > "$T/v2.log" 2>&1 && fail "a hand edit verified" \
  || { grep -q "maps-shim/server.js: different on the server" "$T/v2.log" && pass "a hand edit is caught, and named" || fail "edit caught but not named"; }
# Now hide it the way a careful hand would: rewrite the server's own record.
cp "$T/box/.shipped.files" "$T/shipped.files.orig"
NEWH=$(h "$T/box/maps-shim/server.js")
python3 -c "
import sys; p, newh = sys.argv[1], sys.argv[2]
out = []
for l in open(p):
    d, m, r = l.rstrip('\n').split('  ', 2)
    out.append('  '.join((newh if r == 'maps-shim/server.js' else d, m, r)))
open(p, 'w').write('\n'.join(out) + '\n')" "$T/box/.shipped.files" "$NEWH"
$R status 2>&1 | grep -q "exactly as released" && pass "(the server's own status is fooled, as expected)" || fail "status not fooled -- test is wrong"
$R hashes > "$T/hashes" 2>&1
$V "$T/hashes" --manifest "$T/rel/MANIFEST" > "$T/v3.log" 2>&1 && fail "a forged record verified" \
  || pass "a forged record is still caught: the expected side is not the server's"
cp "$T/shipped.files.orig" "$T/box/.shipped.files"
sed -i '$ d' "$T/box/maps-shim/server.js"
rm "$T/units/movin-backup.timer"
$R hashes > "$T/hashes" 2>&1
$V "$T/hashes" --manifest "$T/rel/MANIFEST" > "$T/v4.log" 2>&1 && fail "a missing unit verified" \
  || { grep -q "installed unit movin-backup.timer: not installed" "$T/v4.log" && pass "a missing unit is caught" || fail "missing unit not named"; }
cp "$HERE/stack/systemd/movin-backup.timer" "$T/units/"

echo "== 6. rollback"
$R rollback > "$T/rb.log" 2>&1 && pass "rollback succeeded" || { fail "rollback failed"; tail -20 "$T/rb.log"; }
[ -f "$T/box/algeria-tariff.sql" ] && [ ! -f "$T/box/db/algeria-tariff.sql" ] && pass "the old layout is back" || fail "layout not restored"
bad=0; while read -r d m f; do [ -f "$T/box/$f" ] || continue; [ "$(h "$T/box/$f")" = "$d" ] || bad=$((bad+1)); done < "$T/rel/PREVIOUS"
[ $bad -eq 0 ] && pass "every previous file is back, byte for byte" || fail "$bad files differ from before"
[ ! -f "$T/box/.shipped" ] && pass ".shipped gone again (there was none before)" || fail ".shipped still there"
[ "$(stat -c %i "$T/box/edge/nginx.conf")" = "$NGINX_INODE" ] && pass "nginx.conf inode still the same" || fail "inode changed in rollback"
[ "$(h "$T/units/movin-backup.service")" = "$OLD_UNIT_H" ] && pass "the old backup unit is back" || fail "unit not restored"
[ ! -f "$T/units/movin-backup.timer" ] && pass "the timer it never had is gone again" || fail "timer left behind"

echo "== 7. tidy"
$R tidy > "$T/tidy.log" 2>&1
[ ! -f "$T/box/docker-compose.yml.bak-20260831-123757" ] && ls "$T/root-snapshots"/leftovers-*/docker-compose.yml.bak-20260831-123757 >/dev/null 2>&1 \
  && pass ".bak archived" || fail ".bak not archived"
[ -f "$T/box/docker-compose.yml" ] && [ "$(h "$T/box/.env")" = "$ENV_H" ] && pass "nothing else moved" || fail "tidy moved too much"

echo
[ $fails -eq 0 ] && echo "release rehearsal: all checks passed" || { echo "release rehearsal: $fails FAILED"; exit 1; }
