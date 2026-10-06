#!/usr/bin/env bash
# Release Backend/dev/local-stack/stack to the server. One command.
#
#     ops/deploy.sh --dry-run      what would change on the server; writes nothing
#     ops/deploy.sh                release the commit you are on
#     ops/deploy.sh rollback       put back what the last release replaced
#     ops/deploy.sh status         what is deployed, and has anyone edited it since
#     ops/deploy.sh tidy           archive the old .bak / .before-* copies (root only)
#
# Run from the laptop, in WSL, from anywhere in the repository. Phase 3 of the
# backend restructuring plan (2026-10-06). The work on the server is done by
# ops/release-remote.py; its header says what a release may and may not touch.
#
# ── Before anything leaves the laptop ──────────────────────────────────────
#   * the working tree is clean -- a release is a commit, not whatever happens
#     to be on disk;
#   * that commit is pushed -- so `.shipped` names something anyone can check out.
# Then `stack/` is taken from the COMMIT (git archive), not from the folder.
#
# ── Two connections, never more ────────────────────────────────────────────
# The server's firewall (`ufw limit`) refuses the sixth SSH connection in 30
# seconds, silently. A release is one scp and one ssh.
#
# ── The very first release ─────────────────────────────────────────────────
# Before 2026-10-06 the server had no record of what was deployed. Phase 1
# proved it identical to commit c6562a1926 (old layout), so the first release
# passes that commit as the PREVIOUS manifest: it is how the release knows the
# old top-level .sql copies are ours to remove, and that nothing was edited by
# hand since. Once `.shipped.files` exists on the server it is used instead.
set -euo pipefail

HOST="${HOST:-ny}"
FIRST_RELEASE_BASE="c6562a1926"

MODE="apply"
FORCE=""
for arg in "$@"; do
  case "$arg" in
    --dry-run|plan) MODE="plan" ;;
    rollback|status|tidy) MODE="$arg" ;;
    --force) FORCE="--force" ;;
    -h|--help) sed -n '2,9p' "$0"; exit 0 ;;
    *) echo "unknown argument: $arg" >&2; exit 2 ;;
  esac
done

ROOT="$(git -C "$(dirname "$0")" rev-parse --show-toplevel)"
LS="Backend/dev/local-stack"
cd "$ROOT"
WORK="$(mktemp -d)"
trap 'rm -rf "$WORK"' EXIT

remote_only() {
  scp -q "$LS/ops/release-remote.py" "$HOST:/tmp/release-remote.py"
  ssh "$HOST" "python3 /tmp/release-remote.py $1; rc=\$?; rm -f /tmp/release-remote.py; exit \$rc"
}

case "$MODE" in
  rollback|status|tidy) remote_only "$MODE"; exit $? ;;
esac

# ── the commit ─────────────────────────────────────────────────────────────
if [ -n "$(git status --porcelain)" ]; then
  echo "the working tree is not clean -- commit or stash first:" >&2
  git status --short >&2
  exit 1
fi
COMMIT="$(git rev-parse HEAD)"
BRANCH="$(git rev-parse --abbrev-ref HEAD)"
if ! git branch -r --contains "$COMMIT" | grep -q .; then
  echo "commit ${COMMIT:0:10} is not pushed -- push it first, so .shipped names a commit anyone can see" >&2
  exit 1
fi

# ── the release: stack/ of that commit, and a hash for every file ──────────
mkdir -p "$WORK/rel"
git archive "$COMMIT" "$LS/stack" | tar -x -C "$WORK"
mv "$WORK/$LS/stack" "$WORK/rel/stack"
cp "$LS/ops/release-remote.py" "$WORK/rel/"

manifest() {   # <dir>  ->  "sha256  mode  path" for every file under it
  (cd "$1" && find . -type f -printf '%P\n' | LC_ALL=C sort | while IFS= read -r f; do
     printf '%s  %s  %s\n' "$(sha256sum "$f" | cut -d' ' -f1)" "$(stat -c '%a' "$f")" "$f"
   done)
}
manifest "$WORK/rel/stack" > "$WORK/rel/MANIFEST"

# First release only: the files the server was proven to hold (old layout).
mkdir -p "$WORK/base"
git archive "$FIRST_RELEASE_BASE" "$LS" | tar -x -C "$WORK/base"
manifest "$WORK/base/$LS" > "$WORK/rel/PREVIOUS"

python3 - "$WORK/rel/INFO.json" "$COMMIT" "$BRANCH" <<'PY'
import json, subprocess, sys
path, commit, branch = sys.argv[1:4]
subject = subprocess.run(['git', 'log', '-1', '--format=%s', commit], capture_output=True, text=True).stdout.strip()
by = subprocess.run(['git', 'config', 'user.name'], capture_output=True, text=True).stdout.strip()
json.dump({'commit': commit, 'branch': branch, 'subject': subject, 'by': by}, open(path, 'w'))
PY

echo "release ${COMMIT:0:10} ($BRANCH): $(wc -l < "$WORK/rel/MANIFEST") files from $LS/stack"
tar -czf "$WORK/release.tgz" -C "$WORK/rel" .

# ── one copy, one command ──────────────────────────────────────────────────
scp -q "$WORK/release.tgz" "$HOST:/tmp/movin-release.tgz"
REMOTE_MODE="$MODE"
ssh "$HOST" "R=/tmp/movin-release; rm -rf \$R; mkdir -p \$R &&
  tar -xzf /tmp/movin-release.tgz -C \$R && rm -f /tmp/movin-release.tgz || exit 1;
  rc=0; python3 \$R/release-remote.py $REMOTE_MODE \$R $FORCE || rc=\$?; rm -rf \$R; exit \$rc"
