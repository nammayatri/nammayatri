#!/usr/bin/env bash
# Clone-or-pull the control-center frontend and run `npm run dev` against
# the local nammayatri stack. Invoked by test-context-api when the user
# clicks "Launch Control Center" in the test-dashboard.
#
# Modern Node (>=20.19 / >=22.12) is required by Vite. We source it from the
# repo's `nixpkgs-unstable` flake input so this works inside the nix devshell
# without depending on any system-installed node.
#
# Working dir on disk: <repo-root>/data/control-center (gitignored, same
# convention metabase / redis-commander use for runtime state).

set -euo pipefail

# Resolve repo root: this script lives at
# Backend/dev/test-tool/context-api/setup/control_center/frontend/
REPO_ROOT="$(cd "$(dirname "$0")/../../../../../../.." && pwd)"
DATA_DIR="$REPO_ROOT/data"
CC_DIR="$DATA_DIR/control-center"

mkdir -p "$DATA_DIR"

if [ ! -d "$CC_DIR/.git" ]; then
  echo "control-center: cloning into $CC_DIR"
  git clone https://github.com/nammayatri/control-center "$CC_DIR"
else
  echo "control-center: pulling latest in $CC_DIR"
  git -C "$CC_DIR" pull --ff-only || echo "control-center: pull failed, continuing with existing checkout"
fi

# Optional: a specific ref (branch name or commit SHA) requested by the
# dashboard's launcher dropdown. Fetched fresh so refs that landed since
# the last pull are still resolvable, then detached onto the resolved
# commit so the build runs from exactly that tree.
if [ -n "${CC_REF:-}" ]; then
  echo "control-center: checking out ref $CC_REF"
  git -C "$CC_DIR" fetch origin "$CC_REF" --tags 2>/dev/null \
    || git -C "$CC_DIR" fetch origin --tags
  if ! git -C "$CC_DIR" checkout --detach FETCH_HEAD 2>/dev/null; then
    git -C "$CC_DIR" checkout --detach "$CC_REF"
  fi
  git -C "$CC_DIR" log -1 --oneline
fi

cd "$CC_DIR"

# Hand off to a `nix shell` with modern node on PATH. `--inputs-from` lets us
# refer to the repo's `nixpkgs-unstable` flake input by name without naming a
# specific commit hash.
exec nix shell --inputs-from "$REPO_ROOT" \
  "nixpkgs-unstable#nodejs_22" "nixpkgs-unstable#git" "nixpkgs-unstable#gh" \
  --command bash -c '
  set -euo pipefail
  echo "node: $(node --version)  npm: $(npm --version)"
  npm install
  export VITE_BPP_URL=http://localhost:8018
  export VITE_BAP_URL=http://localhost:8017
  exec npm run dev
'
