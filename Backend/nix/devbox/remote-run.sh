#!/usr/bin/env bash
# Remote orchestrator for `, run-cabal-build-devbox`. Invoked over `ssh -tt` on
# the dev-box after the local repo has been rsync'd and Backend/.ci-cache-sha
# written. Runs cache-restore (download the CI dist-newstyle from MinIO) then
# `cabal build all`, streaming to the caller's terminal.
#   $1 = absolute path of the deployed Backend dir (…/nammayatri/Backend)
#   $2 = clean flag: "1" → cabal clean (wipe dist-newstyle) before cache-restore
set -euo pipefail

BACKEND_DIR="${1:?usage: remote-run.sh <backend-dir> [clean:0|1]}"
CLEAN="${2:-0}"

# Ensure nix is on PATH for a non-login ssh shell.
export PATH="$HOME/.nix-profile/bin:/nix/var/nix/profiles/default/bin:$PATH"

cd "$BACKEND_DIR"

HERE="$BACKEND_DIR/nix/devbox"

if [ "$CLEAN" = "1" ]; then
  echo "== cabal clean (wiping dist-newstyle + hie for a fresh cache-restore) =="
  # Keep .ci-cache-sha — it's the cache pointer cache-restore needs. Drop the
  # build tree, the HIE storage/symlinks, and the CI-path markers so cache-restore
  # pulls a clean CI cache instead of skipping on the existing build.
  rm -rf dist-newstyle hie .ci-project-root .ci-cabal-dir .cabal-dir .nix-deps
  find . -maxdepth 5 -type l -name .hie -delete 2>/dev/null || true
fi

echo "== cache-restore =="
# cache-restore needs mc/zstd/tar/strings(binutils)/jq/git etc. — the same
# runtimeInputs the process-compose cache-restore pins — not the backend devshell.
nix shell \
  nixpkgs#coreutils nixpkgs#gnutar nixpkgs#gnugrep nixpkgs#gnused \
  nixpkgs#git nixpkgs#jq nixpkgs#minio-client nixpkgs#zstd \
  nixpkgs#findutils nixpkgs#binutils \
  -c bash "$HERE/remote-cache-restore.sh"

echo "== cabal build all =="
# cabal-build needs the backend devshell (ghc/ghc-pkg/cabal) plus util-linux
# (unshare) for the CI mount namespace.
nix develop .#backend -c \
  nix shell nixpkgs#util-linux \
  -c bash "$HERE/remote-cabal-build-all.sh"

echo "== done =="
