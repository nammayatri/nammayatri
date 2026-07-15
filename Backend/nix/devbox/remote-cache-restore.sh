#!/usr/bin/env bash
# Standalone port of the `cache-restore` process body
# (Backend/nix/services/nammayatri.nix cache-restore) for use by
# `, run-cabal-build-devbox`. Runs in the Backend/ cwd on the dev-box; reads
# .ci-cache-sha (written by the deploy step) and restores dist-newstyle + .hie +
# .ci-ghc-path + .ci-project-root + .ci-cabal-dir from MinIO so that the
# subsequent `cabal build all` only rebuilds the delta. Byte-faithful to the
# process-compose version — keep them in sync.
set -euo pipefail

DIST_DIR="dist-newstyle"

# ── Skip download if local build already has HIE files ──
# After a full build, local dist-newstyle has HIE files.
# Re-downloading the CI cache would lose them and trigger a
# full rebuild. Only download when HIE files are missing.
if [ -d "$DIST_DIR/build" ]; then
  HIE_FILE=$(find -L . -name "*.hie" -path "*/.hie/*" -print -quit 2>/dev/null || true)
  if [ -n "$HIE_FILE" ]; then
    echo "cache-restore: .hie files found, skipping download"
    exit 0
  fi
fi

# ── MinIO credentials ──
# Prefer /etc/service_discovery.json (written by github-runner daemon);
# fall back to chicken's local IP for devbox use.
MINIO_ENDPOINT=""
MINIO_ACCESS_KEY=""
MINIO_SECRET_KEY=""
if [ -f /etc/service_discovery.json ]; then
  MINIO_ENDPOINT=$(jq -r '.minio_endpoint // empty' /etc/service_discovery.json)
  MINIO_ACCESS_KEY=$(jq -r '.minio_access_key // empty' /etc/service_discovery.json)
  MINIO_SECRET_KEY=$(jq -r '.minio_secret_key // empty' /etc/service_discovery.json)
fi
if [ -z "$MINIO_ENDPOINT" ]; then
  MINIO_ENDPOINT="http://192.168.10.120:9000"
  MINIO_ACCESS_KEY="minioadmin"
  MINIO_SECRET_KEY="MinioAdmin@123"
fi

# Set up the alias (just saves config, doesn't test connectivity).
mc alias set nycache "$MINIO_ENDPOINT" "$MINIO_ACCESS_KEY" "$MINIO_SECRET_KEY" || true
# Actually test connectivity by listing the bucket.
if ! mc ls nycache/haskell-cache/ >/dev/null 2>&1; then
  echo "cache-restore: cannot reach MinIO at $MINIO_ENDPOINT, skipping"
  exit 0
fi
echo "cache-restore: connected to MinIO at $MINIO_ENDPOINT"

# ── Pick the commit whose dist-newstyle cache to restore ──
# The dev's Mac (which has .git) computes the single nearest
# 'minio-pushed'-tagged commit and drops it in .ci-cache-sha.
# This lets deploys skip .git entirely — no git history here.
# Empty file (no tagged ancestor) or not-in-MinIO => build from
# scratch.
FOUND_SHA=$(tr -d '[:space:]' < .ci-cache-sha 2>/dev/null || true)
if [ -z "$FOUND_SHA" ]; then
  echo "cache-restore: no minio-pushed cache commit in .ci-cache-sha, building from scratch"
  exit 0
fi
if ! mc stat "nycache/haskell-cache/cabal-build/$FOUND_SHA/dist-newstyle.tar.zst" >/dev/null 2>&1; then
  echo "cache-restore: cache commit $FOUND_SHA not in MinIO, building from scratch"
  exit 0
fi
echo "cache-restore: found cache for commit $FOUND_SHA"

# ── Download and extract ──
TMPFILE=$(mktemp /tmp/dist-newstyle-XXXXXX.tar.zst)
trap 'rm -f "$TMPFILE"' EXIT

echo "cache-restore: downloading dist-newstyle.tar.zst from MinIO..."
if ! mc cp "nycache/haskell-cache/cabal-build/$FOUND_SHA/dist-newstyle.tar.zst" "$TMPFILE" 2>/dev/null; then
  echo "cache-restore: download failed, skipping"
  exit 0
fi

echo "cache-restore: extracting..."
rm -rf "$DIST_DIR"
tar -I 'zstd -T0' -xf "$TMPFILE" -C .
if [ ! -d "$DIST_DIR" ]; then
  echo "cache-restore: ERROR — tar extraction did not create $DIST_DIR"
  echo "cache-restore: tar contents:" && tar -I 'zstd -T0' -tf "$TMPFILE" | head -5
  echo "cache-restore: cwd=$(pwd), ls:" && ls -la
  exit 1
fi
echo "cache-restore: extracted $DIST_DIR ($(du -sh "$DIST_DIR" | cut -f1))"

# ── Link HIE dirs from tarball ──
# CI includes hie/ in the tarball alongside dist-newstyle.
# Set up .hie symlinks so GHC finds them and skips
# [HIE file is out of date] recompilation.
if [ -d "hie" ]; then
  grep -E '^[[:space:]]+(app|lib|test)' cabal.project 2>/dev/null | awk '{print $1}' | while read -r pkg; do
    [ -d "$pkg" ] || continue
    if [ -d "hie/$pkg" ]; then
      rm -rf "$pkg/.hie"
      ln -sfn "$(pwd)/hie/$pkg" "$pkg/.hie"
    fi
  done
  echo "cache-restore: linked .hie dirs from CI cache"
else
  echo "cache-restore: no hie/ in tarball (older cache), HIE files will be rebuilt"
fi

# ── Detect CI's GHC derivation from the cache ──
# The cache was built with a specific ghc-9.2.7-with-packages
# derivation. We must use the SAME one for incremental builds,
# otherwise GHC sees [.a changed] on every module because
# different derivations have different package libraries.
#
# Extract GHC path from setup-config (small ~20KB file) instead
# of scanning the entire 6GB+ build directory.
CI_GHC=""
SETUP_CFG_GHC=""
SETUP_CFG_GHC=$(find "$DIST_DIR" -name "setup-config" -type f -print -quit 2>/dev/null || true)
if [ -n "$SETUP_CFG_GHC" ]; then
  CI_GHC=$(strings "$SETUP_CFG_GHC" \
    | grep -m1 -oE '/nix/store/[a-z0-9]+-ghc-[0-9.]+-with-packages/bin/ghc' || true)
fi

if [ -n "$CI_GHC" ] && [ -x "$CI_GHC" ]; then
  echo "$CI_GHC" > "$DIST_DIR/.ci-ghc-path"
  CURRENT_GHC=$(command -v ghc || echo "")
  if [ "$CI_GHC" != "$CURRENT_GHC" ]; then
    echo "cache-restore: CI used $CI_GHC"
    echo "cache-restore: shell has $CURRENT_GHC"
    echo "cache-restore: cabal-build will use CI's GHC for incremental build"
  else
    echo "cache-restore: GHC derivation matches current shell — perfect"
  fi
elif [ -n "$CI_GHC" ]; then
  echo "cache-restore: WARNING — found CI GHC at $CI_GHC but it is not executable"
  echo "cache-restore: check if NFS mount from chicken includes this derivation"
else
  echo "cache-restore: WARNING — could not detect CI's GHC derivation from cache"
  echo "cache-restore: incremental build may recompile everything"
fi

# ── Read CI's project root and cabal-dir from MinIO ──
# CI pushes build-path.txt and cabal-dir-path.txt alongside the tarball.
CI_PROJECT_ROOT=""
CI_CABAL_DIR=""
BUILD_PATH_TMP=$(mktemp /tmp/build-path-XXXXXX.txt)
CABAL_DIR_TMP=$(mktemp /tmp/cabal-dir-path-XXXXXX.txt)
if mc cp "nycache/haskell-cache/cabal-build/$FOUND_SHA/build-path.txt" "$BUILD_PATH_TMP" 2>/dev/null; then
  CI_PROJECT_ROOT="$(cat "$BUILD_PATH_TMP")/Backend"
fi
if mc cp "nycache/haskell-cache/cabal-build/$FOUND_SHA/cabal-dir-path.txt" "$CABAL_DIR_TMP" 2>/dev/null; then
  CI_CABAL_DIR="$(cat "$CABAL_DIR_TMP")"
fi
rm -f "$BUILD_PATH_TMP" "$CABAL_DIR_TMP"

if [ -n "$CI_PROJECT_ROOT" ]; then
  echo "$CI_PROJECT_ROOT" > .ci-project-root
  echo "cache-restore: CI project root: $CI_PROJECT_ROOT"
fi
if [ -n "$CI_CABAL_DIR" ]; then
  echo "$CI_CABAL_DIR" > .ci-cabal-dir
  echo "cache-restore: CI CABAL_DIR: $CI_CABAL_DIR"
fi

echo "cache-restore: done — dist-newstyle restored from $FOUND_SHA"
