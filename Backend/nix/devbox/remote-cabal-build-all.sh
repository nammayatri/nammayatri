#!/usr/bin/env bash
# Standalone port of the `cabal-build` process body
# (Backend/nix/services/nammayatri.nix cabal-build) for use by
# `, run-cabal-build-devbox`. The ONLY functional change vs. the process-compose
# version is TARGETS="all" (build every package) instead of the enumerated
# service exes. Keeps the unshare CI namespace so the restored dist-newstyle
# fingerprints match CI's original project-root / cabal-dir — dropping it forces
# a full from-scratch rebuild. Runs in the Backend/ cwd, inside `nix develop
# .#backend` (ghc/ghc-pkg/cabal) with util-linux (unshare) on PATH. Linux only.
set -euo pipefail

GHC_FLAG=""
if [ -f dist-newstyle/.ci-ghc-path ]; then
  CI_GHC=$(cat dist-newstyle/.ci-ghc-path)
  if [ -x "$CI_GHC" ]; then
    GHC_FLAG="-w $CI_GHC"
    echo "cabal-build: using CI's GHC: $CI_GHC"
  else
    echo "cabal-build: CI GHC not found at $CI_GHC, using shell GHC"
  fi
fi

# devbox variant: build every package/component (the process-compose version
# builds only the enumerated service exes accumulated from CABAL_TARGET).
TARGETS="all"

# If .ci-project-root doesn't exist but dist-newstyle has CI
# paths (from a previous cache-restore), detect them from
# setup-config so we still use the namespace.
if [ ! -f .ci-project-root ] && [ -d dist-newstyle/build ]; then
  SETUP_CFG=$(find dist-newstyle -name "setup-config" -type f -print -quit 2>/dev/null || true)
  if [ -n "$SETUP_CFG" ]; then
    DETECTED_ROOT=$(strings "$SETUP_CFG" \
      | grep -m1 -oE '/[^ "]+/nammayatri/Backend' \
      | sed 's|/Backend.*|/Backend|' || true)
    if [ -n "$DETECTED_ROOT" ] && [ "$DETECTED_ROOT" != "$(pwd)" ]; then
      echo "$DETECTED_ROOT" > .ci-project-root
      # Detect cabal-dir too
      DETECTED_STORE_DB=$(strings "$SETUP_CFG" \
        | grep -oE '/[^ "]+/store/ghc-[0-9.]+/package\.db' \
        | grep -v '/nix/store/' | head -1 || true)
      if [ -n "$DETECTED_STORE_DB" ]; then
        echo "${DETECTED_STORE_DB%%/store/*}" > .ci-cabal-dir
      fi
      echo "cabal-build: auto-detected CI project root from setup-config: $DETECTED_ROOT"
    fi
  fi
fi

if [ -f .ci-project-root ]; then
  CI_PROJECT_ROOT=$(cat .ci-project-root)
  CI_CABAL_DIR=$(cat .ci-cabal-dir 2>/dev/null || echo "")
  LOCAL_ROOT=$(pwd)
  echo "cabal-build: local root:  $LOCAL_ROOT"
  echo "cabal-build: CI root:     $CI_PROJECT_ROOT"
  echo "cabal-build: CI cabal-dir: $CI_CABAL_DIR"

  # Derive CI user's home (/home/<user>) from the project root.
  # We mount tmpfs there so we can create both the project root
  # and cabal-dir directories inside the namespace.
  CI_HOME=$(echo "$CI_PROJECT_ROOT" | cut -d/ -f1-3)
  GHC_VER=$(ghc --numeric-version)

  # Build overlay commands in the outer shell to avoid
  # escaping issues inside bash -c "...".
  # Overlays non-reproducible .a files from CI tarball over
  # the nix store so GHC's addDependentFile fingerprint matches.
  # Uses /tmp/ns-root reference — LOCAL_ROOT is hidden after tmpfs /home.
  # Overlay individual .a files from CI tarball over nix store.
  # Must bind-mount FILES not directories — mounting a directory
  # hides all other files (shared libs, .conf) that ghc-pkg needs.
  NIX_OVERLAY=""
  if [ -d "$LOCAL_ROOT/.nix-deps/nix/store" ]; then
    while IFS= read -r dep_file; do
      rel="${dep_file#"$LOCAL_ROOT"/.nix-deps}"
      if [ -f "$rel" ]; then
        NIX_OVERLAY="$NIX_OVERLAY mount --bind /tmp/ns-root/.nix-deps$rel $rel &&"
        echo "cabal-build: will overlay file $rel"
      else
        echo "cabal-build: WARNING — $rel not found on this machine, cannot overlay"
      fi
    done < <(find "$LOCAL_ROOT/.nix-deps/nix/store" -name "*.a" -type f 2>/dev/null)
  fi

  # shellcheck disable=SC2086
  unshare --user --mount --map-root-user bash -c "
    set -e
    export LC_ALL=C
    # Save local root before tmpfs hides /home
    mkdir -p /tmp/ns-root
    mount --bind $LOCAL_ROOT /tmp/ns-root
    mount -t tmpfs tmpfs /home
    mkdir -p $CI_HOME
    mkdir -p $CI_PROJECT_ROOT
    mount --bind /tmp/ns-root $CI_PROJECT_ROOT

    if [ -n '$CI_CABAL_DIR' ]; then
      mkdir -p $CI_CABAL_DIR
      if [ -d /tmp/ns-root/.cabal-dir ]; then
        mount --bind /tmp/ns-root/.cabal-dir $CI_CABAL_DIR
        # Create empty store (excluded from tarball to avoid
        # stale package registrations from CI runners)
        mkdir -p $CI_CABAL_DIR/store/ghc-$GHC_VER
        ghc-pkg init $CI_CABAL_DIR/store/ghc-$GHC_VER/package.db 2>/dev/null || true
        echo cabal-build: bind-mounted CI cabal-dir from cache
      else
        mkdir -p $CI_CABAL_DIR/store/ghc-$GHC_VER
        ghc-pkg init $CI_CABAL_DIR/store/ghc-$GHC_VER/package.db 2>/dev/null || true
        echo cabal-build: created empty cabal-dir '(no .cabal-dir in cache)'
      fi
      export CABAL_DIR=$CI_CABAL_DIR
      echo cabal-build: CABAL_DIR=$CI_CABAL_DIR
    fi

    $NIX_OVERLAY true

    cd $CI_PROJECT_ROOT
    echo cabal-build: pwd=\$(pwd)
    cabal build $GHC_FLAG $TARGETS 2>&1
  "
else
  echo "cabal-build: no CI project root detected, building normally"
  # Don't use CI's GHC without the namespace — it references
  # CI runner paths (/home/khuzema/...) that don't exist here.
  # shellcheck disable=SC2086
  cabal build $TARGETS
fi
