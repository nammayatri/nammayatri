#!/usr/bin/env bash
# Mirror rider-dashboard's RiderPlatform modules into provider-dashboard.
#
# WHY: during the dashboard unification (docs/access-unification/PLAN.md in the
# control-center repo) provider-dashboard serves BOTH route trees, but NammaDSL
# writes the BAP side into rider-dashboard (its dsl-config.dhall points there,
# and rider-dashboard must keep receiving them because it serves prod BAP
# traffic until the cutover). So after every codegen run, new/changed BAP
# modules must be mirrored across.
#
# The generated code is now provider-compatible as-is: the RiderPlatform specs
# carry `importPackageOverrides` for every module that both platform packages
# expose, so NammaDSL emits `"rider-app"`-qualified imports (and `"this"` on the
# rider-app side). The qualify() pass below is therefore a NO-OP for correctly
# specced APIs — it stays as a safety net for a new spec that forgets an
# override, and --check will flag the resulting drift.
#
# Run this after `run-generator` (or any spec change) and before building:
#     Backend/dev/sync-rider-dashboard-modules.sh
#     Backend/dev/sync-rider-dashboard-modules.sh --check   # CI: fail on drift
#
# Retire this script (and the copies) in Phase 7 when rider-dashboard is deleted
# and the generator is pointed straight at the merged app.
set -euo pipefail

ROOT="$(git rev-parse --show-toplevel)"
RD="$ROOT/Backend/app/dashboard/rider-dashboard"
PD="$ROOT/Backend/app/dashboard/provider-dashboard"
CHECK_ONLY=false
[[ "${1:-}" == "--check" ]] && CHECK_ONLY=true

# Never copied: provider has its own, or the module is intentionally dropped.
EXCLUDES=(
  "API.hs"                          # app-level API tree; provider mounts BAP itself
  "App.hs"                          # app entrypoint
  "Storage/Beam/CommonInstances.hs" # schema-bound orphan instances, per app
  "API/BharatTaxi"                  # excluded from the merge (PLAN 2026-08-05)
)

is_excluded() {
  local rel="$1"
  for e in "${EXCLUDES[@]}"; do [[ "$rel" == "$e" || "$rel" == "$e"/* ]] && return 0; done
  return 1
}

# provider-dashboard depends on BOTH platform apps, so module names exposed by
# rider-app AND dynamic-offer-driver-app become ambiguous there. rider-dashboard
# only depends on rider-app, so its imports are unqualified — qualify them on the
# way in. (Provider's own generated modules already do this for driver-app.)
AMBIGUOUS_LIST="$(mktemp)"
trap 'rm -f "$AMBIGUOUS_LIST"' EXIT
extract_modules() {
  awk '/^ *(exposed-modules|other-modules):/{f=1;next} /^ *[a-z-]+:/{f=0} f && $1 ~ /^[A-Z]/{print $1}' "$1"
}
comm -12 \
  <(extract_modules "$ROOT/Backend/app/rider-platform/rider-app/Main/rider-app.cabal" | sort -u) \
  <(extract_modules "$ROOT/Backend/app/provider-platform/dynamic-offer-driver-app/Main/dynamic-offer-driver-app.cabal" | sort -u) \
  > "$AMBIGUOUS_LIST"

qualify() { # stdin -> stdout, adding "rider-app" to ambiguous imports
  awk -v amb="$AMBIGUOUS_LIST" '
    BEGIN { while ((getline m < amb) > 0) ambiguous[m] = 1 }
    /^import / && $0 !~ /"/ {
      if ($2 == "qualified") {
        if ($3 in ambiguous) sub(/^import qualified /, "import qualified \"rider-app\" ")
      } else if ($2 in ambiguous) {
        sub(/^import /, "import \"rider-app\" ")
      }
    }
    { print }'
}

# The copies are formatted by treefmt/ormolu after landing, while the generated
# originals are raw generator output, so byte comparison reports permanent false
# drift (e.g. ormolu parenthesises promoted constructors: 'DSL -> ('DSL)).
# Compare with formatting normalised: collapse whitespace and drop parens, which
# ormolu only ever adds/removes around already-valid expressions.
normalise() { tr -d '() \t' < "$1" | grep -v '^$'; }
same_modulo_formatting() { diff -q <(normalise "$1") <(normalise "$2") >/dev/null 2>&1; }

changed=0
copied=0
for base in src src-read-only; do
  [[ -d "$RD/$base" ]] || continue
  while IFS= read -r rel; do
    is_excluded "$rel" && continue
    src="$RD/$base/$rel"; dst="$PD/$base/$rel"
    tmp="$(mktemp)"; qualify < "$src" > "$tmp"
    if [[ ! -f "$dst" ]] || ! same_modulo_formatting "$tmp" "$dst"; then
      changed=$((changed + 1))
      if $CHECK_ONLY; then
        echo "OUT OF SYNC: $base/$rel"
      else
        mkdir -p "$(dirname "$dst")"; cp "$tmp" "$dst"; copied=$((copied + 1))
        echo "synced: $base/$rel"
      fi
    fi
    rm -f "$tmp"
  done < <(cd "$RD/$base" && find . -name '*.hs' | sed 's|^\./||' | sort)
done

if $CHECK_ONLY; then
  if (( changed > 0 )); then
    echo
    echo "$changed rider-dashboard module(s) are missing or stale in provider-dashboard."
    echo "Run: Backend/dev/sync-rider-dashboard-modules.sh"
    exit 1
  fi
  echo "provider-dashboard is in sync with rider-dashboard ($(( ${#EXCLUDES[@]} )) exclusions)."
else
  echo
  echo "synced $copied file(s)."
  (( copied > 0 )) && echo "NOTE: new modules must also be added to provider-dashboard.cabal (hpack regenerates it)."
fi
