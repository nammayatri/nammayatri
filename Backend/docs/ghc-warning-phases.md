# GHC Warning Migration Phases

Implementation checklist for migrating from blanket `-Werror` to selective `-Werror=*`.

See [ghc-warning-strategy.md](ghc-warning-strategy.md) for rationale and classification.

---

## Phase 1: Developer Ergonomics (Week 1)

- [ ] Add `Backend/cabal.project.local.example` with `package * ghc-options: -Wwarn`
- [ ] Verify `cabal.project.local` is in `.gitignore`
- [ ] Announce to team: developers can copy the example file for faster local iteration

## Phase 2: Centralize Warning Flags (Week 2)

- [ ] Audit all 51 `package.yaml` files for `ghc-options` inconsistencies
- [ ] Move shared flags (`-Wall`, `-Wcompat`, `-Widentities`, `-fwrite-ide-info`, `-hiedir=.hie`) to `cabal.project` under `package *`
- [ ] Remove duplicated flags from individual `package.yaml` files
- [ ] Verify build still succeeds with `cabal build all`

## Phase 3: Selective `-Werror` (Week 3)

- [ ] Add selective `-Werror=*` flags to `cabal.project` for crash-class warnings:
  - `-Werror=incomplete-patterns`
  - `-Werror=incomplete-uni-patterns`
  - `-Werror=missing-fields`
  - `-Werror=missing-methods`
  - `-Werror=overlapping-patterns`
  - `-Werror=inaccessible-code`
  - `-Werror=incomplete-record-updates`
- [ ] Add selective `-Werror=*` flags for likely-bug warnings:
  - `-Werror=unused-matches`
  - `-Werror=unused-foralls`
  - `-Werror=missing-home-modules`
  - `-Werror=identities`
  - `-Werror=deriving-defaults`
  - `-Werror=partial-fields`
  - `-Werror=unused-do-bind`
  - `-Werror=typed-holes`
  - `-Werror=deferred-type-errors`
  - `-Werror=deferred-out-of-scope-variables`
- [ ] Verify build still succeeds with `cabal build all`

## Phase 4: Remove Blanket `-Werror` (Week 3)

- [ ] Remove `-Werror` from all 50 `package.yaml` files that have it
- [ ] Remove `-Wwarn=ambiguous-fields` from 38 stanzas (no longer needed when `-Werror` is gone)
- [ ] Remove `-Wincomplete-uni-patterns` from 20 stanzas (now centralized)
- [ ] Remove `-Widentities` from stanzas (now centralized)
- [ ] Remove `-Wunused-imports` from stanzas (now centralized via `-Wall`)
- [ ] Verify build still succeeds with `cabal build all`
- [ ] Verify warnings appear but do not block compilation for noisy categories

## Phase 5: Fix Remaining Errors (Week 4-5)

- [ ] Run `cabal build all 2>&1 | grep "error:"` to find any new failures under selective `-Werror=*`
- [ ] Fix all crash-class warning violations (incomplete patterns, missing fields, etc.)
- [ ] Fix all likely-bug warning violations (unused matches, partial fields, etc.)
- [ ] Run full test suite to verify no regressions
- [ ] Document any cases where `-Wwarn=*` overrides are needed for specific packages

## Phase 6: CI Enforcement and Cleanup (Week 6)

- [ ] Update CI pipeline to use `cabal.project` as single source of truth for warnings
- [ ] Remove any CI-specific `-Werror` overrides
- [ ] Add CI step to count total warnings (for tracking reduction over time)
- [ ] Set up optional CI job: build with `-Werror` to track "zero-warning" progress
- [ ] Update onboarding docs to reference `cabal.project.local.example`
- [ ] Schedule quarterly review of warning classification (especially before GHC upgrades)
