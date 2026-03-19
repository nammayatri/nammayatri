# GHC Warning Strategy

## 1. Current State Analysis

**51 packages** in `Backend/`, of which **50 use blanket `-Werror`** (only `hunit-tests` does not).

Common flag set across all 50 packages (set in each `package.yaml`, propagated to `.cabal`):

| Flag | Count | Purpose |
|------|-------|---------|
| `-Wall` | 73 stanzas | Enable all standard warnings |
| `-Wcompat` | 73 | Forward-compatibility warnings |
| `-Widentities` | 73 | Useless conversions (e.g., `fromIntegral` on same type) |
| `-Werror` | 73 | **All warnings are fatal** |
| `-Wwarn=ambiguous-fields` | 38 | Downgrade ambiguous-fields back to warning |
| `-Wincomplete-uni-patterns` | 20 | Incomplete pattern matches (non-exhaustive) |
| `-Wunused-imports` | varies | Unused imports |
| `-Wno-unrecognised-pragmas` | 1 | Suppress unrecognised pragma warnings |

Additionally, `cabal.project` sets `-Wunused-packages` globally via `package *`.

**Key observation**: Every library and application uses `-Wall -Werror` with a single carve-out (`-Wwarn=ambiguous-fields`). All warnings are treated equally as errors.

---

## 2. The Problem with Blanket `-Werror`

### GHC Upgrades
Each GHC release adds new warnings to `-Wall`. A GHC upgrade immediately breaks the entire build even when code is correct. The team must fix every new warning across 50 packages before any work can proceed.

### Auto-Generated Code
NammaYatri uses `alchemist` to generate Haskell modules. Generated code may trigger warnings (unused imports, name shadowing) that developers cannot fix at the source.

### Developer Experience
A developer adding a function gets blocked by an unused import in a module they touched. This interrupts flow and discourages incremental compilation during development.

### Equal Treatment of Warnings
`-Werror` makes `-Wincomplete-patterns` (a crash waiting to happen) and `-Wname-shadowing` (a style preference) equally fatal. They are not equally dangerous.

### Cross-Package Cascade
A new warning in one `lib/` package forces fixes in every downstream `app/` package before anything compiles.

---

## 3. Warning Classification

All warnings enabled by `-Wall` (GHC 9.2 -- 9.8), classified by severity.

### MUST BE ERROR (crash-class)

These warnings indicate code that **will crash or silently misbehave** at runtime.

| Warning | Why |
|---------|-----|
| `-Wincomplete-patterns` | Non-exhaustive case: runtime crash |
| `-Wincomplete-uni-patterns` | Non-exhaustive in lambda/let: runtime crash |
| `-Wmissing-fields` | Record construction missing fields: bottom at runtime |
| `-Wmissing-methods` | Typeclass instance missing methods: bottom at runtime |
| `-Woverlapping-patterns` | Dead code, indicates logic error |
| `-Winaccessible-code` | Type system proves code unreachable |
| `-Wincomplete-record-updates` | Partial record update: runtime crash |

### SHOULD BE ERROR (likely bugs)

These warnings indicate code that is **probably wrong** but may not crash immediately.

| Warning | Why |
|---------|-----|
| `-Wunused-matches` | Bound variable never used; may indicate dropped value |
| `-Wunused-foralls` | Quantified type variable unused; polymorphism error |
| `-Wmissing-home-modules` | Module not listed in exposed/other-modules |
| `-Widentities` | Useless conversion (e.g., `fromIntegral @Int @Int`) |
| `-Wderiving-defaults` | Ambiguous deriving strategy |
| `-Wpartial-fields` | Record field accessor is partial function |
| `-Wunused-do-bind` | Monadic result silently discarded |
| `-Wtyped-holes` | `_` in term position: incomplete code |
| `-Wdeferred-type-errors` | Type error deferred to runtime |
| `-Wdeferred-out-of-scope-variables` | Out-of-scope variable deferred to runtime |

### WARNING ONLY (noisy but not dangerous)

These warnings are **useful signals** but should not block compilation.

| Warning | Why noise |
|---------|-----------|
| `-Wunused-imports` | Constantly triggered during development; auto-fixable |
| `-Wunused-top-binds` | Functions written for future use |
| `-Wunused-local-binds` | Intermediate bindings during debugging |
| `-Wname-shadowing` | Common with `do`-notation and record fields |
| `-Worphans` | Sometimes architecturally necessary |
| `-Wredundant-constraints` | Noisy with MTL-style code |
| `-Wambiguous-fields` | Already demoted via `-Wwarn=ambiguous-fields` in 38 stanzas |
| `-Wmissing-signatures` | Not in `-Wall` but often enabled; noisy for local binds |
| `-Wtype-defaults` | Numeric defaulting; usually harmless |
| `-Wduplicate-exports` | Rare, harmless |
| `-Wdeprecations` | Upstream deprecations outside our control |
| `-Wunused-packages` | Already set globally in `cabal.project`; slow to fix |
| `-Wmissed-specialisations` | Performance hint, not correctness |

---

## 4. Per-Package Strategy

### Business Logic (`app/provider-platform/`, `app/rider-platform/`)

Highest severity. Crash-class and likely-bug warnings as errors. Noisy warnings remain warnings.

### Libraries (`lib/`)

Same as business logic. Libraries are shared, so catching bugs early is valuable.

### Auto-Generated Code (`alchemist` output)

Only crash-class warnings as errors. Likely-bug and noisy warnings remain warnings. Developers cannot meaningfully fix generated code.

### Tests (`test/`, `hunit-tests/`)

Warnings only (no `-Werror`). Tests change rapidly and unused bindings are expected during development.

### Mocks (`app/mocks/`)

Only crash-class warnings as errors. Mock code is not production.

---

## 5. Recommended `cabal.project` Configuration

Replace per-package `-Werror` with centralized warning policy in `cabal.project`:

```cabal
-- =============================================================
-- Warning policy: selective -Werror
-- =============================================================

package *
  ghc-options:
    -Wall
    -Wcompat
    -Wunused-packages

    -- MUST BE ERROR (crash-class)
    -Werror=incomplete-patterns
    -Werror=incomplete-uni-patterns
    -Werror=missing-fields
    -Werror=missing-methods
    -Werror=overlapping-patterns
    -Werror=inaccessible-code
    -Werror=incomplete-record-updates

    -- SHOULD BE ERROR (likely bugs)
    -Werror=unused-matches
    -Werror=unused-foralls
    -Werror=missing-home-modules
    -Werror=identities
    -Werror=deriving-defaults
    -Werror=partial-fields
    -Werror=unused-do-bind
    -Werror=typed-holes
    -Werror=deferred-type-errors
    -Werror=deferred-out-of-scope-variables

    -- Everything else stays as warning (unused-imports, name-shadowing, etc.)
    -Wwarn=ambiguous-fields

-- Tests: no -Werror at all (fast iteration)
package hunit-tests
  ghc-options: -Wwarn

package nammayatri-test
  ghc-options: -Wwarn

-- Mocks: only crash-class as errors
package mock-sms
  ghc-options: -Wwarn -Werror=incomplete-patterns -Werror=incomplete-uni-patterns -Werror=missing-fields -Werror=missing-methods

package mock-fcm
  ghc-options: -Wwarn -Werror=incomplete-patterns -Werror=incomplete-uni-patterns -Werror=missing-fields -Werror=missing-methods

-- Note: alchemist-generated modules live inside app packages.
-- Use per-module {-# OPTIONS_GHC -Wwarn #-} pragmas for generated files
-- rather than per-package overrides, since generated and hand-written code
-- coexist in the same package.
```

This gives CI the same safety guarantees for dangerous warnings while allowing noisy warnings to exist without blocking builds. The per-package overrides implement the tiered strategy from Section 4.

---

## 6. Developer Workflow

### `cabal.project.local` (gitignored)

Developers who want zero warning noise during iteration:

```cabal
-- Copy from cabal.project.local.example
package *
  ghc-options: -Wwarn
```

This overrides all `-Werror=*` flags, letting developers compile freely. CI still enforces the policy.

### Editor Integration

HLS respects `cabal.project.local`. With `-Wwarn`, squiggly lines appear but compilation succeeds.

### Pre-Push Check

Developers can verify CI compliance before pushing:

```bash
# Remove cabal.project.local temporarily, or:
cabal build all --project-file=cabal.project
```

---

## 7. Implementation Phases (The Ratchet Pattern)

See [ghc-warning-phases.md](ghc-warning-phases.md) for the detailed checklist.

| Phase | Description | Timeline |
|-------|-------------|----------|
| **1** | Add `cabal.project.local.example` | Week 1 |
| **2** | Move common flags from `package.yaml` to `cabal.project` | Week 2 |
| **3** | Replace `-Werror` with selective `-Werror=*` in `cabal.project` | Week 3 |
| **4** | Remove per-package `-Werror` from all 50 `package.yaml` files | Week 3 |
| **5** | Fix existing warnings that are now errors under new policy | Week 4-5 |
| **6** | CI enforcement and cleanup | Week 6 |

The ratchet pattern: each phase only tightens or maintains the warning level, never loosens it for crash-class warnings.

---

## 8. What Major Haskell Projects Do

| Project | Strategy |
|---------|----------|
| **GHC itself** | `-Wall` in CI, no `-Werror`; uses `-Werror` only in validate mode |
| **Pandoc** | `-Wall -fno-warn-unused-do-bind`, no `-Werror` |
| **Servant** | `-Wall -Wno-redundant-constraints`, no `-Werror` |
| **IHP** | `-Wall`, `-Werror` in CI only |
| **Hasura** | Selective `-Werror=incomplete-patterns` and friends, not blanket |
| **Cabal (the tool)** | `-Wall`, no `-Werror` in package; CI script adds it |
| **Stack** | `-Wall`, `-Werror` in CI via `stack.yaml` flag |

The consensus in the Haskell ecosystem is: **`-Wall` always, `-Werror` selectively or only in CI**, never blanket `-Werror` in the package description.
