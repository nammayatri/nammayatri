# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.
Detailed topic docs live in `.cursor/docs/` — read the relevant one(s) for your current task.

## Critical Rules (Always Apply)

1. **NEVER edit files in `src-read-only/`** — these are generated from YAML specs via NammaDSL
2. **Always run `cabal build all`** after code generation to verify correctness
3. **Project uses `-Werror`** — all GHC warnings are compile errors (unused imports, dodgy imports, etc.)
4. **ID generation**: `newId <- generateGUID` (from `Kernel.Utils.Common`)
5. **Error handling**: `entity <- QEntity.findById id >>= fromMaybeM (EntityNotFound id.getId)`
6. **DB inserts**: Call `create` from `Storage.Queries` directly; never wrap single creates in `runInTransaction`
7. **YAML imports**: Use full module paths (e.g., `Domain.Types.IntegratedBPPConfig`), not short names
8. **Beckn tags**: Must be defined in `Backend/lib/beckn-spec/src/BecknV2/OnDemand/Tags.hs` before use
9. **Orphan instances**: Go in `Domain/Types/Extra/*.hs` files
10. **Logging**: Use `logInfo`, `logDebug`, `logError` from `Kernel.Utils.Logging`

## Build & Development

```bash
# Environment setup (one-time, from project root)
ln -sf .envrc.backend .envrc && direnv allow

# Backend
cd Backend
cabal build all                  # Build everything
cabal build <package-name>       # Build specific package (e.g., rider-app)

# Code generation (run from Backend/ inside nix shell)
, run-generator                  # Only changed specs
, run-generator --all            # All specs
, run-generator --apply-hint     # With HLint auto-fixes

# Utilities
, run-mobility-stack-dev         # Start external services (Postgres, Redis, Kafka, etc.)
, ghcid lib/<package-name>       # Fast compile feedback loop
, hpack                          # Regenerate .cabal files from package.yaml
, kill-svc-ports                 # Kill lingering service processes
```

Full build details: `.cursor/docs/02-build-and-dev.md`

## Architecture — Quick Reference

| Service | Port | Path |
|---------|------|------|
| rider-app (BAP) | 8013 | `app/rider-platform/rider-app/` |
| dynamic-offer-driver-app (BPP) | 8016 | `app/provider-platform/dynamic-offer-driver-app/` |
| driver-offer-allocator | 9996 | `app/provider-platform/dynamic-offer-driver-app/Allocator/` |

| Database | Rider Schema | Driver Schema |
|----------|-------------|---------------|
| PostgreSQL | `atlas_app` | `atlas_driver_offer_bpp` |
| Redis | Single 6379 / Cluster 30001 | Same |

BAP (rider-app) initiates BECKN calls → BPP (driver-app) responds with callbacks.
ACL modules translate between BECKN protocol types and internal domain types.

Full architecture: `.cursor/docs/01-architecture-overview.md`

## Key Directory Patterns

| Purpose | Path Pattern |
|---------|-------------|
| Domain types | `*/src-read-only/Domain/Types/` |
| Business logic | `*/src/Domain/Action/UI/` |
| DB queries | `*/src-read-only/Storage/Queries/` |
| Extra queries | `*/src/Storage/Queries/` |
| Cached queries | `*/src/Storage/CachedQueries/` or `*/src-read-only/Storage/CachedQueries/` |
| YAML API specs | `*/spec/API/*.yaml` |
| YAML Storage specs | `*/spec/Storage/*.yaml` |
| Beckn ACL | `*/src/Beckn/ACL/` |
| SharedLogic | `*/src/SharedLogic/` |
| Migrations | `dev/migrations/<service-name>/` |

## Code Generation (NammaDSL)

- **API specs**: `spec/API/*.yaml` → generates `src-read-only/API/`
- **Storage specs**: `spec/Storage/*.yaml` → generates `src-read-only/Domain/Types/`, `Storage/Beam/`, `Storage/Queries/`
- Generator also creates stub files in `src/Domain/Action/UI/` for business logic
- Use camelCase for endpoint paths, full module paths for imports
- Common auto-imported types: `Text`, `Maybe`, `Int`, `Bool`, `Id`, `UTCTime`, `HighPrecMoney`, `Currency`

Full DSL reference: `.cursor/docs/07-namma-dsl.md`

## Haskell Conventions

- `cabal build <target>` for checks; `cabal repl` alone doesn't guarantee compilability
- If a `.hs` file is deleted, run `, hpack` to update `.cabal` file
- Use `fromTType`/`toTType` in YAML for domain-to-beam type transformations
- `extraOperations`: `EXTRA_QUERY_FILE`, `EXTRA_DOMAIN_TYPE_FILE`, `EXTRA_CACHED_QUERY_FILE`

Full conventions: `.cursor/docs/15-conventions.md`

## BECKN Protocol

Flow: `search` → `on_search` → `select` → `on_select` → `init` → `on_init` → `confirm` → `on_confirm`
Protocol details: `.cursor/docs/05-beckn-protocol-flow.md`
Ride lifecycle: `.cursor/docs/06-ride-flow.md`

## FRFS (Public Transport)

- BAP-only — BPPs are external operators (CMRL, CRIS, EBIX)
- Two paths: ONDC (async Beckn) and Direct (synchronous API)
- `mkCloudBapUri` in `Beckn/ACL/FRFS/Utils.hs` handles multi-cloud callback routing
- Constraint propagation: add to type aliases in `ExternalBPP/CallAPI/Types.hs` and `SharedLogic/CallFRFSBPP.hs`

Full FRFS details: `.cursor/docs/10-frfs-public-transport.md`

## Commit / Branch Conventions

```
Commit: <sub-project>/<type>: <issue-number> <summary>
Branch: <sub-project>/<type>/<issue-number><description>
Types: feat, fix, chore, ci, docs, perf, refactor, test
```

## Deep Dive Docs (`.cursor/docs/`)

| Doc | Read when working on... |
|-----|------------------------|
| `01-architecture-overview.md` | Service map, ports, packages |
| `02-build-and-dev.md` | Build commands, nix, compilation |
| `03-rider-app.md` | Customer-facing features |
| `04-driver-app.md` | Driver-facing features |
| `05-beckn-protocol-flow.md` | BECKN protocol interactions |
| `06-ride-flow.md` | End-to-end ride lifecycle |
| `07-namma-dsl.md` | Creating/modifying YAML specs |
| `08-database-patterns.md` | Queries, caching, migrations |
| `09-dashboards.md` | Dashboard services |
| `10-frfs-public-transport.md` | Metro/bus/public transport |
| `11-libraries.md` | Shared libraries |
| `12-multi-cloud.md` | Multi-cloud, KV connector, Redis |
| `13-external-integrations.md` | Juspay, OSRM, Idfy, SMS, FCM |
| `14-testing-and-debugging.md` | Debugging patterns |
| `15-conventions.md` | Haskell conventions |
| `16-status-definitions.md` | Status enums, state transitions |
