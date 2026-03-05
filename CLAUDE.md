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

# Backend (run from Backend/ directory)
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

### Repository Structure

```
nammayatri/
├── Backend/                        # Haskell backend (monorepo, ~48 packages)
│   ├── app/                        # Deployable services
│   │   ├── rider-platform/         # Customer-side (BAP)
│   │   │   ├── rider-app/          # Main rider service
│   │   │   ├── rider-app-drainer/  # Redis-to-DB drainer
│   │   │   └── public-transport-rider-platform/
│   │   ├── provider-platform/      # Driver-side (BPP)
│   │   │   ├── dynamic-offer-driver-app/  # Main driver service + Allocator
│   │   │   └── dynamic-offer-driver-drainer/
│   │   ├── dashboard/              # Operations dashboards
│   │   │   ├── CommonAPIs/         # Shared dashboard types
│   │   │   ├── Lib/                # Shared dashboard logic
│   │   │   ├── rider-dashboard/
│   │   │   └── provider-dashboard/
│   │   ├── safety-dashboard/
│   │   ├── unified-dashboard/
│   │   ├── alchemist/              # NammaDSL code generator
│   │   ├── kafka-consumers/
│   │   ├── mocks/                  # Mock external services
│   │   └── special-zone/
│   ├── lib/                        # Shared libraries (see Libraries section)
│   ├── dev/migrations/             # SQL migration files
│   └── dhall-configs/dev/          # Service configuration (Dhall)
├── Frontend/                       # PureScript + Android native
│   ├── ui-customer/                # Customer app UI
│   ├── ui-driver/                  # Driver app UI
│   ├── ui-common/                  # Shared UI components
│   └── android-native/             # Android shell app
├── .cursor/docs/                   # Detailed topic documentation
└── memory-bank/                    # Cline/Windsurf context files
```

### Services & Ports

| Service | Port | Path |
|---------|------|------|
| rider-app (BAP) | 8013 | `app/rider-platform/rider-app/` |
| beckn-gateway | 8015 | (routing service) |
| dynamic-offer-driver-app (BPP) | 8016 | `app/provider-platform/dynamic-offer-driver-app/` |
| rider-dashboard | 8017 | `app/dashboard/rider-dashboard/` |
| provider-dashboard | 8018 | `app/dashboard/provider-dashboard/` |
| mock-google | 8019 | `app/mocks/` |
| mock-registry | 8020 | `app/mocks/` |
| safety-dashboard | 8025 | `app/safety-dashboard/` |
| driver-offer-allocator | 9996 | `app/provider-platform/dynamic-offer-driver-app/Allocator/` |

### Databases

| Database | Rider Schema | Driver Schema |
|----------|-------------|---------------|
| PostgreSQL | `atlas_app` | `atlas_driver_offer_bpp` |
| Redis | Single 6379 / Cluster 30001 | Same |
| ClickHouse | Analytics data | Analytics data |

BAP (rider-app) initiates BECKN calls → BPP (driver-app) responds with callbacks.
ACL modules translate between BECKN protocol types and internal domain types.

Full architecture: `.cursor/docs/01-architecture-overview.md`

## Key Directory Patterns

| Purpose | Path Pattern |
|---------|-------------|
| Domain types (generated) | `*/src-read-only/Domain/Types/` |
| Domain type extensions | `*/src/Domain/Types/Extra/` |
| Business logic | `*/src/Domain/Action/UI/` |
| DB queries (generated) | `*/src-read-only/Storage/Queries/` |
| Extra queries (hand-written) | `*/src/Storage/Queries/` |
| Beam types (generated) | `*/src-read-only/Storage/Beam/` |
| Cached queries | `*/src/Storage/CachedQueries/` or `*/src-read-only/Storage/CachedQueries/` |
| YAML API specs | `*/spec/API/*.yaml` |
| YAML Storage specs | `*/spec/Storage/*.yaml` |
| Beckn ACL | `*/src/Beckn/ACL/` |
| SharedLogic | `*/src/SharedLogic/` |
| Migrations | `dev/migrations/<service-name>/` |
| Dhall configs | `dhall-configs/dev/<service>.dhall` |

### Migration Directories

`dev/migrations/` contains SQL migrations for: `rider-app`, `dynamic-offer-driver-app`, `rider-dashboard`, `provider-dashboard`, `safety-dashboard`, `public-transport-rider-platform`, `mock-registry`, `scheduler`, `scheduler-example`, `special-zone`.

## Code Generation (NammaDSL)

- **API specs**: `spec/API/*.yaml` → generates `src-read-only/API/`
- **Storage specs**: `spec/Storage/*.yaml` → generates `src-read-only/Domain/Types/`, `Storage/Beam/`, `Storage/Queries/`
- Generator also creates stub files in `src/Domain/Action/UI/` for business logic
- The generator tool is the `alchemist` package at `Backend/app/alchemist/`
- Use camelCase for endpoint paths, full module paths for imports
- Common auto-imported types: `Text`, `Maybe`, `Int`, `Bool`, `Id`, `UTCTime`, `HighPrecMoney`, `Currency`
- Use `fromTType`/`toTType` in YAML for domain-to-beam type transformations when Haskell types differ from DB columns
- `extraOperations`: `EXTRA_QUERY_FILE`, `EXTRA_DOMAIN_TYPE_FILE`, `EXTRA_CACHED_QUERY_FILE`

Full DSL reference: `.cursor/docs/07-namma-dsl.md`

## Shared Libraries (`Backend/lib/`)

| Library | Purpose |
|---------|---------|
| `beckn-spec` | BECKN protocol types, BookingStatus, RideStatus, FRFSTicketBookingStatus |
| `beckn-services` | BECKN HTTP client for BAP/BPP communication |
| `payment` | Juspay integration (createOrder, webhookService, refund, payout) |
| `scheduler` | Redis-based background job scheduling |
| `location-updates` | OSRM tracking with snap-to-road |
| `yudhishthira` | Business rule engine (NammaTag) |
| `finance-kernel` | HighPrecMoney type and financial calculations |
| `shared-services` | IGM (Issue & Grievance Management) |
| `utils` | Common utilities shared across services |
| `external` | External service integrations |
| `special-zone` | Geofencing / special zone logic |
| `webhook` | Webhook handling |
| `producer` | Kafka producer utilities |
| `sessionizer-metrics` | Session metrics collection |
| `dashcam` | Dashcam integration |

Full library details: `.cursor/docs/11-libraries.md`

## Dashboard Services

- **Four dashboard packages**: rider-dashboard, provider-dashboard, safety-dashboard, unified-dashboard
- **CommonAPIs** (`app/dashboard/CommonAPIs/`): Shared dashboard types
- **Lib** (`app/dashboard/Lib/`): Shared dashboard logic
- Use `DashboardAuth` in YAML API specs for dashboard endpoints
- Dashboard migrations are separate: `dev/migrations/rider-dashboard/`, `dev/migrations/provider-dashboard/`, etc.

Full dashboard details: `.cursor/docs/09-dashboards.md`

## Haskell Conventions

- `cabal build <target>` for checks; `cabal repl` alone doesn't guarantee compilability
- If a `.hs` file is deleted, run `, hpack` to update `.cabal` file
- Module naming: PascalCase modules, camelCase functions, UPPER_SNAKE constructors
- Query module aliases: `Q<Entity>` for queries, `CQ<Entity>` for cached queries
- API endpoint paths: camelCase (no hyphens)
- Multiple DB operations: wrap in `runInTransaction`; single creates: no transaction needed

Full conventions: `.cursor/docs/15-conventions.md`

## BECKN Protocol

Flow: `search` → `on_search` → `select` → `on_select` → `init` → `on_init` → `confirm` → `on_confirm`

Additional steps: `status`/`on_status`, `track`/`on_track`, `cancel`/`on_cancel`, `update`/`on_update`, `rating`

- **Outgoing ACL**: domain types → BECKN protocol types
- **Incoming ACL**: BECKN protocol types → domain types
- OnDemand transformer modules handle ride-hailing specifics
- IGM (Issue & Grievance Management) has its own ACL modules on both BAP and BPP

Protocol details: `.cursor/docs/05-beckn-protocol-flow.md`
Ride lifecycle (8 phases): `.cursor/docs/06-ride-flow.md`
Status definitions & state machines: `.cursor/docs/16-status-definitions.md`

## Multi-Cloud Architecture

- Deployed on AWS and GCP as independent clouds
- Drivers register to one cloud; riders can reach drivers on either cloud via cross-cloud BECKN calls
- DB replicates AWS→GCP via logical replication; Redis does NOT replicate
- KV connector abstracts DB/Redis: `findOneWithKV`, `findAllWithKV`, `updateWithKV`, `deleteWithKV`, `createWithKV`
- **Key debugging rule**: Secondary Redis is checked only on MISS; stale cache hits bypass it (common bug source)

Full multi-cloud details: `.cursor/docs/12-multi-cloud.md`

## FRFS (Public Transport)

- BAP-only — BPPs are external operators (CMRL, CRIS, EBIX)
- Two paths: ONDC (async Beckn) and Direct (synchronous API)
- `mkCloudBapUri` in `Beckn/ACL/FRFS/Utils.hs` handles multi-cloud callback routing
- Constraint propagation: add to type aliases in `ExternalBPP/CallAPI/Types.hs` and `SharedLogic/CallFRFSBPP.hs`
- Key domain types: `FRFSTicketBooking`, `FRFSQuote`, `FRFSSearch`, `FRFSConfig`, `FRFSRouteDetails`

Full FRFS details: `.cursor/docs/10-frfs-public-transport.md`

## Frontend

- **Tech stack**: PureScript + Android native (Kotlin/Java shell)
- **Setup**: `nix develop .#frontend`, then `npm i` and `npm start` in `ui-customer/` or `ui-driver/`
- `ui-customer/`: Customer-facing app UI
- `ui-driver/`: Driver-facing app UI
- `ui-common/`: Shared UI components
- `android-native/`: Android shell application

## External Integrations

| Integration | Purpose |
|-------------|---------|
| Juspay | Payment gateway (orders, webhooks, refunds, payouts) |
| Google Maps / OSRM | Geocoding, distance matrix, routing, snap-to-road |
| Idfy / HyperVerge | Document verification (DL, RC, Aadhaar) |
| FCM | Push notifications |
| SMS (multiple) | OTP and transactional messages |
| WhatsApp | Message delivery |
| Exotel | Voice calls / IVR |

Mock services for local development are started via `, run-mobility-stack-dev`.

Full integration details: `.cursor/docs/13-external-integrations.md`

## CI/CD

Key GitHub Actions workflows:
- `nix-main-push.yaml` / `nix-main-pull.yaml` — Main branch build checks
- `hlint.yaml` — Haskell linting
- `db-check.yaml` — Database migration validation
- `fe-check-*.yaml` — Frontend checks (Android, customer, driver)
- `purs-lint.yaml` — PureScript linting

## Commit / Branch Conventions

```
Commit: <sub-project>/<type>: <issue-number> <summary>
Branch: <sub-project>/<type>/<issue-number><description>
Types: feat, fix, chore, ci, docs, perf, refactor, test
```

## Deep Dive Docs (`.cursor/docs/`)

| Doc | Read when working on... |
|-----|------------------------|
| `00-index.md` | Navigation hub, cross-reference guide |
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

### Task-Based Reading Guide

- **Adding a new API endpoint**: Read `07-namma-dsl.md` → `15-conventions.md`
- **Adding a new DB table**: Read `07-namma-dsl.md` → `08-database-patterns.md`
- **Debugging a ride issue**: Read `06-ride-flow.md` → `05-beckn-protocol-flow.md` → `16-status-definitions.md`
- **Payment integration**: Read `13-external-integrations.md` → `11-libraries.md`
- **FRFS feature**: Read `10-frfs-public-transport.md` → `05-beckn-protocol-flow.md`
- **Multi-cloud bug**: Read `12-multi-cloud.md` → `08-database-patterns.md`
- **Dashboard work**: Read `09-dashboards.md` → `07-namma-dsl.md`
