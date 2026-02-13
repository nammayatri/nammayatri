# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

Namma Yatri is an open-source mobility platform (ride-hailing + delivery + public transport) built on the BECKN/ONDC open network protocol. It's a monorepo with a Haskell backend, PureScript frontend, and native Android components.

## Build & Development Commands

### Environment Setup (one-time, from project root)
```bash
ln -sf .envrc.backend .envrc   # For backend work
direnv allow
# This drops you into a Nix develop shell with all dependencies
```

### Backend (Haskell)
```bash
cd Backend
cabal build all                  # Build everything
cabal build <package-name>       # Build specific package (e.g., rider-app)
cabal test all                   # Run all tests
cabal repl <package-name>        # Interactive REPL for a specific package
```

### Code Generation (NammaDSL)
```bash
# Generate Haskell from YAML specs (run from Backend/ inside nix shell)
, run-generator                  # Only changed specs
, run-generator --all            # All specs
, run-generator --apply-hint     # With HLint auto-fixes
```

### Development Utilities (comma commands, available in nix shell)
```bash
, run-mobility-stack-dev         # Start all external services (Postgres, Redis, Kafka, etc.)
, ghcid lib/<package-name>       # Fast compile feedback loop
, hpack                          # Regenerate .cabal files from package.yaml
, docs                           # Run Hoogle documentation server
, kill-svc-ports                 # Kill lingering service processes
```

### Frontend (PureScript)
```bash
nix develop .#frontend
cd Frontend/ui-customer          # or ui-driver
npm install
npm start                        # Dev server
```

### Linting
```bash
hlint .                          # Haskell linting (uses .hlint.yaml)
```

## Architecture

### Backend Service Layout

The backend is a multi-package Cabal project (~48 packages) following a microservices architecture:

- **Rider Platform** (`app/rider-platform/`): Customer-facing services
  - `rider-app` (port 8013): Main customer APIs, search, booking, payment
  - `rider-app-scheduler`: Background jobs for rider platform
  - `public-transport-rider-platform`: FRFS bus/metro services
- **Provider Platform** (`app/provider-platform/`): Driver/fleet services
  - `dynamic-offer-driver-app` (port 8016): Main driver APIs, ride management
  - `driver-offer-allocator` (port 9996): Core driver allocation engine
- **Dashboards** (`app/dashboard/`): Operations dashboards (rider, provider, safety)
- **Shared Libraries** (`lib/`): Cross-cutting concerns
  - `beckn-spec`: BECKN protocol types and API definitions
  - `payment`: Juspay payment gateway integration
  - `scheduler`: Redis-based job scheduling
  - `location-updates`: Real-time tracking with OSRM
  - `yudhishthira`: Business rule/decision engine
  - `finance-kernel`: Financial primitives

### Databases
- **PostgreSQL**: Primary OLTP (schemas: `atlas_app`, `atlas_driver_offer_bpp`)
- **Redis**: Caching, location data, job queues (single instance port 6379, cluster port 30001)
- **ClickHouse**: Analytics and event tracking
- **Kafka**: Event streaming between services

### BECKN Protocol Flow
The system implements BAP (Beckn Application Platform = rider side) and BPP (Beckn Provider Platform = driver side). The core ride flow follows: `search` -> `on_search` -> `select` -> `on_select` -> `init` -> `on_init` -> `confirm` -> `on_confirm`. ACL (Anti-Corruption Layer) modules translate between BECKN protocol types and internal domain types.

## Code Generation (NammaDSL) - Critical Workflow

YAML specification files are the source of truth for APIs and database schemas:
- **API specs**: `spec/API/*.yaml` within each service package
- **Storage specs**: `spec/Storage/*.yaml` within each service package

The generator produces code into `src-read-only/` directories. **Never edit files in `src-read-only/` directly** - always modify the source YAML and re-run the generator.

The generator also creates stub files in `src/` directories (e.g., `src/Domain/Action/UI/`) for manual business logic implementation.

### API YAML conventions
- Use camelCase for endpoint path segments (e.g., `/nyRegular/`), not hyphens
- Auth options: `TokenAuth`, `AdminTokenAuth`, `NoAuth`, `DashboardAuth`, `ApiAuth`

### Storage YAML conventions
- Fields, constraints, beam mappings, queries, and cached queries are all defined in YAML
- Common auto-imported types: `Text`, `Maybe`, `Int`, `Bool`, `Id`, `UTCTime`, `HighPrecMoney`, `Currency`
- Use `fromTType`/`toTType` for domain-to-beam type transformations
- `extraOperations` like `EXTRA_QUERY_FILE` and `EXTRA_DOMAIN_TYPE_FILE` generate editable extension files

### After code generation
Always compile (`cabal build all`) after running the generator to verify correctness before proceeding with further changes.

## Haskell Conventions

### Module Organization
- `Domain/Types/`: Domain entity definitions (many use phantom types for safety: `UsageSafety`, `EncryptionStatus`)
- `Domain/Types/Extra/`: Supplementary types, orphan instances for external types
- `Domain/Action/`: Business logic handlers
- `Storage/Queries/`: Database query modules (naming: `Queries.*`)
- `Storage/CachedQueries/`: Redis-cached queries (naming: `CachedQueries/*`)
- `API/`: Servant API type definitions
- `Beckn/ACL/`: BECKN protocol translation layers

### Common Patterns
- **ID generation**: `newId <- generateGUID` (from `Kernel.Utils.Common`)
- **Error handling**: `person <- QPerson.findById personId >>= fromMaybeM (PersonNotFound personId.getId)`
- **DB inserts**: Call `create` from `Storage.Queries` directly; don't wrap single creates in `runInTransaction`
- **Logging**: `logInfo` from `Kernel.Utils.Logging`
- **YAML imports**: Use full module paths (e.g., `Domain.Types.IntegratedBPPConfig`), not short names
- **Beckn tags**: Must be defined in `Backend/lib/beckn-spec/src/BecknV2/OnDemand/Tags.hs` before use

### Compilation Notes
- Project uses `-Werror`: all GHC warnings are treated as errors (unused imports, dodgy imports, etc.)
- Use `cabal build <target>` for comprehensive checks; `cabal repl` alone doesn't guarantee full compilability
- If a `.hs` file is deleted, ensure the `.cabal` file is updated (run `, hpack` or the generator)
- Orphan instances for external types go in `Domain/Types/Extra/*.hs` files

## Database Migrations
- Located in `Backend/dev/migrations/` and `Backend/dev/migrations-read-only/`
- Migrations are SQL files organized by service schema

## Commit Message Convention
```
<sub-project>/<type>: <issue-number> <short summary>
```
Types: `feat`, `fix`, `chore`, `ci`, `docs`, `perf`, `refactor`, `test`
Example: `backend/feat: #341 Driver onboarding flow`

## Branch Naming Convention
```
<sub-project>/<type>/<issue-number><short-description>
```
Example: `backend/fix/GH-123/driver-allocation-bug`

## Key External Integrations
- **Juspay**: Payment gateway (webhooks for payment + payout)
- **Idfy/HyperVerge**: Document verification
- **OSRM**: Routing, snap-to-road, distance calculations
- **Dhall**: Typed configuration language (configs in `dhall-configs/`)
- **Paseto**: Token-based API authentication
