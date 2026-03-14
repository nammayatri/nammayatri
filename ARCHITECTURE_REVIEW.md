# Namma Yatri — Architecture Review

**Date**: 2026-03-14
**Scope**: Backend Haskell codebase (`/Backend`)

---

## 1. Architecture Overview

Namma Yatri is an open-source mobility platform (ride-hailing, delivery, public transport) built on the **BECKN/ONDC** protocol. The backend is a Haskell monorepo with ~51 Cabal packages following a **protocol-mediated microservices** architecture.

### Core Design Philosophy

The system is split along the BECKN protocol's two-party model:

- **BAP (Beckn Application Platform)** — `rider-app`: consumer-facing, initiates protocol calls
- **BPP (Beckn Provider Platform)** — `dynamic-offer-driver-app`: provider-facing, responds to protocol calls

An **ACL (Anti-Corruption Layer)** on each side translates between BECKN wire types and internal domain types, keeping the domain model protocol-agnostic.

### Request Flow

```
┌─────────────────────────────────────────────────────────────────────────┐
│                         BECKN PROTOCOL LAYER                          │
│   search → on_search → select → on_select → init → on_init →         │
│   confirm → on_confirm → status → on_status → update → on_update     │
└─────────────────────────────────────────────────────────────────────────┘

┌─────────────────────┐          BECKN/HTTP          ┌──────────────────────┐
│    rider-app (BAP)  │ ◄──────────────────────────► │  driver-app (BPP)    │
│    Port 8013        │                              │  Port 8016           │
│                     │                              │                      │
│  ┌───────────────┐  │                              │  ┌────────────────┐  │
│  │ Servant API   │  │                              │  │ Servant API    │  │
│  ├───────────────┤  │                              │  ├────────────────┤  │
│  │ Beckn ACL     │  │                              │  │ Beckn ACL      │  │
│  ├───────────────┤  │                              │  ├────────────────┤  │
│  │ Domain Action │  │                              │  │ Domain Action  │  │
│  ├───────────────┤  │                              │  ├────────────────┤  │
│  │ SharedLogic   │  │                              │  │ SharedLogic    │  │
│  ├───────────────┤  │                              │  ├────────────────┤  │
│  │ Storage/Query │  │                              │  │ Storage/Query  │  │
│  ├───────────────┤  │                              │  ├────────────────┤  │
│  │ Beam ORM      │  │                              │  │ Beam ORM       │  │
│  └───────┬───────┘  │                              │  └───────┬────────┘  │
└──────────┼──────────┘                              └──────────┼───────────┘
           │                                                    │
    ┌──────┴──────┐                                     ┌───────┴──────┐
    │  atlas_app  │                                     │ atlas_driver │
    │  (Postgres) │                                     │ _offer_bpp   │
    └─────────────┘                                     └──────────────┘
           │                                                    │
    ┌──────┴──────┐                                     ┌───────┴──────┐
    │   Redis     │                                     │   Redis      │
    │ (Cache/KV)  │                                     │ (Cache/KV)   │
    └─────────────┘                                     └──────────────┘
```

### Component Diagram

```
┌─────────────────────────────────────────────────────────────────────────────┐
│                              APPLICATIONS                                  │
│                                                                            │
│  ┌──────────────┐  ┌───────────────────┐  ┌──────────┐  ┌──────────────┐  │
│  │  rider-app   │  │ dynamic-offer-    │  │ Allocator│  │  Dashboards  │  │
│  │  (BAP)       │  │ driver-app (BPP)  │  │ (9996)   │  │  (rider/     │  │
│  │  1,416 files │  │ 1,617 files       │  │          │  │   provider/  │  │
│  │  Port 8013   │  │ Port 8016         │  │          │  │   safety/    │  │
│  └──────┬───────┘  └────────┬──────────┘  └────┬─────┘  │   unified)  │  │
│         │                   │                   │        └──────┬──────┘  │
│  ┌──────┴───────────────────┴───────────────────┴───────────────┘        │
│  │                                                                       │
│  │  ┌────────────┐  ┌──────────┐  ┌───────────┐  ┌───────────────────┐  │
│  │  │ kafka-     │  │ special- │  │ public-   │  │ drainers          │  │
│  │  │ consumers  │  │ zone     │  │ transport │  │ (rider/provider)  │  │
│  │  └────────────┘  └──────────┘  └───────────┘  └───────────────────┘  │
│  │                                                                       │
│  │  ┌────────────┐  ┌──────────────────────────┐                        │
│  │  │ sdk-event  │  │ mocks (google, fcm,      │                        │
│  │  │ pipeline   │  │ idfy, sms, payment)       │                        │
│  │  └────────────┘  └──────────────────────────┘                        │
└──┴───────────────────────────────────────────────────────────────────────┘

┌─────────────────────────────────────────────────────────────────────────────┐
│                           SHARED LIBRARIES                                  │
│                                                                            │
│  ┌──────────────┐  ┌──────────────┐  ┌───────────┐  ┌─────────────────┐  │
│  │ beckn-spec   │  │ payment      │  │ scheduler │  │ shared-services │  │
│  │ (224 files)  │  │ (Juspay)     │  │ (Redis)   │  │ (IGM, etc.)    │  │
│  └──────────────┘  └──────────────┘  └───────────┘  └─────────────────┘  │
│  ┌──────────────┐  ┌──────────────┐  ┌───────────┐  ┌─────────────────┐  │
│  │ location-    │  │ yudhishthira │  │ finance-  │  │ external        │  │
│  │ updates      │  │ (rules eng.) │  │ kernel    │  │                 │  │
│  └──────────────┘  └──────────────┘  └───────────┘  └─────────────────┘  │
│  ┌──────────────┐  ┌──────────────┐  ┌───────────┐  ┌─────────────────┐  │
│  │ producer     │  │ webhook      │  │ special-  │  │ utils           │  │
│  │ (Kafka)      │  │              │  │ zone      │  │                 │  │
│  └──────────────┘  └──────────────┘  └───────────┘  └─────────────────┘  │
└─────────────────────────────────────────────────────────────────────────────┘

┌─────────────────────────────────────────────────────────────────────────────┐
│                       EXTERNAL (shared-kernel, euler-hs)                    │
│                                                                            │
│  KV Connector  │  Beam Functions  │  FlowRuntime  │  Hedis  │  Monitoring  │
└─────────────────────────────────────────────────────────────────────────────┘

┌─────────────────────────────────────────────────────────────────────────────┐
│                            INFRASTRUCTURE                                   │
│                                                                            │
│  PostgreSQL    │  Redis (Single/Cluster)  │  Kafka  │  ClickHouse  │ OSRM  │
└─────────────────────────────────────────────────────────────────────────────┘
```

### Code Generation via NammaDSL

A distinctive feature is the **NammaDSL code generator** (`alchemist` package), which generates ~27% of the Haskell codebase from YAML specs:

| Category | Files | Lines |
|----------|-------|-------|
| Generated (`src-read-only/`) | 1,946 | ~131,000 |
| Hand-written (`src/`) | 2,600 | ~356,000 |
| **Total** | **4,546** | **~487,000** |

Generated artifacts include: Servant API types, Beam ORM types, domain entity types, standard CRUD queries, and cached query wrappers. Business logic (`Domain/Action/`, `SharedLogic/`) is always hand-written.

### Multi-Cloud Architecture

Production runs across AWS and GCP with:
- PostgreSQL: logical replication AWS → GCP
- Redis: **no replication** (independent per cloud — a significant source of bugs)
- KV Connector: abstracts Redis-then-DB reads with dual-Redis fallback

---

## 2. Core Modules and Responsibilities

### Layered Architecture (per service)

| Layer | Path | Responsibility |
|-------|------|---------------|
| **API** | `src-read-only/API/` | Servant type-level route definitions (generated) |
| **Beckn ACL** | `src/Beckn/ACL/` | Protocol ↔ domain type translation |
| **Beckn Transformers** | `src/Beckn/OnDemand/` | On-demand-specific transformations |
| **Domain Action/UI** | `src/Domain/Action/UI/` | User-facing business logic handlers |
| **Domain Action/Beckn** | `src/Domain/Action/Beckn/` | Protocol callback handlers |
| **Domain Action/Dashboard** | `src/Domain/Action/Dashboard/` | Admin/ops handlers |
| **SharedLogic** | `src/SharedLogic/` | Reusable business logic (fare calc, pools, etc.) |
| **Domain Types** | `src-read-only/Domain/Types/` | Entity type definitions (generated) |
| **Storage Queries** | `src-read-only/Storage/Queries/` | Standard DB queries (generated) |
| **Extra Queries** | `src/Storage/Queries/` | Complex hand-written queries |
| **Cached Queries** | `src/Storage/CachedQueries/` | Redis-cached query wrappers |
| **Beam Types** | `src-read-only/Storage/Beam/` | ORM table definitions (generated) |
| **Tools** | `src/Tools/` | Error types, notifications, utilities |

### Key SharedLogic Modules (driver-app)

| Module | Lines | Responsibility |
|--------|-------|---------------|
| `DriverPool.hs` | 1,318 | Geospatial driver pool computation |
| `FareCalculator.hs` | 950 | Core fare computation engine |
| `FarePolicy.hs` | ~1,500 | Fare policy evaluation |
| `DriverOnboarding/Status.hs` | ~1,600 | Driver document verification FSM |
| `DynamicPricing.hs` | — | Surge/demand pricing |
| `Allocator/Jobs/SendSearchRequestToDrivers/` | ~2,000+ | Driver allocation pipeline |

### Key SharedLogic Modules (rider-app)

| Module | Lines | Responsibility |
|--------|-------|---------------|
| `Search.hs` | — | Search request orchestration |
| `Confirm.hs` | — | Booking confirmation logic |
| `Payment.hs` | 845 | Payment orchestration |
| `CallBPP.hs` | — | BPP HTTP client |
| `FRFS*.hs` (7 files) | — | Public transport (metro/bus) |
| `JourneyModule/` | ~3,000+ | Multi-modal journey orchestration |

### Background Job System

The scheduler library powers both services with Redis-based job queues. The driver-app's `Allocator` service runs ~30 distinct job types:

- Driver search/allocation
- Driver fee computation and reconciliation
- Mandate/subscription processing
- Payout scheduling
- Overlay/notification dispatch
- Document verification retries
- Scheduled ride management

---

## 3. Technical Debt

### TD-1: God Modules (Critical)

Several files have grown far beyond maintainable size:

| File | Lines | Concern |
|------|-------|---------|
| `Dashboard/Fleet/Driver.hs` | 4,356 | Fleet driver operations (mixing CRUD, reports, bulk ops) |
| `Dashboard/Management/Merchant.hs` | 4,016 | Merchant config (mixing every config type in one file) |
| `Domain/Action/UI/Driver.hs` | 3,522 | Driver endpoints (registration, status, fees, history — everything) |
| `Domain/Action/UI/MultimodalConfirm.hs` | 2,716 | Multi-modal booking (complex but single file) |
| `Domain/Action/UI/TicketService.hs` | 2,583 | Ticket/event management |
| `Tools/Error.hs` | 1,938 | Every error type in a single file |

**Impact**: Hard to review, test, and reason about. High merge-conflict probability.

### TD-2: FareCalculator V1/V2 Co-existence

`FareCalculator.hs` (950 lines) and `FareCalculatorV2.hs` (297 lines) exist side-by-side. This suggests an incomplete migration to a new fare calculation approach, creating ambiguity about which to use and maintaining two code paths.

### TD-3: Minimal Test Coverage

| Metric | Count |
|--------|-------|
| Test files (hunit-tests) | 8 |
| Test files (test/) | 32 |
| Total LoC under test | < 5,000 |
| Production LoC | ~356,000 |

**Coverage ratio is below 1.5%**. Critical business logic (fare calculation, driver allocation, payment processing) has no automated test safety net.

### TD-4: 897 Migration Files

The migration directory has grown to nearly 900 SQL files across services. There's no evidence of squashing or baselining, meaning fresh environment setup requires replaying all migrations sequentially.

### TD-5: Error Type Monolith

`Tools/Error.hs` at 1,938 lines defines every error type for the entire driver-app service in a single file. This is a maintenance burden and creates unnecessary recompilation cascades when any error is added.

### TD-6: Incomplete Abstraction in KV Connector

The dual-Redis multi-cloud architecture has a documented pitfall: "stale cache hits bypass secondary" (rule #4). This is an architectural defect — the KV connector provides convenience at the cost of consistency guarantees that are easy to violate accidentally.

---

## 4. Duplication

### D-1: SharedLogic Clones Between rider-app and driver-app

**15 files** share identical names across both services' SharedLogic directories:

| File | rider-app | driver-app | Similarity |
|------|-----------|------------|-----------|
| `GoogleTranslate.hs` | 88 lines | 89 lines | ~98% identical (trivial import diff) |
| `LocationMapping.hs` | 74 lines | 74 lines | ~99% identical (constraint order diff) |
| `Cac.hs` | — | — | Likely similar |
| `Merchant.hs` | — | — | Likely similar |
| `MessageBuilder.hs` | — | — | Likely similar |
| `OTP.hs` | — | — | Likely similar |
| `PersonDefaultEmergencyNumber.hs` | — | — | Likely similar |
| `ScheduledNotifications.hs` | — | — | Likely similar |
| `Type.hs` | — | — | Likely similar |

Files like `GoogleTranslate.hs` and `LocationMapping.hs` are **near-identical copies** differing only in import ordering or constraint parameter order. These should be extracted to `lib/shared-services` or a new `lib/common-logic` library.

### D-2: Repeated BPP Detail Lookup Pattern

The following pattern appears verbatim in at least 5 locations across rider-app:

```haskell
bppDetailList <- forM ((.providerId) <$> quotes) (\bppId ->
  CQBPP.findBySubscriberIdAndDomain bppId Context.MOBILITY
    >>= fromMaybeM (InternalError $ "BPP details not found for providerId:-"
        <> bppId <> "and domain:-" <> show Context.MOBILITY))
```

Found in: `Select.hs` (twice), `Quote.hs` (twice), `OnSelect.hs` (twice).

### D-3: Invoice Lookup Pattern in Driver Fees

The `mapM + findActiveManualInvoiceByFeeId` pattern appears at least 6 times in `Driver.hs` and `DriverFee.hs`:

```haskell
invoices <- mapM (\fee -> runInReplica
  (QINV.findActiveManualInvoiceByFeeId fee.id Domain.MANUAL_INVOICE Domain.ACTIVE_INVOICE))
  dueDriverFees
```

### D-4: Dashboard Action Handlers

`Dashboard/Management/Driver.hs` (driver-app, ~1,700 lines) and `Dashboard/Merchant.hs` (rider-app, ~1,700 lines) contain structurally similar config-update handlers that follow the same pattern: fetch config → validate → update → return. A generic config-update combinator could replace dozens of near-identical handlers.

---

## 5. Performance Risks

### P-1: N+1 Query Patterns (High Risk)

Multiple instances of `mapM` or `forM` wrapping individual `findById` calls:

```haskell
-- SharedLogic/Allocator/Jobs/Reminder/ProcessReminder.hs:644
drivers <- mapM (QPerson.findById . (.driverId)) rcAssocs

-- Domain/Action/Internal/FleetVehiclesAssociation.hs:74
persons <- mapM (\foId -> QP.findById (Id foId)) fleetOwnerIds

-- Domain/Action/Dashboard/Fleet/Driver.hs:4086
unlinkedDrivers <- catMaybes <$> mapM QPerson.findById unlinkedDriverIds
```

Each generates N sequential DB roundtrips. With fleets of hundreds of drivers, this could mean hundreds of queries where a single `WHERE id IN (...)` would suffice.

**Estimated occurrence**: 20+ instances across the codebase.

### P-2: Driver Pool Computation Complexity

`DriverPool.hs` (1,318 lines) + `DriverPoolUnified.hs` (533 lines) perform geospatial queries, filtering, sorting, and batch operations on every search request. This is the **hottest path** in the system — every rider search triggers this.

Risk factors:
- Multiple sequential Redis GEO queries per search
- In-memory sorting and filtering of driver candidates
- No evidence of query result caching for overlapping searches in the same area/time window

### P-3: Multi-Cloud Redis Consistency

The documented behavior — "Updates only happen in the Redis where data is found" and "stale cache hit bypasses secondary" — creates a class of bugs where:
- Driver updates in one cloud are invisible to riders in the other
- Cache invalidation doesn't propagate across clouds
- No TTL consistency enforcement

This is not a performance risk per se but a **correctness risk that manifests as performance-adjacent bugs** (stale data, retries, fallback queries).

### P-4: Large Transaction Scopes

The `runInTransaction` pattern is used for multi-entity writes, but some transaction scopes include:
- External API calls (Juspay webhooks, notification sends)
- Redis writes
- Multiple unrelated entity updates

Long-held transactions under load can cause connection pool exhaustion and lock contention.

### P-5: No Batch Insert/Update APIs

The generated query layer provides only single-record `createWithKV`. Bulk operations like onboarding multiple fleet vehicles or processing batch payouts default to sequential inserts wrapped in a transaction, missing the opportunity for PostgreSQL's efficient batch operations.

### P-6: Location Updates Write Amplification

High-frequency driver location updates (every few seconds for active drivers) flow through the KV connector, which writes to both PostgreSQL and Redis. With thousands of active drivers, this creates significant write amplification. The `location-updates` library handles snap-to-road via OSRM, adding external API latency to the hot path.

---

## 6. Refactoring Opportunities

### R-1: Extract Common SharedLogic to a Library

**Impact: High | Effort: Medium**

Create `lib/common-logic/` containing the 15 duplicated SharedLogic modules. Start with the trivially identical ones (`GoogleTranslate.hs`, `LocationMapping.hs`) and progressively migrate others.

### R-2: Decompose God Modules

**Impact: High | Effort: High**

Split the largest files by subdomain:

```
Domain/Action/UI/Driver.hs (3,522 lines)
  → Domain/Action/UI/Driver/Registration.hs
  → Domain/Action/UI/Driver/Status.hs
  → Domain/Action/UI/Driver/Fee.hs
  → Domain/Action/UI/Driver/History.hs
  → Domain/Action/UI/Driver/Activity.hs

Domain/Action/Dashboard/Fleet/Driver.hs (4,356 lines)
  → Domain/Action/Dashboard/Fleet/Driver/Onboarding.hs
  → Domain/Action/Dashboard/Fleet/Driver/Management.hs
  → Domain/Action/Dashboard/Fleet/Driver/Reports.hs
  → Domain/Action/Dashboard/Fleet/Driver/Routes.hs
```

### R-3: Introduce Batch Query Combinators

**Impact: High | Effort: Low**

Add `findAllByIds :: [Id a] -> m [a]` as a first-class query pattern in the generated queries (NammaDSL enhancement) or as a utility in `shared-kernel`. Replace the 20+ `mapM findById` instances.

### R-4: Complete FareCalculator V2 Migration

**Impact: Medium | Effort: Medium**

Audit all call sites of `FareCalculator` vs `FareCalculatorV2`, complete the migration to V2, and remove V1. Two parallel calculation engines are a source of bugs and confusion.

### R-5: Introduce Property-Based Testing

**Impact: High | Effort: Medium**

The fare calculation, dynamic pricing, and driver pool algorithms are prime candidates for QuickCheck/Hedgehog property tests. Key properties:
- Fare is always non-negative
- Fare is monotonically increasing with distance (for same policy)
- Driver pool always returns the closest eligible drivers
- Payment amounts sum correctly across splits

### R-6: Split Error Types by Domain

**Impact: Low | Effort: Low**

Break `Tools/Error.hs` into domain-specific error modules:
- `Tools/Error/Booking.hs`
- `Tools/Error/Driver.hs`
- `Tools/Error/Payment.hs`
- `Tools/Error/Fleet.hs`

### R-7: Extract BPP Lookup Helper

**Impact: Low | Effort: Low**

The repeated `forM providers → findBySubscriberIdAndDomain → fromMaybeM` pattern should be a single helper:

```haskell
lookupBPPDetails :: [Text] -> Context.Domain -> m [BPPDetails]
lookupBPPDetails providerIds domain =
  forM providerIds (\bppId ->
    CQBPP.findBySubscriberIdAndDomain bppId domain
      >>= fromMaybeM (BPPDetailsNotFound bppId domain))
```

---

## 7. Monitoring and Observability Improvements

### O-1: Add Request-Level Tracing

The codebase uses `logInfo`/`logDebug`/`logError` from `Kernel.Utils.Logging`, but there's no evidence of distributed tracing (OpenTelemetry/Jaeger). Given the BAP→BPP async protocol flow and multi-cloud deployment:

**Recommendation**: Instrument all BECKN protocol calls with trace context propagation. A single rider search touches: rider-app → network → driver-app → allocator → driver-app → network → rider-app. Without trace IDs, debugging latency or failures across this chain requires manual log correlation.

### O-2: Add N+1 Query Detection

**Recommendation**: Add a middleware/hook in the KV connector layer that logs warnings when the same query function is called more than N times within a single request handler. This would catch N+1 patterns in development before they hit production.

### O-3: Add Cache Hit/Miss Ratio Metrics

The KV connector's dual-Redis fallback strategy is critical for performance but invisible. Add Prometheus counters for:
- `kv_cache_hit_primary` / `kv_cache_miss_primary`
- `kv_cache_hit_secondary` / `kv_cache_miss_secondary`
- `kv_db_fallback_total`
- `kv_stale_cache_hit` (when secondary returns data that's been updated in primary)

### O-4: Monitor Driver Pool Computation Latency

The `DriverPool` + `DriverPoolUnified` computation is the critical path for search latency. Add:
- Histogram for pool computation time per search
- Counter for pool size (how many drivers returned)
- Gauge for active drivers per geo-cell
- Timer for Redis GEO query latency

### O-5: Job Queue Depth and Processing Latency

The scheduler runs 30+ job types via Redis queues. Monitor:
- Queue depth per job type
- Processing latency per job type (time from enqueue to completion)
- Job failure/retry rates
- Alert on queue depth exceeding threshold (indicates processing bottleneck)

### O-6: Multi-Cloud Consistency Monitoring

Given the documented consistency issues with dual Redis:
- Periodically compare key counts between primary and secondary Redis
- Track cross-cloud callback failure rates
- Monitor PostgreSQL replication lag (AWS → GCP)
- Alert on Redis key expiry divergence between clouds

### O-7: Fare Calculation Audit Trail

Fare disputes are likely a support burden. Instrument fare calculation to emit structured events with all inputs and intermediate values:
- Base fare, distance fare, time fare
- Surge multiplier and source
- Tolls detected
- Discounts/offers applied
- Final breakdown

This aids both debugging and compliance.

---

## 8. Improvements Ranked by Impact

| Rank | Improvement | Category | Impact | Effort | Risk |
|------|-------------|----------|--------|--------|------|
| 1 | **Add batch query combinators** (R-3) | Performance | High | Low | Low |
| 2 | **Add distributed tracing** (O-1) | Observability | High | Medium | Low |
| 3 | **Introduce property-based testing for core algorithms** (R-5) | Quality | High | Medium | Low |
| 4 | **Extract common SharedLogic to library** (R-1) | Duplication | High | Medium | Low |
| 5 | **Decompose god modules** (R-2) | Maintainability | High | High | Medium |
| 6 | **Add cache hit/miss metrics** (O-3) | Observability | Medium | Low | Low |
| 7 | **Monitor driver pool latency** (O-4) | Observability | Medium | Low | Low |
| 8 | **Complete FareCalculator V2 migration** (R-4) | Tech Debt | Medium | Medium | Medium |
| 9 | **Add job queue monitoring** (O-5) | Observability | Medium | Low | Low |
| 10 | **Multi-cloud consistency monitoring** (O-6) | Reliability | Medium | Medium | Low |
| 11 | **Fare calculation audit trail** (O-7) | Operations | Medium | Low | Low |
| 12 | **N+1 query detection middleware** (O-2) | Performance | Medium | Medium | Low |
| 13 | **Split error types** (R-6) | Maintainability | Low | Low | Low |
| 14 | **Extract BPP lookup helper** (R-7) | Duplication | Low | Low | Low |
| 15 | **Squash/baseline migrations** (TD-4) | DevEx | Low | Medium | Medium |

---

## 9. Architecture Strengths

The review also highlights significant architectural strengths:

1. **NammaDSL code generation** eliminates an entire class of boilerplate bugs and ensures consistent query/type patterns across 180+ entities
2. **BECKN protocol compliance** via ACL pattern keeps domain logic decoupled from protocol evolution
3. **Clean layered separation** — the Servant API → ACL → Domain Action → SharedLogic → Storage stack is consistently applied
4. **Multi-cloud resilience** — despite the consistency challenges, the architecture enables geo-distributed deployment
5. **Comprehensive mock services** — the `app/mocks/` directory enables local development without external dependencies
6. **Typed configuration via Dhall** — reduces config-related runtime errors

---

*End of review.*
