# Unified Trip Database — BAP/BPP Shared Storage Plan

Status: PLANNED (scoped 2026-09-30). Supersedes the standalone state-machine plan as the
active big refactor — but that plan's Phase 1 (status enum unification) is a hard
**prerequisite** of this one (see `booking-ride-state-machine-plan.md`).

## Goal

Ride-hailing only (FRFS/public-transport out of scope). Keep **two services** (rider-app BAP,
driver-app BPP) and keep domain handlers structurally unchanged. Unify the **storage** of the
shared trip-lifecycle entities — SearchRequest, Estimate/Quote, Booking, Ride — into one
database/schema that both services read and write. An action taken by either actor (driver or
customer) is immediately visible to the other side with **no sync handlers and no duplicate
row creation**. Domain-specific tables (Person vs DriverInformation, Vehicle, FarePolicy,
DriverFee, payments, fleet, FRFS) stay app-owned and untouched.

Beckn remains, demoted: for **internal traffic** (our BAP ↔ our BPP) messages become thin
triggers/events (or internal API per the one-shot-assign direction) — they orchestrate and
notify but no longer carry state to be re-persisted. For **external counterparties**
(external BAP → our BPP; our BAP → external BPPs) the full ACL parse→persist path remains,
producing one-sided rows in the same tables. This also cleanly supports the
"BPP-only for some partners / maybe BAP-only someday" future: role is a property of the row,
not of the schema.

## Grounding facts (verified 2026-09-30)

1. **Both apps already share one Postgres database in dev**: `atlas_dev` with schemas
   `atlas_app` / `atlas_driver_offer_bpp` (`dhall-configs/dev/*.dhall`). Physical
   colocation is a config knob, not an architecture change. ⚠️ Prod topology must be
   confirmed (multi-cloud: `cloudType`/master-cloud-forwarder plumbing implies BAP and BPP
   may not be colocated everywhere — gating question #1).
2. **Per-table schema override exists**: NammaDSL/`HasSchemaName` machinery (used by
   shared-services/IssueManagement tables, 375 constraint mentions) is the in-tree precedent
   for pointing a table group at a shared schema while the connection default stays
   app-local.
3. **KV/Redis-first writes are addressable cross-app**: Redis key =
   `redisKeyPrefix <> tableName <> PK` (`Kernel/Beam/Functions.hs:209`,
   `getLookupKeyByPKey`), with per-table prefix override (`tableRedisKeyPrefix`). Cross-app
   visibility requires, for each shared table: same Redis cluster, same key prefix, same PK
   derivation, and **aligned KV enable flags in both apps** — otherwise one side writes
   Redis-first and the other reads stale Postgres until the drainer catches up. This is the
   single most important correctness detail in the whole plan.
4. **The mirrored tables are not identical**: BPP Booking/Ride carry dispatch/fare-policy
   internals; BAP Booking carries rider payment state; ids differ (rider links via
   `bppBookingId`/`bppRideId`); status enums differ (driver's lacks
   `CONFIRMED`/`AWAITING_REASSIGNMENT`). Unification = **shared core + role-owned column
   zones**, not a naive union.
5. **Cardinality is 1:1 only for Booking and Ride.** SearchRequest/Estimate/Quote are
   asymmetric (BAP aggregates estimates from many BPPs incl. external; BPP's DriverQuote is
   not BAP's Quote). Booking/Ride are where "same data entry twice" is literally true —
   and where the sync-bug class (reallocation races, on_status repair) lives.

## Target design

### One shared schema: `atlas_trip`

Shared lifecycle tables live in a new schema in the (per-region) shared cluster. Each app's
Beam definition for these tables points at `atlas_trip` via the schema-name mechanism; all
other tables stay in `atlas_app`/`atlas_driver_offer_bpp` untouched.

### Table shape: core + ownership zones

For each shared entity:
- **Core columns** (id, status, timestamps, references): written under the state-machine
  transition rules; single status enum from beckn-spec (prerequisite).
- **`bap_*` zone**: rider-owned columns (payment linkage, rider preferences). Nullable —
  NULL for rides from external BAPs.
- **`bpp_*` zone**: driver-owned columns (dispatch internals, fare-policy refs). Nullable —
  NULL for bookings placed on external BPPs.
- **`counterparty` columns**: `bap_subscriber_id` / `bpp_subscriber_id` + an
  `is_internal :: Bool` (both zones populated ⟺ internal ride).

Each app's Beam table maps core + its own zone (+ the columns it reads from the other zone,
read-only by convention). Beam INSERT only sets known columns; the other zone defaults NULL —
which is exactly the external-counterparty representation. Convention to enforce with a lint:
**an app never writes the other role's zone.**

### Identity: dual-key during transition, canonical forever after

- Canonical `id` = the creator side's id (BPP for Ride, BAP for Booking-as-ordered /
  decide per entity).
- Secondary unique column carries the other side's legacy id (`bap_booking_id` etc.) so each
  app's existing FKs (rider fare_breakup.booking_id, ratings, cancellation reasons; driver
  driver_quote refs) and handler lookups (`findById`, `findByBPPBookingId`) keep working
  unchanged. Post-migration, new rows set both to the same value and the indirection decays.

### What replaces the sync handlers

Persistence mirroring is deleted; **side-effects are not**. Rider's on_update/on_status
handlers today do two jobs: (a) copy BPP state into rider tables, (b) fire
notifications/payment/analytics. (a) disappears for internal traffic; (b) must survive. Two
mechanisms, choose per flow:
- **Thin events** (default): keep the existing message hop (Beckn callback or internal API à
  la one-shot-assign) but the payload is just `entityId + transition`; the handler reads the
  shared row and runs side-effects. Handler skeletons survive; their parse→create bodies go.
- **Outbox/CDC** (later, optional): side-effects subscribe to transition events emitted by
  the guarded `transitionBooking`/`transitionRide` writers — this is where the state-machine
  plan and this plan converge: the transition function becomes the single write door to the
  shared tables, and its event stream replaces bespoke notification triggers.

External traffic keeps the full ACL path unchanged (one-sided rows).

## Prerequisites (do first, each independently useful)

1. **Status enum unification** = state-machine plan Phase 1 (driver-app adopts beckn-spec
   `BookingStatus`/`RideStatus` via YAML `imports:`; superset is safe; no data migration).
   A shared column cannot hold two enums. Also do at least the shadow-mode transition table
   (Phase 2a) — with two writers on one row, unguarded status writes become *more*
   dangerous, so CAS-guarded transitions should land with, not after, the shared table.
2. **Topology confirmation**: per city/region, do BAP and BPP hit the same PG cluster and
   the same Redis in prod? If any region splits them, that region needs colocation first or
   stays on the legacy path behind the same flag everyone else uses.
3. **KV namespace design**: pick the shared prefix/cluster for `atlas_trip` tables; audit
   both apps' per-table KV flags (`enableKVForWriteAlso`/read flags) and force agreement for
   shared tables; verify drainer behavior when two apps write the same key space (single
   drainer ownership per table — decide which deployment drains `atlas_trip`).
4. **Traffic mix measurement**: % of BPP rides from external BAPs per city (ClickHouse) —
   sizes the win and the external-path test burden.

## Rollout: entity by entity, risk-ascending

Per entity, the same 5-step dance (feature-flagged per merchant/city):

  a. Create the `atlas_trip` table (core + zones); backfill from the owner side.
  b. **Owner-writes-shared**: the creating side writes the shared table (dual-write to its
     legacy table for rollback); other side still on legacy + sync.
  c. **Shadow-read**: non-owner reads shared row alongside its legacy copy, logs diffs
     (grep-able tag), still serves from legacy. Soak until diff-free.
  d. **Cutover**: non-owner reads/writes shared; its sync-handler persistence and legacy
     inserts are disabled for internal traffic. Legacy table kept read-only for a release.
  e. Delete sync-persistence code + legacy table.

Order:
1. **Ride** first — perfect 1:1, clear owner (BPP creates), rider mostly reads + a few
   CANCELLED writes; the LTS/driver-mode side-effects already hang off driver-side code
   that doesn't move. Biggest consistency payoff (start/end/cancel visibility with zero
   lag).
2. **Booking** second — 1:1 but two-sided writes (rider: CONFIRMED/CANCELLED/deposit flows;
   driver: TRIP_ASSIGNED/COMPLETED), the ACBL rider cache and driver scheduled-booking
   Redis index need to move behind the transition function. This is where CAS guards earn
   their keep.
3. **Quote/Estimate** third — asymmetric; model as role-owned rows in one table
   (BPP-authored quotes visible to BAP directly; BAP's external-BPP estimates coexist).
   Deletes the on_search/on_select persistence translation for internal traffic — the
   single biggest ACL-code deletion.
4. **SearchRequest** last (or explicitly never) — highest volume, shortest-lived, and the
   BAP→many-BPP fan-out makes 1:1 unification least natural. Decide after measuring; the
   storage win may not justify the hot-path risk. An acceptable end-state is: search stays
   duplicated, everything from quote onward is unified.

## What gets deleted at the end (the payoff)

- Rider `Beckn/ACL/On{Update,Status,Confirm,Cancel}` persistence-mirroring for internal
  traffic (the parse→copy halves; side-effect halves remain on thin events).
- Driver→rider status-repair flows (`on_status` reconciliation) for internal rides.
- Duplicate Booking/Ride/Quote row creation (~most rows in these tables, given internal
  traffic share).
- Eventually the `bppBookingId`/`bppRideId` indirection and the id-mapping lookups.
- The sync-lag bug class: reallocation races, stale-status notifications, cancel/assign
  crossings — structurally gone for internal traffic (one row, CAS transitions).

## Risks & open questions

- **Prod/multi-cloud topology** (gating): shared DB requires role colocation per region.
- **Two writers, one row**: without the transition-table CAS this makes races *worse*, not
  better — hence the prerequisite ordering.
- **KV drainer**: two apps producing to one keyspace; drainer ownership and ordering must be
  singular per table. Unexercised territory — needs a dedicated spike before step (b).
- **Combined load**: booking/ride QPS is fine; quote/search on one cluster needs sizing.
  DDL/migrations on shared tables now freeze two services' deploy trains — process change.
- **Schema evolution discipline**: two codebases generating Beam defs for one physical
  table — the NammaDSL spec for shared tables should live in ONE place (a shared spec dir à
  la shared-services) so the apps can't drift. This is the mechanism that keeps "domain
  handlers unchanged" true long-term.
- **ONDC compliance**: protocol behavior at the network boundary is unchanged (external
  parties still see a conformant BPP/BAP); internal storage is our business. Non-issue, but
  document for audits.
- **Rollback story**: every step keeps the legacy table dual-written until the following
  step proves out; flag-off returns to legacy per city.

## Relationship to other tracks

- **State-machine plan**: Phase 1 + 2a are prerequisites; the transition function becomes
  the write door of the shared tables (its scope shrinks: one table, not two mirrors).
- **One-shot-assign**: the internal API it introduces is the prototype of the "thin event"
  mechanism; extend rather than duplicate.
- **nouns-and-verbs/building-blocks**: orthogonal (code-level); resume later if desired —
  a unified DB makes the eventual shared-flow framework simpler, not harder.
