# Fare Policy & Dynamic Pricing Revamp — Plan (v2)

**Date:** 2026-08-26
**Scope:** nammayatri BPP (`dynamic-offer-driver-app`) + provider-dashboard + control-center repo
**Status:** Implemented in working tree (2026-08-27), pending generator run + build + review.
Phase 0 (ML removal), Phase 1 (FarePolicyV2 typed API + maker-checker + coverage alerts),
Phase 2 (Fare Policy Studio frontend), Phase 3 (observability: estimate explain + city health
from the estimate stream, DYNAMIC_PRICING_PARSE_FAILURE marker, feedback damping) and
Phase 4 (SurgeConfig table + evaluator + shadow + dashboard CRUD) are all built.
Remaining in Phase 4: city-by-city migration of live json-logic rules into SurgeConfig
(operational, via shadow comparison), then retirement of the DYNAMIC_PRICING_UNIFIED path.

## Decisions taken

1. **Typed granular JSON APIs replace the CSV** as the primary fare-policy contract; CSV kept
   only as bulk import/export for city onboarding, reimplemented over the same core functions.
2. **Policy model stays full-copy per combo** (tier × tripCategory × area × timeBound ×
   searchSource) with **server-side bulk-apply**; base+override inheritance deferred but the
   API is shaped so it can slot in later (`GET` can later return `{effective, overrides}`).
3. **UI becomes task-oriented screens** (rate card / peaks & areas / bid bounds / advanced
   drawer), not a 101-field form.
4. **ML pricing service is removed entirely.** Dynamic pricing = rules → static fallback.
5. **Json-logic surge rules are replaced by a typed, versioned surge table (`SurgeConfig`)**
   evaluated directly by the backend. Rationale: the ManageSurge UI already edits a typed
   `PeakRow`/matrix model and compiles it to json-logic purely as transport (up to 250 KB of
   nested if/else). The logic layer adds opacity, silent parse failures, and encoding
   workarounds while providing no expressiveness that is actually used.
6. Target-based control (ops sets target QAR band + caps, system self-adjusts with damping) is
   a **future experiment on top of SurgeConfig**, single-city trial, not in scope now.
7. **UI decisions** (wireframes: claude.ai/code/artifact/bc2e393e-59ec-424f-a384-aed243b82cd0):
   Rate Card edits **one combo at a time** with save-time propagation (no cross-tier matrix);
   **all trip categories in v1** — the parameter table renders each variant's shape;
   **structural-vs-demand peak split** — fare-policy timeBounds keep only structural pricing
   (night hours, airport hours), demand-responsive multipliers move to SurgeConfig, and
   existing demand-peak combos migrate into surge during Phase 4.
   The IA is **two screens + save flow**: (1) Rate Card — core params incl. **per-minute rate
   sections** (with total-duration vs traffic-delay basis) and **driver bid bounds folded in**
   (no separate bounds screen), city charges + advanced as gated accordions; (2) Peaks & Areas
   — a **combo browser where every combo is a full rate card** with its own headline numbers
   (combos may share nothing with Default), a computed "differs in N fields" secondary line,
   and a side-by-side Compare view (incl. fare preview of both combos on the same trip);
   coverage gaps as inline nudges. No diff-chips-as-primary representation.

The former "stop the bleeding" phase is dissolved: its items are either absorbed as
prerequisites of the API work (stable ids, validate-before-write, cache invalidation,
cross-city scoping) or parked in the bug ledger (CSV-only defects that matter less once the
CSV is demoted).

---

## Phase 0 — ML removal (do first; small, independent)

- Delete `calculateCongestionChargeViaML` and the override branch in `getFullFarePolicy`
  (`SharedLogic/FarePolicy.hs:348`, precedence at :289-303 collapses to rules → static).
- Delete `SharedLogic/CallInternalMLPricing.hs` and the `/internal/getCongestionCharge` client.
- Deprecate `isMLBasedDynamicPricingEnabled` on TransporterConfig (leave column, remove reads;
  drop in a later migration). Check dashboard/config UIs for toggles referencing it.
- Side effect: removes the synchronous HTTP call from the search hot path.

## Phase 1 — Typed granular API (NammaDSL; `Merchant.yaml` spec + handlers)

Resource model: **FareProduct combo** ↔ **FarePolicy** (stable id). Everything lands in the
OpenAPI spec so control-center's `openapi-typescript` sync + contract tests cover it.

| Endpoint | Purpose |
|---|---|
| `GET /config/fareProduct/list` (extend) | Combo → policy mapping + fare summary (base fare, per-km headline, night/peak flags, enabled). Filterable by tier/category/area. Replaces parsing the CSV export to build the combo list. |
| `GET /config/farePolicy/{id}/details` (extend) | Full typed JSON: common fields, variant details (sections/slabs/buffers), driverExtraFeeBounds, cancellation policy. Structured into sections (core / charges / taxes / advanced) so the UI tiers by structure, not by YAML metadata. |
| `PUT /config/farePolicy/{id}` (new) | Full typed replace, **in place, stable id**; explicit `null` clears a field. Validates everything → writes atomically → invalidates Redis + derived caches (incl. airport per-km). Enforces merchant/city scope on the id. `?dryRun=true` returns structured per-field errors + a diff vs current, no write. |
| `POST /config/farePolicy/bulkUpdate` (new) | `{policyIds, patch, dryRun}` — one typed patch applied to N combos atomically, one combined diff report. **This replaces the control-center save-cart + propagation checkboxes.** |
| `POST /config/fareProduct` (new) | Create policy + bind to a combo, or bind an existing policy (enables copy-from-tier/city without CSV). |
| `DELETE /config/fareProduct/{id}` (new) | Unbind; delete policy only if orphaned across all cities. Replaces `setEnabled`'s destructive semantics. |
| `POST /config/farePolicy/preview` (new) | Policy id **or** unsaved policy JSON + sample trips → real `calculateFareParameters` + `fareSum` breakups. Retires the 378-line client-side simulator; enables preview-before-save. |

Prerequisites absorbed from the old bug list: stable-id in-place update path, full cache
invalidation on write (fixes the disappearing-policy class), validate-before-write,
cross-city scoping, `setEnabled` cache clear, `driverExtraFeeBounds` stepFee no-op (endpoint
gets superseded by PUT anyway).

Capabilities: reuse `system-config.fare_policy.read|write|export`; `preview` under `.read`.

## Phase 2 — control-center: task-oriented ops UI

Feature-flagged per merchant via existing `farePolicyEditorConfig.tabs`; CSV tab remains for
onboarding until parity confirmed.

Screens (replacing the combo-list + 101-field form):

1. **Rate card** — per tier × trip category: base fare, base distance, per-km sections,
   pickup, waiting, night shift in one compact editable table. Covers ~90% of daily ops edits.
   Inline edit → dry-run diff → preview (5/10/20 km before/after) → confirm (keep the >25%
   guardrail dialog).
2. **Peaks & areas** — combos that differ from the Default-area/unbounded baseline, rendered
   *as diffs* against it. "Add peak/area" starts from the baseline, not from blank. The old
   health-checker becomes implicit: missing combos are visible gaps in this view.
3. **Driver bid bounds** — distance bands per tier (min/max/step/default step).
4. **Advanced drawer** — FareChargeConfig slots, conditional charges, booth/return fees,
   platform fee/GST, insurance, card charge; collapsed by default, permission-gated per
   merchant. Per-variant templates so new combos start from a city default.

Deletions once Phase 1 is live for a merchant: whole-segment re-send + three pre-flight aborts
(`savePayload.ts`, `useFarePolicySave.ts`), per-km backfill, integer normalization, forced
platform-fee headers (incl. the `pare_policy_level` typo), localStorage save-cart (bulkUpdate
takes over), hand-maintained `utils/validation.ts` (server dry-run is the single validation
source; `fare_policy_fields.yml` keeps only display metadata), `fareSimulator.ts`.

## Phase 3 — Observability

1. **Persist the pricing decision** at estimate creation: inputs (QAR, supply-demand, rain,
   congestion history), engine (surge-table version / static), bucket/cell hit, outputs.
   All values are already in hand at `SharedLogic/FarePolicy.hs:1114-1267`.
2. `GET /pricing/estimate/{id}/explain` — "why this price", for support/debug.
3. `GET /pricing/dynamicPricing/health?city=...` — last-N-hours: multiplier distribution by
   tier/geohash, % searches surged, config-version hit rates, **data-missing rate**, eval
   failure count. Failures also become a Prometheus counter + alert.
4. **control-center "Pricing Pulse" panel** fed by (3): active version, rollout state, actuals;
   replaces guesswork about whether dynamic pricing is working.
5. **Shadow mode**: evaluate a candidate SurgeConfig version on live traffic, log deltas,
   apply nothing — standard pre-rollout step.

## Phase 4 — SurgeConfig: typed surge table replacing json-logic

**Model** (NammaDSL storage spec, new table `surge_config`):

- Key: merchantOperatingCityId × vehicleServiceTier × timeBounds; `version`,
  `status ∈ {Active, Shadow, Archived}`, author, createdAt.
- **Table**: rows of `{signalBuckets → outputs}` where v1 signals are QAR, supply-demand
  ratio, and distance bin (the exact shapes ManageSurge's matrices already encode), and v1
  outputs are `congestionChargeMultiplier` and optional `congestionFeePerMin`.
- **Guardrails in schema**: min/max multiplier per tier, max delta vs previous version
  (server-rejected, not advisory), and an explicit **missing-data policy** — no data ⇒
  multiplier 1.0, never "treat as max scarcity" (fixes the `fromMaybe 0.0` class by design).
- Open item (deliberately out of v1): dynamic driverExtraFeeBounds and smartTip outputs.
  Bid bounds revert to the static policy values; smart tip is dropped unless product asks.

**Evaluation**: `getFullFarePolicy` looks up the Active SurgeConfig for (city, tier, time),
resolves the cell from live signals, applies guardrails. No json-logic, no rollout-percentage
stickiness — one Active version per key, with history. Rollback = activate previous version,
effective immediately (sticky-key problem eliminated rather than fixed).

**Write API**: `GET/PUT /config/surgeConfig` (+ list, versions, activate, shadow) — typed,
dry-run diff, validation at write time. Change logs continue to ClickHouse.

**UI**: rebuild ManageSurge as a grid editor over the typed table (the existing matrix-editor
components map almost 1:1 — they already edit this shape). Add "which cell fires" simulator
(given time + location + signals → highlighted cell) and the Pricing Pulse panel alongside.

**Migration**: per city — decompile the current `PeakRow` rules into SurgeConfig rows
(control-center already holds the decompiled model in memory when it loads a rule; a one-off
converter reuses that), run in Shadow against live traffic, compare deltas via the health
endpoint, then activate and retire the `DYNAMIC_PRICING_UNIFIED` path for that city. The
json-logic path stays evaluable until the last city migrates, then is removed.

**Also in this phase**: dampen/retire the `CongestionChargeAvg` feedback job (with explicit
signal inputs + guardrails, feeding yesterday's multiplier back in is no longer needed);
pipeline the QAR Redis reads into one round trip; consolidate congestion writes — the
airport-ops `CongestionMultiplierView` and the rate-card multiplier field both go through
`PUT /farePolicy/{id}`, and Pricing Pulse shows which source (surge table vs static) is
winning per tier.

**Future experiment** (after SurgeConfig is stable in ≥1 city): target-based control — ops
sets a target QAR band + caps; the system adjusts the multiplier within guardrails with
damping. Ops moves from tuning cells to setting objectives.

## Sequencing

| Order | Work | Size | Notes |
|---|---|---|---|
| 1 | Phase 0: ML removal | S | Independent, ship immediately |
| 2 | Phase 1: preview + details(extend) + PUT with dryRun | M | Preview is cheap (calculator exists); PUT carries the stable-id/cache/validation prerequisites |
| 3 | Phase 1: list(extend) + bulkUpdate + create/delete | M | Completes the contract; nightly OpenAPI sync gives control-center types |
| 4 | Phase 2: rate-card screen first, then peaks/areas + bounds + drawer | M–L | Per-merchant flag; delete frontend scar tissue as each backend guarantee lands |
| 5 | Phase 3: decision persistence + explain + health + Pulse panel | M | Independent of Phase 2; do in parallel if staffed |
| 6 | Phase 4: SurgeConfig model + evaluator + shadow, city-by-city migration | L | Requires Phase 3 (shadow comparison needs the health endpoint) |

## Bug ledger (CSV-path items, fix only if they bite before the CSV is demoted)

Disappearing policy on partial upsert (frontend whole-segment re-send keeps it latent); Slabs/
Ambulance export missing branches (blocks CSV round-trip — fix if onboarding needs export
before Phase 1 lands); validate-after-wipe; silent half-pair/decimal failures; discarded
`govtCharges` column. control-center hygiene: commit ClickHouse DDL for `fare_policy_logs`/
`congestion_logs`; `POST /api/fare-policy-logs` gated on `.read` and validating only
`entries[0]`; `/api/congestion-logs` missing from Vite dev proxy; duplicated CSV unwrap.
