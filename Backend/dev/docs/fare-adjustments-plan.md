# Fare Adjustments: A/B Experiments + Auto-Expiring Spikes

Plan v1 — 2026-09-24. Follow-up phase to `fare-policy-revamp-plan.md` (typed SurgeConfig,
FarePolicyV2 APIs, Fare Policy Studio). All decisions below settled with Hemant.

**Implementation status (2026-09-28, uncommitted):** Phases 0–2 built. Deltas from
the plan as written:
- Overlap rule TIGHTENED: activation rejects ANY tier/area slice overlap with a live
  adjustment (not just same-parameter) — one estimate carries one adjustment stamp,
  keeping arm attribution unambiguous.
- Dashboard endpoints live in a separate spec module `PricingAdjustment.yaml`
  (URL base `/pricingAdjustment`), not inside `Pricing.yaml` — the status enums
  share constructor names (DRAFT/ACTIVE) and one generated module cannot hold both.
- Phase 0 resolved as SUM: when a policy carries both a congestion multiplier and a
  per-min charge, the calculator now adds both components (was: per-min silently won).
- Scope decisions from review: city × tiers × optional pickup areas, **OneWay +
  Progressive policies only (v1)**; caps ±50% / spikes ≤ 24h / experiments ≤ 60d;
  no maker-checker, alert emails via FareAlertSubscription (new ADJUSTMENTS type).
- Randomization: per-rider salted SHA256 of (adjustmentId : customerPhoneNum),
  decided + pinned once per search transaction (`driver-offer:FareAdjustment:Pin:<txnId>`,
  30d TTL); no phone ⇒ control. Estimate/quote-cached FullFarePolicy carries the
  applied scales, so end-ride keeps the priced fare; the pin is the cache-miss fallback.
- Capability seeds are generator-emitted from the spec `migrate:` annotations
  (`API_Management_PricingAdjustment.sql`), not hand-written.

## 1. Requirements (as agreed)

1. **A/B experiments on fare pricing**: split live traffic between current pricing
   (control) and adjusted pricing (treatment), measure per arm.
2. **Spikes**: sudden fare changes valid for a few hours that **auto-expire** and never
   disturb the standing peak / non-peak structure of the rate card.
3. Json-logic dynamic pricing (`DYNAMIC_PRICING_UNIFIED`) is backward-compat only —
   ignored by this design entirely. Runtime pricing = resolved fare policy + optional
   SurgeConfig table.
4. Parameters that may be changed (closed set, both modes): **base fare, per-km rate,
   per-minute rate, congestion multiplier**. Nothing else.
5. Fare Policy Studio must get simpler for ops, not busier. The two existing Surge tabs
   are both confusing → replace with ONE new unified Surge UI ("best of both worlds").

## 2. Decisions log

| Decision | Choice |
|---|---|
| Change semantics | **Scale only** (±X% on top of the resolved policy). No absolute sets. Applies uniformly to every section of a rate ladder. Preserves peak/non-peak proportions by construction. |
| A/B randomization unit | **Per rider**: deterministic `hash(adjustmentId <> customerPhoneNum) mod 100`. Salted per adjustment so experiment populations are independent. Phone available BPP-side at search (`sReq.customerPhoneNum`, from BAP `CUSTOMER_PHONE_NUMBER` personTag). No phone ⇒ control arm. |
| Congestion vs ACTIVE surge table | **Adjustment replaces surge**: while an adjustment targets the congestion multiplier, the surge table is bypassed for affected traffic. UI must state this explicitly ("pauses surge for <tier> until <time>"). |
| Guardrails | Hard server caps, **no maker-checker** (speed is the point of a spike): each scale within **±50%**, spike window **≤ 24h**, experiment lifetime **≤ 60d** (auto-conclude). Full audit + **alert emails** via existing FareAlertSubscription rails on activate / expire / conclude / abort. |
| Scope | **city × service tier(s) × optional pickup areas**. **OneWay trips + Progressive fare policies only** (v1). Nothing = all areas. |
| Surge UI | Neither existing tab survives: design **Surge v3**, one tab, merging classic's honest row-grid + tester with surgeNew's peak-first overview + real area picker. Cart dropped. |

## 3. Concept: `FareAdjustment` — one entity, two modes

An adjustment is an **overlay on the resolved fare policy**. It never mutates the rate
card, so expiry/abort requires zero cleanup and the standing timeBounds (peak) structure
keeps resolving exactly as today.

| | EXPERIMENT | SPIKE |
|---|---|---|
| Traffic | `rolloutPercentage` (1–99) of riders, salted hash | 100% of scope |
| Window | ACTIVE until concluded/aborted; hard auto-conclude at 60d | `validFrom`/`validTill` (absolute UTC), ≤ 24h; auto-expires at evaluation time |
| Measurement | arm stamped on estimates → ClickHouse → Pulse arm-vs-arm | adjustment id stamped for audit |

### 3.1 Parameter mapping (Progressive details only, v1)

| API field | Applies to | Effect of scale `p` (%) |
|---|---|---|
| `baseFareScalePct` | `FPProgressiveDetails.baseFare` | `baseFare × (1 + p/100)` |
| `perKmRateScalePct` | every `perExtraKmRateSections[].perExtraKmRate` | each rate `× (1 + p/100)`; ladder thresholds untouched |
| `perMinRateScalePct` | every `perMinRateSections[].perMinRate` | each rate `× (1 + p/100)`; thresholds + durationBasis untouched |
| `congestionScalePct` | effective congestion multiplier | surge BYPASSED for affected traffic; effective multiplier = `(static congestionChargeMultiplier or 1.0) × (1 + p/100)`, tagged `BaseFareAndExtraDistanceFare` when policy had none |

At least one scale required; each within ±50. All independent — an adjustment may set
any subset.

> **Pre-existing calculator nuance (fix in Phase 0):** when both `congestionChargePerMin`
> and `congestionChargeMultiplier` are set on a policy, `FareCalculator.hs:581` does
> `perMin <|> multiplier` — the multiplier is silently dropped despite the "apply both"
> comment at `SharedLogic/FarePolicy.hs:319`. Decide intended semantics (sum both vs
> per-min-wins) and make code + comment agree BEFORE layering adjustments on congestion.

## 4. Backend design

### 4.1 Storage spec — `spec/Storage/FareAdjustment.yaml` (driver-app)

```yaml
FareAdjustment:
  tableName: fare_adjustment
  types:
    FareAdjustmentMode:   { enum: "EXPERIMENT, SPIKE" }
    FareAdjustmentStatus: { enum: "DRAFT, ACTIVE, ENDED, EXPIRED" }  # CONCLUDED/ABORTED collapsed 2026-09-28: mechanically identical, and "Conclude" misread as "apply the fare to everyone". ENDED = manual, EXPIRED = window lapsed.
  fields:
    id: Id FareAdjustment
    merchantId: Id Merchant
    merchantOperatingCityId: Id MerchantOperatingCity
    vehicleServiceTiers: "[ServiceTierType]"   # one adjustment may span tiers
    areas: Maybe [Area]                        # Nothing = all pickup areas
    mode: FareAdjustmentMode
    status: FareAdjustmentStatus
    baseFareScalePct: Maybe Double
    perKmRateScalePct: Maybe Double
    perMinRateScalePct: Maybe Double
    congestionScalePct: Maybe Double
    rolloutPercentage: Maybe Int               # EXPERIMENT only, 1..99
    validFrom: Maybe UTCTime                   # SPIKE: required
    validTill: Maybe UTCTime                   # SPIKE: required ≤24h; EXPERIMENT: auto-set +60d cap
    reason: Text                               # mandatory human "why"
    createdBy: Text
```

Same storage conventions as SurgeConfig: `areas`/`vehicleServiceTiers` as single JSON
text columns via toTType/fromTType (KV-drainer list-column hazard — see SurgeConfig.yaml
comment), CachedQuery keyed by city (cleared on every status change), status transitions
serialized under a per-city Redis lock (mirror `surgeStatusLockKey`).

**Write-time validation** (mirror `validateConfigReq` strictness):
- at least one scale; every scale in [-50, 50]; rolloutPercentage in [1, 99] iff EXPERIMENT;
- SPIKE: validFrom < validTill, span ≤ 24h; EXPERIMENT: validTill ≤ now + 60d;
- areas must pass shape validation (`Default | Pickup_<id> | …`), pickup-side only;
- **overlap rejection**: activation fails if another ACTIVE adjustment intersects on
  (city ∩ tiers ∩ areas ∩ same parameter). One live lever per parameter per slice —
  no stacking, no ambiguity.

### 4.2 Evaluation wiring

Single integration point: `getFullFarePolicy` (`SharedLogic/FarePolicy.hs`), right after
the policy is loaded and the congestion model resolved — the same place
`updateCongestionChargeMultiplier` already rewrites the policy today.

1. **Match**: cached ACTIVE adjustments for city, filtered by tier ∈ `vehicleServiceTiers`,
   `fareProduct.area` ∈ areas (or areas = Nothing), tripCategory = OneWay, policy details =
   Progressive, `validFrom ≤ now < validTill` (checked at evaluation time — this IS the
   auto-expiry; no sweeper needed for correctness, a lazy status-flip job just keeps the
   list tidy).
2. **Arm** (initial pricing only):
   - SPIKE → applied to everyone in scope.
   - EXPERIMENT → `md5(adjustmentId <> phone) mod 100 < rolloutPercentage` ⇒ treatment;
     missing phone ⇒ control.
   - **Pin per transaction** (pattern of `surgePinKey`, same 30d TTL, cross-app Redis):
     `driver-offer:FareAdjustment:Pin:<ctxId>` → `{adjustmentId, arm}` (or explicit
     `none`). Select-phase re-resolution and end-ride recompute replay the pin — a ride
     priced under a spike keeps its pricing at end-ride even after expiry, and never
     gains an adjustment it wasn't priced with. Mirrors surge-pin semantics exactly.
3. **Apply** (treatment only): scale the FullFarePolicy's progressive fields per §3.1.
   When `congestionScalePct` is set, skip the surge/congestion model call entirely for
   this evaluation (decision: replaces surge) and stamp
   `dpVersion = "FareAdjustment:<shortId>"` so engine attribution stays truthful.
4. **Stamp**: thread `fareAdjustmentId :: Maybe Text` + `fareAdjustmentArm :: Maybe Text`
   ("treatment"/"control") through `CongestionChargeDetails` → `buildEstimate`, same
   plumbing as `shadowSurgeMultiplier`/`shadowSurgeVersion` in the revamp.

Note: `customerPhoneNum` lives on `DSearchReq` but pricing runs per fare product inside
`selectDriversAndMatchFarePolicies` → thread it (or the precomputed bucket) into
`getFullFarePolicy` alongside `txnId`. End-ride path needs no phone — the pin carries the arm.

### 4.3 Estimate / ClickHouse / measurement

- `Estimate.yaml`: + `fareAdjustmentId`, `fareAdjustmentArm` (both Maybe Text).
- CH Estimate model: + both columns (verify prod CH schema before cutover, as with
  `dp_version`).
- Pulse queries: arm-vs-arm aggregate per adjustment — estimates, avg fare, conversion
  (estimate→booking), cancellation rate, grouped by arm. Reuses the
  `pricingShadowComparisonByTier` query shape.

### 4.4 Dashboard API — extend `spec/.../Management/API/Pricing.yaml`

Same module/capability (`system-config.dynamic_logic.*`), proxy stamps `createdBy`:

- `GET  pricing/adjustment/list`
- `POST pricing/adjustment/create` (→ DRAFT)
- `POST pricing/adjustment/{id}/update` (DRAFT only)
- `POST pricing/adjustment/{id}/status` (activate / abort / conclude; per-city lock;
  activation runs overlap + cap validation; promotion of an experiment to 100% = ops
  applies the change via the normal rate-card save, then concludes — adjustments are
  never permanent)
- `POST pricing/adjustment/preview` — given sample fare inputs, return control vs
  treatment fare breakdown via the real calculator (reuse FarePolicyV2 preview plumbing)
- `GET  pricing/observability/adjustment/{id}` — arm-vs-arm results (§4.3)

Alerts: on ACTIVE / EXPIRED / CONCLUDED / ABORTED, fork an email to FareAlertSubscription
subscribers of the city (existing AREA_VEHICLES rails; add alert type ADJUSTMENTS).

## 5. Control-center UI plan

### 5.1 New "Pricing Changes" tab (the ops-facing simplification)

Three-verb mental model, one entry point:

1. **Edit rate card** → existing Rate Card save flow (permanent).
2. **Temporary boost** (SPIKE): wizard — pick tier(s) → optional areas → sliders for the
   4 params (±50 hard stop) → until <time> (≤24h) → reason → live **preview** (control vs
   boosted fare for a sample trip) → activate. Active spikes render as cards with
   countdown badge + one-click "End now". If congestion is touched and a surge table is
   ACTIVE: explicit warning "this pauses surge for <tier> until <time>".
3. **Run a test** (EXPERIMENT): same wizard + rollout % slider (copy `ManageSurge`'s
   `RolloutInput` pill pattern) → results panel inline: arm-vs-arm table (estimates,
   avg fare, conversion, delta with tone) once data flows. Conclude / abort buttons with
   consequence sentences (pattern of `SurgeStatusActions`).

History list (all past adjustments with reason/creator/outcome) doubles as the audit log.

### 5.2 Surge v3 (replaces BOTH existing surge tabs)

Design brief — merge, don't pick:
- **Overview first** (from surgeNew): per-tier matrix of what surges when (peaks ×
  status), read-only at a glance, real `ExcludedAreasPicker`.
- **Editing** (from classic): the row grid IS the truth — keep it, with the
  which-row-fires tester beside it. Direct save with confirm dialog; **no cart**.
- One lifecycle strip: version pills (DRAFT/SHADOW/ACTIVE/ARCHIVED), clone-to-new-version,
  shadow-vs-applied link into Pulse.
- Banner surface for interactions: active adjustment pausing surge; static-multiplier
  precedence note (absorb `CongestionSection`'s notice logic).
- Classic `surge/` and `surgeNew/` directories retired when v3 ships.

### 5.3 Pulse

Add "Adjustments" section: active/recent adjustments, arm-vs-arm table, and per-estimate
explain gains "Adjustment" row (id + arm + scales applied).

## 6. Phasing

- **Phase 0** — resolve the perMin/multiplier precedence nuance (§3.1 note); decide and
  fix calculator + comment.
- **Phase 1** — backend core: FareAdjustment spec + queries + cache, validation, status
  lifecycle + locks, evaluation wiring + pin, estimate stamping. Migration + CH columns.
- **Phase 2** — dashboard APIs + preview + alerts + capability seeds.
- **Phase 3** — UI: Pricing Changes tab (spike first — smallest loop), then experiment
  results; Pulse extension.
- **Phase 4** — Surge v3 consolidation (independent of 1–3; can proceed in parallel).
  **SKIPPED for now (user call, 2026-09-28)** — both existing surge tabs stay; revisit
  after the adjustments feature ships.
- Rollout: dogfood a SPIKE in one small city; first EXPERIMENT at ≤10% rollout.

## 7. Open items

- CH prod schema check for the two new estimate columns (same caveat as `dp_version`).
- Should spike/experiment creation honor `useSurgeConfigPricing = off` cities the same
  way? (Design says yes — adjustments are engine-independent; only the congestion param
  interacts with surge.)
- Multi-tier adjustment vs per-tier rows: spec models tiers as a list on one row; revisit
  if per-tier results reporting wants separate ids.
- Lazy EXPIRED status flip job (cosmetic only) — piggyback an existing scheduler?
