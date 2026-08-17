# QAR-driven Tip Module Cadence Config for the Rider Search Screen

**Date:** 2026-08-16
**Service:** rider-app (BAP) + one enum constructor in `lib/yudhishthira`
**Scope:** Backend only. Rider frontend consumes the new fields; the UI work lives in a separate repo.

## Problem

While a search is in progress the rider app can show an "add tip" module. Adding a tip
cancels the current search and re-fires `/select2` with a `customerExtraFee`. Today the
app has no signal for **when to first show** that module, **how often to repeat** it, or
**how many times** — and product wants that cadence to depend on how likely drivers are
to accept in the current city/area right now (**QAR — quote acceptance rate**), so that
low acceptance nudges early and often while healthy acceptance nudges little or not at all.

Nothing exists for this today: `RiderConfig` has no tip fields, `/select2` returns no
timer, and no per-estimate cadence is exposed. What *does* exist:

- The BPP computes QAR at search time and already ships it in `on_search` as the `QAR`
  INFO tag; rider-app persists it as `estimate.qar` (`spec/Storage/estimate.yaml:138`)
  but does not return it in the estimate API.
- The BPP's `DYNAMIC_PRICING_UNIFIED` JSON-logic domain already turns QAR into
  `smartTipSuggestion` / `smartTipReason` / `tipOptions`, all exposed on
  `EstimateAPIEntity`.
- `/select2` is already the "cancel previous estimate + re-select with tip" primitive
  (`API/UI/Select.hs:112-129`, per-person Redis lock).
- Rider-app already hosts small JSON-logic decision domains modelled on
  `SharedLogic/PickupETA.hs` (`PICKUP_ETA_CALCULATION`): input record → versioned rules
  → output record, with per-city percentage rollout and a ClickHouse debug log.

## Decisions taken during design

| Question | Decision | Why |
|---|---|---|
| What the UI needs | **Time-based**: `showAfterSec`, `repeatIntervalSec`, `maxPrompts` | Simple contract; independent of allocator batch internals |
| Where the QAR → cadence mapping runs | **BPP `DYNAMIC_PRICING_UNIFIED`** — one more output field (`tipModuleConfig`) of the domain that already turns QAR into `smartTipSuggestion` | One rule set for all tip behaviour, tuned by the same people; rules see the full `DynamicPricingData` (QAR current+past, S/D ratio, congestion, tier, distance…) at native granularity; no new domain, no new engine call site |
| Transport | New `on_search` INFO tag `TIP_MODULE_CONFIG` (JSON-encoded `TipModuleConfig`), VNP-gated like `SMART_TIP_SUGGESTION` | Same path the existing tip signals take |
| Fallback when the BPP sends nothing | **Per-city default on `RiderConfig`**, else `null` (UI hides the module) | Nudging still works before rules exist / for non-VNP BPPs; ops control per city |
| Extras in scope | Expose `estimate.qar` on the API entity; `validTill` check on `/select2` | Cheap, adjacent |

History: an earlier revision implemented this as a rider-side domain (`TIP_MODULE_CONFIG`) on the persisted
`estimate.qar`. It was switched to the BPP domain so tip suggestion and tip cadence are one policy;
the rider-side domain, module and dashboard arms were removed.

Out of scope: mid-search re-evaluation of QAR (the value is the search-time snapshot either way),
the `mbActualQARCity = Nothing` quirk in BPP `FarePolicy.hs`, making the `1..100000` tip bound
configurable, multimodal (`Lib/JourneyLeg/Taxi.hs` hardcodes no tip).

## Design

### 1. Shared type and API contract

`lib/beckn-spec/src/Domain/Types/TipModuleConfig.hs` (used by both apps):

```haskell
data TipModuleConfig = TipModuleConfig
  { showAfterSec      :: Int   -- first prompt after N seconds of searching (from select2)
  , repeatIntervalSec :: Int   -- re-prompt cadence; 0 = never repeat
  , maxPrompts        :: Int   -- hard cap per search
  } deriving (Show, Eq, Ord, Generic, FromJSON, ToJSON, ToSchema)
```

`EstimateAPIEntity` (rider `Domain/Action/UI/Estimate.hs`) gains:

- `tipModuleConfig :: Maybe TipModuleConfig` — `estimate.tipModuleConfig <|> riderConfig.tipModuleConfig`, else `null`.
- `qar :: Maybe Double` — the persisted `estimate.qar`, informational.

UI behaviour: on `select2` start a timer; show at `showAfterSec`; repeat every `repeatIntervalSec`
while prompts shown `< maxPrompts` (`0` = never repeat); `null` → never show.

### 2. BPP: `DYNAMIC_PRICING_UNIFIED` output

- `SharedLogic/DynamicPricing.hs` — `DynamicPricingResult.tipModuleConfig :: Maybe TipModuleConfig`.
  Rules set it like any other output key; the dashboard verify/schema for the domain pick it up
  automatically (`Proxy DynamicPricingResult`).
- `SharedLogic/FarePolicy.hs` — carried on `CongestionChargeDetailsModel` (all result branches) and
  into `Domain.Types.FarePolicy.CongestionChargeDetails` / `FullFarePolicy.tipModuleConfig`.
- `spec/Storage/Estimate.yaml` — `tipModuleConfig :: Maybe TipModuleConfig` (json column);
  `Domain/Action/Beckn/Search.hs` sets it from `fullFarePolicy.tipModuleConfig`.
- `Beckn/OnDemand/Utils/Common.hs` — `Pricing.tipModuleConfig`; `mkGeneralInfoTagGroup` emits
  `Tags.TIP_MODULE_CONFIG ~=? guardVNP (encodeToText <$> pricing.tipModuleConfig)`.
- `lib/beckn-spec/.../Tags.hs` — `TIP_MODULE_CONFIG` in the enum and the `INFO` group.

Rule authoring: `tipModuleConfig` must be set **before** the final per-tier selector step
(`{"var":{"var":"serviceTier"}}`) or inside each tier's object, because the selector drops the
top-level keys. Example step (nested `if`s — `json-logic-hs` `if` is 3-arg only):

```
{"cat":[{"var":""},{"tipModuleConfig":{"cat":[
  {"showAfterSec":     {"if":[{"==":[{"var":"qar"},null]},45,{"if":[{"<":[{"var":"qar"},30]},15,{"if":[{"<":[{"var":"qar"},60]},30,60]}]}]}},
  {"repeatIntervalSec":{"if":[{"==":[{"var":"qar"},null]},60,{"if":[{"<":[{"var":"qar"},30]},30,{"if":[{"<":[{"var":"qar"},60]},45,0]}]}]}},
  {"maxPrompts":       {"if":[{"==":[{"var":"qar"},null]},1, {"if":[{"<":[{"var":"qar"},30]},3, {"if":[{"<":[{"var":"qar"},60]},2, 1]}]}]}}]}}]}
```
(`qar` here is the percentage the existing v20 program derives from `actualQAR` in its step 0.)

### 3. Rider: parse, persist, expose

- `Beckn/OnDemand/Utils/OnSearch.hs` — `getTipModuleConfig :: Spec.Item -> Maybe TipModuleConfig`
  (`getTagV2 INFO TIP_MODULE_CONFIG >>= decodeFromText`).
- `Beckn/OnDemand/Transformer/OnSearch.hs`, `Domain/Action/Beckn/OnSearch.hs` (`EstimateInfo`) →
  `spec/Storage/estimate.yaml` `tipModuleConfig :: Maybe TipModuleConfig` (json column).
- `spec/Storage/RiderConfig.yaml` — `tipModuleConfig :: Maybe TipModuleConfig` (json), the per-city
  fallback; served through config-pilot like every RiderConfig field.
- `Domain/Action/UI/Estimate.hs` — `mkEstimateAPIEntity` takes the city default and returns
  `estimate value <|> default`; `Domain/Action/UI/Quote.hs getEstimates` passes
  `riderConfig >>= (.tipModuleConfig)`.

### 4. `select2` expiry guard

`API/UI/Select.hs select2'`: before `cancelSearchUtil`, if not a multimodal leg, throw
`InvalidRequest "Estimate expired …"` when `estimate.validTill < now` (v1 `select` unchanged).

### 5. Seed and tests

- `dev/feature-migrations/0049-tip-module-config.sql` — RiderConfig default `{45,60,1}` for
  NAMMA_YATRI cities where NULL. Production rules are authored via the provider dashboard.
- Integration suite `dev/integration-tests/collections/TipModuleConfigFlow/` (`./run-tests.sh tip-module`):
  verify QAR bands on the provider dashboard, author + roll out a pricing-neutral test version, assert
  `/results` (rules path, poll stability, fallback at 0%), `select2` 400, restore the rollout.

### Data flow

```
BPP /search: DynamicPricingData{actualQAR,…} → DYNAMIC-PRICING-UNIFIED rules → DynamicPricingResult.tipModuleConfig
   → FullFarePolicy → Estimate.tipModuleConfig → on_search INFO tag TIP_MODULE_CONFIG (VNP only)
BAP on_search: tag → EstimateInfo → estimate.tipModuleConfig (persisted)
UI polls /rideSearch/{id}/results → EstimateAPIEntity.tipModuleConfig = estimate value <|> RiderConfig default; + qar
UI: select2 → timer(showAfterSec, repeatIntervalSec, maxPrompts) → add tip → select2 again
```

### Error handling

| Situation | Result |
|---|---|
| No DP rollout / rules don't set `tipModuleConfig` / non-VNP BPP | tag absent → `estimate.tipModuleConfig = Nothing` → RiderConfig default |
| Rule output doesn't decode as `DynamicPricingResult` | existing BPP fallback (static pricing, no tip fields) |
| Tag present but not valid JSON | `decodeFromText` → `Nothing` → RiderConfig default |
| RiderConfig default unset | `tipModuleConfig: null` → UI hides |
| `select2` on expired estimate | `InvalidRequest "Estimate expired …"`, search not cancelled |
