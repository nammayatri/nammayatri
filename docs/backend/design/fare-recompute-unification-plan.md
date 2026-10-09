# Fare Recompute Unification — Plan

Status: IMPLEMENTED IN SHADOW MODE (2026-10-07) — see §6
Owner: hemant
Scope: `app/provider-platform/dynamic-offer-driver-app` end-ride fare recomputation

## 0. Implementation status (2026-10-07)

All phases landed in shadow mode; the legacy ladder still bills.

| Piece | State | Where |
|---|---|---|
| Pure decision core `decideRecompute` | DONE (shadow) | `src/Domain/Action/UI/Ride/EndRide/RecomputeDecision.hs` |
| Shadow wiring + mismatch log/metric | DONE | `EndRide.hs` `shadowRecomputeDecision`; log tags `RecomputeDecisionShadow` / `RecomputeDecisionShadowMismatch`; counter label `recomputeDecisionShadowMismatch` |
| `ride.recompute_reason` persistence | DONE | Ride spec + `RideExtra.updateAll`; stores the full ShadowRecord JSON: reason, modifiers, distance/duration/pricing sources, predicted vs billed distance, mismatch flag. The recomputed FARE is not duplicated here — it is already on `ride.fare` / `fare_parameters` (vs `booking.estimated_fare`), and equals the shadow fare whenever mismatch=false; a true shadow fare on mismatch rides needs the side-effect-free recompute variant that is cutover-PR work |
| Toll matrix extraction (behavior-identical) | DONE (live) | `src/Domain/Action/UI/Ride/EndRide/TollDecision.hs`, replaces the inline if-tree |
| Config consolidation `mkRecomputeConfig` | DONE | decision core reads only `RecomputeConfig` |
| Dead configs removed from spec | DONE | `actualRideDistanceDiffThresholdIfWithinPickupDrop`, `approxRideDistanceDiffThreshold` dropped from Merchant.yaml + dashboard API spec (DB columns intentionally left in place) |
| `tripCategoriesForNoRecalc` configurable | DONE (live) | new `noRecomputeTripCategories` (defaults to old hardcoded list) |
| Duration gating levers | DONE (live, default-off) | band duration criteria, `actualRideDurationDiffThreshold`, `gateExtraTimeChargeByRecompute` |
| Golden table + toll truth-table tests | DONE | `hunit-tests/src/FareRecomputeDecisionTests.hs`, `TollDecisionTests.hs` |
| Cutover (bill from the decision core) | TODO | separate PR after shadow mismatches are zero in prod |
| Toll re-derivation on approx routes | TODO | deliberate follow-up (kept behavior-identical this round) |

Shadow blind spots (accepted): approx-route distance is IO-dependent so those
rides log `FailedOutsideUnknownApprox` and skip the numeric comparison; product
flags are read from the quoted policy, which can differ from the latest policy
on repriced branches; duration is not compared (levers are off by default).

### Unified policy + studio view (2026-10-09)

- **`fareRecomputePolicy`** (new nullable TransporterConfig column, typed
  `RecomputePolicy` in `Domain.Types.Extra.TransporterConfig`): ONE nested
  JSON — `upward{allowWithinThreshold,bands,smallOverageForgivenessMeters,
  bufferMeters,bufferPercentage,dailyExtraKmsBudget,weeklyExtraKmsBudget}`,
  `downward{allowForChangedDestination,forgivenessMeters,
  passThroughMinEstimateMeters}`, `time{overageForgivenessSeconds,
  gateExtraTimeOnEstimateBilled}`, `pinnedTripCategories` — superseding the 12
  scattered recompute columns. `mkRecomputeConfig` resolves policy-first,
  field-by-field, falling back to the legacy columns; the live ladder and the
  shadow core both read only the resolved config, so NULL policy = today's
  behavior exactly. Backfill (behavior-preserving, per city):
  `dev/sql-seed/fare-recompute-policy-backfill.sql`. Legacy columns stay (no
  drops); they go dormant per city after backfill.
- **Fare Policy Studio "Recomputation" tab** (control-center repo):
  `src/modules/config/FarePolicyStudio/components/recompute/RecomputeTab.tsx`
  + `utils/recomputeConfig.ts` (+ tests). Reads the effective TransporterConfig
  row via Config Pilot, shows every lever grouped by section with a
  policy/legacy/default source badge, the raw policy JSON, and a suggestions
  panel encoding the scenario-matrix recommendations (backfill nudge, 2-min
  time floor, band duration criteria, gating sanity, buffer% ceiling).

### Policy-only cutover + per-minute time billing (2026-10-09, later)

- **Legacy columns removed from the spec** (13 recompute columns dropped from
  Merchant.yaml incl. the DistanceRecomputeConfigs type; DB columns remain for
  rollback). `mkRecomputeConfig` now resolves fareRecomputePolicy -> CODE
  defaults only (defaults mirror the old column defaults, incl. the 4-band
  40/30/20/10 table as `defaultRecomputeBands`). **DEPLOY ORDER: run the
  backfill for every city with custom legacy values FIRST** — the SQL header
  says the same.
- **New time levers** (policy `time.*`, default off):
  `billPerMinOnChargeableDuration` — perMinRateSections price the chargeable
  duration at recompute (implemented by feeding chargeable duration into the
  calculator's duration input; extra-time charge self-cancels on unfloored
  rides, so no double counting); `floorAtEstimateWithinThreshold` — no time
  refunds at the booked destination. Decision core mirrors both
  (`TimeFlooredAtEstimate` modifier).
- **Dead state removed**: `SnapToRoadState.distanceTravelledOutSideDropThreshold`
  + `getTravelledDistanceOutsideThreshold` deleted from lib/location-updates
  (tracked on every ride, read by nothing). Old Redis entries decode fine
  (extra/missing Maybe key).
- **Studio**: all seven time levers always listed (configured or not, incl.
  the two fare-policy-level ones), scenario rows react to the new levers, and
  the suggestions panel flags per-min-on-actual without a floor / without a
  duration cap.

### Adversarial audit outcome (2026-10-09, line-by-line re-verification)

An independent audit of the full diff CONFIRMED: shadow/live branch-for-branch
arithmetic parity, duration guard ordering and mutual exclusion, policy-only
resolution with correct defaults, extra-time suppression completeness, and
band edge cases (strict comparisons). It found and we FIXED:

1. 5-min time forgiveness was cross-category — it would have zeroed rental
   per-extra-minute and intercity extra-time billing for sub-5-min overruns.
   Duration levers (forgiveness/gating) now bind only on city pricing
   (Progressive/Slabs policies) via ProductFlags.timeLeversApplicable.
2. Backfill hole: a city with NULL legacy bands (upward recompute disabled)
   would have fallen to the default bands and silently re-enabled it. The
   backfill now writes an explicit `bands: []` for NULL.
3. Shadow could never evaluate the failed-distance ladder (approx distance
   was not threaded; every such ride persisted FailedOutsideUnknownApprox).
   The Directions-API approx is now returned by
   calculateFinalValuesForFailedDistanceCalculations and fed to the shadow.
4. ShadowRecord mislabeling: the downward floor overwrote the pass-through
   override's UseEstimatedDuration with UseFlooredDuration; now preserved.

ACCEPTED + DOCUMENTED (not bugs, but real semantics to know):
- Per-min-sections override side effects: night-shift PRORATION
  (pickupBufferInSecsForNightShiftCal path) runs over chargeable instead of
  estimated minutes (small, capped); per-min CONGESTION uses chargeable
  minutes only when no estimated congestion charge exists or on the
  congestion-recompute path (estimate takes precedence at
  FareCalculator.hs:618, and the component is capped).
- Shadow reads product flags from the QUOTED policy; LatestPolicy branches
  may use a different policy — known blind spot, mismatch-rate noise only.
- Weekly extra-km budget boundary: live compares fractional pre-persist,
  shadow reads the rounded window — divergence only exactly at the boundary.
- A malformed fareRecomputePolicy JSON decodes to Nothing (fleet defaults)
  with no error — same failure mode as every JSON config column here.
- Pre-existing: per-min section minutes are FLOORED (`div 60` before
  ceiling), so up to 59s of actual overage per ride is uncharged — relevant
  now that sections bill actual minutes.

### Scenario-matrix gaps (not yet expressible; follow-ups)

| Gap | Scenarios | Needed |
|---|---|---|
| Enforceable TOTAL-fare recompute ceiling (e.g. 1.3x) | 6, 13 | `totalFareCapMultiplier` in policy, enforced in `checkRecomputedFareCeiling` (log-only today) |
| Auto support ticket on decision reasons | 13, 14 | hook issue-management on `RecomputeReason` (large-deviation / anomaly) |
| Destination-edit ">50% travelled -> no recompute" rule | 9 | new rule in edit flow keyed on travelled/estimate ratio |
| Physical-anomaly detection (speed bounds) -> force estimate | 14 | pre-decision sanity check feeding a new reason |
| Exact max(current, recomputed) fare | 4 | approximated today by estimate-floor + extra-time |
| Force estimate (skip approx ladder) on failed+outside | 15 strict | policy flag short-circuiting the approx route |

### Config lever reference (complete)

Decision ladder (TransporterConfig unless noted):

| Lever | Default | Effect |
|---|---|---|
| `pickupLocThreshold` / `dropLocThreshold` | per city | define `pickupDropOutsideOfThreshold`, the master branch switch |
| `recomputeIfPickupDropNotOutsideOfThreshold` | true | allow upward recompute within threshold; also gates mid-ride snap-on-deviation |
| `recomputeDistanceThresholds` bands | 40/30/20/10% by est. distance | when an upward diff qualifies; NEW per-band `minThresholdDurationSeconds` / `minThresholdDurationPercentage` (null = off) let time overage qualify |
| `actualRideDistanceDiffThreshold` | 1200 m | overage tolerance on changed-destination rides |
| `upwardsRecomputeBuffer` / `upwardsRecomputeBufferPercentage` | 2000 m / null | upward distance growth ceiling |
| `fareRecomputeDailyExtraKmsThreshold` / `fareRecomputeWeeklyExtraKmsThreshold` | 5 km / 20 km | per-driver anti-fraud extra-km budget |
| `enableDownwardRecomputeForDifferentDestination` | null (=true) | downward recompute for `noRecomputeTripCategories` rides |
| `downwardRecomputeDistanceThreshold` | null (=0, off) | shortfall below this is forgiven back to the estimate |
| `minThresholdForPassThroughDestination` | null (=always) | min estimate for the pass-through-drop override |
| NEW `noRecomputeTripCategories` | null (= OneWayRideOtp + OneWayOnDemandDynamicOffer) | categories pinned to estimate in pass-through/downward rules |
| NEW `actualRideDurationDiffThreshold` | null (off) | duration overage strictly below this bills the estimated duration (no extra-time charge) |
| NEW `gateExtraTimeChargeByRecompute` | null (off) | estimate-billed rides bill estimated duration, so extra-time charge never fires on forgiven rides |

Fare step (FareProduct / FarePolicy):

| Lever | Effect |
|---|---|
| `disableRecompute` | total bypass: estimate distance + estimated fare, no new FareParameters |
| `disableDownwardRecompute` | ratchet: distance and duration floored at the estimate |
| `fareRecomputeCapConfig` | per-component caps on `FCRecompute`; uncapped components can never grow |
| `perMinuteRideExtraTimeCharge` + `rideExtraTimeChargeGracePeriod` | the extra-time charge itself |
| `recomputeCongestionChargeOnEndRide` (TransporterConfig) | re-run congestion model at end ride |
| `enableEstimatedTollFallback` (TransporterConfig) | estimated toll when GPS dark at gates |

## 1. Problem

The end-ride fare recomputation is correct but scattered and hard to reason about:

- **Decision logic lives in 5+ places**: the inline branch point in `endRideHandler`
  (`EndRide.hs:569-575`), `calculateFinalValuesForCorrectDistanceCalculations`
  (`:979-1029`), `calculateFinalValuesForFailedDistanceCalculations` (`:1031-1064`),
  `shouldUpwardRecompute` (`:1066-1074`), the pass-through-drop override buried
  *inside* `recalculateFareForDistance` (`:792`), plus special-cased Cron /
  odometer / rectification branches (`:449-471`).
- **Config is scattered**: ~15 live TransporterConfig fields, 2 dead ones
  (`actualRideDistanceDiffThresholdIfWithinPickupDrop`,
  `approxRideDistanceDiffThreshold` — read nowhere), FareProduct kill-switches
  (`disableRecompute`, `disableDownwardRecompute`), FarePolicy cap config, and
  Redis-held per-driver extra-km budgets.
- **Boolean soup**: `recomputeWithLatestPricing`, `pickupDropOutsideOfThreshold`,
  `distanceCalculationFailed`, `passedThroughDrop` are threaded as positional
  `Bool`s; the *reason* a ride was charged estimate-vs-actual is implicit and
  unlogged, making production "why was the fare X?" questions archaeology.
- **Asymmetry**: recompute gating is distance-only. Duration has no diff
  threshold, no bands, no anti-fraud budget — it only enters as a charge
  (`perMinuteRideExtraTimeCharge` + `rideExtraTimeChargeGracePeriod` in
  FarePolicy → `RideExtraTimeFareComponent`) and via rental/intercity details.
- **The toll reconciliation matrix** (`EndRide.hs:507-563`) is a ~60-line nested
  if-tree with no tests and two standing TODOs (tolls not re-derived on the
  approx-distance recompute branches, `:1049`, `:1057`).

## 2. Current decision inventory (what must be preserved)

Signals collected at end ride:

| Signal | Source |
|---|---|
| `traveledDistance` | snap-to-road accumulation (`SnapToRoadState`) |
| `distanceCalculationFailed` | Redis `<driverId>:locationUpdatesFailed` |
| `pickupDropOutsideOfThreshold` | `pickupLocThreshold` / `dropLocThreshold` vs actual endpoints |
| `passedThroughDrop` | `SnapToRoadState.passThroughDropThreshold` latch |
| odometer delta | `endOdometer − startOdometer` (odometer categories) |
| approx route distance | Directions API over ≤7 sampled raw points (failure path) |
| extra-km budgets | Redis `DailyExtraKms:` + 7-day sliding window |
| actual duration | `now − tripStartTime` |

Current outcome space (chargeable distance source × pricing source):

| # | Branch | Distance charged | Fare policy |
|---|---|---|---|
| 1 | CronJob | estimate | quoted |
| 2 | Odometer category | odometer delta | quoted |
| 3 | Rectification category | traveled (estimate if failed) | quoted |
| 4 | OK + within threshold + upward band + budget OK | min(traveled, maxEstimated+buffer) | quoted |
| 5 | OK + within threshold otherwise | estimate | quoted |
| 6 | OK + outside + shorter (downward enabled) | traveled | **latest** |
| 7 | OK + outside + shorter (downward disabled) | estimate | quoted |
| 8 | OK + outside + longer < `actualRideDistanceDiffThreshold` | estimate | **latest** |
| 9 | OK + outside + longer ≥ threshold | traveled | **latest** |
| 10 | Failed + within threshold | estimate | quoted |
| 11 | Failed + outside (ladder on approx distance) | approx / estimate / estimate+buffer cap | mixed |
| 12 | Pass-through-drop override (OTP/dyn-offer) | estimate (forced) | — |
| 13 | `farePolicy.disableRecompute` | estimate, no new FareParameters | — |
| 14 | `disableDownwardRecompute` | max(chosen, estimate), duration max'd too | — |

Duration today: billed via `calculateExtraTimeFare` (`FareCalculator.hs:1033-1041`)
= `max 0 (actual − (estimated + gracePeriod))` minutes × per-minute rate, capped as
`RideExtraTimeFareComponent`. No gating, no budget, no threshold config outside the
fare policy. `perMinRateSections` price *estimated* duration, not actual. Night
shift end time derives from actual duration.

## 3. Target design

### Phase 1 — Pure decision core (no behavior change)

New module `Domain.Action.UI.Ride.EndRide.RecomputeDecision` (pure, no IO):

```haskell
data RecomputeInput = RecomputeInput
  { requestSource        :: RequestSource          -- Driver | Dashboard | CallBased | CronJob
  , tripCategory         :: TripCategory
  , estimated            :: TripMeasure            -- distance + duration + maxEstimatedDistance
  , actual               :: ActualMeasure          -- traveled, odometer, approxRoute, duration
  , gps                  :: GpsQuality             -- distanceCalculationFailed, numberOfSelfTuned
  , endpoints            :: EndpointMatch          -- pickupDropOutsideOfThreshold, passedThroughDrop
  , budgets              :: ExtraKmBudget          -- daily/weekly spent vs thresholds
  , cfg                  :: RecomputeConfig        -- see Phase 2
  , productFlags         :: ProductFlags           -- disableRecompute, disableDownwardRecompute
  }

data DistanceSource = UseEstimate | UseActual | UseActualCapped Meters
                    | UseOdometer | UseApproxRoute
data PricingSource  = QuotedPolicy | LatestPolicy

data RecomputeDecision = RecomputeDecision
  { distanceSource :: DistanceSource
  , durationSource :: DurationSource               -- symmetric, Phase 3
  , pricingSource  :: PricingSource
  , reason         :: RecomputeReason              -- enum, one per row of §2 table
  }

decideRecompute :: RecomputeInput -> RecomputeDecision
```

- `endRideHandler` becomes: **gather inputs → decide → execute**. Exactly one
  call into `recalculateFareForDistance`, parameterized by the decision; the
  pass-through-drop override moves out of `recalculateFareForDistance` into the
  decision (it is a decision, not a fare step).
- Table-driven unit tests: one test row per row of the §2 table, generated-case
  coverage over the input space. This is the first time the ladder becomes
  exhaustively testable.

### Phase 2 — Config consolidation

- New read-side record `RecomputeConfig` assembled from TransporterConfig in one
  place (`mkRecomputeConfig :: TransporterConfig -> RecomputeConfig`), so the
  decision module never touches raw TransporterConfig. Fields: pickup/drop
  thresholds, `recomputeIfPickupDropNotOutsideOfThreshold`,
  `recomputeDistanceThresholds`, `actualRideDistanceDiffThreshold`, upward
  buffer + percentage, extra-km thresholds, downward toggle,
  `minThresholdForPassThroughDestination`, congestion recompute flag, toll
  fallback flag.
- Drop the two dead columns (migration + spec cleanup):
  `actual_ride_distance_diff_threshold_if_within_pickup_drop`,
  `approx_ride_distance_diff_threshold`.
- Replace the hardcoded `tripCategoriesForNoRecalc` list with a field on
  `RecomputeConfig` (defaulted to today's list) so per-category behavior is
  data, not code.

### Phase 3 — Symmetric duration gating (new capability)

Answering "recompute if time also increases above threshold":

- Generalize `recomputeDistanceThresholds` to `recomputeThresholds ::
  [RecomputeBand]` where a band can carry distance criteria, duration criteria,
  or both (`minThresholdDurationSeconds`, `minThresholdDurationPercentage`).
  Backward compatible: existing rows have no duration criteria.
- `decideRecompute` emits `durationSource` the same way it emits
  `distanceSource` (estimate vs actual vs capped), replacing today's implicit
  `(recalcDistance', actualDuration)` pairing and the ad-hoc max in
  `disableDownwardRecompute`.
- Extra-time charge keeps living in the fare calculator, but becomes gated by
  the same decision (no extra-time billing on rows that charge the estimate)
  and optionally by a per-driver extra-minutes budget mirroring the extra-km
  one. Today `calculateExtraTimeFare` fires whenever the policy defines a rate,
  even on estimate-charged rides — decide explicitly whether that is intended;
  if yes, encode it as a documented rule in the decision table.

### Phase 4 — Toll decision extraction

- Pure `decideTollBilling :: TollInput -> TollBilling` replacing
  `EndRide.hs:507-563`, with the truth table written out in the module header
  and tested. Inputs: detected tolls, pending-validated tolls, estimated tolls,
  `distanceCalculationFailure` (incl. self-tuned), `pickupDropOutsideOfThreshold`,
  `driverDeviatedToTollRoute`, `enableEstimatedTollFallback`. Output: charges,
  names, ids, confidence.
- Resolve the two TODOs: on `UseApproxRoute` / downward branches, re-derive
  tolls from the approx route polyline (TollsDetector already exposes
  route-based lookup) instead of carrying possibly-stale detected tolls.

### Phase 5 — Observability

- Persist `recomputeReason` (the enum) on `fare_parameters` (or ride) and emit
  one structured log line + a counter metric labeled by reason. Support/ops can
  then answer "why this fare" without reading code.
- Keep the existing fare/distance diff metrics (`putDiffMetric`) unchanged.

## 4. Rollout & safety

1. Phases 1–2 are **behavior-identical refactors**: land with golden tests that
   pin the §2 table; CI fails if any row's outcome changes.
2. Shadow mode for one release: compute the new `RecomputeDecision` alongside
   the old ladder, log mismatches (`recompute_decision_mismatch` metric), make
   no behavioral use of it. Zero mismatches in prod for N days → switch.
3. Phase 3 ships dark (duration criteria absent from all configured bands) and
   is enabled per city via config.
4. Phases 4–5 follow independently; Phase 4 also shadow-compares toll outputs.

## 5. Non-goals

- No change to the during-ride snap-to-road pipeline or its Redis layout.
- No change to fare *component* math (`calculateFareParameters`), cap
  strategies, or Beckn messaging.
- Not part of the parked mobility-flows/building-blocks branch; this is an
  on-main refactor of existing modules.
