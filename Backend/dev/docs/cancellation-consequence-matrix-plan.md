# Cancellation Consequence Matrix — Design Plan (Phase D)

Status: **CORE IMPLEMENTED** (2026-08-20) — table spec, resolver, and runtime wiring are in;
dashboard CRUD/CSV + validations are NOT yet built (rows are seeded via SQL until then).
Decisions locked earlier: no JsonLogic fallback on matrix miss (matrix is authoritative),
rule-name registry is GLOBAL (not per city), SharedFault needs explicit rows, coins+money
additive with an upsert warning.

## Implemented

- `spec/Storage/CancellationConsequenceMatrix.yaml` — table + `ConsequenceChargeType` /
  `ConsequenceCollectionMode` enums (run `, run-generator`; migrations are generator-emitted).
- `spec/Storage/CancellationDuesDetails.yaml` — new `cancellationConsequenceRowId` audit
  column; the applied row id flows outcome → `ApplyCancellationChargeReq` → dues row.
- `Storage/CachedQueries/CancellationConsequenceMatrix.hs` — per-city cache; deletes via
  `runInMultiCloudRedisWrite` (both clouds). Writers must call `clearCacheByCity`.
- `SharedLogic/CancellationConsequence.hs` — `resolveConsequence` (most-specific-wins,
  weights 32/16/8/4/2/1+1, ties by lowest id), `getOrResolveConsequence` (once-per-ride
  Redis cache so charge calc and coin fork apply the SAME row), `computeCustomerCharge`
  (flat / % of fare, min-floor then max-cap so max wins a misconfigured min>max),
  global registry reader over system_configs (`cancellation_fault_rule_registry`).
- **Runtime cutover done**: `customerCancellationChargesCalculation` now reads the matrix
  (USER_CANCELLATION_DUES JsonLogic no longer runs); `Coins.validateCancellation` takes
  `driverCoins` from the matrix (CANCELLATION_COIN_POLICY no longer runs). The
  `transporterConfig.cancellationFeePaymentMethodExceptions` check was removed — Cash
  exemption is now a Cash-dimension row. `canAddCancellationFee` remains the kill-switch;
  the stage-3 verdict/tag charge-eligibility gate still applies before the matrix.

Registry seeding example:
```sql
INSERT INTO atlas_driver_offer_bpp.system_configs (id, config_value)
VALUES ('cancellation_fault_rule_registry',
        '[{"name":"pickup_stall","description":"Driver stalled en route to pickup","active":true},
          {"name":"customer_no_show","description":"Customer did not show up at pickup","active":true}]');
```

## Deduction schema (2026-08-21, user's design)

Amounts are SUM TYPES stored as JSON columns (`Domain.Types.Extra.CancellationConsequenceMatrix`,
`mkBeamInstancesForJSON`), separate fields per party:

- `customerDeduction` / `driverDeduction :: Maybe ConsequenceDeduction` where
  `ConsequenceDeduction = CoinDeduction {coins, expirySeconds} | MoneyDeduction MoneyDeduction`
  (coin vs money is EXCLUSIVE per party by construction — supersedes the earlier
  "additive with warning" decision), and
  `MoneyDeduction = FixedMoney {amount, overdueAmount} | PercentageMoney {percentage, minAmount, maxAmount}`
  (percentage of estimated fare, clamped; max cap wins a misconfigured min>max).
- `customerCommissionAndTax :: Maybe CommissionAndTax` — `taxPercentage` (tax is ALWAYS a
  percentage of the base) + `commission :: Maybe ChargeRate` where
  `ChargeRate = FixedRate {amount} | PercentageRate {percentage}` (of the base).
- A customer-side CoinDeduction is meaningless (riders have no coins) and yields no charge.

## Wired (all consequences now matrix-driven)

- Customer money incl. overdue (FixedMoney.overdueAmount) → dues pipeline.
- Driver coins + expiry override (`CoinDeduction.expirySeconds` beats coin_config expiry).
- Driver monetary penalty → `DriverCancellationPenalty` (replaced the
  `CancellationPenaltyApplicable` tag gate and `farePolicy.driverCancellationPenaltyAmount`).
- `blacklistDriverForRiderSeconds` (replaced the unconditional transporterConfig-TTL
  blacklist; applied before reallocation so it fires on reallocated cancels too).
- `countsTowardDriverCancellationRate` (replaced the `DriverCancellation#Valid` tag gate on
  the rate counter) and `countsTowardCustomerCancellationStats` (validCancellations counter).
- `waiveOffAllowed` — hard pre-check in the waive-off flow via the dues row's
  `cancellationConsequenceRowId` (the history-based waive-off logic still runs after it).
- `isDashboardBooking` threaded from `booking.isDashboardRequest` (`exemptDashboardBookings`).
- The stage-3 verdict/tag charge gate (`customerAtFaultOrLegacy`) is DELETED — eligibility
  lives entirely in the matrix dimensions. Resolution happens once, early in both cancel
  flows, via `buildConsequenceInputFromBooking` + the per-ride Redis cache.

## Completed in the final pass (2026-08-21)

- **Dashboard CRUD**: `CancellationConsequence` management API (CommonAPIs spec) — matrix
  list/create/update + global registry list/upsert — handler
  `Domain/Action/Dashboard/Management/CancellationConsequence.hs` with dimension parsing,
  registry cross-validation, and duplicate-dimension-tuple rejection; cache cleared
  cross-cloud on every write. (Requires the dashboard-side generator run.)
- **DriverScore**: `OnDriverCancellation` event carries `countsTowardCancellationRate`
  from the matrix (rideTags field removed); the behavior snapshot's
  `validDriverCancellation` variable now also feeds from it, so blocking rules keep their
  input name with the matrix as source.
- **Penalty preview parity**: `postPenaltyCheck` computes a NON-persisting verdict
  (`computeFaultVerdict` directly, no Redis/ride-row writes) + `resolveConsequence`
  (non-caching) — preview now returns exactly the matrix's driver money penalty.
  `buildPenaltyCheckContext` and the PenaltyCheck tag simulation were removed.
- **Driver notification**: `driverNotificationKey` → overlay via
  `ConsequenceDispatcher.sendOverlayByKey` in both cancel flows.
- **collectionMode (BPP side)**: persisted on the dues row
  (`cancellation_collection_mode`) via the outcome → apply pipeline.
- **Backfill**: `dev/feature-migrations/0050-cancellation-consequence-matrix-backfill.sql`
  — registry seed + per-city legacy-parity rows (blacklist TTL from transporter_config,
  Cash exemption where configured, driver penalty from fare_policy amounts, coins from
  coin_config), charge templates inserted **active=false** for per-city amount review.

## Refinements (2026-08-21, second pass)

- **Typed dimensions**: `cancelledBy :: Maybe CancellationType` and
  `paymentInstrument :: Maybe PaymentInstrument` are proper types now (CancellationType
  gained Eq/Ord/Read + beam instances). Stored text values are unchanged
  ("CancellationByDriver", "Cash", "Card_VISA"...).
- **Explicit additions (2026-08-22, supersedes the signed-amount scheme)**: direction
  lives in the CONSTRUCTOR, amounts are always positive — `CoinDeduction`/`CoinAddition`
  and `MoneyDeduction`/`MoneyAddition` (the signed scheme was confusing: legacy coin
  deductions were negative while money deductions were positive). The adapters in
  `CancellationConsequence` translate to the signed conventions the engines expect
  (coin engine: − deduct / + give; DCP: + penalty / − wallet credit; SCD: − fee =
  dues credit clamped at 0). Customer money addition only offsets outstanding dues (no
  payout rail); driver money addition needs the wallet (DriverFee rail logs + skips);
  tax/commission/overdue never apply to additions. The dashboard handler REJECTS
  non-positive amounts.
- **`USER_CANCELLATION_DUES_WAIVE_OFF` retired**: waive-off is fully matrix-driven —
  `waiveOffAllowed` + new `maxWaiveOffsPerPeriod` / `waiveOffPeriodDays` (default 30),
  enforced by counting WAIVED dues rows in the window. Pre-matrix dues rows (no
  consequence row id) are allowed through with a warning.
- **Deprecated code REMOVED** (LogicDomain/ApplicationEvent constructors kept only for
  backward compatibility with persisted rollout rows): `SharedLogic.CancellationCoins`
  deleted; `SharedLogic.UserCancellationDues` reduced to `CancellationLedgerAction`;
  NammaTag verify/schema/genToSchema wiring for the three retired domains removed;
  `PenaltyCheckTagData` + its `getLogicInputDef` case removed;
  `validCancellationPenaltyApplicable` constant removed.

## Orchestrator (2026-08-22, final consolidation)

All cancellation consequence triggering now lives in ONE module:
`SharedLogic/CancellationOrchestrator.hs`. Both cancel flows run the same three calls:

1. `decideCancellationConsequences` — signals → fault verdict → matrix row, resolved once
   (Redis-cached per ride) into a `CancellationDecision`.
2. `applyImmediateConsequences ctx doRateBlocking` — fires on EVERY cancel with a ride,
   before/regardless of reallocation. One dedicated executor per output column, each
   self-gating on its column: blacklist (`blacklistDriverForRiderSeconds`), driver overlay
   (`driverNotificationKey`), coin event (driverDeduction COIN, via the coin engine fork,
   ByUser/ByDriver sources only), driver money (driverDeduction MONEY — now row-driven for
   ALL sources, so customer-at-fault rows can pay the driver), rate counting
   (`countsTowardDriverCancellationRate`: ByDriver → full DriverScore event incl.
   blocking; ByUser → sliding-window counter only). Wrapped in try/catch — consequences
   never block the cancel.
3. `applyTerminalConsequences ctx ledgerCallback` — customer-side money. Since
   2026-08-22 (user decision) this ALSO runs when the booking reallocates: the matrix
   alone decides whether the customer pays (driver-at-fault rows carry no customer
   deduction; customer-at-fault cancels can no longer dodge charges via
   userReallocationEnabled). On a reallocated booking the charge lands as dues,
   collected next ride — the reallocation on_cancel carries no fee term. Owns:
   soft-cancel fee reuse (ByUser only), the matrix charge calc + dropZeroCharge, the SCD
   dues write, the ride charge total, rider counters (`cancelledRides` ByUser;
   `validCancellations` row-gated; `dueRides` ByUser), and finance ledger entries via a
   callback (ledger stays in CancelRide.Internal — no import cycle). Never throws.
   RideCancel tags + cancellation analytics likewise now run on reallocated customer
   cancels (previously skipped).

The driver penalty preview (`postPenaltyCheck`) uses `previewCancellationConsequences` —
the dry-run twin of `decideCancellationConsequences` (same pipeline via the shared
`buildRideCancellationSignals` + `CancellationFault.computeFaultVerdictDryRun`, but no
Redis caches and no ride-row persistence). The soft-cancel preview intentionally stays on
the caching path: its persisted quote is reused at real cancel so quoted == charged.
`SharedLogic.UserCancellationDues` was deleted; `CancellationLedgerAction` lives in
`SharedLogic.CancellationDues` (wire format unchanged; rider mirror in CallBPPInternal).

Moved INTO the orchestrator (CancelRide.Internal re-exports them for old callers):
`CancellationChargesOutcome`, `buildCancellationContext`,
`customerCancellationChargesCalculation`, `getCancellationCharges`, `dropZeroCharge`,
`userNoShowCancellationReason`, `validCancellationPenaltyReasonCodes`,
`driverDistanceToPickup`, `getDistanceToPickup`. `cancelRideTransaction` dropped its
charge params/logic (now purely status/persistence); Fleet vehicle-exchange still calls
it and thus still (deliberately) applies no consequences.

Deliberate semantic unifications vs legacy (all matrix/row-driven):
- driver-rate counting for customer cancels now fires even when the booking reallocates
  (same rationale as stall recording: reallocation-because-driver-at-fault must count);
- customer-side charges/counters/tags also fire on reallocated customer cancels
  (2026-08-22 follow-up decision, see orchestrator section);
- `validCancellations` on driver cancels is row-gated (`countsTowardCustomerCancellationStats`)
  instead of unconditional-on-fee — backfill 2b sets the flag true, preserving parity;
- driver money is applied for any source when the row says so (was ByDriver-only);
- zero-total customer charges are dropped in both flows (customer flow previously wrote
  zero-amount dues rows);
- soft-cancel fee reuse is ByUser-gated (a driver cancel after a customer soft-cancel
  preview no longer risks reusing the preview fee);
- the coin event reads `decision.disToPickup` instead of a second LTS fetch.

## Deliberately remaining

- `collectionMode` **BAP handoff**: the immediate-capture decision still lives in
  rider-app config; carrying the persisted mode across (on_cancel tag or internal API)
  is a cross-app protocol change deserving its own PR with rider-app testing.
- `customerNotificationKey`: rider push is BAP-owned; consume it via the same handoff.
- coin_config Cancellation rows still GATE which coin event functions run (intentional —
  that is the per-city/vehicle coin-feature switch); amounts + expiry live in the matrix. Builds on the shipped pieces: canonical `CancellationSignals` (incl.
`pickupStallCase` from the pickup-stall monitor), `CANCELLATION_FAULT_VERDICT` (verdict +
mandatory self-named `rule`), verdict persistence on the ride row, and the unified writers
(`SharedLogic.CancellationDues`, coin event writer, `DriverCancellationPenalty`).

## The unified pipeline

```
ride in progress ──► pickup-stall monitor job ──► stall case (STALLED/RETREATING/LOCATION_DARK or none)
                                                        │
cancellation ──► buildCancellationSignals (incl. stall) │
                                                        ▼
                 CANCELLATION_FAULT_VERDICT rules ──► (verdict, rule)
                                                        │
                                                        ▼
                 CancellationConsequenceMatrix lookup (this table)
                                                        │
                 ┌──────────────┬──────────────┬────────┴────────┬──────────────────┐
                 ▼              ▼              ▼                 ▼                  ▼
          customer money   driver coins   driver money      collection mode    side effects
          (applyCancel-    (coin event    (penalty fee /    (next-ride dues /  (blacklist pair,
           lationCharge)    writer)        wallet debit)     immediate capture) rate counters,
                                                                                notifications)
```

One row answers: *given who was at fault and why, in this city, for this kind of trip —
what exactly happens to whom, and how is it collected.* JsonLogic remains ONLY for fault
attribution; every consequence becomes a legible, diffable table row.

## Dimensions (lookup key)

| Column | Type | Null = wildcard | Why |
|---|---|---|---|
| `merchantOperatingCityId` | Id | NO (mandatory) | per-city policy is the base unit |
| `faultVerdict` | FaultParty | yes | DriverAtFault / CustomerAtFault / SharedFault / NoFault |
| `faultRule` | Text | yes | the rule name from the verdict output — lets `customer_no_show` carry a different charge than `late_cancel` under the same CustomerAtFault verdict |
| `cancelledBy` | CancellationType | yes | same verdict can arise from either side's cancel (driver cancels a no-show vs customer cancels late); collection and messaging differ |
| `tripCategory` | TripCategory | yes | user-proposed; intercity/rental cancellations carry different stakes |
| `vehicleServiceTier` | ServiceTierType | yes | user-proposed |
| `area` | SL.Area | yes | SUGGESTED: airport/special-zone cancellations routinely need distinct policy (FareProduct is already area-keyed) |
| `paymentInstrument` | Cash \| Prepaid… | yes | SUGGESTED: replaces `cancellationFeePaymentMethodExceptions` — a Cash row with zero charge instead of a code-level exemption |
| `timeBounds` | TimeBound | yes | OPTIONAL, phase 2: peak-hour no-show penalties; skip initially |

Deliberately NOT dimensions: `searchSource` (dashboard bookings — handle as a boolean
exemption output instead, see below), rider/driver history counters (that's the fault
rules' job), vehicleCategory (derivable from serviceTier; add later only if row explosion
becomes a problem).

## Resolution: most-specific-wins

Fetch all active rows for the city where every non-null dimension matches the event
(null matches anything). Winner = highest specificity score; score = sum of weights of
matched non-null dimensions with fixed precedence so ties are impossible by construction:

```
faultRule (32) > faultVerdict (16) > cancelledBy (8) > tripCategory (4) > vehicleServiceTier (2) > area/paymentInstrument (1 each)
```

- A city-only row (all wildcards) is the city default — exactly the user's "one entry with
  only merchant_operating_city_id" case.
- Upsert-time validation rejects two rows with identical dimension tuples; the weighted
  precedence makes any remaining overlap resolve deterministically.
- No row matched → **no consequences applied** (logError "matrix miss"). There is NO
  fallback to the old JsonLogic amount rules — the matrix is authoritative from day one.
  Consequence: a city must have its rows authored before cancellations there carry any
  charge/coins; `canAddCancellationFee` remains the master kill-switch.
- Cache per city (`CachedQueries`, cross-app namespace, deletes via
  `runInMultiCloudRedisWrite` — both clouds, per the fare-cache convention).

## Outputs (what the row decides)

**Customer money** (executed via `SharedLogic.CancellationDues.applyCancellationCharge`):
- `customerChargeType :: NoCharge | Flat | PercentOfEstimatedFare` with `customerChargeAmount`,
  `customerChargePercent`, `minCharge`/`maxCharge` caps
- `customerChargeTaxPercent`, `commissionPercent` (gross, ALV split stays in Haskell as today)
- `collectionMode :: NextRideDues | ImmediateCapture | ImmediateThenDues` — see BAP note below
- `waiveOffAllowed :: Bool` (+ optional `maxWaiveOffsPerMonth`) — absorbs the
  `USER_CANCELLATION_DUES_WAIVE_OFF` decision for the common case

**Driver coins** (executed via the existing coin event writer, replacing the
`CANCELLATION_COIN_POLICY` amount logic):
- `driverDeduction` COIN variant: `CoinDeduction` (positive count, engine event
  `BookingCancellationPenalisaton`) or `CoinAddition` (positive count,
  `BookingCancellationCompensation`), each with optional `expirySeconds`

**Driver money** (executed via `DriverCancellationPenalty`):
- `driverPenaltyAmount :: Maybe HighPrecMoney` — absorbs `farePolicy.driverCancellationPenaltyAmount`
  and the `CancellationPenaltyApplicable` tag gate (row match IS the gate)

**Side effects** (booleans/values instead of scattered configs):
- `blacklistDriverForRiderSeconds :: Maybe Seconds` (absorbs `driverRiderBlacklistDurationSeconds`)
- `countsTowardDriverCancellationRate :: Bool` (feeds DriverScore / behavior engine — replaces
  the `DriverCancellation#Valid` tag gate in stage 4)
- `countsTowardCustomerCancellationStats :: Bool` (validCancellations counter)
- `exemptDashboardBookings :: Bool` (replaces searchSource special-casing)
- `driverNotificationKey` / `customerNotificationKey :: Maybe Text` (overlay/FCM template keys)

Amount expressiveness is deliberately capped at flat + %-of-fare + min/max. Anything that
needs per-minute-waited arithmetic belongs in the fault rules' *choice of rule name*
(e.g. `no_show_long_wait` vs `no_show`), each mapping to a different row — keeps the table
legible, which is its whole point.

## Removed legacy fields + misattribution fix (2026-08-24)

**Verdict misattribution fixed** (was: most cancels showing early_customer_cancel):
1. The soft-cancel PREVIEW persisted+cached a CancellationByCustomer verdict per ride
   (riders opening the cancel screen froze early_customer_cancel; later real cancels —
   even driver ones — reused it from the 1h Redis cache). getCancellationCharges is now
   fully dry-run via previewCancellationConsequences. 0051 §1 clears the polluted ride
   rows; pre-fix verdict data is unreliable for analysis.
2. cancelledBy is now DERIVED from the stored BookingCancellationReason.source via
   cancellationSourceToType (single mapping in the orchestrator) — never hardcoded per
   flow. CancellationType gained ByMerchant/ByAllocator/ByApplication/ByFleetOwner so
   ops cancels are no longer mislabelled as driver cancels (they fall through to
   no_fault_default; the coin engine never sees them).

**Deprecated fields removed from code/specs** (physical columns left in place; see 0051):
- `farePolicy.driverCancellationPenaltyAmount` + its `fareParams` snapshot (domain, beam,
  fare-policy CSV upsert/export). The pickup-stall overlay situation (FEE_APPLIES vs
  FREE_CANCEL) now reads `cityHasDriverCancelMoneyPenalty` from the matrix. The RIDE's
  driverCancellationPenaltyAmount (applied-penalty tracking, dashboard waive flow,
  reconciliation) is a different field and stays.
- `transporterConfig.cancellationFeePaymentMethodExceptions`, `.driverRiderBlacklistDurationSeconds`
  (spec fields removed; 0050 still reads the physical blacklist column for seeding).
- `ride.cancellationChargesLogicVersion` (spec field + both query params + all threading;
  calc/getCancellationCharges no longer return a version).
- LogicDomain Enumerable list no longer offers USER_CANCELLATION_DUES /
  USER_CANCELLATION_DUES_WAIVE_OFF / CANCELLATION_COIN_POLICY for new configs
  (constructors + Show/Read kept). 0051 §2 deletes the persisted rollout/element rows —
  run only post-stability, it removes the binary-rollback path.

### coin_config cut out of cancellation (2026-08-24)

`driverCoinsEvent` intercepts `Cancellation` before the coin_config lookup and runs
`handleCancellationCoinsFromMatrix`: amount + direction + expiry from the matrix row
(via the once-per-ride cache, so charge and coins use the SAME row), event function
derived from the delta's sign (penalisation vs compensation) for history/notifications,
driver/fleet coin blacklists still applied. `hCancellation` and calculateCoins'
Cancellation case are deleted; coin_config Cancellation rows are dead config (0051
deactivates them). A matrix coin row now fires with no coin_config prerequisite —
consequently coin rows must not be configured in cities without the coin feature.

## Configs this table absorbs (deprecation targets)

| Today | Where | Becomes |
|---|---|---|
| `USER_CANCELLATION_DUES` amounts | JsonLogic per city | customer-money columns |
| `CANCELLATION_COIN_POLICY` amounts | JsonLogic per city | `driverCoins` |
| `coin_config` Cancellation rows | coin_config | gating no longer needed; matrix row is the gate |
| `farePolicy.driverCancellationPenaltyAmount` + `CancellationPenaltyApplicable` tag | fare policy + tag rules | `driverPenaltyAmount` |
| `cancellationFeePaymentMethodExceptions` | transporterConfig | `paymentInstrument` dimension row with NoCharge |
| `driverRiderBlacklistDurationSeconds` | transporterConfig | `blacklistDriverForRiderSeconds` |
| `DriverCancellation#Valid` / `CustomerCancellation#Valid` gates | tag rules | `countsToward*` booleans |
| `USER_CANCELLATION_DUES_WAIVE_OFF` (common case) | JsonLogic | `waiveOffAllowed` |
| `canAddCancellationFee` | transporterConfig | KEEP as master kill-switch |

**BAP constraint:** `settleCancellationFeeBeforeNextRide` / `immediateCapture*` live in
rider-app config and drive the Stripe flow. Phase 1: matrix's `collectionMode` is advisory
on the BPP (persisted, sent in on_cancel as today); BAP keeps its flags. Phase 2: carry the
mode to the BAP (internal API / on_cancel tag) and retire the BAP flags.

## Implementation sketch

1. NammaDSL spec `spec/Storage/CancellationConsequenceMatrix.yaml` (KV-enabled, cached
   find-all-by-city), plus dashboard CRUD + CSV upload/export mirroring the fare-policy
   endpoints, with upsert-time overlap validation.
2. `SharedLogic/CancellationConsequence.hs`: `resolveConsequence :: matcher inputs -> m (Maybe Row)`
   + `applyConsequences :: Row -> ... -> m ()` fanning into the three unified writers.
   Called once from the cancel flows right after the verdict (both flows already build
   signals+verdict in one place — `buildCancellationContext`).
3. Persist the chosen row id on the ride (or in the dues-details row) for audit:
   verdict says *why*, the row id says *what was applied*.
4. Migration: per city — author fault rules (phase C stage 1) → author matrix rows → the
   matrix replaces the JsonLogic amount rules outright (no fallback). Cities without rows
   apply no consequences, so authoring rows IS the enablement step; deactivating rows
   disables consequences again.
5. **Global fault-rule name registry**: one registry (name + description + active), NOT
   per city — rule names like `pickup_stall` / `customer_no_show` mean the same thing
   everywhere. Dashboard validation: a city's fault rules may only output registered names;
   matrix rows may only reference registered names. Kills the silent-typo-falls-to-wildcard
   failure mode and keeps cross-city analytics joinable on rule name.

## Resolved decisions

- Matrix miss → nothing applied, logged; no JsonLogic fallback ever.
- Rule-name registry is global (see 5 above).
- SharedFault: explicit rows only — no implicit half-charging.
- Coins + driver money from one row are additive; upsert warns when both
  `driverCoins < 0` and `driverPenaltyAmount` are set, requiring an explicit confirm flag.
