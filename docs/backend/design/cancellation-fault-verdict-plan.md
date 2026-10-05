# Cancellation Fault Verdict — Design Plan (Phase C)

Status: **Steps 1 AND 3 IMPLEMENTED** — charge eligibility is verdict-gated with a per-city
legacy fallback, the verdict is threaded into the RideCancel tag rules' input, and it is
persisted on the ride row (`ride.cancellation_fault_verdict` / `ride.cancellation_fault_rule`).

> **Gating semantics (`customerAtFaultOrLegacy`):** when a city has
> `CANCELLATION_FAULT_VERDICT` rules, the verdict ALONE decides charge eligibility
> (`atFault == CustomerAtFault`). When it has none, the legacy behaviour applies unchanged —
> tag gate (`CustomerCancellation#Valid` / `CustomerNoShowCancellation#Valid`) on the charge
> paths, always-quote on the soft-cancel preview — and a logWarning
> ("falling back to legacy charge gating") marks the city as unmigrated. Cities therefore
> migrate one at a time by authoring fault rules; no deploy-order prerequisite. The tag
> fallback is deleted in step 4 once every charging city has fault rules.

Steps 2 (rule-set simplification per city) and 4 (drop the legacy fallback, Haskell
invariants for coins/driver-penalty, retiring the remaining driver-side tag gates) remain.
Prerequisites shipped earlier: shared `SharedLogic.CancellationSignals` (one signal builder for
coins / dues / tags / penalty preview, including `pickupStallCase`) and
`SharedLogic.CancellationDues` (single writer for the rider dues balance).

## Implemented (step 1)

- `CANCELLATION_FAULT_VERDICT` logic domain (`lib/yudhishthira/.../Types.hs`, wire string
  `CANCELLATION-FAULT-VERDICT`).
- `SharedLogic/CancellationFault.hs`: `FaultVerdictData` (input = cancelledBy + reason +
  canonical signals), `FaultVerdict {atFault, rule}`, and `getOrComputeFaultVerdict` —
  evaluated once per ride (Redis-cached 1h under `CancellationFaultVerdict:rideId-<id>`), so
  the dues calculation and the coin-event fork see the same outcome.
- **Rule provenance is self-reported and MANDATORY**: every rule that sets `atFault` must
  also set `rule` — a name the rule author gives it inside the JsonLogic (e.g.
  `"pickup_stall"`, `"customer_no_show"`); a later rule overriding `atFault` must override
  `rule` too. The mandatory field is enforced at authoring time — dashboard verification
  parses the output as `FaultVerdict`, so a rule set that produces a verdict without naming
  its source fails verification. At runtime a missing name is logged as an error and recorded
  as `UNNAMED_RULE`; no rule matching yields `NoFault` / `NO_RULE_MATCHED`. No rule order,
  logic version, or ride tag is stored.
- **Where the verdict lives**: (1) the Redis cache above (1h, so all consumers of one
  cancellation agree); (2) the fault domain's own row in the Yudhishthira ClickHouse debug
  log (`json_logic_transactions`, keyed by booking transaction id) — the durable audit
  trail; (3) as `faultVerdict` / `faultRule` inputs inside the dues and coin rule
  evaluations, which are themselves debug-logged. Nothing is persisted on ride/booking
  tables in this phase.
- Advisory threading: `faultVerdict` / `faultRule` fields added to `UserCancellationDuesData`
  and `CancellationCoinData` (additive JSON — existing rules unaffected).
- Dashboard authoring: verify + domain-schema cases in
  `Domain/Action/Dashboard/Management/NammaTag.hs`.
## Implemented (step 3 — verdict-gated charging, no backward compatibility)

- `buildCancellationContext` (`CancelRide/Internal.hs`) builds signals + verdict ONCE at the
  top of both cancel flows (customer via `Beckn/Cancel.cancel`, driver via `cancelRideImpl`),
  before tag computation. The coin fork later reuses the Redis-cached verdict.
- `CancelRideTagData` now carries `faultVerdict` / `faultRule`, so `RideCancel` tag rules can
  become thin mappings from the verdict (per-city config migration).
- **Charge gates are verdict-first with legacy fallback**: `getCancellationCharges` and the
  Beckn cancel flow gate on `customerAtFaultOrLegacy` — verdict decides where fault rules
  exist; the old tag gate (or always-quote, for soft cancel) applies where they don't. The
  `validCancellations` rider counter follows the same hybrid gate.
- **Ride-row persistence**: on first computation the verdict + rule name are written to
  `ride.cancellation_fault_verdict` / `ride.cancellation_fault_rule`
  (spec `ride.yaml`, query `updateCancellationFaultVerdict`, DDL migration
  `0855-ride-cancellation-fault-verdict.sql`) — the durable per-ride audit record.
- Still tag-based (step 4): `DriverCancellation#Valid` (cancellation-rate counters/blocking)
  and `CancellationPenaltyApplicable#Valid` (driver monetary penalty).

## Problem

"Who was at fault for this cancellation?" is currently decided **independently by three rule sets**,
each of which can reach a different conclusion for the same event:

| Decider | Domain | Effect |
|---|---|---|
| `RideCancel` NammaTags (`CustomerCancellation`, `CustomerNoShowCancellation`, `DriverCancellation`, `CancellationPenaltyApplicable`) | tag rule engine | gates whether the customer charge / driver penalty applies at all |
| `USER_CANCELLATION_DUES` | JsonLogic | sets the customer charge amount |
| `CANCELLATION_COIN_POLICY` | JsonLogic | sets the driver coin penalty/compensation |

Because each re-derives fault from raw signals, contradictions are possible: a customer charged
*and* the driver coin-compensated, or a stalled driver penalised in coins while the customer is
still charged. Rule authors also have to encode the same fault heuristics three times per city.

## Goal

One per-city fault computation, evaluated **once per cancellation**, whose *output* becomes an
*input* to all three downstream deciders. Rule authors configure fault logic in exactly one place.

## Design

1. **New logic domain** `CANCELLATION_FAULT_VERDICT` in `Lib.Yudhishthira.Types.LogicDomain`
   (`Backend/lib/yudhishthira/src/Lib/Yudhishthira/Types.hs`), with dashboard authoring/verify
   wiring in `Domain/Action/Dashboard/Management/NammaTag.hs` (mirror the existing
   `CANCELLATION_COIN_POLICY` / `USER_CANCELLATION_DUES` verify cases).

2. **Input**: a `FaultVerdictData` record built from `CancellationSignals` (already canonical) plus
   `cancelledBy`, `cancellationReasonSelected`, and the rider/driver history counters already fed to
   the dues logic. New types live in `SharedLogic/CancellationFault.hs`.

3. **Output**:
   ```haskell
   data FaultParty = DriverAtFault | CustomerAtFault | SharedFault | NoFault
   data FaultVerdict = FaultVerdict { atFault :: FaultParty, rule :: Text }
   ```
   `rule` is MANDATORY: the rule author's own name for the deciding rule, set inside the
   JsonLogic alongside `atFault` (a later rule overriding `atFault` overrides `rule` too).
   Dashboard verification enforces it; runtime records `UNNAMED_RULE` (with an error log)
   if a live rule forgets.

4. **Evaluation point**: once per cancellation via `getOrComputeFaultVerdict` (Redis-cached
   per ride), invoked from the signal-consuming sites so all consumers agree. Audit trail is
   the domain's ClickHouse debug log; no ride tag / table column in this phase.

5. **Threading**: add `faultVerdict` / `faultRule :: Maybe Text` to `UserCancellationDuesData`,
   `CancellationCoinData`, and (step 3) `CancelRideTagData` (additive JSON fields — deployed
   rules are unaffected until they opt in). The driver monetary penalty gate
   (`CancellationPenaltyApplicable` tag computation) reads it the same way.

6. **Invariants** (phase 2, after rules migrate): enforce cross-system consistency in Haskell,
   not rules — e.g. `atFault == DriverAtFault` forces customer charge to 0 regardless of the dues
   rule output. Do NOT hard-code these before the verdict logic has been observed in production.

## Rollout

1. Ship the domain + plumbing with verdict **advisory only** (nothing consumes it). Default rule
   template: `pickupStallCase` set → DriverAtFault; valid no-show (reason + waited past threshold)
   → CustomerAtFault; else NoFault.
2. Shadow-compare via Yudhishthira debug logs: verdict vs. actual charge/coin outcomes per city.
3. Migrate city rules to key off `faultVerdict`; simplify the three rule sets to amount-only.
4. Enable the Haskell invariants once shadow data shows agreement.

Template migration: extend `dev/feature-migrations/0034-cancellation-fee-consolidated.sql` with the
verdict logic rows, or add a sibling `00XX-cancellation-fault-verdict.sql`.

## Appendix: reference CANCELLATION_FAULT_VERDICT rule (legacy parity)

Combined from the legacy CANCELLATION_COIN_POLICY amounts rule and the legacy
driver-cancellation-penalty (Valid/Invalid) tag rule. Amounts live in the consequence
matrix, keyed by these rule names; precedence is top-down. Single order:

```json
{"if":[
  {"and":[{"==":[{"var":"cancelledBy"},"CancellationByDriver"]},
          {"or":[{"var":"isAdvanceBooking"},
                 {"var":"isPickupOrDestinationEdited"},
                 {"<":[{"var":"timeOfCancellation"},20]}]}]},
  {"cat":[{"var":""},{"atFault":"NoFault"},{"rule":"driver_excused_cancel"}]},
  {"if":[
    {"!=":[{"var":"pickupStallCase"},null]},
    {"cat":[{"var":""},{"atFault":"DriverAtFault"},{"rule":"pickup_stall"}]},
    {"if":[
      {"and":[{"==":[{"var":"cancelledBy"},"CancellationByCustomer"]},
              {"<":[{"var":"timeOfCancellation"},30]}]},
      {"cat":[{"var":""},{"atFault":"NoFault"},{"rule":"early_customer_cancel"}]},
      {"if":[
        {"and":[{"==":[{"var":"cancelledBy"},"CancellationByCustomer"]},
                {"or":[{"var":"isArrivedAtPickup"},
                       {"!=":[{"var":"driverWaitingTime"},null]}]}]},
        {"cat":[{"var":""},{"atFault":"CustomerAtFault"},{"rule":"customer_cancelled_driver_arrived"}]},
        {"if":[
          {"and":[{"==":[{"var":"cancelledBy"},"CancellationByCustomer"]},
                  {"or":[{"==":[{"var":"cancellationReasonSelected"},"DRIVER_NOT_MOVING"]},
                         {"==":[{"var":"cancellationReasonSelected"},"WAIT_TIME_TOO_LONG"]}]},
                  {"<":[{"if":[{"==":[{"var":"actualCoveredDistance"},null]},-1000000,{"var":"actualCoveredDistance"}]},
                        {"max":[{"*":[{"if":[{"==":[{"var":"expectedCoveredDistance"},null]},0,{"var":"expectedCoveredDistance"}]},0.5]},150]}]},
                  {">":[{"var":"timeOfCancellation"},60]}]},
          {"cat":[{"var":""},{"atFault":"DriverAtFault"},{"rule":"driver_not_moving_complaint"}]},
          {"if":[
            {"and":[{"==":[{"var":"cancelledBy"},"CancellationByCustomer"]},
                    {">":[{"if":[{"==":[{"var":"actualCoveredDistance"},null]},0,{"var":"actualCoveredDistance"}]},500]}]},
            {"cat":[{"var":""},{"atFault":"CustomerAtFault"},{"rule":"customer_late_cancel_driver_moved"}]},
            {"if":[
              {"and":[{"==":[{"var":"cancelledBy"},"CancellationByDriver"]},
                      {"or":[{"<":[{"if":[{"==":[{"var":"currentDistanceToPickup"},null]},999999999,{"var":"currentDistanceToPickup"}]},
                                   {"max":[{"*":[{"if":[{"==":[{"var":"initialDistanceToPickup"},null]},0,{"var":"initialDistanceToPickup"}]},0.1]},100]}]},
                             {">":[{"if":[{"==":[{"var":"driverWaitingTime"},null]},0,{"var":"driverWaitingTime"}]},180]}]}]},
              {"cat":[{"var":""},{"atFault":"CustomerAtFault"},{"rule":"customer_no_show"}]},
              {"if":[
                {"and":[{"==":[{"var":"cancelledBy"},"CancellationByDriver"]},
                        {"<":[{"var":"timeOfCancellation"},90]}]},
                {"if":[
                  {">":[{"if":[{"==":[{"var":"actualCoveredDistance"},null]},0,{"var":"actualCoveredDistance"}]},50]},
                  {"cat":[{"var":""},{"atFault":"DriverAtFault"},{"rule":"driver_avoidable_cancel_early_moving_toward"}]},
                  {"if":[
                    {"<":[{"if":[{"==":[{"var":"actualCoveredDistance"},null]},0,{"var":"actualCoveredDistance"}]},-50]},
                    {"cat":[{"var":""},{"atFault":"DriverAtFault"},{"rule":"driver_avoidable_cancel_early_moving_away"}]},
                    {"cat":[{"var":""},{"atFault":"DriverAtFault"},{"rule":"driver_avoidable_cancel_early_stationary"}]}
                  ]}]},
                {"if":[
                  {"==":[{"var":"cancelledBy"},"CancellationByDriver"]},
                  {"cat":[{"var":""},{"atFault":"DriverAtFault"},{"rule":"driver_avoidable_cancel"}]},
                  {"cat":[{"var":""},{"atFault":"NoFault"},{"rule":"no_fault_default"}]}
                ]}]}]}]}]}]}]}]}]}
```

Legacy-parity matrix rows per rule name (amounts from the two legacy rules):

Coin values use the matrix constructors (2026-08-22 schema): `CoinDeduction n` takes n
coins from the driver, `CoinAddition n` gives n — counts always positive, direction in
the constructor (never a sign).

| rule | verdict | driver coins | driver money | customer charge |
|---|---|---|---|---|
| driver_excused_cancel | NoFault | — | — | — |
| pickup_stall | DriverAtFault | (suggest CoinDeduction 30) | per city | — |
| early_customer_cancel (<30s) | NoFault | — | — | — |
| driver_not_moving_complaint | DriverAtFault | 0 (legacy) | legacy penalty amount | — |
| customer_cancelled_driver_arrived | CustomerAtFault | CoinAddition 50 | — | dues amount |
| customer_late_cancel_driver_moved | CustomerAtFault | CoinAddition 30 | — | dues amount |
| customer_no_show | CustomerAtFault | CoinAddition 50 | — | no-show amount |
| driver_avoidable_cancel_early_moving_toward (20-90s, covered > 50m) | DriverAtFault | 0 (legacy) | legacy penalty amount | — |
| driver_avoidable_cancel_early_moving_away (20-90s, covered < -50m) | DriverAtFault | 0 (legacy) | legacy penalty amount | — |
| driver_avoidable_cancel_early_stationary (20-90s, |covered| <= 50m or unknown) | DriverAtFault | 0 (legacy) | legacy penalty amount | — |
| driver_avoidable_cancel (>=90s) | DriverAtFault | CoinDeduction 30 | legacy penalty amount | — |
| no_fault_default | NoFault | — | — | — |

Notes on legacy reconciliation:
- The two legacy systems disagreed on the driver "early" boundary (20s for money, 90s for
  coins). The split `driver_avoidable_cancel_early` / `driver_avoidable_cancel` rules
  preserve BOTH via matrix rows (money-only vs money+coins).
- 2026-08-22 tuning from production distribution: customer free window cut 90s → 30s
  (side effect: `driver_not_moving_complaint` (>60s) is now reachable for 61-90s customer
  cancels, which the 90s window used to swallow; 30-90s customer cancels without other
  signals fall to `no_fault_default` — same NoFault outcome, different label).
  `driver_avoidable_cancel_early` split three ways by movement state at cancel, using net
  covered distance with a ±50m GPS dead-band: `_moving_toward` (covered > 50m),
  `_moving_away` (covered < -50m), `_stationary` (|covered| <= 50m or location unknown).
  Unknown location folds into stationary; give it its own bucket if the distinction
  matters operationally.
- The legacy penalty rule required a call attempt to excuse a driver's no-show cancel;
  per product decision (2026-08-21) `customer_no_show` does NOT require a call — being at
  the pickup (within max(10%, 100m)) or having waited > 180s suffices.
  `callAttemptByDriver` / `callAttemptCount` remain available as inputs for cities that
  want the stricter variant.
- The legacy not-moving corroboration (`distanceToPickup >= driverDistToPickup`, i.e.
  covered >= 0) was WRONG: it also holds when the driver made excellent progress, so the
  combined rule replaces it with a progress-DEFICIT check —
  `actualCoveredDistance < max(expectedCoveredDistance * 0.5, 150)` (driver covered less
  than half of what the ETA predicted by now, with a 150m absolute floor when no ETA).
  Retreating drivers (negative covered) satisfy it trivially.
- **The jsonLogic engine (json-logic-hs) is FULLY EAGER and strict about nulls**: only
  filter/sort/map/var are special-cased — every other operator, INCLUDING `if`, has all
  its arguments evaluated before dispatch, and numeric ops throw "expected number, got
  Null". Therefore guard-`if`s AROUND arithmetic do NOT help (the guarded branch still
  evaluates). The working idiom is OPERAND COALESCING:
  `{"if":[{"==":[{"var":"x"},null]}, <literal>, {"var":"x"}]}` inside the arithmetic —
  branches are literals/vars so eager evaluation is safe, and the outer op always gets a
  number. Sentinels chosen: covered→-1000000 in the deficit check (null = corroborated,
  legacy-loose; use +1000000 to fail-safe instead), covered→0 in the >500 check (fail
  closed), currentDistance→999999999 (fail closed), expected/initial→0 (floor applies),
  wait→0 (fail closed).
- Branch ORDER fix for the same failure mode: `customer_cancelled_driver_arrived` is
  checked BEFORE `driver_not_moving_complaint`, so a customer citing DRIVER_NOT_MOVING /
  WAIT_TIME_TOO_LONG when the driver has actually arrived (or is waiting) resolves to
  CustomerAtFault instead of blaming the driver.
