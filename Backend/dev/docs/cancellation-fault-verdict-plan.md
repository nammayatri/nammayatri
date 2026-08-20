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
