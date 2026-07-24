# International Onboarding Collections

End-to-end onboarding for the **BRIDGE_*/* international deployments** (Helsinki
is the reference city). Consolidates the four-file `HelsinkiOnboarding/` layout
into the shared **3-flow shape** used by `MSILOnboarding/` — Fleet, Driver,
Vehicle — and augments each with pm.test assertions on the new refactor's flag
model (`verified` · `approved` · `enabled` · `blocked` + sticky
`disabledReasonFlag` + `onboardingAs`↔FDA sync).

## Layout — 3 flows

| # | File | Persona | Ends with |
|---|---|---|---|
| 01 | `01-FleetOwnerOnboarding.json` | Fleet owner (self-onboards, 4 FLEET_BUSINESS docs) | fleet verified + approved + enabled by JUSPAY_ADMIN doc-approve |
| 02 | `02-AddDriver.json` | Fleet owner adds a driver (driver-app OTP path) | driver profile created, onboardingAs=INDIVIDUAL until vehicle-link |
| 03 | `03-AddVehicle.json` | Fleet owner adds a vehicle to the driver | driver onboardingAs=FLEET_DRIVER (via addVehicle?fleetOwnerId), VRC verified+approved, driver enabled |

These are ports of the existing HelsinkiOnboarding files (01 / 03-Admin / 04-Admin);
the fleet/admin split (03-FleetPath / 04-FleetPath) is skipped — the admin path
is the "always works" one per `HelsinkiOnboarding/Rules.md`, and the fleet path
becomes a follow-up once the FLEET-role dashboard APIs stabilize.

## Prerequisites

Same as `HelsinkiOnboarding/`:

1. **Config sync from master** — `python config_transfer.py --from prod_international --to local`.
   Brings BRIDGE_FINLAND_PARTNER merchant + FODVC/DVC rows + Helsinki
   payment methods.
2. **Local-testing-data seeds** — re-run `provider-dashboard.sql` /
   `rider-dashboard.sql` after any `test-context-api` restart. Grants the
   `local-admin-token-bangalore-namma-yatri` dev token cross-merchant access
   to BRIDGE_FINLAND_PARTNER.
3. **Helsinki feature migrations** (already-existing SQLs, run once per
   fresh DB): `0001-helsinki-online-payment-offers`,
   `0003-enable-invoice-generation-for-helsinki-delhi`,
   `0005-enable-cancellation-fee-helsinki`, `0007-helsinki-vat-config`.
4. Mock-servers on `:8080`, rider-app `:8013`, driver-app `:8016`, BPP
   dashboard `:8018`, BAP dashboard `:8017`, LTS `:8081`.

`Local/Local_International.postman_environment.json` is a copy of the existing
`InternationalRideBookingFlow/Local/Local_BF_Helsinki.postman_environment.json`
with the id/name relabeled — same base URLs, tokens, and merchant/city.

## Assertions tied to the refactor

The new pm.test's on the tail of each collection assert what the ADR "ONE
STATUS / FLAG-TRANSITION OWNER" makes an invariant:

| Flow | Step | Assertion | Backing code / doc reference |
|---|---|---|---|
| 01 | 20 | fleet `verified=true`, `approved ∈ {true, false, null}`, `disabledReasonFlag=null` | `recomputeFleetVerifiedAndEnabled` · doc lines 219-256 |
| 02 | 04 | driver `onboardingAs ∈ {INDIVIDUAL, null}` (no fleet link yet), `verified=false`, `enabled=false`, `disabledReasonFlag=null` | initial DI state · doc line 42 |
| 03 | 07 | driver `onboardingAs=FLEET_DRIVER` (via `syncDriverOnboardingAsWithFDA` triggered by `addVehicle?fleetOwnerId=`), `verified=true`, `approved=true`, `enabled=true`, `blocked=false`, sticky flag null | `recomputeDriverVerifiedAndEnabled` · doc lines 96-152 |
| 03 | 08 | VRC `verified=true`, `approved` derived from `FODVC.isApprovalSupported`, **no** `enabled` field on VRC | `forkRecomputeVehicleVerified` · doc lines 297-353 ("no enabled/blocked on VRC") |

Any of these tests failing points to a regression in the funnel
(`processOnboardingEvent` → recompute) rather than a bad endpoint call.

## Difference vs `HelsinkiOnboarding/`

| Concern | `HelsinkiOnboarding/` | `InternationalOnboarding/` (this folder) |
|---|---|---|
| File count | 6 files (Fleet + Stripe + AddDriver × Admin/Fleet + AddVehicle × Admin/Fleet) | 3 files (Fleet + Driver + Vehicle) |
| Stripe onboarding | separate file (02-StripeOnboarding.json) | not modeled — belongs to a payout follow-up |
| Fleet-vs-Admin path variants | both | admin-only (the "always works" one) |
| Refactor flag assertions | no | yes (new — 4 pm.test blocks) |
| Orchestrator wiring | `run-helsinki-e2e.sh` chains them | not wired to any orchestrator yet — run manually per flow with `--export-environment` |

`HelsinkiOnboarding/` is intentionally kept as-is; the `run-helsinki-e2e.sh`
orchestrator still references it. This folder is the maintenance-target for
future international onboarding regression tests; new features go here and get
back-ported to Helsinki only if the orchestrator needs them.

## Running standalone

```
cd Backend/dev/integration-tests
newman run collections/InternationalOnboarding/01-FleetOwnerOnboarding.json \
  -e collections/InternationalOnboarding/Local/Local_International.postman_environment.json \
  --export-environment /tmp/intl-env-after-01.json \
  --bail --timeout-request 60000

newman run collections/InternationalOnboarding/02-AddDriver.json \
  -e /tmp/intl-env-after-01.json \
  --export-environment /tmp/intl-env-after-02.json \
  --bail --timeout-request 60000

newman run collections/InternationalOnboarding/03-AddVehicle.json \
  -e /tmp/intl-env-after-02.json \
  --export-environment /tmp/intl-env-after-03.json \
  --bail --timeout-request 60000
```

`--export-environment` carries `fleet_owner_id`, `driver_id`, `driver_token`,
`vehicle_reg_no` forward. Downstream `InternationalRideBookingFlow/*` collections
consume the same env vars for ride combos.

## Doc reference

Flag model authority is `big-time-doc-refactor.md` (repo root). The tests here
map directly to the following sections:

- Driver flag transition: lines 96-152 (four flags + FDA gate + linked-vehicle gate).
- Fleet flag transition: lines 219-256 (four flags minus consent/vehicle).
- Vehicle flag transition: lines 297-353 (verified + approved + DRCA.isRcActive per driver; **no** enabled/blocked on VRC).
- onboardingAs ↔ FDA sync invariant: lines 566 (P0 delta — must round-trip via one common function).
