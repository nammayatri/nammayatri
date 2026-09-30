# MSIL Onboarding Collections

End-to-end onboarding for the **MSIL_PARTNER / Delhi** environment. Mirrors the
Postman collections used against the c2 sandbox (`MSIL Driver Onboarding` +
`Fleet Driver, Vehicle onboarding`) but rewritten as newman-runnable flows that
share `Local/Local_MSIL.postman_environment.json` and exercise the new onboarding
refactor's flag model (verified/approved/enabled/blocked + sticky
`disabledReasonFlag` + `onboardingAs`↔FDA sync).

## Layout — 3 flows

| # | File | Persona | Ends with |
|---|---|---|---|
| 01 | `01-FleetOwnerOnboarding.json` | Fleet owner (self-onboards) | fleet enabled by Bot Review |
| 02 | `02-AddDriver.json` | Fleet owner adds a driver | driver enabled by Bot Review, `onboardingAs=FLEET_DRIVER` |
| 03 | `03-AddVehicle.json` | Fleet owner adds a vehicle | VRC verified + approved by Bot Review |

Each flow embeds the operator hub-inspection + bot-review steps inline —
that's the actual production approval chain for MSIL and the only way to see
the flag transitions our recompute path is designed to drive. If you need to
run just the driver-app or just the ops sections, split the collection by
picking specific request ranges via `--folder` / `--iteration-data` in newman.

## Prerequisites

1. **Local mobility stack running** — `, run-mobility-stack-dev` (Postgres,
   Redis, Kafka, mock server on `:8080`, dashboard on `:8018`, driver-app on
   `:8016`).
2. **JUSPAY_ADMIN cross-merchant access** for `MSIL_PARTNER` — re-run
   `dev/local-testing-data/provider-dashboard.sql` after any restart of
   `test-context-api`. The seed grants
   `local-admin-token-bangalore-namma-yatri` cross-merchant access; the flow
   still calls `POST /user/switchMerchantAndCity` to scope it to MSIL_PARTNER/Delhi.
3. **Fake-SMS OTP** — `transporter_config.useFakeSms = true` and OTP `7891`
   for MSIL_PARTNER/Delhi (matches the value baked into the master seeds).
   If OTP verify 4XX's at step 03 of flow 01, this is the reason.
4. **At least one Operation Hub** configured for MSIL_PARTNER/Delhi — the
   `driver/operation/getAllHubs` call in flow 02 and 03 populates
   `operation_hub_id` from the first hub in the list. If the array is
   empty, `Create Request` in step 15 (flow 02) / step 09 (flow 03) will 4XX.
5. **Bot Review credentials** (`bot_email` / `bot_password` env vars) —
   optional. If unset or login fails, the flow falls back to `dashboard_token`
   for the bot-review requests; those requests either 4XX (skipped by the
   guard in the test script) or succeed depending on ACL. Set proper
   credentials to actually exercise the enable/approve transitions.

## Assertions tied to the refactor

The following pm.test's are the reason these flows exist beyond just
"does the endpoint 200":

| Flow | Step | Assertion | Backing code |
|---|---|---|---|
| 01 | 11 | fleet `verified` reflects doc completion, `enabled ≠ true` pre-approval, `disabledReasonFlag=null` | `recomputeFleetVerifiedAndEnabled` |
| 01 | 15 | fleet `enabled=true` and `approved=true` after bot approve; sticky flag still null | `botApproveAndReconcile` synchronous recompute (H1 fix) |
| 02 | 22 | driver `onboardingAs=FLEET_DRIVER`, sticky flag null, `enabled/approved=true` post-bot-approval | `syncDriverOnboardingAsWithFDA` (fires from `verifyJoiningOtp` → FDA create) + `recomputeDriverVerifiedAndEnabled` |
| 03 | 16 | VRC `verified=true` and `approved=true` post-bot-approval | `forkRecomputeVehicleVerified` + `computeApprovedFromDocs` reading `FODVC.isApprovalSupported` |

Any of these tests failing points to a regression in the funnel
(`processOnboardingEvent` → recompute) rather than a bad endpoint call.

## Running standalone

```
cd Backend/dev/integration-tests
newman run collections/MSILOnboarding/01-FleetOwnerOnboarding.json \
  -e collections/MSILOnboarding/Local/Local_MSIL.postman_environment.json \
  --export-environment /tmp/msil-env-after-01.json \
  --bail --timeout-request 60000

newman run collections/MSILOnboarding/02-AddDriver.json \
  -e /tmp/msil-env-after-01.json \
  --export-environment /tmp/msil-env-after-02.json \
  --bail --timeout-request 60000

newman run collections/MSILOnboarding/03-AddVehicle.json \
  -e /tmp/msil-env-after-02.json \
  --export-environment /tmp/msil-env-after-03.json \
  --bail --timeout-request 60000
```

`--export-environment` between flows carries `fleet_owner_token`,
`fleet_owner_id`, `driver_id`, `ops_token`, `bot_token` etc. forward.

## Things these flows do NOT do (intentional)

- **No SQL backstops.** Unlike HelsinkiOnboarding (which uses
  `mock/sql/select` because Helsinki configs come from `config-sync`), MSIL
  configs are master-domestic — the API responses ARE the source of truth.
  If an API-level assertion fails and you want a SQL backstop, add one
  targeting `driver_information` / `vehicle_registration_certificate` /
  `fleet_owner_information` (columns: `verified`, `approved`, `enabled`,
  `disabled_reason_flag`, `onboarding_as`).
- **No Bank Account / Payout wiring.** MSIL uses `fleet/payout/account`
  (Juspay), not Stripe. Not modeled here — add if payout-onboarding
  regressions become in-scope for the refactor.
- **No Idfy/HVSDK verification mock.** Aadhaar/PAN verify calls
  (`/onboarding/verify/…`) hit the real onboarding handler which calls
  `Idfy` / `Kaptcha`. Locally these are stubbed via `mock-server`; make
  sure `merchant_service_config` for MSIL_PARTNER points to
  `http://localhost:8080` on the verifier providers.

## Known gaps to close before landing in CI

1. **`operation_hub_id` seed** — no dev seed currently creates hubs for
   MSIL_PARTNER/Delhi. Either add
   `dev/local-testing-data/msil-op-hubs.sql` OR make the collection
   create-a-hub-if-missing (needs the admin-only endpoint).
2. **Bot login on local** — the c2 collection uses `bpp/user/login` with
   MFA OTP. Local dev has no MFA. Either add a dev-only bot-account seed
   with fixed password to `local-testing-data/`, or accept the "fallback
   to dashboard_token" behavior currently in the flow.
3. **Address docType enum** — flow 02 step 13 sends
   `addressDocumentType: "AadhaarCard"`; verify the enum still allows it
   after the Common doc refactor (see PR #15877 in git log).
