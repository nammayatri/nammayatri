# TipModuleConfigFlow

End-to-end check of `EstimateAPIEntity.tipModuleConfig` / `qar` on `GET /rideSearch/{id}/results`.
The cadence is produced by the **BPP** `DYNAMIC-PRICING-UNIFIED` dynamic-logic domain
(`DynamicPricingResult.tipModuleConfig`), shipped in `on_search` as the `TIP_MODULE_CONFIG` INFO tag,
persisted on the rider `estimate`, and falls back to `RiderConfig.tipModuleConfig`.
**Local env only** — it authors and rolls out a `DYNAMIC-PRICING-UNIFIED` version through the
provider dashboard and restores the previous rollout at the end.

## Prerequisites (local stack)

- `, run-mobility-stack-dev` up (rider-app :8013, driver-app :8016, rider-dashboard :8017,
  provider-dashboard :8018).
- Migrations applied: generated `dev/migrations-read-only/{rider-app,dynamic-offer-driver-app}/estimate.sql`
  and `dev/migrations-read-only/rider-app/rider_config.sql` (`tip_module_config json` columns),
  `dev/feature-migrations/0049-tip-module-config.sql` (RiderConfig default). `setup-tip-module-config.sql`
  (run by `run-tests.sh tip-module`) is idempotent and covers all of that plus
  `transporter_config.is_dynamic_pricing_qar_cal_enabled = true`, which the BPP needs before it evaluates
  the dynamic-pricing model at all.
- `transporter_config.referral_link_password` = `19071` (master-synced local value); env var
  `bpp_dynamic_logic_password`.
- Config-pilot caches `transporter_config` / `rider_config` in Redis and in-process: after the seed,
  flush Redis and restart dynamic-offer-driver-app + rider-app once if they already served the city.

## What it does

1. Driver + rider onboarding (copied from `RideBookingFlow/01-AutoRideFlow`).
2. Provider dashboard: `getDomainSchema?domain=DYNAMIC-PRICING-UNIFIED` (input sample exposes
   `actualQAR`), capture the current DP rollout; rider dashboard: capture `RiderConfig.tipModuleConfig`.
3. `POST nammaTag/appDynamicLogic/verify` (no save) with a QAR-band program for actualQAR 0.2 / 0.45 /
   0.7 / null → `tipModuleConfig` `{15,30,3}` / `{30,45,2}` / `{60,0,1}` / `{45,60,1}` — pure engine check.
4. Save a pricing-neutral test version (only `tipModuleConfig`: `7/8/9` when `actualQAR` is null,
   `70/80/90` otherwise), roll it out at 100%.
5. Ride search → `/results`: every estimate has `qar` and `tipModuleConfig == {7,8,9}` (local BPP has no
   QAR data yet, so `actualQAR` is null); a second poll returns identical values (persisted per estimate).
6. Roll the test version out at 0% → new search → `/results` returns the RiderConfig default (or `null`).
7. `POST /estimate/<unknown-uuid>/select2` → 400 `ESTIMATE_DOES_NOT_EXIST` (guard path).
8. Restore the pre-test DP rollout exactly (or clear if none was active).

Each run leaves one extra `DYNAMIC-PRICING-UNIFIED` version behind (dashboard versions are append-only).
While steps 4-8 run, local dynamic pricing uses the test version (no congestion/smart-tip output — static
fare policy pricing) — do not run pricing-sensitive suites concurrently.

## Run

```bash
./dev/integration-tests/run-tests.sh tip-module NY_Bangalore
# or directly:
newman run dev/integration-tests/collections/TipModuleConfigFlow/01-TipModuleConfigFlow.json \
  -e dev/integration-tests/collections/TipModuleConfigFlow/Local/Local_NY_Bangalore.postman_environment.json --bail
```
