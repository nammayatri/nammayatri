# IncentiveJourneyFlow — Integration Test Rules

Provider/driver **Incentive Journey** E2E for **NAMMA_YATRI** (Bangalore): dashboard creates a Daily journey with two ride milestones (each `RideCompleted` GTE **1**) and one `Earnings` milestone the rides do not finish. The same cohort gets a second journey (`RideCompleted` GTE **2**, 200 coins) through a second cohort-journey mapping and a second assign. A different cohort and journey are auto-applied for `AUTO_CATEGORY` (the onboarded driver's vehicle) with no user mapping. Two AUTO cash rides complete those ride targets, the earnings milestone is waived, and the first mapping's streak-end reward pays 500 coins.

## Prerequisites

0. **Code cutover** — see [`Backend/lib/incentive-journey/CUTOVER.md`](../../../../lib/incentive-journey/CUTOVER.md): `, run-generator`, delete temp API shims, apply new `Local_API_IncentiveJourney*.sql`, `cabal build`.

1. **Services** (from `Backend/`, nix shell):
   ```bash
   , run-mobility-stack-full
   ```
   Must include `dynamic-offer-driver-app` (`:8016`), `provider-dashboard` (`:8018`), `rider-app` (`:8013`), Postgres (`:5434`), Redis, Kafka, and **`kafka-consumers`**.

2. **Kafka consumer** — ride-end processing updates journey milestones via `DriverCoinsAndJourney` (`SharedLogic.RideEvents.DriverCoinsAndJourney` / `handleDriverCoinsAndJourney`). Without `kafka-consumers`, the poll step will fail.

3. **Dashboard auth** (auto-run by `./run-tests.sh incentive-journey`, or after DB reset):
   ```bash
   psql -h localhost -p 5434 -U atlas_superuser -d atlas_dev \
     -f Backend/dev/local-testing-data/provider-dashboard.sql \
     -f Backend/dev/migrations-read-only/provider-dashboard/API_IncentiveJourney_IncentiveJourney.sql \
     -f Backend/dev/migrations-read-only/provider-dashboard/Local_API_IncentiveJourney_IncentiveJourney.sql
   ```
   The last two files map `PROVIDER_INCENTIVE_JOURNEY/*` endpoints to `system-config.coins.*` and grant those capabilities to the local admin role. Without them, every dashboard IJ call returns 403.

## Peer URL path (after generator cutover)

Dashboard APIs are **not** under `management`. After the IssueManagement-style peer move:

```
{{dashboard_base_url}}/bpp/driver-offer/{{dashboard_merchant_id}}/{{city}}/incentiveJourney/...
```

Examples:

| Action | Method + path |
|--------|----------------|
| Create cohort | `POST .../cohort/create` |
| Create journey | `POST .../create` (name, description, journeyType only) |
| Create milestone | `POST .../milestone/create` |
| List milestones | `GET .../milestone/{journeyId}/list` |
| Cohort↔journey map | `POST .../cohortJourney/create` (enabled, maxWaiveOffCount, streak-end reward) |
| Assign driver | `POST .../assign` (one user mapping per cohort-journey mapping) |
| Unassign | `DELETE .../unassign` |
| Disable cohort mapping | `PUT .../cohortJourney/update` (`cohortJourneyMappingId`, `enabled: false`) |
| Auto-apply a mapping for the driver's category | `POST .../autoApplyCohort/create` (`vehicleCategory` `AUTO_CATEGORY`, `allowIfNoMapping` false) |
| Disable auto-apply | `PUT .../autoApplyCohort/update` (`enabled: false`) |
| Driver assignments | `GET .../driver/{driverId}/assignments` |
| Stats history | `GET .../stats/history/{driverId}?fromDate=&toDate=&journeyId=` |
| Waive milestone | `POST .../stats/waiveOff` (`driverId`, `journeyId`, `milestoneId`, `periodKey`) |

There is no journey update API, and no delete for a cohort-journey mapping. Set `enabled: false` on `PUT .../cohortJourney/update`. Eval and the driver list already skip a disabled mapping. Stats rows stay.

Driver coins balance (token = driver session):

- `GET {{baseURL_namma_P}}/coins/transactions?date=` — `coinBalance` before and after waive (streak-end adds 500)

Driver UI (token = driver session):

- `GET {{baseURL_namma_P}}/incentive/journey/list`
- `GET {{baseURL_namma_P}}/incentive/journey/history`

Auth header for dashboard: `token: {{dashboard_token}}` (refreshed by `switchMerchantAndCity`).

## Running

```bash
cd Backend/dev/integration-tests
./run-tests.sh incentive-journey              # Bangalore (only Local env)
./run-tests.sh incentive                      # alias
./run-tests.sh incentive-journey NY_Bangalore
./run-tests.sh incentive-journey NY_Bangalore 01-IncentiveJourneyRideProgressFlow
```

Skip automatic SQL seed: `NY_TEST_SKIP_INCENTIVE_SEED=1 ./run-tests.sh incentive-journey`

Newman directly:

```bash
newman run collections/IncentiveJourneyFlow/01-IncentiveJourneyRideProgressFlow.json \
  -e collections/IncentiveJourneyFlow/Local/Local_NY_Bangalore.postman_environment.json \
  --bail --timeout-request 60000
```

## Collection

| File | What it tests |
|------|----------------|
| `01-IncentiveJourneyRideProgressFlow.json` | One cohort, two journeys (two mappings, two assigns). Journey 2 is RideCompleted GTE 2 for 200 coins and completes on ride 2. A second cohort is auto-applied for `AUTO_CATEGORY` with no user mapping and completes on ride 1. Then waive earnings → coins +500 → unassign both mappings, disable them, and disable auto-apply |

## Milestone shape in this collection

Eval does not carry progress into the next milestone, and it does not start the next milestone in the same ride. After milestone 1 is completed, the next ride starts milestone 2 at 0.

| Order | Condition | Value | What the test does |
|-------|-----------|-------|--------------------|
| 1 | RideCompleted GTE | 1 | Completed by ride 1 (10 coins) |
| 2 | RideCompleted GTE | 1 | Still open after ride 1; completed by ride 2 (10 coins) |
| 3 | Earnings GTE | 1000000 | Left open, then `POST .../stats/waiveOff` |

Same cohort, second mapping (separate assign; a user mapping points at one cohort-journey mapping, not the whole cohort):

| Order | Condition | Value | What the test does |
|-------|-----------|-------|--------------------|
| 1 | RideCompleted GTE | 2 | Open at 1 after ride 1; completed on ride 2 (200 coins) |

Separate auto-apply cohort (no user mapping, `allowIfNoMapping` false so it still applies while the driver has the other mappings, `vehicleCategory` `AUTO_CATEGORY`, matching Add Vehicle):

| Order | Condition | Value | What the test does |
|-------|-----------|-------|--------------------|
| 1 | RideCompleted GTE | 1 | Completed by ride 1 (30 coins) |

`periodKey` is taken from the completed ride stats row (`Day:YYYY-MM-DD`, city local day). Cohort mapping `startDate` is noon UTC on that same IST calendar day so streak-end (streakRange 1, 500 coins) matches the ride period. `maxWaiveOffCount` is 1.

## Idempotency

- Random `_test_driver_number`, `_test_rider_number`, `_test_reg_no`, `_test_ij_suffix` per run (collection prerequest).
- Journey/cohort names: `INT_TEST_IJ_*_{{suffix}}`.
- Never hardcode phones, reg nos, or entity ids.

## Env notes

- `ij_eval_wait_ms` (default `12000`) — busy-wait before each list poll so Kafka can catch up.
- `_test_start_date` — noon UTC of today's IST day, so streak period keys match ride eval.
- `envType=Local` enables mock-server steps; non-Local auto-skips URLs containing `mockServerUrl` / `mock_fcm_url`.
