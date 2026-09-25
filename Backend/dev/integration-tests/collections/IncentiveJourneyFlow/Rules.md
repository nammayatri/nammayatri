# IncentiveJourneyFlow — Integration Test Rules

Provider/driver **Incentive Journey** E2E for **NAMMA_YATRI** (Bangalore): dashboard creates a Daily journey, assigns a driver, completes an AUTO cash ride, then asserts driver UI list/history progress (Kafka-driven).

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
| Create journey | `POST .../create` |
| Create milestone | `POST .../milestone/create` |
| List milestones | `GET .../milestone/{journeyId}/list` |
| Cohort↔journey map | `POST .../cohortJourney/create` |
| Assign driver | `POST .../assign` |
| Unassign | `DELETE .../unassign` |
| Disable journey | `PUT .../update` |
| Driver assignments | `GET .../driver/{driverId}/assignments` |
| Stats history | `GET .../stats/history/{driverId}?fromDate=&toDate=` |

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
| `01-IncentiveJourneyRideProgressFlow.json` | Switch city → create cohort/journey/milestone/mapping → assign → AUTO cash ride → poll list → history → optional dashboard stats → unassign + disable |

## Idempotency

- Random `_test_driver_number`, `_test_rider_number`, `_test_reg_no`, `_test_ij_suffix` per run (collection prerequest).
- Journey/cohort names: `INT_TEST_IJ_*_{{suffix}}`.
- Never hardcode phones, reg nos, or entity ids.

## Env notes

- `ij_eval_wait_ms` (default `12000`) — busy-wait before the first list poll so Kafka can catch up.
- `envType=Local` enables mock-server steps; non-Local auto-skips URLs containing `mockServerUrl` / `mock_fcm_url`.
