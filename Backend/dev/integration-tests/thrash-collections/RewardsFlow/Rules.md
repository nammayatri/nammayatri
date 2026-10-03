# RewardsFlow — Integration Test Rules

BAP rider-dashboard **Rewards** management APIs and rider unlock flow for **NAMMA_YATRI** (Bangalore) and **BHARAT_TAXI** (Delhi).

## Prerequisites

1. **Services** (from `Backend/`, nix shell): mobility stack including `rider-app` (`:8013`), `rider-dashboard` (`:8017`), Postgres (`:5434`), Redis, Kafka.
2. **Kafka consumer** (for `02-RewardsRiderUnlockFlow` only): `kafka-consumers` with `REWARDS_EVAL_CONSUMER` must be running so `triggerEval` events are consumed.
3. **Dashboard auth** (one-time or after DB reset):
   ```bash
   psql -h localhost -p 5434 -U atlas_superuser -d atlas_dev \
     -f Backend/dev/local-testing-data/rider-dashboard.sql
   ```
4. **Rewards seed** (auto-run by `./run-tests.sh rewards`):
   ```bash
   psql -h localhost -p 5434 -U atlas_superuser -d atlas_dev \
     -f Backend/dev/local-testing-data/rewards-dashboard-setup.sql
   ```

## Running

```bash
cd Backend/dev/integration-tests
./run-tests.sh rewards                  # all cities
./run-tests.sh rewards NY_Bangalore     # Namma Yatri Bangalore only
./run-tests.sh rewards BT_Delhi         # Bharat Taxi Delhi only
./run-tests.sh rewards BT_Delhi 01-RewardsDashboardCrud
```

Skip automatic SQL seed: `NY_TEST_SKIP_REWARDS_SEED=1 ./run-tests.sh rewards`

## Collections

| File | What it tests |
|------|----------------|
| `01-RewardsDashboardCrud.json` | Switch BAP dashboard → create campaign → cohort → upload pool CSV → activate → get/list/stats → edit → pause → end |
| `02-RewardsRiderUnlockFlow.json` | Dashboard setup + rider auth → `triggerEval` → poll `GET /rewards` for unlock with coupon code |

`01` does not need Kafka. `02` requires the rewards-eval Kafka consumer.

## API base path

All dashboard steps use:

```
{{bap_dashboard_url}}/bap/{{bap_short_id}}/{{city}}/rewards/...
```

Rider steps use `{{baseUrl_app}}/rewards` with `token: {{rider_token}}`.
