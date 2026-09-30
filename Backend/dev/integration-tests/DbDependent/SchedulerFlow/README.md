# Scheduler Integration Tests

Tests for rider-app scheduler jobs (DailyPassStatusUpdate, PassExpiryReminderMaster, etc.).

## How it works

1. **Cleanup** — `POST /mock/scheduler/clear` purges any leftover ZSET entries and dedup-lock keys for this jobType so the suite is idempotent across runs
2. **Rider Auth** — Creates a test rider via API
3. **Seed data** — Inserts/updates test records in DB via `POST /mock/sql/update` (mock server)
4. **Trigger job** — Calls `POST /mock/scheduler/trigger` (or the rider-app internal dashboard API) to enqueue the job
5. **Wait** — Polls `POST /mock/scheduler/peek` until no imminent jobs of this type remain for the MOC (chain drain)
6. **Verify** — Queries DB via `POST /mock/sql/select` to check the job's effects
7. **Cleanup** — Deletes test data

## Prerequisites

The full dev stack must be running (`, run-mobility-stack-dev`), including:
- rider-app (8013) — handles auth + dashboard trigger
- rider-app-scheduler (8058) — executes jobs
- rider-producer (9990) — moves jobs from Redis sorted set to stream
- mock-server (8080) — provides `/mock/sql/select` + `/mock/sql/update` for DB seeding/verification
- mock-fcm (4545) — captures push notifications; required by Chennai-only tests that assert on FCM delivery (e.g. `02-PassExpiryReminderMaster.json`, polled via `GET {{mock_fcm_url}}/read/<token>`; `mock_fcm_url` is only set in `Local_NY_Chennai.postman_environment.json`)
- PostgreSQL (5434), Redis cluster (30001-30006)

## Adding a new scheduler job test

The cleanup, trigger, wait, and verify steps reference `{{scheduler_job_type}}`
from the env file — so when you copy `01-DailyPassStatusUpdate.json`, you only
change one line in the env file plus the steps that are inherently job-specific
(seed SQL, `jobData` shape, verify SQL).

1. **Create the collection** as `NN-YourJobName.json` in this directory
2. **Set `scheduler_job_type` in `Local_*.postman_environment.json`** to your `RiderJobType`/`DriverJobType` constructor name (e.g. `FRFSSeatHoldReaper`). All four scheduler endpoint calls (clear/trigger/peek-from-wait/peek-from-verify) pick it up automatically.
3. **Copy the pattern** from `01-DailyPassStatusUpdate.json`. Generic steps you keep as-is:
   - **Step 1 — Cleanup Scheduler State**: `POST /mock/scheduler/clear` with `{"jobType":"{{scheduler_job_type}}", "target":"rider", "merchantOperatingCityId":"{{bap_merchant_operating_city_id}}"}` — ZREMs pending entries and DELs `*mobility:locker:<jobType>:*` lock keys so the suite is re-runnable.
   - **Steps 2-3 — Rider Auth + OTP Verify**: reusable as-is.
   - **Step (wait) — Wait for Chain to Drain**: pre-request script polls `/mock/scheduler/peek` and exits when no imminent (<60s away) jobs of this type for the MOC remain. **Don't busy-wait** — `while(...){}` blocks Postman's 30s script timeout. Use `setTimeout` + `pm.sendRequest`.
4. **Customize the inherently job-specific bits**:
   - Seed catalog SQL (your job's config tables)
   - Seed test data SQL (the rows your job will process)
   - The trigger's `jobData` (matches your `JobContent` Haskell type)
   - Verify SQL (your job's expected side effects)
   - Cleanup SQL (delete your test rows)
5. **Run**: `./run-tests.sh scheduler NY_Chennai`

### `/mock/scheduler/clear` body

| Field | Default | Notes |
|---|---|---|
| `jobType` | — (required) | Constructor name to clear |
| `target` | `rider` | `rider` or `driver` — picks ZSET name, Redis port, shard count preset |
| `merchantOperatingCityId` | (none) | When set, only ZREMs jobs matching this MOC. Always narrows the lock-key scan to keys containing this MOC. Recommended in tests so you don't clobber other devs' state. |
| `maxShards` / `schedulerSetName` / `redisPort` | (from `target` preset) | Override scheduler ZSET location |
| `lockPorts` | `[30001..30006]` | Cluster nodes to scan for `*mobility:locker:<jobType>:*` keys |

Returns `{removedJobs, removedLocks}`.

### `/mock/scheduler/trigger` body

| Field | Default | Notes |
|---|---|---|
| `jobType` | — (required) | Must match a `RiderJobType`/`DriverJobType` constructor, e.g. `DailyPassStatusUpdate` |
| `jobData` | `{}` | JobContent for that jobType — shape defined in `SharedLogic/JobScheduler.hs` |
| `delaySeconds` | `10` | Fires at `now + delaySeconds`. Keep ≥ 10s when the handler reads rows that were written earlier in the same test — the KV layer needs time to settle. |
| `merchantId` / `merchantOperatingCityId` | `null` | Set if the job handler reads them from `AnyJob` |
| `target` | `rider` | `rider` or `driver` — picks the scheduler set name, Redis port, and shard count preset |
| `maxShards` / `schedulerSetName` / `redisPort` | (from `target` preset) | Override if you need to point somewhere non-standard |

The mock server builds the `AnyJob` JSON, computes the shard from the UUID, and `ZADD`s
it to the scheduler's Redis ZSET — reproducing what `createJobIn` does server-side.

Fragility note: the Python handler mirrors the Haskell `ToJSON (AnyJob t)` instance at
`lib/scheduler/src/Lib/Scheduler/Types.hs:115`. If that shape changes, the scheduler will
log `Failed to restore AnyJobInfo` and silently drop the job.

### `/mock/sql/select` + `/mock/sql/update` body

Generic parameterized SQL against any dev-DB table. Identifiers (schema, table, columns)
are quoted via psycopg2.sql.Identifier; values are bound as parameters; only whitelisted
operators are accepted (`=`, `!=`, `<`, `<=`, `>`, `>=`, `LIKE`, `IN`, `IS NULL`, `IS NOT NULL`).

Common fields:

| Field | Required | Notes |
|---|---|---|
| `db_name` | yes | e.g. `atlas_dev` |
| `db_schema` | yes | e.g. `atlas_app` |
| `table_name` | yes | target table |
| `where_clause` | `update` requires; `select` optional | list of `{column_name, val, op}` — AND-joined |

Select-only:

| Field | Default | Notes |
|---|---|---|
| `select` | `["*"]` | list of column names to return |
| `limit` | none | positive integer cap |

Update-only:

| Field | Default | Notes |
|---|---|---|
| `set` | — (required, non-empty object) | `{column: value}` map |
| `touch_updated_at` | `false` | if `true`, appends `updated_at = NOW()` |

`/mock/sql/update` refuses an empty `where_clause` to avoid accidental unscoped updates.

## Key variables

| Variable | Source | Purpose |
|----------|--------|---------|
| `baseUrl_app` | Environment | rider-app API (auth, dashboard trigger) |
| `mockServerUrl` | Environment | Mock server for `/mock/sql/select` + `/mock/sql/update` |
| `dashboard_token` | Environment | Internal dashboard auth token |
| `bap_short_id` | Environment | Merchant short ID for dashboard route |
| `city` | Environment | City for operating city lookup |
| `scheduler_wait_ms` | Environment | How long to wait for job execution (default 12s) |

## Troubleshooting

- **Job trigger returns 500**: Check rider-app logs at `/tmp/rider-app-eul.log`
- **Job stays Pending**: Check if rider-producer is running (`lsof -iTCP:9990`)
- **Job executed but Failed**: Check scheduler logs at `/tmp/rider-app-scheduler-internal.log`
- **FLUSHALL kills consumer group**: `run-tests.sh` recreates it automatically
