# OperationHubFlow Collection — Change Log

## Issues Found and Fixes Applied

---

### Issue 1: Step 01 — "Failed to construct 'URL': Invalid URL"

**Before fixing:**
Step 01 (Fleet Owner Login OTP) failed with `Failed to construct 'URL': Invalid URL` in the test tool.

**Root cause:**
`baseURL_BPP_Driver_Direct` was missing from the `URL_VAR_TO_SERVICE` map in
`Backend/dev/test-tool/dashboard/src/services/postman-parser.ts`.
Unknown base URL variables fell back to `service: 'internal'`, which left the full
template string (with `{{var}}` intact) as the path. After variable substitution the
result was an absolute URL that got concatenated after the proxy base → invalid URL.

**Fix:**
Added `'baseURL_BPP_Driver_Direct': { service: 'driver' }` to `URL_VAR_TO_SERVICE`.
`resolveService()` then extracts `/dashboard` as the base path from the env value
`http://localhost:8016/dashboard` and routes calls through `/proxy/driver-raw`.

File changed: `Backend/dev/test-tool/dashboard/src/services/postman-parser.ts`

---

### Issue 2: Step 05 — "At least one operation hub exists — expected 0 to be above 0"

**Before fixing:**
Step 05 (Get All Operation Hubs) returned an empty array `[]`.

**Root cause:**
No `operation_hub` row existed for the `NAMMA_YATRI_PARTNER / Bangalore`
merchant operating city in the local dev DB. The `getAllHubs` endpoint filters
by `merchant_operating_city_id`, so a missing seed produced an empty response.

**Fix:**
1. Added an idempotent seed row (`test-hub-001`) to
   `Backend/dev/local-testing-data/dynamic-offer-driver-app.sql` so the hub is
   seeded automatically on every fresh dev DB setup.
2. Also provided `setup-local-operation-hub.sql` alongside the collection for
   one-shot manual seeding on existing DBs.

File changed: `Backend/dev/local-testing-data/dynamic-offer-driver-app.sql`
File added:   `Backend/dev/integration-tests/collections/OperationHubFlow/setup-local-operation-hub.sql`

---

### Issue 3: Step 09 — "At least one request returned via creator (fleet owner) mobile — expected 0 to be above 0"

**Before fixing:**
Step 09 (Fetch Hub Requests by Creator Mobile Number — Backward Compat) returned
`{ requests: [], summary: { count: 0, totalCount: 10000 } }` even though the hub
request and its creator person existed in Postgres and the direct SQL query confirmed
1 matching row.

**Root cause (diagnosed):**
The `findAllRequestsInRange` query for the `Nothing` branch (no driver mobile filter)
uses a Beam INNER JOIN on the `person` table and filters by
`creator.mobile_number_hash = hash`. The fleet owner person is written to Redis KV
first; the KV drainer then flushes it to Postgres. Step 07 already retries until the
*hub request* appears in Postgres (ID-based join), but it does not verify that the
`mobile_number_hash` bytea column has been written. There is a brief window where:
- The person row exists in Postgres (enabling step 07's ID-based inner join to
  succeed), but
- The `mobile_number_hash` column is still NULL in Postgres (the drainer may write
  some columns in a subsequent pass).

During this window, step 09's hash-equality filter returns 0 rows.

**After fixing confirmed:**
When called directly with `curl` after the drainer completes, the API returns the
expected result (`count: 1`). The API logic is correct; only the test timing was wrong.

**Fix:**
Added the same retry-poll pattern used by step 07 to step 09:
- Pre-request: 2 s busy-wait on every attempt.
- Test script: if `requests.length > 0` → run assertions and reset counter;
  if `requests.length === 0` and `attempt < 8` → increment counter and
  `postman.setNextRequest(pm.info.requestName)`;
  if `attempt >= 8` → fail with a clear message.

This mirrors the KV-drainer wait already present in step 07 and handles the lag for
the `mobile_number_hash` column.

File changed: `Backend/dev/integration-tests/collections/OperationHubFlow/01-FetchHubRequestsByDriverMobile.json`
  — Step 09: added `prerequest` event + updated `test` event with retry logic.
