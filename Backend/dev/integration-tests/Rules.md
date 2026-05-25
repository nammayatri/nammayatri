# Integration Test Collection Rules

Guidelines for writing and maintaining Newman/Postman integration test collections.

## Concurrency & Idempotency

- **All dynamic identifiers must be random per run** — mobile numbers, vehicle registration numbers, OTPs, etc. This ensures collections can run concurrently and multiple times without conflicts.
- Generate random values in the **collection-level prerequest script**, store in `pm.collectionVariables`:
  ```js
  if (!pm.collectionVariables.get('_test_rider_number')) {
      var rNum = '8' + Math.floor(100000000 + Math.random() * 899999999);
      pm.collectionVariables.set('_test_rider_number', rNum);
  }
  ```
- Prefix generated variables with `_test_` to distinguish from environment config.
- **Never hardcode** phone numbers, registration numbers, or any per-entity identifiers in collection request bodies.

## What Goes in Environment Files

**Hardcode (city/merchant config):**
- Base URLs (`baseUrl_app`, `baseURL_namma_P`, `baseUrl_lts`, `mockServerUrl`, `dashboard_base_url`)
- Merchant IDs (`driver_merchant_id`, `bap_merchant_id`, `dashboard_merchant_id`)
- City name, state, coordinates (`city`, `state`, `origin_lat`, `origin_lon`, `dest_lat`, `dest_lon`)
- Vehicle config (`vehicle_variant`, `vehicle_class`, `vehicle_category`)
- Service-specific config (`vehicle_type`, `platform_type` for FRFS)
- Auth defaults (`login_otp`, `dashboard_token`)

**Never hardcode (generated per run):**
- Mobile numbers (rider, driver)
- Vehicle registration numbers
- Auth tokens (set dynamically from API responses)
- Booking/search/estimate IDs (set from responses)
- Payment order IDs

## Mock Server Overrides (`POST /mock/override`)

The mock server has a single override mechanism. Each rule matches incoming
requests by extracting a field value; when it matches, `response` is deep-merged
into the handler's default response.

```json
{"service": "juspay", "extract": "path.2", "value": "{{payment_order_id}}",
 "match": "/orders", "response": {"status": "CHARGED", "amount": 10.0}}
```
```json
{"service": "cris", "extract": "body.mob", "value": "9876543210",
 "response": {"respCode": 500}}
```
- **Extract syntax:** `body.<path>`, `path.<index>`, `query.<param>`, `header.<name>`
- `value` is matched with strict equality against the extracted field
- `match` (optional) is a path-substring gate — rule fires only when the request path contains it
- For encrypted request bodies (CRIS, CMRL), registered body decoders decrypt before matching
- Overrides auto-expire after 5 minutes
- **Always `DELETE /mock/override` after use** to avoid leaking between tests

For payment-order updates, register the override **after** capturing the
order id from the create-order / payment-info response. Collections register
the rule twice — once per id form (`payment_order_id` and `payment_order_short_id`) —
because rider-app may poll status using either form.

## Collection Structure

- Each collection is self-contained: auth → setup → action → verify
- Use `setTimeout` / busy-wait for delays (Newman doesn't support async sleep)
- Use `pm.execution.setNextRequest()` for conditional flow (retry logic)
- Assertions go in test scripts — fail fast with `--bail`

## Adding a New City

1. Create a new environment file under the suite's `Local/` subfolder: `Local/Local_<PREFIX>_<City>.postman_environment.json` (use `Master/Master_<PREFIX>_<City>.postman_environment.json` for the master env type)
2. Set city-specific values (merchant IDs, coordinates, merchant short IDs)
3. Set the `envType` variable: `"Local"` in `Local/*.postman_environment.json`, `"Master"` in `Master/*.postman_environment.json` — this drives the mock-server auto-skip behavior described below.
4. Collections are shared across cities — no city-specific logic in collections
5. If the city needs a dashboard token, ensure `merchant_access` exists (via `dev/feature-migrations/0001-dashboard-access-setup.sql`)

## Upstream Config Environment (Sync From)

Each city's environment file is auto-mapped to one or more upstream config-sync bundles (the same set listed in `CONFIG_SYNC_BUNDLE_URLS` in `Backend/dev/test-tool/context-api/server.py`):

- `master` — always available (every city).
- `prod_international` — Helsinki cities only (detected when the env file's `city` or filename contains `helsinki`, case-insensitive).
- `prod` — every non-Helsinki city.

The test-dashboard surfaces this as a **Sync From** dropdown next to **Env Type**. On Run, the dashboard checks `/api/config-sync/status`; if `last_synced.from` already matches the chosen upstream env, the sync is skipped. Otherwise it triggers `/api/config-sync/import` and blocks until done. The last successful source is persisted at `data/config-sync/.last-synced-env` so the marker survives `test-context-api` restarts.

Compatibility is derived in `_derive_compatible_envs()` (`server.py`) — to change a city's mapping, edit that helper. No change to the postman environment file is needed; newman keeps consuming `Local/Local_*.postman_environment.json` unchanged (the `compatibleEnvs` field is computed at scan time and never written into the JSON).

## Environment Types & Mock-Server Auto-Skip

Each suite has two environment-type subfolders:

- `Local/` — points at the local dev stack. Mock servers (`mockServerUrl`, `mock_fcm_url`, …) are running here.
- `Master/` — points at any non-local stack (cloud / staging). Mock servers are **not** running.

Requests that hit mock endpoints are skipped automatically on non-Local envs:

- **In Newman / `run-tests.sh`**: every collection has a collection-level prerequest that reads `envType` from the environment and calls `pm.execution.skipRequest()` for requests whose URL contains `mockServerUrl` or `mock_fcm_url`. Newman 6+ reports these as `skipped` (not failures).
- **In the test dashboard**: the same predicate is evaluated at render time and mock-only steps are hidden from the step list when the selected env type is not `Local`. The Run button shows the visible step count with a "N hidden" suffix.

Authors: do not gate mock requests with manual `if (envType ...)` blocks inside each request — the collection-level prerequest already handles it. Just ensure mock requests are addressed via the `{{mockServerUrl}}` / `{{mock_fcm_url}}` variables (which is the existing convention).

### Per-city BPP integration variations (`frfs_integration_type`)

Some rider-app endpoints are only implemented for a subset of FRFS BPP integration types — e.g. `POST /frfs/ticket/verify` (`ExternalBPP/ExternalAPI/CallAPI.hs:244-248`) only handles `DIRECT` and throws `Unimplemented!` for `ONDC`. To keep one collection runnable across all cities, each FRFS city env declares its integration type:

```json
{ "key": "frfs_integration_type", "value": "DIRECT", "type": "default" }
```

Values: `DIRECT` (synchronous BPP, e.g. Chennai bus) or `ONDC` (async Beckn via mock-server, e.g. Bhubaneshwar bus / Chalo).

Steps that depend on a DIRECT-only code path should add a step-level prerequest that skips when the value isn't `DIRECT`:

```javascript
(function () {
  var t = pm.environment.get('frfs_integration_type') || 'DIRECT';
  if (t === 'DIRECT') return;
  if (pm.execution && typeof pm.execution.skipRequest === 'function') {
    pm.execution.skipRequest();
  }
})();
```

Newman reports those steps as `skipped`, not `failed`, so the collection still exits 0 on ONDC cities. See `BusTicketBookingFlow/01-DirectBusBooking.json` — the `Verify Ticket` and `Verify Ticket Status` steps use exactly this pattern.

## Adding a New Flow

1. Create `<NN>-<FlowName>.json` in the appropriate directory
2. Reuse the same collection prerequest for random number generation
3. Follow the step naming convention: `Verb + Object (Context)` e.g., `Get Booking Status (Trigger Confirm)`
4. Add mock override steps where external service behavior needs to be controlled
