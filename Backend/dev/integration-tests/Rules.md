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

## Mock Server Overrides

### ID-based status (`POST /mock/status`)
For overriding responses by a known identifier (e.g., payment order ID):
```json
{"service": "juspay", "id": ["order-123", "short-456"], "status": "CHARGED", "data": {"amount": 150}}
```
- `id` can be string or array — stored under all IDs
- `data` fields are deep-merged into the default response

### Request-matching overrides (`POST /mock/override`)
For overriding responses by matching a field in the incoming request:
```json
{"service": "cris", "extract": "body.mob", "value": "9876543210", "response": {"respCode": 500}}
```
- **Extract syntax:** `body.<path>`, `path.<index>`, `query.<param>`, `header.<name>`
- For encrypted request bodies (CRIS, CMRL), registered body decoders decrypt before matching
- Overrides auto-expire after 5 minutes
- **Always `DELETE /mock/override` after use** to avoid leaking between tests

## Collection Structure

- Each collection is self-contained: auth → setup → action → verify
- Use `setTimeout` / busy-wait for delays (Newman doesn't support async sleep)
- Use `pm.execution.setNextRequest()` for conditional flow (retry logic)
- Assertions go in test scripts — fail fast with `--bail`

## Adding a New City

1. Create a new environment file: `Local_<PREFIX>_<City>.postman_environment.json`
2. Set city-specific values (merchant IDs, coordinates, merchant short IDs)
3. Collections are shared across cities — no city-specific logic in collections
4. If the city needs a dashboard token, ensure `merchant_access` exists (via `dev/feature-migrations/0001-dashboard-access-setup.sql`)

## Adding a New Flow

1. Create `<NN>-<FlowName>.json` in the appropriate directory
2. Reuse the same collection prerequest for random number generation
3. Follow the step naming convention: `Verb + Object (Context)` e.g., `Get Booking Status (Trigger Confirm)`
4. Add mock override steps where external service behavior needs to be controlled
