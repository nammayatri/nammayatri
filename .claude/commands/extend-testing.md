# Extend Testing Framework

You are extending the NammaYatri testing framework. Read `Backend/.cursor/docs/17-testing-framework.md` for the full architecture.

## Key directories
- `Backend/dev/config-sync/` — Config sync tool (export/patch/import from master DB)
- `Backend/dev/integration-tests/` — Newman/Postman E2E test collections
- `Backend/dev/mock-servers/` — Unified Python mock server for all external services
- `Backend/dev/test-tool/` — Dashboard UI + context API

## When adding a new integration test collection

1. Read `Backend/dev/integration-tests/Rules.md` for guidelines
2. Place the collection in the appropriate subdirectory under `collections/`
3. All dynamic identifiers (mobile numbers, vehicle registrations) MUST be random — generate in collection prerequest script using `pm.collectionVariables.set('_test_*', ...)`
4. City/merchant config goes in environment files (`Local_<PREFIX>_<City>.postman_environment.json`), never hardcoded in collections
5. For dashboard API access, use the Switch City pattern: call `POST {{dashboard_base_url}}/user/switchMerchantAndCity` to get a city-specific token
6. Use `POST {{mockServerUrl}}/mock/status` to control payment states by order ID
7. Use `POST {{mockServerUrl}}/mock/override` to control responses by request field matching (e.g., `body.mob` for encrypted CRIS requests)
8. Always `DELETE /mock/override` with specific `{service, extract, value}` after use — never clear all overrides (concurrent safety)

## When adding a new mock service

1. Create `Backend/dev/mock-servers/services/<name>.py` with a `handle(handler, path, body)` function
2. Every handler MUST call `handler._get_override("service_name", *path_ids)` and deep-merge `extra` into the response
3. Add the route to `ROUTES` in `Backend/dev/mock-servers/server.py`: `("/prefix", module, "service_name")`
4. If the service has encrypted request bodies, register a body decoder: `register_body_decoder("service_name", decoder_fn)` so the override middleware can extract fields
5. Match response types to the Haskell `FromJSON` types — check `shared-kernel` or `ExternalBPP/` for the exact type definition. Wrong field names or types cause `DecodeFailure`
6. Use `from status_store import extract_path_ids, deep_merge` for shared utilities

## When adding a new config table to sync

1. Add the table name to `Backend/dev/config-sync/assets/config.json` under the correct schema
2. If the table has encrypted fields, add `ENCRYPT:S"mock-value"` overrides in `patches.json` under `dimension_overrides.<schema>.<table>`
3. If the table has external service URLs, verify they're covered by `global_replacements` in `patches.json`
4. Run `python config_transfer.py discover` to check for new CachedQuery tables not yet in config.json

## When adding a new city/merchant for tests

1. Create a new environment file: `Local_<PREFIX>_<City>.postman_environment.json`
2. Set city-specific values: `city`, `state`, `origin_lat/lon`, `dest_lat/lon`, `driver_merchant_id`, `bap_merchant_id`, `dashboard_merchant_id`
3. Add the city to `post-import-setup.sql` merchant_access loop (so dashboard switchMerchantAndCity works)
4. If the city uses a different country, update the collection prerequest for proper phone number format and country code

## When modifying patches.json

1. `patches.json` is gitignored — it contains `ENCRYPT:` values. Only `patches.json.example` is tracked
2. `ENCRYPT:` values MUST use `S""` format: `ENCRYPT:S"plaintext"` (passetto requirement)
3. For AES keys, ensure correct byte length: 32 bytes for AES-256
4. `merge_json` does deep-merge into existing JSON columns — specify the correct column name (e.g., `config_json`, `config_value`, `ext_webhook_configs`)
5. `global_replacements` are simple string find/replace on raw file content — order matters for overlapping patterns
6. After modifying, re-run `python config_transfer.py import --from master --to local` and flush Redis
