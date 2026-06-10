# MerchantDocumentFlow — Fix Summary

## What this flow tests

End-to-end CRUD lifecycle for merchant documents via Dashboard APIs + Provider UI read verification:
1. Driver authenticates (sendOTP + verify)
2. Dashboard: Create a `MerchantDocument` (TermsAndConditions)
3. Dashboard: List documents — verify created doc appears
4. Dashboard: Get document by type — verify fields
5. Provider UI: List documents with driver token (TokenAuth)
6. Provider UI: Get document by type (NoAuth, uses merchantId query param)
7. Dashboard: Update document — verify updated fields
8. Dashboard: Verify update via Get
9. Dashboard: Delete document
10. Dashboard: Verify deletion — doc no longer in list

---

## Issues found and fixed

### Issue 1 — `bpp_url` and `dashboard_url` not in proxy routing map → `Failed to construct 'URL': Invalid URL`

**Error**:
```
✗ Status 200 — expected response to have status code 200 but got 0
✗ authId present — expected undefined to be string
{ "error": "Failed to construct 'URL': Invalid URL" }
```

**Root cause**: The test dashboard proxies requests by resolving the base URL variable
(e.g. `{{bpp_url}}`) through a `URL_VAR_TO_SERVICE` map in
`dev/test-tool/dashboard/src/services/postman-parser.ts`.

This map tells the proxy which backend service to route to (`driver`, `provider-dashboard`, etc.).
`bpp_url` and `dashboard_url` — the two URL variables used by `MerchantDocumentFlow` — were
**not registered** in this map. So `resolveService()` fell through to the "internal" branch
which leaves the URL template unresolved. When the runner executed the request, `new URL(undefined)`
threw "Failed to construct 'URL': Invalid URL", the request got status 0, and no response
body was returned.

**Fix**: Added the two missing entries to `URL_VAR_TO_SERVICE`:
```ts
'bpp_url':      { service: 'driver' },           // http://localhost:8016/ui
'dashboard_url': { service: 'provider-dashboard' }, // http://localhost:8018
```

After this change the proxy correctly routes:
- `{{bpp_url}}/auth` → `driver` service (port 8016, path `/ui/auth`)
- `{{dashboard_url}}/bpp/driver-offer/...` → `provider-dashboard` service (port 8018)

Then rebuild the dashboard:
```bash
cd Backend/dev/test-tool/dashboard && npm run build
```

---

### Issue 2 — `403 ACCESS_DENIED` on Dashboard: Create MerchantDocument

**Error**:
```
✗ Status 200 — expected response to have status code 200 but got 403
{ "errorCode": "ACCESS_DENIED", "errorMessage": "You have no access to this operation." }
```

**Root cause**: The dashboard enforces per-endpoint access via an `access_matrix` table.
The `JUSPAY_ADMIN` role (id `37947162-3b5d-4ed6-bcac-08841be1534d`) had no rows in
`atlas_bpp_dashboard.access_matrix` for any of the four MerchantDocument action types:
- `PROVIDER_MANAGEMENT/MERCHANT/GET_MERCHANT_MERCHANT_DOCUMENT_LIST`
- `PROVIDER_MANAGEMENT/MERCHANT/POST_MERCHANT_MERCHANT_DOCUMENT_CREATE`
- `PROVIDER_MANAGEMENT/MERCHANT/POST_MERCHANT_MERCHANT_DOCUMENT_UPDATE`
- `PROVIDER_MANAGEMENT/MERCHANT/POST_MERCHANT_MERCHANT_DOCUMENT_DELETE`

These endpoints were added to the API but the access grants were never seeded for local dev.

**Fix**: Feature migration `0024-merchant-document-access-matrix-juspay-admin.sql` — inserts
`USER_FULL_ACCESS` rows for all four action types for the `JUSPAY_ADMIN` role.

Run it once against your local `atlas_bpp_dashboard`:
```bash
psql -d atlas_dev -f Backend/dev/feature-migrations/0024-merchant-document-access-matrix-juspay-admin.sql
```

---

### Issue 3 — `MERCHANT_DOCUMENT_ALREADY_EXISTS` on re-run

**Error**:
```
✗ Status 200 — expected response to have status code 200 but got 400
{ "errorCode": "MERCHANT_DOCUMENT_ALREADY_EXISTS", "errorMessage": "MerchantDocument already exists for TermsAndConditions/Driver/ENGLISH/..." }
```

**Root cause**: A previous run (that failed mid-way due to issues 1 or 2) created the document
but never reached the delete step at the end. The `TermsAndConditions/Driver/ENGLISH` row
was left in the DB, so the next Create call got 400.

**Fix**: Added two pre-cleanup steps at the start of the collection:
1. **Pre-cleanup: Find existing MerchantDocument** — lists documents and saves the ID of any
   existing `TermsAndConditions/Driver` doc into `existing_doc_id`
2. **Pre-cleanup: Delete existing MerchantDocument if any** — deletes it if `existing_doc_id`
   is set, skips silently if not

This makes the collection safe to re-run at any time without manual DB cleanup.

---

## Files changed

| File | Change |
|------|--------|
| `dev/test-tool/dashboard/src/services/postman-parser.ts` | Added `bpp_url` and `dashboard_url` to `URL_VAR_TO_SERVICE` proxy map |
| `dev/feature-migrations/0024-merchant-document-access-matrix-juspay-admin.sql` | Grants JUSPAY_ADMIN `USER_FULL_ACCESS` for all 4 MerchantDocument dashboard APIs |
| `collections/MerchantDocumentFlow/01-MerchantDocumentCRUD.json` | Added 2 pre-cleanup steps to delete leftover doc before Create, making the flow re-runnable |

## Re-run checklist

1. Run migration `0024-merchant-document-access-matrix-juspay-admin.sql` against your local DB (one-time)
2. Make sure local services are running: `dynamic-offer-driver-app` (port 8016) and `provider-dashboard` (port 8018)
3. Select environment **"Local - NAMMA_YATRI Bangalore (MerchantDocument)"** in the test runner
4. The flow creates and deletes its own document — safe to re-run without any DB reset
