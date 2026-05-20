# Helsinki Onboarding Collections

End-to-end onboarding for the **BRIDGE_FINLAND_PARTNER / Helsinki** environment.
Built to feed `InternationalRideBookingFlow/*` collections downstream (a real onboarded
driver + vehicle vs. relying on legacy seeds), and `HelsinkiInvoices/*` to verify
ledger/invoice generation post-ride.

## Prerequisites

1. **Config sync from master** (mandatory — brings BRIDGE_FINLAND_PARTNER merchant,
   role rows, access_matrix rows):
   ```
   cd Backend/dev/config-sync
   python config_transfer.py --from prod_international --to local
   ```
2. **Local-testing-data seeds** (re-run on every `test-context-api` restart;
   guarantees the JUSPAY_ADMIN dev token + cross-merchant access):
   - `Backend/dev/local-testing-data/provider-dashboard.sql`
   - `Backend/dev/local-testing-data/rider-dashboard.sql`
3. **Helsinki feature migrations** (already-existing files; run once per fresh DB):
   - `dev/feature-migrations/0001-helsinki-online-payment-offers.sql`
   - `dev/feature-migrations/0003-enable-invoice-generation-for-helsinki-delhi.sql`
   - `dev/feature-migrations/0005-enable-cancellation-fee-helsinki.sql`
   - `dev/feature-migrations/0007-helsinki-vat-config.sql`
4. Mock-servers running on `:8080` (Stripe etc.).
5. rider-app (`:8013`), dynamic-offer-driver-app (`:8016`), BPP dashboard (`:8018`),
   BAP dashboard (`:8017`), LTS (`:8081`) all up.

The orchestrator `Backend/dev/integration-tests/run-helsinki-e2e.sh` chains all of
the above; running individual collections directly is supported for debugging.

## Roles and Auth Model

After config-sync + local-testing-data seeds:

- **JUSPAY_ADMIN** seeded person `3680f4b5-dce4-4d03-aa8c-5405690e87bd`
  (email `juspay_admin@dashboard.com`, password `juspay_admin`).
- A dev `registration_token` row pins token
  `local-admin-token-bangalore-namma-yatri` to that admin **plus** cross-merchant
  access (including `BRIDGE_FINLAND_PARTNER`) via the seed's
  `INSERT INTO merchant_access ... CROSS JOIN unnest(supported_operating_cities)`.
- Every collection starts with **`/user/switchMerchantAndCity`** to re-scope the
  token to `BRIDGE_FINLAND_PARTNER` + `Helsinki` and stores the rotated `authToken`
  back into `dashboard_token`.

This is why there is no separate "Admin Fleet" collection — that persona is the
seeded JUSPAY_ADMIN under merchant-scoped access. The role-creation Q from the plan
(Q2: "Same as JUSPAY_ADMIN, merchant-scoped") is satisfied by the seed.

## Env-variable Propagation

All collections in this directory share `Local_BF_Helsinki.postman_environment.json`.
`run-helsinki-e2e.sh` invokes newman with `--export-environment` between steps so
generated IDs flow forward:

```
01-FleetOwnerOnboarding.json   →  fleet_owner_id, fleet_owner_token, doc_id_*
02-StripeOnboarding.json       →  stripe_account_id
03-AddDriver_*.json            →  driver_id, driver_token, driver_mobile
04-AddVehicle_*.json           →  vehicle_id, vehicle_reg_no, vehicle_rc_image_id
```

Downstream `InternationalRideBookingFlow/*` collections consume `driver_id`,
`driver_token`, and `vehicle_reg_no` rather than registering a one-shot driver
inline (as `01-StripeRideFlow.json` currently does).

## 01-FleetOwnerOnboarding.json (this collection)

**Auth model:** uses the **V2 fleet flow** (`/fleet/v2/...`) — this is critical
because the legacy `/fleet/register` only writes to
`atlas_bpp_dashboard.person`, NOT to `atlas_driver_offer_bpp.person`. Without
the driver-app person row, downstream Stripe onboarding cannot resolve the
fleet owner via `checkRequestorAccessToFleet` (which queries the driver-app
schema). The V2 OTP login + register sequence cross-writes to both.

| # | Step | Endpoint | Why |
|---|---|---|---|
| 01 | Switch Dashboard to Helsinki | `POST /user/switchMerchantAndCity` | Scope the admin token |
| 02 | Fleet Owner Login OTP | `POST /{m}/{c}/fleet/v2/login/otp` (NoAuth) | Triggers OTP; creates driver-app person if new |
| 03 | Fleet Owner Verify OTP | `POST /{m}/{c}/fleet/v2/verify/otp` (NoAuth) | Verifies OTP; returns `authToken` → `fleet_owner_token` |
| 04 | Fleet Owner Register V2 | `POST /{m}/{c}/fleet/v2/register` (ApiAuthV2) | Completes profile (firstName, lastName, BUSINESS_FLEET) |
| 05 | SQL: Resolve fleet_owner_id | `POST /mock/sql/select` against `atlas_driver_offer_bpp.person` | V2 dashboard endpoints return `APISuccess`; personId pulled directly from DB |
| 06–08 | TaxiTransportLicense: upload → common → approve | `POST /driver/{id}/document/upload`, `…/documents/common`, `…/driver/documents/update` | 1st of 4 mandatory FLEET_BUSINESS docs |
| 09–11 | FinnishIDResidencePermit: upload → common → approve | (same trio) | 2nd doc |
| 12–14 | BusinessLicense: upload → common → approve | (same trio) | 3rd doc |
| 15–17 | TAXDetails: upload → common → approve | (same trio) | 4th doc — completes `fleet_owner_information.docs_verification_status = ADMIN_APPROVED` |
| 18 | SQL Backstop | `POST /mock/sql/select` against `fleet_owner_information` | Authoritative check: `docs_verification_status = 'ADMIN_APPROVED'` |
| 19 | SQL Backstop | `POST /mock/sql/select` against `common_driver_onboarding_documents` | Authoritative check: each of 4 doc types `verification_status = 'APPROVED'` |

JSON shape note: `/driver/documents/update` body uses Aeson's `{tag, contents}`
encoding for the `UpdateDocumentRequest = Approve | Reject` discriminated union
and nested `ApproveDetails = CommonDocument …`. Mirrors `test-doc-status-flow.sh`.

## Things this collection does **not** do (intentional)

- Does not bootstrap the JUSPAY_ADMIN — relies on config-sync + local-testing-data.
- Does not invoke Stripe onboarding — that is `02-StripeOnboarding.json` (next).
- Does not assert `fleet_owner_information.docs_verification_status` via SQL —
  the API-level assertion in step 16 (all 4 docs listed) is the proxy; SQL-level
  assertion is the orchestrator's job (`run-helsinki-e2e.sh` can `psql -c …`).

## Run standalone

```
cd Backend/dev/integration-tests
newman run collections/HelsinkiOnboarding/01-FleetOwnerOnboarding.json \
  -e collections/HelsinkiOnboarding/Local_BF_Helsinki.postman_environment.json \
  --export-environment /tmp/helsinki-env-after-01.json \
  --bail --timeout-request 60000
```

Inspect `/tmp/helsinki-env-after-01.json` for `fleet_owner_id`, `doc_id_*`.

## 02-StripeOnboarding.json

| # | Step | Endpoint | Why |
|---|---|---|---|
| 01 | Get Stripe Connect Onboarding Link | `POST /{m}/{c}/fleet/register/bankAccount/link?fleetOwnerId=…&paymentMode=LIVE` | Dashboard fleet endpoint; requires `fleet_owner_token` from A.03 |
| 02 | SQL: Resolve Stripe account ID | `POST /mock/sql/select` against `driver_bank_account` | Look up the created Stripe account ID for the mock override |
| 03 | Mock Stripe: Override Account | `POST /mock/override` with `extract: path`, `value: /v1/accounts/<id>` | Makes future `GET /accounts/<id>` return `charges_enabled=true` |
| 04 | Get Bank Account Status | `GET /{m}/{c}/fleet/register/bankAccount/status?fleetOwnerId=…` | Asserts `chargesEnabled = true` via API |
| 05 | SQL Backstop | `POST /mock/sql/select` against `driver_bank_account` | Authoritative check: `charges_enabled = true` in DB |

## End-to-end audit (per step / per combo)

### Source-of-truth principle

**`config-sync from master IS the source of truth for all Helsinki configs.**

`config.json` already includes the relevant tables in the `prod_international_to_local`
direction:
- `merchant`, `merchant_operating_city` (lines 23, 238, 507, 524) — brings
  Helsinki + `supported_operating_cities` array
- `merchant_payment_method` (lines 41, 256) — brings Helsinki's Cash + Stripe rows
- `document_verification_config` (line 301) — brings all driver/vehicle doc rules
- `fleet_owner_document_verification_config` (line 304) — brings the 4 FLEET_BUSINESS docs
- `merchant_service_config`, `merchant_service_usage_config` (lines 244–248) —
  SMS provider, payment provider configs

`patches.json.example` overlays URL rewrites + ENCRYPT: re-encryption for local
mocks. Concretely: `https://api.stripe.com` → `http://localhost:8080/stripe`;
Helsinki-specific `merchant_operating_city_id = 391b4b7a-…` forced to
`create_payment_customer = "Stripe"`; all apiKeys re-encrypted to `test-apiKey`
via local passetto key.

**We do NOT seed Helsinki configs locally.** If a Helsinki ride/onboarding step
fails because a row is missing, the right fix is one of:
1. Add the row in prod_international (so the next sync brings it)
2. Add the table to `config.json` (if it's not currently in scope)
3. Add a patch to `patches.json.example` for selective overrides

Only `feature-migrations/000{1,3,5,7}-helsinki-*.sql` run locally after sync — they
toggle dev-only behaviors (cancellation-fee dynamic logic, invoice generation flag,
VAT rates, online payment offers) that aren't permanently desired in prod.

### Onboarding steps (HelsinkiOnboarding/)

| Step | Source of correctness | Verdict |
|---|---|---|
| A.01 `/user/switchMerchantAndCity` | `merchant.supported_operating_cities` from sync + `local-testing-data/provider-dashboard.sql` re-run (orchestrator Phase 0) gives JUSPAY_ADMIN cross-merchant access | 🟢 |
| A.03 step 1 switch | Same as A.01 | 🟢 |
| A.03 step 2-3 `/fleet/v2/login/otp` + `/verify/otp` | OTP `7891` matches via `dynamic-offer-driver-app.sql` seed `registration_token.auth_value='7891'` for all merchants | 🟢 |
| A.03 step 4 `/fleet/v2/register` | No Helsinki-specific config required | 🟢 |
| A.03 step 5 SQL backstop by first_name | Column exists in Person schema; lookup unique per run | 🟢 |
| A.03 step 6-17 doc upload/approve | `fleet_owner_document_verification_config` for Helsinki comes from master via sync (config.json line 304) | 🟢 |
| A.03 step 18-19 SQL backstops | Tables + filters valid | 🟢 |
| A.05 driver OTP | Same seed token as A.03 | 🟢 |
| A.05 driver profile | No Helsinki-specific config | 🟢 |
| A.06 `/driver/{id}/addVehicle` + RC + VehicleFront | `document_verification_config` for Helsinki comes from master via sync (config.json line 301) | 🟢 |
| A.06 `/driver/{id}/enable` | Requires valid RC (satisfied by approve step); bank account NOT required | 🟢 |

### Ride combos (InternationalRideBookingFlow/) — what each combo needs

| # | Combo | Source of correctness | Verdict |
|---|---|---|---|
| 1 | Online + Complete + NoDisc | Stripe URL rewrite in patches.json verified; Helsinki forced to Stripe via `create_payment_customer="Stripe"` patch | 🟢 |
| 2 | Online + Cancel + NoDisc | `transporter_config.can_add_cancellation_fee=true` from feature-migration 0005 (runs after sync) | 🟢 |
| 3 | Online + Complete + Disc | Offer logic injected inline by collection; `useDomainOffers` enabled by 0001 for Stripe path | 🟢 |
| 4 | Cash + Complete + NoDisc | `merchant_payment_method` Cash row for Helsinki comes from master via sync (config.json lines 41, 256) | 🟢 |
| 5 | Cash + Complete + Disc | Cash from master + offer injected by collection | 🟢 |
| 6 | Online + Cancel + Disc | Cancellation flag from 0005; offer from collection | 🟢 |
| 7 | Cash + Cancel + NoDisc | Cash from master; cancellation flag from 0005 | 🟢 |
| 8 | Cash + Cancel + Disc | Cash + cancellation + offer all in place | 🟢 |

### Latent bug flagged in pre-existing `0005-enable-cancellation-fee-helsinki.sql`

That migration hardcodes the BPP Helsinki city UUID as
`beabba6a-c817-43d2-93b2-a916f5cf2ceb`. If config-sync from master delivers a
different UUID for Helsinki (master may have regenerated it, or the original
local-test UUID never matched master), the migration's `UPDATE … WHERE
merchant_operating_city_id = 'beabba6a-…'` will silently affect 0 rows. The
cancellation-fee flag won't flip and combos 2, 6, 7, 8 will fail at the
"Wait For No-Show Window" / "Cancel Ride" steps.

Not my code to fix; flagging as a pre-existing issue. Quick check after a
config-sync:
```
psql … -c "SELECT id FROM atlas_driver_offer_bpp.merchant_operating_city
           WHERE city='Helsinki' AND merchant_short_id='BRIDGE_FINLAND_PARTNER'"
```
If the returned UUID is NOT `beabba6a-…`, 0005 needs to be rewritten to use a
dynamic city lookup (same pattern as `dev/sql-seed/test-doc-status-refresh.sql`).

### If a step fails at runtime

Don't reach for a local seed. Diagnose by which layer didn't provide:

```
✗ failing step
├─ Is the table in config.json?       → if no, add it there (PR to config-sync)
├─ Is the row in prod_international?  → if no, the master DB needs it (PR upstream)
├─ Is patches.json rewriting a URL?   → if a 3rd-party API is being called for real, add a URL patch
└─ Is it a Helsinki-only dev flag?    → only then, a new feature-migration
```

### Invoice verification (HelsinkiInvoices/) — what makes invoices land

| Item | Verdict | Why |
|---|---|---|
| Invoice generation enabled for Helsinki | 🟢 OK | `0003-enable-invoice-generation-for-helsinki-delhi.sql` (pre-existing) |
| Customer invoice row | 🟢 OK | Created on ride completion |
| Driver invoice row | 🟢 OK | Created on ride completion |
| AggregatedCommission invoice | 🟢 OK | Async job; SQL backstop in collection step 07 is the authoritative check |
| Fleet Owner invoice | 🟢 fixed | All 8 ride combos patched: their `Add Vehicle (Dashboard)` request now passes `?fleetOwnerId={{fleet_owner_id}}` so the freshly-registered driver is linked to the onboarded fleet owner. When the orchestrator chains Phase A → Phase B with --export-environment, `fleet_owner_id` flows through. Standalone runs of a ride collection (no prior onboarding) resolve the variable to empty string — server ignores the param, no regression. |

### Audit findings (post-implementation)

These are issues uncovered after the first-pass implementation. Some are fixed;
some are documented limitations the test will surface at first runtime.

### ✅ Fixed
- **Stripe mock had gaps** — `services/stripe.py` was missing
  `requirements`/`future_requirements`/`capabilities`/`country`/`business_type`
  on `GET /v1/accounts/<id>` (Haskell parser at `PersonBankAccount.hs:210`
  would crash). Added now. Also added `POST /v1/transfers` + `POST /v1/payouts`
  stubs for ride-completion payouts.
- **A.03 step 5 SQL** — was filtering on `unencrypted_mobile_number` which
  doesn't exist. Pivoted to filter by `first_name` (the collection generates
  a unique `Helsinki<9digitrand>` per run, so it's collision-free).
- **A.02 V1 → V2 flow** — `/fleet/register` only writes to
  `atlas_bpp_dashboard.person`; downstream Stripe needs the fleet owner in
  `atlas_driver_offer_bpp.person` too. Rewrote A.03 to use the V2 flow
  (`/fleet/v2/login/otp` → `/fleet/v2/verify/otp` → `/fleet/v2/register`)
  which cross-writes to both schemas.
- **Stripe-via-config-sync** — verified `patches.json.example` for the
  `prod_international_to_local` direction globally replaces
  `https://api.stripe.com` → `http://localhost:8080/stripe` and sets a
  Helsinki-specific override (`merchant_operating_city_id =
  391b4b7a-3cc2-429d-b18f-034dbab6e90d` → `create_payment_customer = "Stripe"`).
  ApiKeys re-encrypted via `ENCRYPT:` prefix to test value. So Stripe calls
  from the driver-app DO go to the local mock after sync.

### ⚠ Known limitations (test will surface at runtime, not silently broken)

- **OTP "7891"** — `/fleet/v2/verify/otp` only succeeds if backend
  `transporter_config.useFakeSms` returns "7891". If not configured, the
  verify step 4XX's and onboarding halts at step 03. Fix: add a
  post-config-sync SQL that sets `useFakeSms` on `transporter_config`, or
  use a Helsinki-specific override in `patches.json` for the SMS provider
  config. The orchestrator's Phase 0 is the right place.
- **Ride flows create FRESH drivers** — `01-StripeRideFlow.json`,
  `02-StripeRideUserCancellation.json`, etc. each call `POST /auth` with a
  newly-generated mobile and set their own `driver_id`. They do NOT consume
  the `driver_id` populated by `HelsinkiOnboarding/03-AddDriver_AdminPath.json`.
  Consequence: the **Fleet Owner invoice** assertion in `HelsinkiInvoices/`
  may return zero results because the ride wasn't attributed to the onboarded
  fleet owner. The Customer/Driver/AggregatedCommission assertions still
  hold against the fresh driver. To wire the chain properly, either modify
  each ride collection's `Add Vehicle (Dashboard)` request to include
  `?fleetOwnerId={{fleet_owner_id}}` (one query-param addition per collection),
  OR accept that `HelsinkiOnboarding/` and `InternationalRideBookingFlow/`
  are independent threads that share only invoice-verification scaffolding.
- **FLEET-role probes (03/04 FleetPath)** — non-gating; will likely 403 or
  404 until rebasing-with-msil-main branch exposes those APIs to FLEET role.

## Complementary path: test-tool Client Application Workflow

Independent of this Postman flow, the new `Backend/dev/test-tool/` (commit
`004a3f4128`, May 2026) spins up real client UIs you can click through:
- `control-center-frontend.yaml` → port 4002 (the JUSPAY admin dashboard UI)
- `control-center-backend.yaml` → port 4001
- `ny-rn-provider.yaml` → driver React Native app
- `ny-rn-consumer.yaml` → consumer React Native app

For the EU/Helsinki branches, edit `control-center-frontend.yaml`'s `source.ref`
to `rebasing-with-msil-main` (or `msil-main` for India). Then onboarding flows
can be exercised manually through the actual dashboard UI as a parallel
verification — useful when API-level Postman testing surfaces a question about
intended UX behavior.

## Open / Known issues

- Step A.03/05 (resolve fleet_owner_id) filters `atlas_driver_offer_bpp.person`
  by `unencrypted_mobile_number` — if your build doesn't have that column,
  the filter won't match. Fall back is `/admin/person/list?searchString=`
  on the dashboard. Adjust the WHERE clause based on first-run results.
- Step A.04/01 (Stripe link) — the dashboard's `checkRequestorAccessToFleet`
  derives `requestorId` from the token. With `fleet_owner_token`, the requestor
  IS the fleet owner; the FLEET_OWNER / FLEET_BUSINESS role branch is hit.
  If you'd rather call as JUSPAY_ADMIN, swap the token to `dashboard_token`
  and add `&requestorId=<admin_driver_app_person_id>` — but note the admin's
  driver-app person ID needs to exist; the dev seed only creates dashboard
  person. The fleet-owner-as-requestor path is the cleaner one.
