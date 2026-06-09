# FRFS Fleet Operator Flow — Changes & Justification

## Context

The `FRFSFleetOperatorFlow` collection tests the bus conductor's end-to-end trip cycle:
`Auth → Current Operation → Trip Start → Get Route → Get Manifest → Trip End`

Two bugs blocked the very first step ("Conductor Auth") from running.

---

## Fix 1 — Test Dashboard: `baseUrl_driver` not routed through proxy

**File:** `Backend/dev/test-tool/dashboard/src/services/postman-parser.ts`

**Problem:**
The `URL_VAR_TO_SERVICE` map tells the dashboard's Postman runtime which backend service
each environment base-URL variable belongs to, so requests can be routed through the
correct CORS proxy. `baseUrl_driver` (used by every step in `FRFSFleetOperatorFlow`) was
missing from this map.

As a result, the parser fell through to `service: 'internal'` and kept the full
`{{baseUrl_driver}}/ui/auth` string as the path template. At execution time the variable
resolved to the literal value `http://localhost:${DRIVER_APP_PORT:8016}` (shell syntax
that the React app does not expand). This was concatenated directly onto the proxy base
URL, producing `http://localhost:7082http://localhost:${DRIVER_APP_PORT:8016}/ui/auth` —
an invalid URL — which caused axios to throw **"Failed to construct 'URL': Invalid URL"**
with HTTP status 0.

**Fix:**
Added `'baseUrl_driver': { service: 'driver' }` to `URL_VAR_TO_SERVICE`. This mirrors the
identical pattern already documented for `baseURL_BPP_Driver_Direct`. With this entry,
the parser extracts just the path (`/ui/auth`) and routes the request through
`/proxy/driver-raw` → `http://localhost:8016`.

---

## Fix 2 — Backend: `CONDUCTORTOKEN` not a recognised `IdentifierType`

After Fix 1, the request reached the driver-app but was rejected with:

```
Error in $.identifierType: parsing Domain.Types.Person.IdentifierType failed,
expected one of the tags ["MOBILENUMBER","AADHAAR","EMAIL","GIMS_EMAIL_PASSWORD"],
but found tag "CONDUCTORTOKEN"
```

The collection sends `"identifierType": "CONDUCTORTOKEN"` in the auth body.
`CONDUCTORTOKEN` is a new auth mode for bus conductors who are stored directly in the
BPP database (unlike `GIMS_EMAIL_PASSWORD` conductors who are validated against an
external government GIMS API).

### 2a — YAML spec: add enum value

**File:** `Backend/app/provider-platform/dynamic-offer-driver-app/Main/spec/Storage/Person.yaml`

Added `CONDUCTORTOKEN` to the `IdentifierType` enum. After `, run-generator`, this
regenerates `src-read-only/Domain/Types/Person.hs` with the new constructor, which makes
the JSON parser accept the tag.

### 2b — Auth handler: implement CONDUCTORTOKEN login

**File:** `Backend/app/provider-platform/dynamic-offer-driver-app/Main/src/Domain/Action/UI/Registration.hs`

**`auth` function — new `SP.CONDUCTORTOKEN` case:**
Local email + password authentication for a `BUS_CONDUCTOR` person.

Flow:
1. Extract `email` and `password` from the request body.
2. Look up the person by email + `BUS_CONDUCTOR` role in the BPP DB.
3. Hash the incoming password with `getDbHash` and compare to the stored `passwordHash`.
4. On success, create a session token directly (no OTP round-trip, same as `GIMS_EMAIL_PASSWORD`).
5. Return `AuthRes { token = Just ..., person = Just ... }` so the collection can store
   `conductor_token` and `conductor_person_id` for subsequent steps.

**Why no OTP?**
The conductor already proves identity via password. Sending an OTP to a bus conductor
mid-shift is impractical. `GIMS_EMAIL_PASSWORD` follows the same OTP-free pattern.

**`authWithOtp` function — guard:**
Added `SP.CONDUCTORTOKEN -> throwError $ InvalidRequest "CONDUCTORTOKEN does not use OTP auth"`
to prevent the OTP path from being reached accidentally.

**`makePerson` function — enum coverage:**
Added `SP.CONDUCTORTOKEN` case (treated like `GIMS_EMAIL_PASSWORD` — uses email field,
no mobile number). Required to satisfy GHC's exhaustive pattern match check (`-Werror`).

### 2c — DriverProfile: block unsupported operations

**File:** `Backend/app/provider-platform/dynamic-offer-driver-app/Main/src/Domain/Action/UI/DriverProfile.hs`

Two pattern-match sites (`triggerUpdateAuthOTP` and `verifyUpdateAuthOTP`) updated to
reject `SP.CONDUCTORTOKEN` with a clear error message. Conductors authenticate via
password, not a driver-style identifier update flow.

### 2d — SharedLogic/OTP: block OTP send

**File:** `Backend/app/provider-platform/dynamic-offer-driver-app/Main/src/SharedLogic/OTP.hs`

`sendOTPByIdentifierType` updated: `Person.CONDUCTORTOKEN` throws "OTP not applicable"
— same guard already present for `GIMS_EMAIL_PASSWORD`.

---

## Files Changed

| File | Change |
|------|--------|
| `Backend/dev/test-tool/dashboard/src/services/postman-parser.ts` | Add `baseUrl_driver` → `driver` to `URL_VAR_TO_SERVICE` |
| `Backend/app/provider-platform/dynamic-offer-driver-app/Main/spec/Storage/Person.yaml` | Add `CONDUCTORTOKEN` to `IdentifierType` enum |
| `Backend/app/provider-platform/dynamic-offer-driver-app/Main/src-read-only/Domain/Types/Person.hs` | Auto-regenerated by `, run-generator` — `CONDUCTORTOKEN` added to `IdentifierType` |
| `Backend/app/provider-platform/dynamic-offer-driver-app/Main/src/Domain/Action/UI/Registration.hs` | Add CONDUCTORTOKEN cases in `auth`, `authWithOtp`, `makePerson` |
| `Backend/app/provider-platform/dynamic-offer-driver-app/Main/src/Domain/Action/UI/DriverProfile.hs` | Add CONDUCTORTOKEN guard in `triggerUpdateAuthOTP` and `verifyUpdateAuthOTP` |
| `Backend/app/provider-platform/dynamic-offer-driver-app/Main/src/SharedLogic/OTP.hs` | Add CONDUCTORTOKEN guard in `sendOTPByIdentifierType` |
| `Backend/dev/local-testing-data/dynamic-offer-driver-app.sql` | Add `BUS_CONDUCTOR` person seed for Chennai FRFS conductor test |

---

## Conductor DB Seed Details

The integration test needs a `BUS_CONDUCTOR` person pre-seeded in the DB.

| Field | Value |
|-------|-------|
| `id` | `md5('frfs-chennai-conductor-1')` → deterministic UUID |
| `email` | `hash_conductor1@example.com` |
| `password` (plaintext) | `pass_hash_456` |
| `password_hash` | `sha256(SALT \|\| "pass_hash_456")` = `59d706...af268` |
| `SALT` | BPP `encHashSalt` from `dhall-configs/dev/secrets/dynamic-offer-driver-app.dhall` |
| `role` | `BUS_CONDUCTOR` |
| `identifier_type` | `CONDUCTORTOKEN` |
| `merchant_id` | `7f7896dd-787e-4a0b-8675-e9e6fe93bb8f` (NAMMA_YATRI_PARTNER) |
| `merchant_operating_city_id` | `f8e9db0a-96c8-49e4-942a-3e3f7265d2da` (Chennai) |

The seed is idempotent (`ON CONFLICT (id) DO NOTHING`).
Applied live to `atlas_dev` DB via `local-testing-data/dynamic-offer-driver-app.sql`.

---

## Fix 3 — Backend: "Operating City not found" on `currentOperation`

**Root cause (from `dynamic-offer-driver-app-exe.log`):**

After auth succeeds, the `POST /ui/frfs/fleetOperator/currentOperation` call throws
`E400 INVALID_REQUEST: Operating City not found`.

The driver log shows:

```
-- Auth UPDATE:
UPDATE atlas_driver_offer_bpp.person
  SET merchant_operating_city_id = '4a95be1d-9052-4715-8cf5-ea8f68ffc85a' ...

-- currentOperation SELECT (returns 0 rows):
SELECT ... FROM atlas_driver_offer_bpp.merchant_operating_city
  WHERE id = '4a95be1d-9052-4715-8cf5-ea8f68ffc85a'
  Executed in: Nothing seconds
```

The MOC id `4a95be1d` comes from the **Redis merchant+city cache**
(`CachedQueries:MerchantOperatingCity:MerchantId-7f7896dd:City-Chennai`).
This cache was set when an older `prod_international` config-sync was applied.
After the current `master` config-sync ran (DELETE all + INSERT), the DB only
has Chennai MOC id `f8e9db0a`, but Redis was never flushed.

So auth (via `CQMOC.findByMerchantIdAndCity` → Redis) obtains `4a95be1d`, embeds
it in the session token, then `currentOperation`'s `findFirstIbppConfigByCityAndVehicle`
calls `QMOC.findById 4a95be1d` (direct DB — no Redis), which returns nothing.

**Fix — two-part:**

### 3a — Populate id-keyed cache from merchant+city cache hit

**File:** `Backend/app/provider-platform/dynamic-offer-driver-app/Main/src/Storage/CachedQueries/Merchant/MerchantOperatingCity.hs`

In `findByMerchantIdAndCity`, when a cached MOC is returned from Redis, the
result is now ALSO written to the id-keyed cache key
(`CachedQueries:MerchantOperatingCity:MerchantOpCityId-{id}`).
Previously only `cachedMerchantIdAndCity` (merchant+city key) was set; the
id-based key was left empty, so a subsequent `findById` call always missed.

```haskell
-- Before
Just a -> return a

-- After
Just a -> whenJust a cacheMerchantOpCityById >> return a
```

Same change applied to the DB-fetch branch (Nothing case).

### 3b — Use cached `findById` in `findFirstIbppConfigByCityAndVehicle`

**File:** `Backend/app/provider-platform/dynamic-offer-driver-app/Main/src/SharedLogic/IntegratedBPPConfig.hs`

Replaced `QMOC.findById` (direct DB, no Redis) with `CQMOC.findById` (checks
id-keyed Redis cache first, then DB) in all three functions:
`findMaybeIntegratedBPPConfig`, `findFirstIbppConfigByCityAndVehicle`, and
`findAllIntegratedBPPConfig`.

With fix 3a in place, by the time `currentOperation` runs, auth has already
populated the id-keyed Redis entry for `4a95be1d`. `CQMOC.findById 4a95be1d`
finds it there without touching the DB, and the flow proceeds with
`city = "Chennai"` → IntegratedBPPConfig lookup succeeds.

**Why this is self-healing:**
Once the Redis merchant+city cache expires and is refreshed from the DB, it
picks up the new id `f8e9db0a`. Fix 3a then caches that under the id key too,
and the system is consistent again. No manual Redis flush needed.

### Files changed

| File | Change |
|------|--------|
| `Backend/app/provider-platform/dynamic-offer-driver-app/Main/src/Storage/CachedQueries/Merchant/MerchantOperatingCity.hs` | `findByMerchantIdAndCity` also calls `cacheMerchantOpCityById` on cache hit |
| `Backend/app/provider-platform/dynamic-offer-driver-app/Main/src/SharedLogic/IntegratedBPPConfig.hs` | Replace `QMOC.findById` with `CQMOC.findById`; add `CQMOC` import, remove `QMOC` import |

---

## Fix 4 — Missing `integrated_bpp_config` seed for Chennai BUS + missing GIMS mock

**Root cause:**

After Fix 3, `findFirstIbppConfigByCityAndVehicle` correctly resolves the MOC to
`city = "Chennai"`, then calls:

```haskell
QIBC.findByDomainAndCityAndVehicleCategoryAnyPlatform "FRFS" (Just "Chennai") (Just "BUS")
```

This queries `atlas_driver_offer_bpp.integrated_bpp_config` for a row with
`domain='FRFS'`, `city='Chennai'`, `vehicle_category='BUS'`. The table was empty
(no config-sync file exists for the driver app's `integrated_bpp_config`) → error:
`INTEGRATED_BPP_CONFIG_NOT_FOUND` (404).

Once that row is present, the handler calls the GIMS API:
```
POST {config.baseUrl}/internal/fleet-operator/{config.feedKey}/currentOperation
POST {config.baseUrl}/internal/fleet-operator/{config.feedKey}/currentTripDetails
```

The mock server at `http://localhost:8080` had no handler for
`/internal/fleet-operator/…` paths — requests fell through to the default
`{"status": "ok"}` response which could not be deserialized as
`GimsCurrentOperationResp` / `GimsCurrentTripDetailsResp` → connection error.

### 4a — Seed `integrated_bpp_config` for Chennai BUS

**File:** `Backend/dev/local-testing-data/dynamic-offer-driver-app.sql`

Added an `ON CONFLICT … DO NOTHING` INSERT into
`atlas_driver_offer_bpp.integrated_bpp_config`:

| Column | Value |
|--------|-------|
| `id` | `md5('frfs-chennai-bus-ibpp-config')::uuid` (deterministic) |
| `agency_key` / `feed_key` | `'chennai_bus'` — used as `gtfsId` in GIMS URL path |
| `domain` | `'FRFS'` |
| `city` | `'Chennai'` |
| `vehicle_category` | `'BUS'` |
| `platform_type` | `'APPLICATION'` |
| `merchant_id` | `'7f7896dd-…'` (NAMMA_YATRI_PARTNER BPP merchant) |
| `merchant_operating_city_id` | `'f8e9db0a-…'` (Chennai, master config-sync id) |
| `config_json` | `{"tag":"DIRECT","contents":{"baseUrl":"http://localhost:8080","cipherKey":"DUMMYCIPHERKEY"}}` |

The `providerConfig = DIRECT` with `baseUrl = http://localhost:8080` points to
the local mock server. The `cipherKey` is a dummy string (it is only used for
QR-code encryption, not in the conductor flow).

### 4b — GIMS mock handler in mock server

**New file:** `Backend/dev/mock-servers/services/gims.py`
**Modified:** `Backend/dev/mock-servers/server.py`

Created `gims.py` that returns fixed stub responses for the GIMS
fleet-operator endpoints. Added the route `("/fleet-operator", gims, "gims")`
to `server.py`'s `ROUTES` list so paths matching `/fleet-operator` are handled.

Mock responses:

| Endpoint | Response |
|----------|----------|
| `currentOperation` | `{"waybill_no": "WAYBILL001", "number_of_trips": 5}` |
| `currentTripDetails` | Full trip details with `current.trip_number=1, route_id="10"` |
| `tripAction` | `{"status": "ok"}` |
| `employee/login` | `{"verified": true, "token": "gims-mock-token-001"}` |
| `verify` | `{"verified": true}` |

### Files changed

| File | Change |
|------|--------|
| `Backend/dev/local-testing-data/dynamic-offer-driver-app.sql` | Add Chennai BUS `integrated_bpp_config` seed row |
| `Backend/dev/mock-servers/services/gims.py` | New file: GIMS fleet-operator + Nandi GTFS mock handler |
| `Backend/dev/mock-servers/server.py` | Import `gims`; add `/route-stop-mapping`, `/example-trip`, `/route/`, `/fleet-operator` routes |

---

## Fix 5 — "Get Route Details" returns 400 — Nandi GTFS endpoints not mocked

**Root cause:**

`getV2FrfsRoute` (called by the "Get Route Details" step) resolves the
`IntegratedBPPConfig` the same way as `currentOperation` — `DIRECT` with
`baseUrl = http://localhost:8080`. It then calls three Nandi GTFS READ
endpoints on that same base URL:

```
GET http://localhost:8080/route/{gtfs_id}/{route_id}             → RouteInfoNandi
GET http://localhost:8080/route-stop-mapping/{gtfs_id}/route/{route_code} → [RouteStopMappingInMemoryServer]
GET http://localhost:8080/example-trip/{gtfs_id}/{route_id}      → TripDetails
```

These paths (`/route/`, `/route-stop-mapping/`, `/example-trip/`) did not
match any entry in the mock server's ROUTES table, so they fell through to
the default `{"status": "ok", "mock": true}` response. That response cannot
be parsed as the expected Haskell types → `getRouteByRouteId` returned
`Nothing` → `fromMaybeM` threw **"Route not found: 10"** (400).

**Fix — extend `gims.py` and `server.py` with Nandi GTFS handlers:**

Added three Nandi GET handlers to `gims.py` with fixed stub data:

| Endpoint | Response |
|----------|----------|
| `GET /route/{gtfs_id}/{route_id}` | `RouteInfoNandi` with id=`{route_id}`, mode=BUS, 3 stops |
| `GET /route-stop-mapping/{gtfs_id}/route/{route_code}` | 3 `RouteStopMappingInMemoryServer` entries |
| `GET /example-trip/{gtfs_id}/{route_id}` | `TripDetails` with 3 `TripStopDetail` entries |

All three responses use the same 3 stop codes (`S001`, `S002`, `S003`) so
the `getV2FrfsRoute` handler can successfully join route-stop-mapping entries
with example-trip entries when building the `FRFSStationAPI` list.

Added three new routes to `server.py` ROUTES (ordered before `/route/` to
prevent substring conflicts):
```python
("/route-stop-mapping", gims, "gims"),
("/example-trip",       gims, "gims"),
("/route/",             gims, "gims"),
```

### Files changed

| File | Change |
|------|--------|
| `Backend/dev/mock-servers/services/gims.py` | Added `_route_by_id`, `_route_stop_mapping`, `_example_trip` handlers + shared `_STOPS` constant |
| `Backend/dev/mock-servers/server.py` | Added `/route-stop-mapping`, `/example-trip`, `/route/` routes pointing to `gims` |

> **Operational note — InMemCache TTL:**
> `OTPRest.getRouteByRouteId` wraps its result in `IM.withInMemCache` with a
> **43 200-second (12-hour) TTL**. If the mock server was running without the
> Nandi routes when the first `/ui/v2/frfs/route/10` request came in, the
> `Nothing` (route-not-found) result gets cached in-process. All subsequent
> calls for the same route return `Nothing` immediately (< 10 ms) without
> hitting the mock at all.
> **Fix:** restart the driver app after fixing the mock server — the in-memory
> cache is process-local and is cleared on restart.

---

## Summary — all fixes at a glance

| # | Symptom | Root cause | Fix |
|---|---------|------------|-----|
| 1 | `Failed to construct URL` (status 0) | `baseUrl_driver` missing from `URL_VAR_TO_SERVICE` | Add entry in `postman-parser.ts` |
| 2 | `parsing IdentifierType failed … "CONDUCTORTOKEN"` | Enum value + auth handler missing | Add `CONDUCTORTOKEN` to `Person.yaml`; implement password-auth in `Registration.hs`; guard OTP paths |
| 3 | `Operating City not found` | Stale Redis MOC cache held old `4a95be1d` id; `findFirstIbppConfigByCityAndVehicle` used direct-DB `findById` (bypassed Redis) | Populate id-keyed cache on city-cache hit; switch to `CQMOC.findById` |
| 4 | `INTEGRATED_BPP_CONFIG_NOT_FOUND` | No `integrated_bpp_config` row for Chennai + BUS; no GIMS mock routes | Seed DB row (DIRECT, `http://localhost:8080`); create `gims.py` mock handler |
| 5 | `Route not found: 10` (400) | Nandi GTFS GET paths (`/route/`, `/route-stop-mapping/`, `/example-trip/`) not handled by mock | Extend `gims.py`; add three routes to `server.py` |
