# Driver Info by Phone (multi-city search) — Design

Date: 2026-07-17
Status: Approved for planning

## Problem

`GET /api/bpp/driver-offer/:merchantId/driver/info` returns "Driver not found" for any
driver registered outside a single hardcoded city.

The cause is not a failed lookup — it is a hardcoded mapping. In
`Backend/app/dashboard/provider-dashboard/src/API/ProviderPlatform/DynamicOfferDriver.hs:64-80`,
the deprecated V1 route derives the city from the merchant short ID by `case`:

```haskell
getCity = \case
  "NAMMA_YATRI_PARTNER" -> City.City "Bangalore"
  "YATRI_PARTNER"       -> City.City "Kochi"
  "JATRI_SAATHI_PARTNER" -> City.City "Kolkata"
  _                     -> City.City "AnyCity"
```

That scalar city is threaded end-to-end: path -> `merchantCityAccessCheck` ->
`callRideBookingAPI` -> `CQMOC.findByMerchantIdAndCity`. In the driver-app
(`Domain/Action/Dashboard/RideBooking/Driver.hs:352`) the operating city is pinned
*before* any search happens:

```haskell
merchantOpCity <- CQMOC.findByMerchantIdAndCity merchant.id opCity >>= fromMaybeM (MerchantOperatingCityNotFound ...)
```

Every lookup branch is then scoped to that one `merchantOpCity`. A driver in another
city is never visible, so the phone branch returns
`PersonDoesNotExist (mobileCountryCode <> mobileNumber)`.

A V2 route already exists and works correctly when the city is known:
`/api/bpp/driver-offer/{merchantId}/{city}/driver/info`. The genuine gap is only:
**the caller knows the merchant and the phone number, but not the city.**

## Goal

A new endpoint taking merchant + driver phone number, which searches the caller's
accessible cities and returns a response byte-for-byte identical to `/driver/info`.

## Non-goals

- Changing `dynamic-offer-driver-app`. This is a dashboard-only change.
- Changing the behaviour of the existing `/driver/info` endpoint.
- Cross-city search by `vehicleNumber`, `dlNumber`, `rcNumber`, `email`, `personId`,
  or `walletId`. Phone-only, by decision.
- Granting fleet owners any new capability.

## Decisions and rationale

### Dashboard-layer loop, not a driver-app query

A single query would be strictly faster. `Storage/Queries/PersonExtra.hs:403`
`findByMobileNumberAndMerchantAndRoles :: Text -> DbHash -> Id Merchant -> [Role] -> m (Maybe Person)`
is scoped to merchant but **not** to city, and `Person` carries `merchantOperatingCityId`.
That resolves the driver's city in one indexed lookup — O(1) rather than O(cities).

We are nonetheless looping in the dashboard, deliberately: it requires zero changes to
`dynamic-offer-driver-app`, so there is no cross-service deployment coupling and no risk
to the existing endpoint. The O(1) query is recorded here as the known upgrade path if
call volume ever justifies it.

### Authorization: caller's granted cities only

`merchantCityAccessCheck` (`Backend/app/dashboard/Lib/src/Tools/Auth/Merchant.hs:27-30`)
is a strict equality check against the **token's** city, which is fixed at login:

```haskell
unless (userMerchantId == merchantId && userCity == city) $ throwError AccessDenied
```

A loop therefore cannot call it per city — every non-token city would be `AccessDenied`.
The loop must use `skipMerchantCityAccessCheck`, which bypasses the check entirely.

Bypassing it over `merchant.supportedOperatingCities` would be a privilege escalation: a
Bangalore-only user could read driver PII in Chennai. Instead the search is restricted to
the caller's own `MerchantAccess` rows for this merchant. This exposes a strict subset of
what the caller could already obtain by logging into each granted city in turn — no new
data becomes visible; the caller merely stops needing to know the city.

This mirrors the established pattern in `API/Fleet/Registration.hs:75`, which validates
the city explicitly before calling `skipMerchantCityAccessCheck`.

### Reuse the `GET_DRIVER_INFO` userActionType

Avoids a new action-type enum and its role-access seeding migration. Justified by the
authorization scoping above: the endpoint grants no access the caller lacks.

## Design

### Route

Hand-written Servant module, mounted into `API'` in `DynamicOfferDriver.hs`. Because
`API'` is shared by `handler` (V1, no city) and `handlerV2` (city in path), the route
appears on both. The handler **ignores** the `city` argument it receives, so the V1 URL
works unchanged:

```
GET /api/bpp/driver-offer/NAMMA_YATRI_PARTNER/driver/infoByPhone
      ?mobileNumber=9876543210&mobileCountryCode=%2B91
```

`infoByPhone` is camelCase per CLAUDE.md. A sibling of `info` rather than `info/byPhone`,
to avoid any ambiguity with the `/info/{fleetOwnerId}/{mbFleet}` helper capture.

Query params `mobileNumber` and `mobileCountryCode` match `/driver/info` exactly, making
this a drop-in. Response type is `Common.DriverInfoRes`, returned verbatim from the
existing per-city client call — consumers need no changes. It already carries
`merchantOperatingCity`, so callers learn which city matched.

### Handler flow

```haskell
getDriverInfoByPhone ::
  ShortId DM.Merchant ->
  City.City ->          -- ignored: this endpoint searches all accessible cities
  ApiTokenInfo ->
  Maybe Text ->         -- mobileNumber
  Maybe Text ->         -- mobileCountryCode
  Flow Common.DriverInfoRes
```

1. Require `mobileNumber`; `InvalidRequest` if absent.
2. Derive `mbFleet` from the caller's role (shared helper, extracted from `getDriverInfo`).
   **Reject fleet owners up front** with the driver-app's existing message
   (`"Fleet Owner can only search with vehicle Number, personId or walletId"`). Fleet
   owners are forbidden phone lookup today; failing fast preserves that behaviour and
   avoids N pointless HTTP hops that would each fail identically.
3. Build the city list from the caller's grants, token city first:

```haskell
merchantAccesses <- QMerchantAccess.findAllMerchantAccessByPersonId apiTokenInfo.personId
let cities = ordNub [ma.operatingCity | ma <- merchantAccesses, ma.merchantId == apiTokenInfo.merchant.id]
    -- token city first: the overwhelmingly common hit, so most calls cost one hop
    orderedCities = sortOn (/= apiTokenInfo.city) cities
```

4. Search with short-circuit, then reuse the driver-app's own not-found error so the
   response matches `/driver/info` exactly:

```haskell
firstJustM (tryCity checkedMerchantId mobileNumber mbMobileCountryCode) orderedCities
  >>= fromMaybeM (PersonDoesNotExist $ mobileCountryCode <> mobileNumber)
```

`firstJustM` (`Control.Monad.Extra`) is lazy and stops at the first hit: O(1) hops in the
common case, O(cities) worst case. If it is unavailable in this dependency set, define
the three-line equivalent locally.

### `tryCity` — the correctness crux

```haskell
tryCity :: CheckedShortId DM.Merchant -> Text -> Maybe Text -> City.City -> Flow (Maybe Common.DriverInfoRes)
```

Calls `Client.callRideBookingAPI checkedMerchantId city (.driverDSL.getDriverInfo) ...`
and converts a *miss* to `Nothing`.

**It must match narrowly.** Catching broadly (`try @SomeException`) would turn a Chennai
outage or a 5xx into a silent "Driver not found" — a misleading bug that hides real
failures. Only these are misses:

- `PersonDoesNotExist` — driver genuinely absent in that city.
- `MerchantOperatingCityNotFound` — merchant does not operate there; legitimately empty.

Everything else (network, 5xx, auth) must propagate. Log each miss at `logInfo` with the
city, and log the matched city on success.

**Open implementation question, to resolve first:** how the driver-app's error surfaces
through `callServerAPI` on the wire — whether the domain error type survives or arrives
as a decoded HTTP error. The matcher must be written against the observed reality, not
assumed. This is step one of implementation.

### Refactor

Extract from the existing `getDriverInfo` into shared helpers, so both endpoints use one
copy and no business logic is duplicated:

- role -> `mbFleet` derivation (currently `QP.findById` + `QRole.findById` inline).
- the per-city client call, which becomes `tryCity`.

`getDriverInfo`'s behaviour must not change.

## Files affected

| File | Change |
|---|---|
| `Backend/app/dashboard/provider-dashboard/src/API/ProviderPlatform/DynamicOfferDriver.hs` | Mount new module into `API'` |
| `Backend/app/dashboard/provider-dashboard/src/API/ProviderPlatform/DriverInfoByPhone.hs` (new) | Hand-written route + auth, modelled on `API/Fleet/Registration.hs` |
| `Backend/app/dashboard/provider-dashboard/src/Domain/Action/ProviderPlatform/RideBooking/Driver.hs` | Add `getDriverInfoByPhone`; extract helpers |
| `Backend/app/dashboard/provider-dashboard/provider-dashboard.cabal` | Register new module (via `, hpack`) |

No `src-read-only/` files and no `dynamic-offer-driver-app` files are touched.

## Testing

Unit — the pure/loop logic:
- City ordering places the token's city first.
- Only the caller's granted cities for this merchant are searched; another merchant's
  grants are excluded.
- Short-circuit: given a hit in city 2 of 4, exactly 2 calls are made.
- Not-found in all cities yields the same `PersonDoesNotExist` error as `/driver/info`.
- A non-miss error (5xx) propagates and does **not** become "Driver not found".
- Fleet-owner caller is rejected before any HTTP call is made.
- Missing `mobileNumber` yields `InvalidRequest`.

Integration:
- Driver in the token city — found in one hop.
- Driver in a granted non-token city — found; `merchantOperatingCity` reports that city.
- Driver in a city the caller lacks grants for — not found (authorization holds).
- Response payload is field-for-field identical to `/driver/info` for the same driver.

## Performance

Sequential is correct here. Token-city-first makes the typical call a single hop.
Parallel fan-out would issue N calls on *every* request to save latency only in the rare
cross-city miss, multiplying driver-app load for the common case. If cross-city misses
ever dominate, the right fix is not parallelism but the single-query approach above,
which removes the loop entirely.
