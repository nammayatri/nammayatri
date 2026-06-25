# Driver Map-Provider Preference (Backend)

**Date:** 2026-06-16
**Service:** dynamic-offer-driver-app (BPP)
**Scope:** Backend only. Frontend (driver app UI) lives in a separate repo and is out of scope.

## Problem

On the driver app, after a ride is assigned / started, a "Maps" button opens Google
Maps with the destination pre-filled and starts navigation. We want drivers to be able
to prefer a different navigation app (Waze, Apple Maps). When a preference is set, the
maps button should open the preferred app instead of Google Maps.

The frontend already supports configurable navigation apps via `navigationAppConfig`
(query + package name per platform). What it lacks is knowledge of the driver's chosen
provider. The backend's job is therefore narrow:

1. **Store** the driver's preferred map provider.
2. **Expose** it: settable via the existing profile-update API, and returned in the
   driver profile response so the frontend can pick the right navigation app.

No navigation URL/intent building happens on the backend.

## Design

### 1. Storage — new enum + field on `DriverInformation`

File: `app/provider-platform/dynamic-offer-driver-app/Main/spec/Storage/DriverInformation.yaml`

Add an enum under `types:`:

```yaml
MapProvider:
  enum: "GOOGLE_MAPS, WAZE, APPLE_MAPS"
  derive: "HttpInstance"
```

Base derivations (`Show, Eq, Ord, Read, Generic, ToJSON, FromJSON, ToSchema`) are added
by the generator automatically; `HttpInstance` adds the HTTP query/param instances,
matching the existing `DriverAutoPayStatus` enum.

Add a field under `fields:`:

```yaml
preferredMapProvider: Maybe MapProvider
```

Run `, run-generator` to regenerate:
- `src-read-only/Domain/Types/DriverInformation.hs` — adds `preferredMapProvider :: Maybe MapProvider` and the `MapProvider` data type.
- `src-read-only/Storage/Beam/DriverInformation.hs` — adds the Beam column.
- A SQL migration under `dev/migrations/dynamic-offer-driver-app/` adding the nullable column.

### 2. API — extend the existing profile endpoint

Reuse `POST /ui/driver/profile` (`UpdateDriverReq` → `updateDriver` → `DriverInformationRes`).
No new endpoint, auth, or frontend round-trip.

File: `app/provider-platform/dynamic-offer-driver-app/Main/src/Domain/Action/UI/Driver.hs`

- `UpdateDriverReq`: add `preferredMapProvider :: Maybe DriverInfo.MapProvider`.
- `DriverInformationRes` (aka `UpdateDriverRes`, also the GET profile response): add
  `preferredMapProvider :: Maybe DriverInfo.MapProvider`.
- `updateDriver` handler: merge using the same `<|>` precedence pattern as the other
  optional preferences:
  ```haskell
  preferredMapProvider = req.preferredMapProvider <|> driverInfo.preferredMapProvider
  ```
  and pass it into persistence.
- `makeDriverInformationRes`: set `preferredMapProvider = driverInfo.preferredMapProvider`.

### 3. Persistence

File: `app/provider-platform/dynamic-offer-driver-app/Main/src/Storage/Queries/DriverInformationExtra.hs`

The profile update is persisted through the hand-written `updateDriverInformation`
function (one KV update for all profile preferences). Thread one new parameter through it:

- Add a `Maybe DriverInfo.MapProvider` argument (and to its type signature).
- Add `Se.Set BeamDI.preferredMapProvider preferredMapProvider` to the `updateOneWithKV` set list.
- Update its single caller in `updateDriver`.

No `LTSSync` (driver-pool) change — this preference is not used in matching.

## Backward Compatibility (hard requirement)

- **DB column is nullable** (`Maybe MapProvider`); existing rows become `NULL`. The
  migration adds the column with no `NOT NULL` and no default. No backfill needed.
- **Request field is `Maybe`**: old driver-app clients that don't send
  `preferredMapProvider` produce `Nothing`; the `<|>` merge preserves the stored value
  (and a fresh row stays `Nothing`).
- **Response field is additive `Maybe`**: JSON gains one optional field. Old frontend
  versions ignore the unknown field; new frontend treats `Nothing`/absent as the
  existing default (Google Maps).
- **Enum is additive**: frontend ignores any value it doesn't support and falls back to
  Google Maps, so adding `APPLE_MAPS` (or future providers) never breaks older clients.
- **Internal signature change** to `updateDriverInformation` is compile-time only
  (single caller), not a wire-format change.
- **Default behavior unchanged**: with no preference set, navigation continues to open
  Google Maps exactly as today.

## Out of Scope

- Frontend changes (separate repo): reading `preferredMapProvider` from the profile
  response and choosing the navigation app/query.
- Navigation URL / Android intent construction.
- Dashboard / operator-facing controls.

## Verification

- `cd Backend && cabal build dynamic-offer-driver-app` — clean under `-Werror`
  (no unused imports/params).
- Confirm the generator produced a migration in
  `dev/migrations/dynamic-offer-driver-app/` adding `preferred_map_provider`.
- Sanity-check that `UpdateDriverReq`, `DriverInformationRes`, and the persistence call
  all reference the same field.

## Affected Files (summary)

| File | Change |
|------|--------|
| `spec/Storage/DriverInformation.yaml` | Add `MapProvider` enum + `preferredMapProvider` field |
| `src-read-only/Domain/Types/DriverInformation.hs` | Generated: enum + field |
| `src-read-only/Storage/Beam/DriverInformation.hs` | Generated: Beam column |
| `dev/migrations/dynamic-offer-driver-app/*.sql` | Generated: nullable column |
| `src/Domain/Action/UI/Driver.hs` | Add field to `UpdateDriverReq` + `DriverInformationRes`; merge in `updateDriver`; set in `makeDriverInformationRes` |
| `src/Storage/Queries/DriverInformationExtra.hs` | Thread new param into `updateDriverInformation` |
