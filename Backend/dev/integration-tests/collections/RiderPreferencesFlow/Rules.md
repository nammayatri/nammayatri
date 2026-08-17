# RiderPreferencesFlow

E2E coverage for the rider's durable ride-configuration toggles — Business, Pet-friendly,
Public transport and Driver Selection — stored server-side as a `RIDE_CONFIG` row in
`atlas_app.rider_preferences`.

Design: `consumer/docs/ride-preferences-backend-migration.md` (ny-react-native repo).

## Why this suite exists

`rider_preferences` is a polymorphic table: a `preference_type` discriminator plus a
`preference_data` JSONB payload. It already held `LOCATION_PICKUP` rows (saved pickup points);
`RIDE_CONFIG` is a second type sharing the same table and the same `POST /riderPreference`
endpoint.

That sharing is the risk this suite is built around. The JSONB decoder throws a hard
`InternalError` on any payload shape it cannot parse, and that decode sits on the *pickup-location*
read path. A regression in the ride-config work can therefore break a previously shipped feature.
Steps 11-14 exist specifically to catch that.

## What the suite asserts

One collection, one random rider, no driver or BPP setup required.

| Step | Asserts |
|------|---------|
| Get Ride Config (Never Saved) | All four fields come back `null` — a rider with no row is not an error |
| Save + Get (After Save) | All four toggles round-trip exactly |
| Save (Partial Payload) + Get | Omitted fields keep their stored value — the save merges, it does not replace |
| Partial `false` + Get | A partial payload carrying `false` is applied, not mistaken for absent |
| Update + Get (After Update) | A second save **overwrites** — the API never returns stale values |
| Save Without Data (Negative) | `RIDE_CONFIG` with no `rideConfigData` is rejected 4xx, not silently accepted |
| Save Pickup Location | A `LOCATION_PICKUP` row can be created while a `RIDE_CONFIG` row exists |
| Get Ride Config (Pickup Row Present) | Ride config still readable once both row types exist |
| Get Pickup Location | The geohash lookup returns the pickup and is not confused by the ride-config row |
| Get All Preferences | `/riderPreference/all` returns both types in their own fields — the ride-config row is under `rideConfig`, never mixed into `locationPickups` |

### The POST merges — omitted fields keep their stored value

A save updates only the fields present in `rideConfigData`. Anything omitted retains whatever the
row already held; it is **not** reset. Steps 6-7 pin this: saving `{"isPetRide": true}` over a row
holding all four values must leave `isBusiness`, `isTransitEnabled` and `isAutoAssign` untouched.

An earlier implementation replaced `preferenceData` wholesale, which nulled every omitted field.
That was a bug — caught by this suite on 2026-08-11 — and merge is the intended contract.

The `false` case has its own steps because it is the likeliest regression: an implementation using
`fromMaybe False` (or otherwise conflating `Nothing` with `false`) passes every all-`true` merge
assertion and then silently drops a partial `false`. Steps 8-9 send `{"isAutoAssign": false}` over a
row where it is `true` and require the flip to stick while the other fields survive.

**Known limitation:** `null` and *omitted* are indistinguishable on the wire (both decode to
`Nothing`), so a field cannot be explicitly reset to "never chose" once set. That is fine for the
product — `null` means the rider has expressed no choice, and riders only ever move to `true` or
`false` — but expressing "clear this field" would need a different wire type
(`Maybe (Maybe Bool)`), not just a different value.

### The partial-payload step is load-bearing

`RideConfigData` declares every field as `Maybe Bool`, permanently. Generic Aeson decoding fails on
an absent key for a non-`Maybe` field, and `fromTType'` converts that failure into a hard
`InternalError` — so a fifth toggle added later would break decoding of every row written before it.
"Omitted stays null" is the observable consequence of that rule; if this step starts failing because
omitted fields come back `false`, the `Maybe` guarantee has been lost.

## No seeding required

Unlike `PhoneShareConsentFlow`, this suite needs no SQL seed and no Redis flush. The endpoints read
and write `rider_preferences` directly with no ConfigPilot-served config in the path, so there is no
in-process cache staleness window and no service restart requirement.

## Running

```
./run-tests.sh rider-prefs                  # all cities
./run-tests.sh rider-prefs NY_Bangalore     # one city
```

## Conventions

- The rider mobile number is random per run (collection prerequest, `_test_rider_number`), so the
  suite is safe to run concurrently and repeatedly. Each run gets a fresh rider with no rows, which
  is what makes the "Never Saved" assertion reliable.
- Coordinates come from `origin_lat` / `origin_lon` so the collection stays city-agnostic. They are
  interpolated **unquoted** in request bodies — the API expects `Double`, and a quoted `"12.9352"`
  fails deserialization.

## Not covered here

- **Per-city config authority.** Which toggles are available in a city, and their default values,
  come from Firebase Remote Config on the client — the backend stores what the rider chose and has
  no view of that config. Client-side normalization is covered by the React Native unit tests.
- **The `remember` flag.** It gates whether the client persists at all and never reaches the server.
- **Concurrent double-save.** The partial unique index on `(rider_id) WHERE preference_type =
  'RIDE_CONFIG'` guarantees one row per rider, but Newman runs steps sequentially so the race is not
  exercised. The upsert path is covered; the constraint itself is not.
