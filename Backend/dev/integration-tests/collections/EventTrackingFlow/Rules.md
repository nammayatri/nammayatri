# EventTrackingFlow

E2E coverage for server-to-server event tracking with all three providers live at once:
MoEngage, CleverTap and Firebase Analytics (GA4 Measurement Protocol). Rider-app fires
`Tools.EventTracking` events from registration, search, quote retrieval and the ride
lifecycle; this suite proves that with every provider targeted the API flow is untouched,
and that the Firebase-specific identity contract works end to end.

Spec: `docs/superpowers/specs/2026-09-08-firebase-analytics-event-provider-design.md`.

## What the suite asserts

One collection, one random rider, one full auto ride (skeleton copied from
`RideBookingFlow/01-AutoRideFlow.json`), under seeded providers
`{Moengage,Clevertap,FirebaseAnalytics}`:

| Step | Event(s) fired server-side | Assertion |
|---|---|---|
| Rider OTP | `ny_user_onboarded` | 200 |
| Set Firebase App Instance Id (Profile) | none | 200 |
| Get Profile (Assert Firebase Id) | none | `firebaseAppInstanceId` equals the id just sent |
| Ride Search, Get Search Results | `ny_user_source_and_destination`, `ny_user_request_quotes` | 200, estimates present |
| Driver Accept Ride … End Ride | `driver_assigned`, `ny_rider_ride_completed`, `ny_auto_ride_completed`, `ny_user_first_ride_completed`, `ny_auto_first_ride_completed` | 200 at every step |
| Rotate Firebase App Instance Id (Profile) | none | 200 |
| Get Profile (Assert Rotated Firebase Id) | none | `firebaseAppInstanceId` equals the new id, not the first |

Every rider-side request carries `x-device: Postman/Pixel 6/ANDROID v13/Google`, as the
real app does. rider-app re-syncs the stored client device from request headers on most
authenticated calls (`Person.updatePersonVersions`), and a request without the header NULLs
it; the Firebase flow then skips on "no client platform". The platform is what selects the
Firebase app.

What this proves: the `POST /profile` and `GET /profile` contract for the id, update-on-change
semantics, that the `FirebaseAnalytics` enum and its `apps[]` config shape round-trip through
`merchant_service_usage_config.event_tracking_providers` and
`merchant_service_config.config_json`, and that provider failures never surface in the ride
flow.

What it cannot prove: that Google would accept the payload. Every provider's `baseUrl` points
at the local mock server, which has no handler for these paths, so each call fails fast and
rider-app logs it. The mock server records every hit for the test dashboard's **Mock calls**
panel, so after a run you can eyeball `/firebase/mp/collect` with `firebase_app_id` and
`api_secret` query params and the JSON body, but Newman cannot read that stream. Payload
correctness against Google is verified once per environment by setting `"debug": true` on a
staging row and confirming an empty `validationMessages` list in the logs.

## Seeding: three service-config rows plus the provider list

`setup-event-tracking.sql` inserts (or updates) `EventTracking_Moengage`,
`EventTracking_Clevertap` and `EventTracking_FirebaseAnalytics` rows in
`atlas_app.merchant_service_config` for every city, all `enabled`, all pointed at
`http://localhost:8080/<provider>`, and sets `event_tracking_providers` on every
`merchant_service_usage_config` row. Secrets are dev-key Passetto blobs borrowed from
`local-testing-data/provider-dashboard.sql`; their plaintext is irrelevant.

Applied to every city so the collection stays city-agnostic.

Three run paths:

1. **`./run-tests.sh event-tracking`** — applies the seed (aborting the run if it fails),
   flushes Redis, and restarts `rider-app-exe` if it is running so the in-process
   ConfigPilot cache cannot serve the pre-seed provider list.
2. **Test dashboard** — invokes newman directly and never runs the seed. Apply the SQL by
   hand first (or add `dimension_overrides` entries to `dev/config-sync/assets/patches.json`
   if the suite should survive config-sync imports).
3. **Raw newman** — apply the SQL and flush Redis manually first.

### The in-process (L1) cache

Both tables are served through ConfigPilot, which memoises each read **in process memory**
for up to an hour before Redis is consulted
(`lib/config-pilot/src/Lib/ConfigPilot/Interface/Getter.hs`). A rider-app that already
served a request for the city keeps answering with the pre-seed provider list; no SQL
update or Redis flush reaches it.

**Rule: seed before the stack starts, or restart rider-app after seeding.** This is the same
caveat `PhoneShareConsentFlow/Rules.md` documents for `transporter_config` and `rider_config`.

## What is deliberately NOT covered here

- **Delivery to a real provider, or the exact payload.** See above; there is no mock handler
  and no hits endpoint Newman can read.
- **The "no app instance id" and "no platform" skip paths.** They only produce log lines. A
  rider registered without the profile step exercises the first; a profile request without
  `x-device` exercises the second.
- **Per-event routing (`event_tracking_overrides`).** The seed sets it to `NULL`, so every
  event goes to every provider. Routing is covered by the existing dispatcher logic and is
  provider-agnostic.
- **The `debug: true` validation path.** It needs a real `api_secret` and outbound access to
  Google.

## Conventions

- Rider/driver numbers, vehicle registration and both Firebase ids are random per run
  (collection prerequest, `_test_*` collection variables) — safe to run concurrently and
  repeatedly.
- The ride skeleton is generated from `RideBookingFlow/01-AutoRideFlow.json` by inserting the
  profile steps after `Rider OTP` and after `End Ride` and dropping the invoice check. If
  AutoRideFlow changes materially (auth, allocator timing), regenerate this collection the
  same way.
