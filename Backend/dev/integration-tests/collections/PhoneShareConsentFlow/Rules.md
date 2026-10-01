# PhoneShareConsentFlow

E2E coverage for the rider phone-sharing consent gate: the driver app **dials** the
rider's real mobile number (`callingNumber`) only when the merchant's
`driver_calling_option` allows direct calling **and** the rider consented
(`SafetySettings.consentToShareMobileNumber`, pushed to the BPP through its
internal consent API whenever the rider changes it while
`rider_config.push_consent_to_bpp` is on, and also sent in the confirm tag
`CONSENT_TO_SHARE_MOBILE_NUMBER`). The legacy
`riderMobileNumber` field is gated by the merchant option alone.

`rider_config.enable_share_number_with_driver` (BAP) controls whether the rider
app shows the consent toggle and auto opt-in, and whether confirm carries the
consent tag. The BPP writes the tag's value only when the tag is present.

## What the suite asserts

One collection, three rides by the same (random) rider, under seeded `DirectCall`.
The rider's consent reaches the BPP (`RiderDetails.consentToShareMobileNumber`)
through the internal consent API and the confirm tag, and is snapshotted on the
booking at confirm (`Booking.numberShareConsent`).

Calling rule: `callingNumber` is `DIRECT` only when `forceDirectCalling` is on, or
the merchant option allows direct calling AND the booking snapshot is `true` AND
the rider's live consent is still `true`. Anything else is `ANONYMOUS`.
`riderMobileNumber` is unchanged: present whenever the option allows direct calling.

| Step | Rider consent state | snapshot | live | `riderMobileNumber` | `callingNumber.numberType` |
|------|---------------------|----------|------|---------------------|----------------------------|
| Ride 1 | never set | `false` (the confirm tag sends `false` for an unset consent) | `false` | the real number | `ANONYMOUS` (number = `exoPhone`, `countryCode` null) |
| Ride 1, after mid-ride grant | granted during the ride | `false` | `true` | the real number | `ANONYMOUS` (grant does not unmask the current ride) |
| Ride 2 | granted before booking | `true` | `true` | the real number | `DIRECT` (bare real number, rider's country code) |
| Ride 2, after mid-ride revoke | revoked during the ride | `true` | `false` | the real number | `ANONYMOUS` (number = `exoPhone`, immediately) |
| Ride 3 | revoked before booking | `false` | `false` | the real number | `ANONYMOUS` |

`callingNumber.number` is always bare, the same format as `riderMobileNumber` and
`exoPhone`. The client applies its own local dialling prefix.

`riderMobileNumber` predates the consent feature and keeps its original
behaviour, so already-released driver app builds are unaffected. Consent gates
only `callingNumber`, the field new builds dial.

The two mid-ride steps (`Grant Consent Mid-Ride (Ride 1)`,
`Revoke Consent Mid-Ride (Ride 2)`) each follow with a `Get Ride After ...` fetch of
the still-active ride. `forceDirectCalling` cities are not covered in-collection
(ConfigPilot in-memory cache, see below).

Between rides it also asserts the rider API's tri-state directly via
`GET /profile/getEmergencySettings`: `null` (never asked) → `true` → explicit
`false` — `null` and `false` are deliberately distinct states.

Ride 2 vs ride 3 additionally exercises the BPP's repeat-rider path: the
`RiderDetails` row created during ride 1 is flipped to `true` then back to
`false` by the pushes (the next confirm's tag carries the same value), and each confirm
snapshots the row's current value onto the booking, proving "consent applies
from the next ride".

Two steps call the BPP internal API directly (`POST /internal/{merchantId}/riderDetails/consent`,
mounted at the driver-app root, not under `/ui`; env var `baseURL_namma_P_root`,
`token` header = env var `bpp_internal_api_key`, `bapId` = env var `bap_id`):

- `Internal Consent Rejects Bad Token`: a wrong token returns HTTP 400 with `errorCode` `AUTH_BLOCKED`.
- `Internal Consent Creates Row For New Rider` then `Internal Consent Updates Existing Row`:
  with a fresh random number (`_test_push_only_number`, never booked)
  the push alone creates the `RiderDetails` row (`created == 1`, `updated == 0`), and a
  second push with consent `true` updates it (`updated == 1`, `created == 0`);
  `failedIndices` is empty in both. This proves the push alone maintains the BPP copy.

## Seeding: the suite needs `DirectCall` locally

`setup-phone-share-consent.sql` seeds, in `atlas_driver_offer_bpp`,
`transporter_config.driver_calling_option = 'DirectCall'` for every city. The
upstream/config-synced value is `'AnonymousCall'` for the test cities, under which
consent can never expose the number: ride 2's positive assertion fails with
`riderMobileNumber = null` even though the consent reached `rider_details` (the
kill switch working as designed).

The seed also sets `atlas_app.rider_config.enable_share_number_with_driver = true`
(BAP), which makes confirm carry the consent tag, and
`atlas_app.rider_config.push_consent_to_bpp = true`, which turns on the push.
Without the push flag, a mid-ride revoke is not seen until the next confirm.

Three run paths, each with its own seeding story:

1. **`./run-tests.sh phone-consent`**: self-contained: applies
   `setup-phone-share-consent.sql` and then **flushes Redis**, because both
   tables are cached and running services would otherwise keep serving stale values.
2. **Test dashboard**: the dashboard invokes newman directly and never runs the
   seed above. Instead, `dev/config-sync/assets/patches.json` carries
   `dimension_overrides` entries for both
   (`atlas_driver_offer_bpp.transporter_config` -> `driver_calling_option =
   DirectCall`, `atlas_app.rider_config` -> `enable_share_number_with_driver =
   true`), so every config-sync import re-applies them and flushes Redis itself.
   Both are synced tables, so without the patch entries each sync silently reverts the seed.
3. **Raw newman**: apply the SQL and flush Redis manually first.

### The in-process (L1) cache — why "seed + flush Redis" can still not be enough

Both `transporter_config` and `rider_config` are served through ConfigPilot,
which caches each read in
**process memory** for up to an hour before the Redis layer is even consulted
(`lib/config-pilot/src/Lib/ConfigPilot/Interface/Getter.hs:77` —
`IM.withInMemCache l1Key 3600` wrapping `Hedis.withRedisCache ... 7200`
wrapping the DB fetch). A running driver-app that has already served a ride
keeps answering from L1; no SQL update or Redis flush can reach it.

Observed on 2026-07-22 (second failed run): DB showed `DirectCall` **and** the
rider's consent `true`, yet ride 2 still returned `riderMobileNumber = null` —
the process was serving the `AnonymousCall` it had memoised before the seed.

**Rule: after seeding, restart both services (dynamic-offer-driver-app for
`transporter_config`, rider-app for `rider_config`) if they were already
running**, or seed before the stack starts. Waiting out the
1-hour TTL also works but only if the in-mem entry is not refreshed by hits in
the meantime — restart is the only deterministic option. This applies equally
to config-sync imports done while services are up: any ConfigPilot-served
table has the same staleness window.

## What is deliberately NOT covered here

- **The merchant kill switch** (`AnonymousCall`/absent option + consent `true` →
  still masked). Toggling `transporter_config` mid-collection would need a cache
  flush between Newman steps, which the framework can't do. **This half of the
  gate is currently untested.** `dynamic-offer-driver-app` has no Haskell test
  suite (no `tests:` stanza in `Main/package.yaml`), so the kill-switch case
  — `AnonymousCall` + consent `true` → still masked — has no automated
  coverage. Adding that suite is tracked separately.
- **`forceDirectCalling`** (`transporter_config` break-glass override that serves
  the rider's real number as `DIRECT` on active rides regardless of the merchant
  option or rider consent, for use while exophones are down). Untestable
  in-collection for the same cache reason as the kill switch: raising it needs a
  Redis flush plus a `dynamic-offer-driver-app` restart between Newman steps. The
  default-false path is what this suite exercises.
- **Third-party BAP/BPP behaviour** — out of scope per the spec.
- **Actual call bridging** (Exotel webhooks) — the suite asserts `exoPhone` is
  present as the fallback, not that a call connects.

## Conventions

- Rider/driver numbers and vehicle registration are random per run
  (collection prerequest, `_test_*` collection variables) — safe to run
  concurrently and repeatedly.
- The ride skeleton is copied from `RideBookingFlow/01-AutoRideFlow.json`; step
  names carry a `(Ride N)` suffix to stay unique. If AutoRideFlow's flow changes
  materially (auth, allocator timing), regenerate/diff this collection against it.
