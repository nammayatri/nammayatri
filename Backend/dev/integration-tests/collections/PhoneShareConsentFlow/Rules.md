# PhoneShareConsentFlow

E2E coverage for the rider phone-sharing consent gate: the driver sees the rider's
real mobile number only when the merchant's `driver_calling_option` allows direct
calling **and** the rider consented (`SafetySettings.consentToShareMobileNumber`,
carried to the BPP as the `CONSENT_TO_SHARE_MOBILE_NUMBER` BECKN tag at confirm).

Spec: `docs/superpowers/specs/2026-07-22-rider-phone-sharing-consent-design.md`.

## What the suite asserts

One collection, three rides by the same (random) rider, under seeded `DirectCall`:

| Ride | Rider consent state       | `riderMobileNumber` in `/driver/ride/list` | `exoPhone` |
|------|---------------------------|--------------------------------------------|------------|
| 1    | never set (tri-state null)| absent                                     | present    |
| 2    | granted (`true`)          | contains the rider's real number           | present    |
| 3    | revoked (`false`)         | absent again                               | present    |

Between rides it also asserts the rider API's tri-state directly via
`GET /profile/getEmergencySettings`: `null` (never asked) → `true` → explicit
`false` — `null` and `false` are deliberately distinct states.

Ride 2 vs ride 3 additionally exercises the BPP's repeat-rider update path
(`unless isNewRider $ updateConsentToShareMobileNumber` at confirm): the
`RiderDetails` row created during ride 1 is flipped to `true` then back to
`false` by subsequent confirms, proving "consent applies from the next ride".

## Seeding: `driver_calling_option` must be `DirectCall` locally

The gate is an AND. The upstream/config-synced `driver_calling_option` is
`'AnonymousCall'` for the test cities, under which consent can never expose the
number — ride 2's positive assertion fails with `riderMobileNumber = null` even
though the consent tag demonstrably reached `rider_details` (this exact failure
was observed on 2026-07-22; the DB showed `consent_to_share_mobile_number =
true` next to `AnonymousCall`, i.e. the kill switch working as designed).

Three run paths, each with its own seeding story:

1. **`./run-tests.sh phone-consent`** — self-contained: applies
   `setup-phone-share-consent.sql` (all cities, keeps collections city-agnostic)
   and then **flushes Redis**, because `transporter_config` is cached and a
   running driver-app would otherwise keep serving the stale `AnonymousCall`.
2. **Test dashboard** — the dashboard invokes newman directly and never runs the
   seed above. Instead, `dev/config-sync/assets/patches.json` carries a
   `dimension_overrides` entry (`atlas_driver_offer_bpp.transporter_config` →
   `driver_calling_option = DirectCall`, present in `patches.json.example` under
   all three `*_to_local` directions), so every config-sync import re-applies
   `DirectCall` and flushes Redis itself. **`transporter_config` is a synced
   table** — without the patch entry, each sync silently reverts the seed.
3. **Raw newman** — apply the SQL and flush Redis manually first.

### The in-process (L1) cache — why "seed + flush Redis" can still not be enough

`transporter_config` is served through ConfigPilot, which caches each read in
**process memory** for up to an hour before the Redis layer is even consulted
(`lib/config-pilot/src/Lib/ConfigPilot/Interface/Getter.hs:77` —
`IM.withInMemCache l1Key 3600` wrapping `Hedis.withRedisCache ... 7200`
wrapping the DB fetch). A running driver-app that has already served a ride
keeps answering from L1; no SQL update or Redis flush can reach it.

Observed on 2026-07-22 (second failed run): DB showed `DirectCall` **and** the
rider's consent `true`, yet ride 2 still returned `riderMobileNumber = null` —
the process was serving the `AnonymousCall` it had memoised before the seed.

**Rule: after seeding, restart the provider service (dynamic-offer-driver-app)
if it was already running**, or seed before the stack starts. Waiting out the
1-hour TTL also works but only if the in-mem entry is not refreshed by hits in
the meantime — restart is the only deterministic option. This applies equally
to config-sync imports done while services are up: any ConfigPilot-served
table has the same staleness window.

## What is deliberately NOT covered here

- **The merchant kill switch** (`AnonymousCall`/absent option + consent `true` →
  still masked). Toggling `transporter_config` mid-collection would need a cache
  flush between Newman steps, which the framework can't do. This half of the
  gate is covered by unit tests: `dynamic-offer-driver-app` test suite,
  `Domain/Action/UI/RideSpec.hs` (full 3×2 matrix) and
  `Beckn/ACL/ConfirmSpec.hs` (tag parser fail-closed cases, including the
  non-value-add-NP refusal).
- **Third-party BAP/BPP behaviour** — out of scope per the spec; the BPP refuses
  the consent tag from non-value-add NPs (unit-covered).
- **Actual call bridging** (Exotel webhooks) — the suite asserts `exoPhone` is
  present as the fallback, not that a call connects.

## Conventions

- Rider/driver numbers and vehicle registration are random per run
  (collection prerequest, `_test_*` collection variables) — safe to run
  concurrently and repeatedly.
- The ride skeleton is copied from `RideBookingFlow/01-AutoRideFlow.json`; step
  names carry a `(Ride N)` suffix to stay unique. If AutoRideFlow's flow changes
  materially (auth, allocator timing), regenerate/diff this collection against it.
