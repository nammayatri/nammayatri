# PhoneShareConsentFlow

E2E coverage for the rider phone-sharing consent gate: the driver app **dials** the
rider's real mobile number (`callingNumber`) only when the merchant's
`driver_calling_option` allows direct calling **and** the rider consented
(`SafetySettings.consentToShareMobileNumber`, carried to the BPP as the
`CONSENT_TO_SHARE_MOBILE_NUMBER` BECKN tag at confirm). The legacy
`riderMobileNumber` field is gated by the merchant option alone.

The consent flow itself is gated on the BAP by
`atlas_app.rider_config.enable_share_number_with_driver` (default `false`). The
BPP infers the flow's state from the tag: absent means the flow is off and the
number is shared regardless of consent; only a present-but-unparseable value
falls back to "no consent". So the suite must seed the flag `true` — see
[Seeding](#seeding-the-suite-needs-directcall-and-the-consent-flow-flag-locally).

Spec: `docs/superpowers/specs/2026-07-22-rider-phone-sharing-consent-design.md`.

## What the suite asserts

One collection, three rides by the same (random) rider, under seeded `DirectCall`
and `enable_share_number_with_driver = true`:

| Ride | Rider consent state       | `riderMobileNumber` | `callingNumber.number` | `callingNumber.countryCode` | `callingNumber.numberType` |
|------|---------------------------|---------------------|-------------------|-----------------------------|---------------------|
| 1    | never set (tri-state null)| the real number     | equals `exoPhone` | `null`                      | `ANONYMOUS`         |
| 2    | granted (`true`)          | the real number     | real number, bare (= `riderMobileNumber`) | the rider's country code (e.g. `+91`) | `DIRECT`            |
| 3    | revoked (`false`)         | the real number     | equals `exoPhone` | `null`                      | `ANONYMOUS`         |

`callingNumber.number` is always bare — no country code, the same format as
`riderMobileNumber` and `exoPhone`. The client applies its own local dialling
prefix.

Note the deliberate split: `riderMobileNumber` predates the consent feature
(PR #15876) and keeps its original behaviour — present whenever the merchant
enables direct calling, regardless of consent — so already-released driver app
builds are unaffected. Consent gates only `callingNumber`, which is the field
new builds dial. Rides 1 and 3 are what prove it: the real number is still in
`riderMobileNumber`, while `callingNumber` correctly falls back to the exophone.

Between rides it also asserts the rider API's tri-state directly via
`GET /profile/getEmergencySettings`: `null` (never asked) → `true` → explicit
`false` — `null` and `false` are deliberately distinct states.

Ride 2 vs ride 3 additionally exercises the BPP's repeat-rider update path
(`unless isNewRider $ updateNightSafetyChecksAndConsent` at confirm): the
`RiderDetails` row created during ride 1 is flipped to `true` then back to
`false` by subsequent confirms, proving "consent applies from the next ride".

## Seeding: the suite needs `DirectCall` **and** the consent-flow flag locally

`setup-phone-share-consent.sql` seeds two rows, one per side:

- `atlas_driver_offer_bpp.transporter_config.driver_calling_option = 'DirectCall'`
  (BPP). The upstream/config-synced value is `'AnonymousCall'` for the test
  cities, under which consent can never expose the number — ride 2's positive
  assertion fails with `riderMobileNumber = null` even though the consent tag
  demonstrably reached `rider_details` (this exact failure was observed on
  2026-07-22; the DB showed `consent_to_share_mobile_number = true` next to
  `AnonymousCall`, i.e. the kill switch working as designed).
- `atlas_app.rider_config.enable_share_number_with_driver = true` (BAP). It
  defaults to `false`, under which the BAP omits the consent tag entirely and
  the BPP therefore shares the number on every ride — rides 1 and 3, which
  assert `ANONYMOUS`, would both fail.

Both are applied to every city so the collection stays city-agnostic.

Three run paths, each with its own seeding story:

1. **`./run-tests.sh phone-consent`** — self-contained: applies
   `setup-phone-share-consent.sql` and then **flushes Redis**, because both
   tables are cached and running services would otherwise keep serving the stale
   `AnonymousCall` / `false`.
2. **Test dashboard** — the dashboard invokes newman directly and never runs the
   seed above. Instead, `dev/config-sync/assets/patches.json` carries
   `dimension_overrides` entries for both
   (`atlas_driver_offer_bpp.transporter_config` → `driver_calling_option =
   DirectCall`, `atlas_app.rider_config` → `enable_share_number_with_driver =
   true`; both present in `patches.json.example` under all three `*_to_local`
   directions), so every config-sync import re-applies them and flushes Redis
   itself. **Both are synced tables** — without the patch entries, each sync
   silently reverts the seed.
3. **Raw newman** — apply the SQL and flush Redis manually first.

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
- **The flow-off case** (`enable_share_number_with_driver = false` → tag absent →
  `DIRECT` regardless of consent). Untestable in-collection for the same reason
  as the kill switch: `rider_config` is ConfigPilot-served, so flipping it
  between Newman steps needs a Redis flush *plus* a rider-app restart to clear
  the in-process cache — neither of which newman can do.
- **Third-party BAP/BPP behaviour** — out of scope per the spec; the BPP refuses
  the consent tag from non-value-add NPs (not currently covered by automated
  tests).
- **Actual call bridging** (Exotel webhooks) — the suite asserts `exoPhone` is
  present as the fallback, not that a call connects.

## Conventions

- Rider/driver numbers and vehicle registration are random per run
  (collection prerequest, `_test_*` collection variables) — safe to run
  concurrently and repeatedly.
- The ride skeleton is copied from `RideBookingFlow/01-AutoRideFlow.json`; step
  names carry a `(Ride N)` suffix to stay unique. If AutoRideFlow's flow changes
  materially (auth, allocator timing), regenerate/diff this collection against it.
