# Countries — two of them, on one stack

The service areas, the move to Mauritania, and running Algeria beside it: merchants, geofences, phone rules, tariffs per country.

> Moved here from the local-stack README on 2026-10-08 (phase 7), word for word: dates and measurements are as they were taken. Commands written `./x.sh` run from `stack/`. Back to the [README](../README.md).

## Algeria service areas

`algeria-geofences.sql` repoints the backend from the upstream Indian service
areas to Algeria. `setup.sh` applies it automatically; `./setup.sh algeria`
re-applies it on its own.

Coverage is a switch, because both sets of boundaries are always loaded and
only the merchant's restriction changes:

```bash
./setup.sh algeria                    # nationwide (default)
COVERAGE=cities ./setup.sh algeria    # Algiers, Oran, Annaba only
```

**Nationwide is one national border, not 58 wilayas.** Note it is a real
border, not "no geofence" — a NULL restriction would mean `Unrestricted`,
i.e. the whole world, which is why Tunis and Oujda still get refused.

Serviceability is one query — `Main/src/Storage/Queries/Geometry.hs`:

```sql
SELECT * FROM atlas_app.geometry
 WHERE region IN (<merchant.origin_restriction>)
   AND ST_Contains(geom, ST_Point(lon, lat));
```

So a city is **two pieces of data and zero lines of code**: a `geometry` row
(name + boundary), and that name listed in the merchant's origin/destination
restriction. A fourth city is one more row and one more array element.

Boundaries come from OpenStreetMap, simplified with
`ST_SimplifyPreserveTopology` to keep the file reviewable:

| Region | OSM relation | Level | Tolerance | Points |
|---|---|---|---|---|
| Algeria | 192756 | 2 (country) | ~200 m | 12109 → 1533 |
| Algiers | 157062 | 4 (wilaya) | ~55 m | 13215 → 655 |
| Oran | 1259187 | 4 | ~55 m | 7737 → 1152 |
| Annaba | 1455599 | 4 | ~55 m | 12047 → 778 |

Verified against the national boundary:

| Point | Serviceable |
|-------|-------------|
| Algiers (centre + airport), Oran, Annaba | ✅ |
| Constantine, Sétif, Batna, Tlemcen, Ghardaïa | ✅ |
| Béchar, Adrar, Tamanrasset (Sahara) | ✅ |
| Tunis 🇹🇳 / Oujda 🇲🇦 | ❌ |
| Bangalore 🇮🇳 | ❌ |

The negatives matter — they prove the Indian areas were *replaced* rather than
added to, and that nationwide still means Algeria rather than everywhere.

> **SRID 0, not 4326.** Matches the existing rows and the `ST_Point()` the
> application builds. PostGIS refuses `ST_Contains` across mismatched SRIDs.

> **Redis caches the merchant.** The service-area restriction lives on the
> merchant row, which `Storage/CachedQueries/Merchant.hs` caches. Change it in
> Postgres without dropping the cache and the API keeps serving the old areas.
> `setup.sh` flushes Redis for you.

### The map — `http://localhost:8025`

A one-page visual of the same thing the terminal checks: the three boundaries
drawn on a map, click anywhere to fire a real
`POST /v2/serviceability/origin` and get green (served) or red (not served).

Two things keep it honest:

- The polygons are **exported from `atlas_app.geometry` on every run**, not
  hand-drawn, so the map cannot drift from what the API enforces.
- The answers are **live API calls**, not a lookup in the page.

It also exists because of CORS: rider-app sends no CORS headers and 404s on
`OPTIONS`, so a page opened from `file://` or any other port cannot call it.
The `map` container (nginx) serves the page *and* reverse-proxies `/v2` to
rider-app, putting both on one origin — no upstream patch needed.

### What is *not* just config: the country code

Cities are data, but the **country is hard-coded**. `POST /v2/auth` rejects an
Algerian number outright:

```
{"errorPayload":[
  {"expectation":"(length(mobileNumber) == 10 and mobileNumber matches regex /^[0-9]*$/)"},
  {"expectation":"mobileCountryCode matches regex /^\\+91$/"}],
 "errorCode":"REQUEST_VALIDATION_FAILURE"}
```

From `Main/src/Domain/Action/UI/Registration.hs`:

```haskell
validateField "mobileCountryCode" mobileCountryCode P.mobileIndianCode
```

Making this configurable is a small source change, but it is a *source* change —
it needs a Haskell rebuild, which this stack (running a prebuilt image)
deliberately avoids. The demo therefore still logs in with the `+91` test rider;
the geofence result is independent of the phone number.

## Mauritania — the switch, and the two things that hid in it

The pilot moved from Algeria to Mauritania on **2026-09-03**, replacing it
rather than running both. Nothing Algerian was deleted: the graph, the tiles,
the place CSV, the geofence row and the tariff are all still here, and going
back is a variable and an image swap.

### The five steps, in the order that keeps each one provable

    1. tariff        SQL, reversible, nothing reads it until a search happens
    2. place index   destroys Algerian search, so it goes before the coverage
    3. map servers   MAP_COUNTRY=mauritania, files already built beside the old
    4. coverage      the geofences, plus the caches that hide them
    5. images        the +222 binaries -- LAST, because after this no Algerian
                     number can sign in and there is no going back cheaply

`COUNTRY=mauritania` on `osrm-prepare.sh`, `tiles-prepare.sh` and
`geocoder-prepare.sh`; `MAP_COUNTRY` in the compose. All default to `algeria`.

    mauritania-latest.osm.pbf    29.1 MB   (Algeria: 285 MB)
    routing graph                16.9 MB
    tiles                        52.5 MB   (Algeria: 309.6 MB)
    places indexed               10 005    (Algeria: 111 555)

### THERE ARE TWO SERVICE AREAS

`atlas_app.geometry` + `atlas_app.merchant.origin_restriction` is the **rider's**
geofence. `atlas_driver_offer_bpp.geometry` + its own **two** merchants is the
**provider's**, and it is a completely separate set of rows.

Switch only the rider's and the search reaches the BPP, is dropped there, and
returns no estimate — no error on either side. The provider-side geometry
column is typed `geometry(MultiPolygon)` where the rider's is untyped, so the
row is built with `ST_Multi()` from the rider's to guarantee they agree.

Both are cached in Redis (`CachedQueries:Merchant`), so the UPDATE alone
changes nothing the API says.

### BECKN CANNOT PARSE A NEGATIVE COORDINATE

This is the one worth reading twice, and it had been true since 2023.

    Error in $.message.intent.fulfillment.start.location.gps:
    (line 1, column 10): unexpected "-" expecting space or float

The gps field travels as a string, `"18.0858, -15.9582"`, and
`Beckn/Types/Core/Taxi/Common/Gps.hs` read it with Parsec's `P.float`, which is
**unsigned**. Column 10 is the character straight after `"18.0858, "`.

**Algeria is at +3 longitude, so every coordinate the pilot ever sent was
positive and this was invisible for its entire life.** Nouakchott is at −15.9
and no search reached the driver pool at all.

Three things made it expensive to find:

  * **The provider logged nothing.** It answered the gateway 400, which from
    its own side is correct — it rejected a malformed request. The reason
    existed only in the response body the *gateway* received.
  * The same file argues with itself: its regex comment allows `[-+]?`, its
    OpenAPI description promises "an optional leading `-` for negative
    numbers", and its validator is `. abs`. Three parts expect a sign; only the
    parser, which is the part that runs, cannot read one.
  * Six healthy things were checked first — both geofences, a dead subscriber
    on :8000, the proxies, the fleet's freshness and the fare policy.

Patched in `apply-patches.py` as a seventh site. `beckn-spec` is shared, so one
site fixes the rider and the provider together.

**When a component reports no error, read what its caller received.**

### The test fleet

**Erased on the live server 2026-10-01, with every other test account (the
owner's decision, before launch).** 37 drivers — the 8 simulated cars, the
pilot's parked Algiers drivers, every « Test » / « Boss Test » / Moha account,
the two `algerian-test-accounts.sh` drivers, and upstream's 2022 sample rows —
and 69 passengers on invented numbers, each through the console's own
`db/deletion/anonymise.sql` (website repo): names, numbers, sessions,
positions, cars and papers erased, rides, bookings and wallet history kept, an
audit row per account (`actor_email` = `owner: test accounts removed
2026-10-01`). Their 23 document files deleted and their cached sessions
(`*authTokenCacheKey:*` in Redis) dropped. `movin-fleet` and `movin-drivers`
uninstalled. Kept: the 11 accounts on real phones (the owner's, the boss's, and
three drivers who signed up by WhatsApp). Before it, a full off-site backup and
a root-only dump of the three account schemas in `/root/pre-removal/`.

Consequence: no car on the map in either country until a real driver is
online, and the bot's « aucun chauffeur en ligne » is now simply true.

**Back on 2026-10-03, for testing only — the launch was delayed** (the
owner's request). `simulate-driver.py` now runs **both** countries: twelve
cars, two per sold variant in each — six around central Nouakchott, and in
Algiers one of each variant in the centre (Belcourt) and one in El Biar / Ben
Aknoun, where the owner's test pickups are. The country follows from the
number (eight digits Mauritania, ten with the trunk zero Algeria) and decides
the dialling code, the merchant (`NAMMA_YATRI_PARTNER` / `MOVIN_DZ_PARTNER`)
and where each car waits. `seed` also gives each a year's working day
(`movin.wallet.day_until` only — no ledger entry, so no payment appears that
nobody made, and revenue is still the sum of real `day` entries); without it
the no-top-up rule would keep them off dispatch. `movin-fleet` installed again
(`fleet-service.sh install`); `movin-drivers` deliberately not — the daemon
heartbeats its own cars, and that timer re-stamped real drivers too.

    Mauritania  22100001..06   Mohamed, Ahmed (SEDAN) · Cheikh, Brahim (HATCHBACK) · Moustapha, Abdallahi (SUV)
    Algeria     0555100001..06 Karim, Bilal (SEDAN) · Yacine, Mehdi (HATCHBACK) · Sofiane, Amine (SUV)

**They answer real ride requests.** Before the first real passenger:
`./fleet-service.sh uninstall`, and erase the twelve accounts **and the two test
passengers below** the same way as on 2026-10-01 (the console's
`anonymise.sql`). Never sign in as one of the cars: it revokes the daemon's
session.

**Each car drives its own ride, in parallel (2026-10-04).** The daemon used to
drive a ride inline, and for the whole trip no other car polled for requests
or sent a heartbeat — in either country. Found by the first full ride test: a
14 km Algiers ride at 3× is nine minutes, an Algerian hatchback request during
it got no offer at all, and every idle car's position was six minutes old. Now
a ride runs on a thread of its own and the loop keeps serving the rest.
Restarting `movin-fleet` mid-ride is safe: the new daemon resumes an
`INPROGRESS` ride towards its destination.

**The ride test** — `probe-two-country-rides.py both all` (on the server, from
`/tmp`): one passenger per country books every row the app sells
(SEDAN, HATCHBACK, SUV), the simulated fleet drives it, and the ride must end
COMPLETED at a price in the country's currency with the driver's wallet
respected. Its passengers are **test accounts**: `+222 22778899` and
`+213 0555000199`, created by signing in directly on the rider API (the fixed
code 7891 behind the guard). First full run 2026-10-04, **six of six**:
Mauritania 102 / 70 / 140 MRU (sedan, hatchback, SUV, 2.5 km), Algeria
917 / 641 / 1212 DZD (14 km, ~7 min each at 3×). The Algerian rows first
"failed" twice for reasons that were not the product: the probe's wait was
shorter than a 14 km trip, and the hatchback got no offer because the
simulator was busy driving another car — the bug fixed above. Run the test
with no simulated ride open: the one passenger cannot hold two bookings
(`ACTIVE_BOOKING_PRESENT`).

`./seed-mauritanian-fleet.sh` — two drivers per sellable variant in Nouakchott,
Mauritanian names and plates, all enrolled in the guard. Two per type because
one means a whole category dies the moment that driver takes a ride, and
because the offers screen is never exercised as designed with one.

The three things that make a seeded driver invisible, none of which produces an
error, are documented at the top of that script: the `point` column rather than
lat/lon, `coordinates_calculated_at` freshness, and a spread wider than the
search radius.

### Proven end to end, 2026-09-03

A `+222` number, eight digits, searching Tevragh Zeina → Ksar:

    AUTO_RICKSHAW     84-119 MRU     2 cars nearby
    HATCHBACK         84-119 MRU     2 cars nearby
    SEDAN            123-158 MRU     2 cars nearby
    SUV              167-202 MRU     2 cars nearby

## Two countries — Algeria beside Mauritania, since 2026-09-13

The client reversed the 3 September *replacement*: the stack now serves **both**
countries at once. Mauritania is live; Algeria is built, priced and routed, and
**open to sign-in since 2026-09-27** — by WhatsApp, and since 2026-09-29 also
by an SMS the person SENDS to the office SIM. We never text Algeria: it has no
SMS provider (see *Sign-in: accepted by the backend, gated by the guard*, and
*[The SMS inbox and sign-in by an SMS he sends](sign-in.md#the-sms-inbox-and-sign-in-by-an-sms-he-sends-2026-09-29)*).

### The design: one merchant per country — on the driver side only

Prices live on the driver side (`fare_policy` is per merchant and variant; this
binary has no operating-city level), so each country is its own **driver
merchant**. The rider side prices nothing and keeps **one** merchant:

| side | merchant | serves |
|---|---|---|
| rider | `YATRI` | `{Mauritania, Algeria}` |
| driver | `favorit0-…` (NAMMA_YATRI_PARTNER, `JUSPAY.MOBILITY.PROVIDER.UAT.3`) | `{Mauritania}` |
| driver | `algeria0-0000-0000-0000-00000algeria` (MOVIN_DZ_PARTNER, `MOVIN.DZ.PROVIDER`) | `{Algeria}` |

The gateway sends every search to both driver merchants; each drops the other
country's as `RIDE_NOT_SERVICEABLE`. The app sends `YATRI` for a passenger in
either country, and the country's own driver merchant id for a driver
(`src/lib/country.ts` in the app).

The Algerian merchant is a **clone** of the Mauritanian one — every
`merchant_id`-keyed config row (service config, usage config, transporter
config, fares, extra-fare caps, operating city) copied from the catalogue, so
it behaves exactly like the merchant that is proven. It must also be in
`atlas_registry.subscriber`, or it never receives a search. The upstream test
merchant `nearest-drivers-testing-organization` was deliberately NOT reused: no
registry row, never ours.

    bash apply-two-countries.sh     # backup, merchants, tariffs, caches, fleet, state
                                    #   (two-countries-merchants.sql + both tariffs)

Backup first, always: `backups/pre-two-countries-<ts>.sql` (data-only, the
tables it touches).

### The tariffs are keyed by merchant now — both files

    ./apply-tariff.sh mauritania-tariff.sql     # favorit0 only
    ./apply-tariff.sh algeria-tariff.sql        # algeria0 only

Both files used to match on `vehicle_variant` alone and rebuild
`restricted_extra_fare` for **every** merchant. With two countries that is a
trap: re-running either one reprices the other. Algeria = the Mauritanian
figures ÷ 0.30, rounded to 5 (Voiture 150/50/65, Scooter & Herbin 100/35/50,
Fourgon 200/65/100) — close to, not equal to, the 13 August Algerian table.

### One map — `./maps-two-countries.sh`

    bash maps-two-countries.sh all      # places, merge, osrm, tiles, switch, check
    bash maps-two-countries.sh rollback # back to the Mauritania-only files

The two Geofabrik extracts are merged with `osmium` in a throwaway container
(the host has none) into `algeria-mauritania-latest.osm.pbf`; the graph and
`algeria-mauritania.mbtiles` (388.6 MB) are built **beside** the old files, and
only `switch` changes what riders get. It checks routes, tiles and search in
both countries and rolls itself back on a failed check.

**`MAP_COUNTRY=algeria-mauritania` is in `.env` now.** It never was: the
compose defaults to `algeria`, and `mauritania` had been typed inline on 3
September, so any later plain `docker compose up` would have silently put the
Algeria-only map back.

The first switch rolled back on a **good** build: the check slept 8 s, and
OSRM loading a graph ten times Mauritania's was still refusing connections. It
now waits for OSRM to answer.

### The place index is APPENDED to, never rebuilt

`geocoder-prepare.sh load` drops `geo.place` — and with it `name_ar`, 3,324
reviewed Mauritanian Arabic names that exist nowhere else. Algeria went in
through `geocoder/append-country.sql` from `places.algeria.csv`: the same four
passes as `index.sql`, inserting only rows not present, Arabic names filled
**for the new rows only** (arabic-names.sql's rule would rewrite reviewed
names), NFKC'd. 121,886 places, 58,329 with Arabic.

### Sign-in: accepted by the backend, gated by the guard

The backend patch accepts `+222`/8 digits and `+213`/10 (the trunk-zero form
Algerian accounts were stored in): `ExactLength 8 Or ExactLength 10`, `"+222"
Or "+213"`. **Which countries may sign in is the guard's**:

    OPEN_COUNTRIES=+222,+213       # who may sign in at all (default +222)
    SMS_COUNTRIES=+222             # who of those is sent an SMS (default +222)

Both are in `docker-compose.yml` under `auth-guard`, and a change is
`docker compose up -d --no-deps --force-recreate auth-guard` — no build, no
APK. A country missing from `OPEN_COUNTRIES` answers `403 COUNTRY_NOT_OPEN`.

**Algeria opened on 2026-09-27, by WhatsApp** — and since 2026-09-29 also by
an SMS he sends us (below, *[The SMS inbox](sign-in.md#the-sms-inbox-and-sign-in-by-an-sms-he-sends-2026-09-29)*), which is not an SMS start and is
not refused here. Moorsyl is Mauritanian, so
a `+213` SMS start answers `403 SMS_NOT_AVAILABLE` *before* the backend is
asked — no person row, no send, nothing off the SMS budget — and the app
(built after that date) says « Appuyez sur « Continuer avec WhatsApp » ». A
resend is refused the same way. Two exceptions, both sending nothing: exempt
numbers, and a driver who holds a personal code. `/healthz` shows the two
lists as `countries`. Proved live through the edge the same day: `+213` by SMS
→ 403 on both the rider and driver routes; `+213` by WhatsApp → 200 with the
`wa.me` link; `+222` unchanged. When Algeria gets an SMS provider, adding
`+213` to `SMS_COUNTRIES` is the whole switch.

**No test number skips the SMS any more, in either country (2026-10-01, the
owner's decision).** `SMS_BYPASS` is empty and so is `driver-codes.json` —
every one of its 21 personal codes belonged to an invented test number (the
Algerian test drivers, the boss's « Patron » accounts, the two `+22222000001/2`
accounts and the simulated fleet). Both old files are kept, root-only, as
`/opt/ny/secrets/*.before-<timestamp>`. The simulated fleet is not affected:
it signs in on the backend's own port (8017) with the backend's fixed code and
never meets the guard. `deploy-shims.sh` no longer probes a sign-in; it reads
`/v2/auth/channels`, which sends nothing.

Until then, numbers on `SMS_BYPASS` passed both gates, which is how the test
accounts worked:

    bash algerian-test-accounts.sh
      passengers  +213 0555000001..3      the private test code
      drivers     +213 0666000001 Voiture a fresh personal code, printed once
                  +213 0666000002 Herbin  a fresh personal code, printed once
                                          (both approved, 1000 DA test credit)

**No code is written here any more, and none may be (2026-09-27).** This
repository is public — a fork of public Namma Yatri — and this block used to
give the passengers' code and both drivers' personal codes, which let anyone
sign in as them. All three were changed that day; the old values are refused.

**All five must go before the first real Algerian rider** (Algeria is open
since 2026-09-27, and they were kept only for the owner's APK test): the
`+213` line in `SMS_BYPASS`
and `enrol-driver.sh --revoke`. `enrol-driver.sh` takes Algerian numbers with
`COUNTRY_CODE=+213 NSN_LENGTH=9 TRUNK_ZERO=1 MOBILE_FIRST=567 FIXED_SECOND=`.

`SMS_BYPASS` is a **folded scalar** (`>-`): a `#` line inside it is part of the
value, not a comment, and would corrupt a number. Comments go above the key.

### Which country the sign-in screen shows — the phone's GPS, and nothing else

The boss did not want users to see the other country, so the phone screen has
**no picker**: the app detects the country. There is deliberately no way to
switch on screen.

**GPS only since 2026-09-28** (owner's decision): matched on the phone against
the two outlines, the position never leaves it. Anything short of a fix is
**Mauritania**, with « Vous devez activer la localisation pour détecter votre
pays » and a button to turn it on. The server has no part in it.

From 2026-09-15 to 2026-09-28 there was an IP fallback: `GET /geo/country` on
the shim (`geo.js`, AfriNIC's delegation list in `ip-countries.json`, the
nginx `/geo/` location), then the last country used on the phone. Both guessed
people into the wrong country without saying so. **Removed the same day** —
route, module, table, `geo-ip-refresh.sh` and `edge/add-geo-location.py` — on
the owner's word that every phone updates; `/geo/country` now answers the
edge's 404. The app's privacy page was corrected to match.

### The wallet, per country

`maps-shim/wallet.js` reads the driver's merchant on every call:

| | day | minimum | gateway |
|---|---|---|---|
| Mauritania | 30 MRU | 30 | Moosyl (driver picks the method on Moosyl's page) |
| Algeria | 100 DZD | 100 | Chargily (`method=edahabia|cib`, asked in the app) |

`WALLET_DAY_PRICE_DZ` / `WALLET_MIN_TOPUP_DZ` override. Credit is still only
written after reading the status back from the gateway with our key, so the
webhook stays a hint for both. `restricted.js` compares each driver against
**his** country's price — typed parameters, because an untyped `CASE` made
Postgres refuse `integer < text` and silently keep the old list.

### The search lock — the bug two merchants in one process exposed

`API/Beckn/Search.hs` guarded the search with
`whenWithLockRedis (searchLockKey messageId)`, which **silently skips** when
the lock is taken. One process, two driver merchants, the same message id from
the gateway: whichever arrived second while the first held the lock dropped
the search — no error, no log beyond "Search API Flow: Reached". Measured: a
Nouakchott search reached the Algerian merchant first (held ~6 ms, refused on
georestrictions) and the Mauritanian one 3 ms later did nothing. The order is
the gateway's and a registry restart reshuffles it — so it presented as a bad
image and was rolled back as one (15:29-15:34, Mauritania without prices).

Patched: the key is **merchant + message**. Stopgap if it ever recurs — take
the second country out of the registry:

    DELETE FROM atlas_registry.subscriber WHERE subscriber_id = 'MOVIN.DZ.PROVIDER';
    # then clear '*egistry*' and '*ubscriber*' in Redis; restore with the
    # INSERT in two-countries-merchants.sql

With more than one merchant per process, **audit every per-message lock**.

### Deploying the pieces

    bash deploy-backend.sh            # patch inside BOTH binaries, rollback tag, swap
    bash deploy-backend.sh rollback   # newest ny-rider:rollback-* back
    bash deploy-shims.sh              # restart guard + shim, prove +213 refused, +222 ok,
                                      #   wallet rows, restriction list republished

`node --check` every shim file before copying it: a backtick in a SQL comment
inside a JS template string crash-looped the shim for ~2.5 minutes.

### Proof

    python3 probe-two-country-rides.py both

Mauritania as passenger only (the simulator drives; never sign in as
22100001-08 or 22100009), Algeria playing a parked `+213` driver; each ride
must end COMPLETED and charge the driver's own day (−30 MRU / −100 DZD).
