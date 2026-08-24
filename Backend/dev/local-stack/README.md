# local-stack — self-hosted Namma Yatri rider backend

Brings up a **working** rider-app backend (API + Postgres + Redis + Kafka +
encryption service) in Docker, with a seeded merchant and a test rider, so the
full login flow works end to end.

```bash
cd Backend/dev/local-stack
./setup.sh
```

First run takes ~10 minutes (it compiles librdkafka). After that, `docker compose up -d`
starts everything in seconds.

When it finishes you get:

```
POST /v2/auth                  200  authId=…
POST /v2/auth/{id}/verify      200  token=…
POST /v2/serviceability/origin 200  Algiers      serviceable=true
POST /v2/serviceability/origin 200  Bangalore    serviceable=false
*** Backend is fully operational ***
```

| URL | What |
|-----|------|
| `http://localhost:8025` | **Service-area map** — click anywhere, the backend answers |
| `http://localhost:8014/swagger` | Rider (BAP) Swagger UI — 60 endpoints (**no trailing slash**) |
| `http://localhost:8014/openapi` | Rider OpenAPI spec (JSON) |
| `http://localhost:8017/swagger` | **Driver (BPP) Swagger UI — 99 endpoints** |
| `http://localhost:8017/openapi` | Driver OpenAPI spec (JSON) |
| `localhost:5434` | Postgres (`postgres` / `root`, db `atlas_dev`) |

Two schemas in one database: `atlas_app` (rider) and `atlas_driver_offer_bpp`
(driver).

Demo script for showing it works: `./demo.sh` (or `demo.ps1` on Windows).

---

## Why this exists — three problems it solves

### 1. The current upstream backend cannot be run by outsiders

The database builds fine (421/422 migrations, 252 tables) but comes up **empty**.
Nothing in the repo inserts a row into `atlas_app.merchant`, and
`dev/local-testing-data/rider-app.sql` only creates a test rider *per merchant
that already exists*. Merchants come from `dev/config-sync`, which pulls from
Namma Yatri's own databases, or from an S3 bundle that returns `AccessDenied`.

So this stack pins upstream commit **`03a7531` (2023-03-02)** — the last
baseline that is self-contained and seeds a real merchant (`YATRI`).

### 2. The published Docker images are broken

`ghcr.io/nammayatri/nammayatri:*` is Ubuntu 18.04 and ships **librdkafka 0.11
(Feb 2018)**, but `rider-app-exe` needs >= 1.0 — it calls `rd_kafka_destroy_flags()`,
which doesn't exist in 0.11, and no newer copy is present anywhere in the image:

```
rider-app-exe: symbol lookup error: undefined symbol: rd_kafka_destroy_flags
```

`Dockerfile.rider` fixes this by building librdkafka 1.9.2 from source **on the
same 18.04 base**. Taking a prebuilt one from a modern distro does not work — it
pulls in OpenSSL 3 and a newer glibc, and the loader then fails with
`libpthread.so.0: symbol __libc_vfork ... not defined`.

### 3. Encryption is mandatory, not optional

rider-app encrypts PII (phone numbers) via **passetto**. Without it, every auth
request returns `500 INTERNAL_ERROR`. `passetto-db` is seeded with the
pre-generated keys that match the encrypted values in the seed data — that's why
the test rider's number decrypts correctly (`999...001`).

---

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

---

## The driver side (BPP)

Runs from the **same image** — it already contains every executable in the
repo — with a different entrypoint (`dynamic-offer-driver-app-exe`), its own
schema and its own migrations. No second build.

Seeded with 2 merchants, 12 drivers, 12 vehicles and 2 fare policies, and
`verify` registers and logs in a new driver on every run.

**Seeding order is not interchangeable**, and getting it wrong fails in a way
that is hard to read later:

1. `sql-seed/dynamic-offer-driver-app-seed.sql` — schema + 13 base tables
   including `organization`. Contains **no data**.
2. `local-testing-data/dynamic-offer-driver-app.sql` — organizations, drivers,
   vehicles, fare policies, inserted into `organization`.
3. Migrations, applied by driver-app at startup. Migration **0050**
   (`rename-org-to-merchant`) renames `organization` → `merchant`, carrying
   those rows across.

So the data must be loaded **before driver-app starts**. Load it afterwards and
every insert fails, because `organization` no longer exists. This is the same
trap as the rider side, which is why `local-testing-data/rider-app.sql` is
deliberately never applied.

> Driver auth takes the merchant **UUID**, not the `shortId` the rider side
> uses. An unknown number is fine — `auth` calls `createDriverWithDetails`, so
> registration and login are the same call.

## OSRM — real Algerian routing

```bash
./osrm-prepare.sh          # download + build the graph (one-off, ~10 min)
docker compose up -d osrm
```

Builds a routing graph from the **real Algerian road network** (OpenStreetMap
via Geofabrik, 285 MB extract → 1.6 GB graph). Free, self-hosted, unlimited, no
API key. Preprocessing peaked at **931 MB RAM**, so it runs comfortably on a
laptop.

Verified directly against the engine on `:5001`:

| Route | Result |
|---|---|
| Algiers centre → Bab Ezzouar | 13.7 km, 17 min |
| Algiers → Oran | **415.6 km, 4.7 h** |

Street names come back as real Algerian data in both languages
(`Rue Larbi Tebessi شارع العربي التبسي`).

### Ride search works

```
POST /v2/rideSearch  ->  searchId, 328 route points, 13687 m, 996 s
```

Those numbers come from the Algerian road graph, through
`rider-app -> maps-shim -> OSRM`, confirmed in OSRM's access log.

### Why a shim is needed at all

`osrm-config.sql` switches `get_distances`, `get_routes` and `snap_to_road` to
OSRM. Distances and snap-to-road are fine. Routes are not:

```
E500 INTERNAL_ERROR: Function getRoutes is not provided by service OSRM
```

`Kernel.External.Maps.Interface.OSRM` in shared-kernel `28bae0f` exports only
`callOsrmMatch`, `getDistances` and `getOSRMTable`. **There is no `getRoutes`
implementation.** So in this 2023 baseline, routing has to come from Google —
and `mock-google` has no Directions endpoint either (it implements
DistanceMatrix, PlaceName and SnapToRoad only).

**`maps-shim` is the answer** (`maps-shim/server.js`, ~150 lines, no
dependencies). It speaks Google's Directions API and answers from OSRM, so the
backend keeps thinking it is talking to Google. `osrm-config.sql` therefore
leaves `get_routes = 'Google'` and repoints `googleMapsUrl` at the shim.

Anything that is not `/directions/json` is forwarded untouched to mock-google,
so one `googleMapsUrl` still covers place names and autocomplete.

OSRM and Google use the same polyline encoding, so geometry passes through
unmodified; it is decoded only to compute step endpoints and route bounds.

The alternative was a paid Google key. This costs nothing and needs no card;
the trade-off is a component we own. If a future backend implements OSRM
routing natively, it can simply be deleted.

## Map tiles — the picture under the route

```bash
./tiles-prepare.sh             # build the tiles (one-off, ~10 min)
docker compose up -d tiles     # serve them on http://localhost:8035
```

OSRM gives us the route. It does not give us the *map* — the streets, water,
parks and labels drawn underneath it. That comes from vector tiles, and the
usual sources (MapTiler, Mapbox, Google) charge per map view and need an
international payment card.

**Decided 2026-08-06: host our own**, the same way we already host routing.

| | |
|---|---|
| Built with | [Planetiler](https://github.com/onthegomap/planetiler), OpenMapTiles schema |
| Input | the Algeria extract `osrm-prepare.sh` already downloaded — not fetched twice |
| Output | `algeria.mbtiles`, **309 MB**, zoom 0–14 |
| Features | 14.2 M, in 485 k tiles |
| Build time | ~10 min, peak heap 1.7 GB |
| Served by | `tileserver-gl` on `:8035` — tiles, style, and fonts from one origin |
| Cost | **€0**, no key, no request limit |

Verified by fetching tiles at computed coordinates — data inside the country,
nothing outside it:

| Place | z14 tile | Result |
|---|---|---|
| Algiers | 14/8331/6391 | 125 KB |
| Oran | 14/8163/6450 | 59 KB |
| Constantine | 14/8493/6413 | 51 KB |
| Annaba | 14/8545/6382 | 56 KB |
| Tamanrasset | 14/8443/7126 | 38 KB |
| Béchar | 14/8091/6673 | 31 KB |
| Tunis 🇹🇳 | 14/8655/6388 | **204, empty** |
| Oujda 🇲🇦 | 14/8105/6507 | **204, empty** |
| Bangalore 🇮🇳 | 14/11723/7596 | **204, empty** |

Oujda matters: it is a few km from the Algerian border, so it shows the cut
follows the border rather than a loose bounding box.

A rendered check of the whole chain:

```
http://localhost:8035/styles/basic-preview/static/3.0588,36.7538,13/800x600.png
```

returns Algiers with Bab El Oued, Casbah, Belcourt, Hydra, Kouba, the port and
the Barcelona ferry route, labelled in French.

**Note the tile URL is `/data/v3/{z}/{x}/{y}.pbf`,** not `/data/algeria/...` —
the id comes from the tileset metadata inside the MBTiles, not the filename.

### What is still rough

- The style is tileserver-gl's bundled *Basic preview*. It looks decent but it
  is not ours; colours and typography are someone else's defaults.
- **No sprite sheet**, so POI icons do not render — lines, areas and labels do.
- Labels use OSM's `name`. OpenMapTiles also carries `name:fr` and `name:ar`, so
  switching the map to one language is a style change, not a rebuild.

## Published on the internet — TLS, and the lock that had to come first

```
https://api.169-58-139-65.sslip.io/v2/...
```

Until now everything was reached through an SSH tunnel: safe, and useless for
handing an APK to anyone else. This publishes **exactly one thing** — the rider
API — over TLS, and nothing else.

### The lock went in first, and that ordering is the whole point

`POST /v2/auth` answers with `attempts: 3`, and the backend enforces nothing.
Measured on this stack: **62 consecutive wrong codes, the counter never moved,
and the same session still accepted the right code afterwards.** Four digits is
10,000 possibilities — about ten minutes.

Harmless while every port but SSH is shut. Not harmless one second after 443
opens. So `auth-guard` was built, deployed and **proved** while the stack was
still private, and only then did the edge go up.

| | Before | Now |
|---|---|---|
| Wrong codes per session | unlimited | **3**, then locked 15 min |
| Auth session lifetime | forever | **10 minutes** |
| Sign-ins per phone number | unlimited | **5/hour** |
| Requests per address | unlimited | 20/min on auth, 240/min otherwise |
| Full 10,000-code sweep | **~10 minutes** | **~28 days per number** |

The session lock alone would have been theatre: an attacker just asks for a new
`authId` and spends three guesses on that. **Throttling session creation is what
makes the session lock mean anything** — and it is the same control that stops
someone burning our SMS credit the day a real gateway exists.

```bash
./prove-lockout.sh https://api.169-58-139-65.sslip.io   # six checks, all must pass
```

### Why the guard is not in the backend

That is where it belongs — the counter already exists in the response and
enforcing it would be a few lines of Haskell. But this stack runs **prebuilt
binaries** from a CI job with a `timeout-minutes: 350` budget and a cache that
accumulates across runs. Rebuilding to change a counter means a multi-hour cycle
and a real chance of ending up with binaries that differ from the ones every
test so far has run against. When the backend is next rebuilt for another
reason, the check should move into it and the guard becomes belt-and-braces.

### What is exposed, and what is not

Verified by scanning from outside, not by reading the config:

| Open | Closed |
|---|---|
| 22 SSH · 80 redirect + ACME · **443 rider API `/v2/`, driver API `/ui/`, tiles** | Postgres, Redis, demo pages, mock-google, OSRM, auth-guard, **Swagger**, and the driver binary's **`/dashboard/` office routes** |

Swagger in particular is a complete, executable description of the API, and
there is no reason to publish it.

The driver binary serves 96 routes: 47 under `/ui/`, which is the driver's own
app, and **41 under `/dashboard/`**, which is the office — the API that enables a
driver, attaches his vehicle and reads his documents. Publishing `/ui/` must not
carry `/dashboard/` with it. Two independent things refuse it: nginx's catch-all
404, and the guard, which routes only prefixes it knows and 404s the rest.

**Every `/v2/` and every `/ui/` request goes through the guard.** That is not a
detail: an nginx `location` that reached either backend directly would quietly
undo the whole thing, so no such location exists.

`edge` runs with `network_mode: host` **on purpose**. A bridged container with
published ports bypasses ufw entirely — the trap that makes `ufw default deny`
insufficient on this box. Bound to the host directly, 80 and 443 are governed by
ufw like anything else.

### Certificate

Let's Encrypt, HTTP-01, issued with `--standalone`, renewed by the `certbot`
container over `--webroot` twice a day. nginx **reloads itself every six hours**
because it reads its certificate once at start — that is how a stack serves an
expired certificate two months after renewing it successfully.

Renewal was dry-run against staging before anything real was requested, and the
challenge path was proved reachable from outside. Two things worth knowing:

- Registered **without an email**, so there are no expiry warnings. Add one with
  `certbot update_account -m <address>` if you want the safety net.
- The hostname is `sslip.io`, which is public DNS resolving any
  `*.169-58-139-65.sslip.io` to this box. Swapping to a real domain is one
  `server_name`, one certificate, and nothing else.

### The map, published too

```
https://api.169-58-139-65.sslip.io/tiles/...
```

A release APK cannot reach a loopback tile server, and **Android refuses
cleartext HTTP in release builds** — so "just open 8035" was never an option
either. Both problems end at the same place: the tiles go through the same TLS
edge, read-only.

The exposure is bandwidth rather than data — it is one `.mbtiles` file we built
ourselves. A map view costs 1–3 MB the first time and close to nothing
afterwards, because the responses are marked `immutable` for a week: a tile at
a given z/x/y cannot change until we rebuild the whole extract, which is a
deliberate act. Rate-limited at 1200 r/min per address, which sounds enormous
and is not — one screenful at z14 is thirty-odd requests and panning fires
hundreds a minute, so a limit tight enough to *feel* like security would just
make the map stutter for real riders. Deleting the `location` block revokes the
whole thing in seconds.

**`--public_url` had to change at the same time.** The style document's
internal URLs — the vector source and the glyphs — are absolute and baked in by
the tile server, so a style served from the public host that still pointed
clients back at `localhost:8035` would load and then draw nothing.

Three things that cost time here, all worth knowing:

- **A single-file bind mount breaks if you replace the file.** Deploying
  `nginx.conf` with `tar -x` unlinks and recreates it, so the container keeps
  the old inode and serves stale config — while `nginx -t` passes and a reload
  reports success, because both are validating the config it still has. Use
  `scp` (which truncates in place), or recreate the container.
- **`expires` generates a `Cache-Control` of its own**, and the tile server
  sends one too, so the naive block emitted the header three times and left the
  client to choose. One `add_header`, with `proxy_hide_header` for the
  upstream's.
- The only font in the tileset is **Noto Sans Regular**. Asking for Open Sans
  returns a 400 that looks exactly like a proxy fault and is not one.

## Place search — finding somewhere to go

The map draws Algeria and OSRM routes across it, but until now nothing could
*name* a place in it. All three of the backend's geocoding calls were broken or
wrong, and the app cannot offer a "Where to?" without them.

```bash
./geocoder-prepare.sh            # extract, load, check   (~5 min, once)
./geocoder-prepare.sh functions  # reload the ranking only (instant)
./geocoder-prepare.sh check      # a few searches, to see it works
```

### What was actually broken

Confirmed by logging what the backend puts on the wire, not from documentation
— the request shapes are in shared-kernel, which is not in this repo:

| Rider endpoint | What the backend calls | Before |
|---|---|---|
| `autoComplete` | `GET /place/autocomplete/json` | **500** — mock-google implements the *new* Places API; the backend calls the legacy one |
| `getPlaceName` | `GET /geocode/json?latlng=` | 200, and answered *"Davangere, Karnataka, India"* for Algiers |
| `getPlaceDetails` | `GET /place/details/json` | **500** — mock-google has no such endpoint |

### The index

`geocoder-prepare.sh` reads the same `algeria-latest.osm.pbf` that already
feeds OSRM and the tiles, and builds **111,555 named things** into `geo.place`
in the Postgres the stack already runs — 132 MB including every index:

| | |
|---|---|
| points of interest | 75,207 |
| streets | 16,930 &nbsp;*(collapsed from 33,327 ways)* |
| neighbourhoods and towns | 13,919 |
| transport stops | 5,499 |

97% of rows carry a locality, which is the second line of every suggestion.

**Not Nominatim, Photon or Pelias**, and the reason is the data rather than the
software. All three are address-first, and addresses are the one thing this
extract does not have: 42,108 road ways in Algiers carry 5,204
`addr:housenumber` between them. *"12 Rue Didouche Mourad"* cannot resolve and
never will. What the data does have is street and landmark names, 97% and 96%
of them reachable by someone typing Latin characters — so the index is
landmark- and street-first by construction. Each of those three would also want
its own datastore (a dedicated PostgreSQL, or an Elasticsearch) on a box with
11 GB and seventeen containers already on it.

Names come out French-first: `name:fr`, then any Latin alternative, then the
primary `name`. Every variant including the Arabic goes into the match text, so
typing either script works even though the display is French.

### Ranking

`0.55 × text + 0.30 × proximity + 0.15 × importance`, with two matchers:
`LIKE '%q%'` for what is being typed (trigram similarity is hopeless at
prefixes — "did" against "rue didouche mourad" scores about 0.15) and pg_trgm
`%` for what was misspelled. *Bab Ezouar* finds **Bab Ezzouar**; *aeroport*
finds **Aéroport**.

Typical 15–50 ms. Getting there needed four fixes worth knowing about, because
none of them changed a single answer — only the time:

- **`jit = off`** on the functions. A 148 ms response was 20 ms of work and
  30 ms of JIT-compiling a query that runs in twenty.
- **`max_parallel_workers_per_gather = 0`**. The workers took longer to start
  than the extra cores saved.
- **Sphere, not spheroid** distance (`st_distance(a, b, false)`) — accurate to
  centimetres either way, for a number used to sort a list.
- **A local variable, not a CTE**, for the point in `geo.reverse`. PostGIS's
  KNN operator only uses the GiST index when one side is a constant or a
  parameter; behind a `with here as (...)` it silently scans and sorts every
  row. That one cost 200 ms a call and was invisible.

### Where it plugs in

`maps-shim` — the container that already speaks Google and answers from OSRM.
It now also answers the three geocoding paths from `geo.place`; anything else
still goes to mock-google untouched. No rider-app change, no config change: its
`googleMapsUrl` already points here.

The shim is built from `Dockerfile.maps-shim` rather than pulled, because it
needs a Postgres driver. The source stays a bind mount, so editing `server.js`
and restarting the container is still the whole loop. Unset `PG_URL` and the
three endpoints fall back to mock-google exactly as before.

### Limits worth knowing

- **No house numbers.** Deliberate — see above.
- **No distance in the results.** The shim sends `distance_meters`, but the
  shared-kernel these binaries were built from (`28bae0f`) has a legacy
  `Prediction` of `{description, place_id}` only. The app cannot show "1.4 km"
  until that moves. Ordering already puts near things first.
- **The backend asks for `country:in`** and there is no Algeria in its country
  enum (India, France, USA, Netherlands, Finland). The shim ignores the filter.
- **There is no FRENCH** in the backend's `Language` enum, so the app must send
  `ENGLISH`. Harmless — the index answers in French regardless.
- **Relations are skipped** by the extractor, so a few large named parks and
  campuses are missing.
- **A street's point is the average of its ways** within one locality. For a
  long street that is the middle of it, not the nearest end.

## Not connected yet: rider → driver

**Fixed.** A search from an Algerian number now comes back with prices:

```
POST /v2/auth  {"mobileCountryCode":"+213", ...}   ->  authId
POST /v2/rideSearch                                ->  13687 m, 996 s, 328 points
GET  /v2/rideSearch/{id}/results                   ->  4 estimates, 258 DZD
                                                       SUV, SEDAN, HATCHBACK, AUTO_RICKSHAW
```

`./setup.sh` asserts exactly this at the end (`verify_connector`), so a
regression fails the run rather than being discovered later.

### What was wrong, and why it was one problem

A BAP does not call a BPP directly. It posts `/search` to a **BECKN gateway**,
which looks participants up in a **registry** and broadcasts. The merchant row
had always said

```
gateway_url  = http://localhost:8015/v1
registry_url = http://localhost:8020
```

but neither had a service behind it. **Neither binary is in the published
image** — it ships `beckn-cli-exe`; `beckn-gateway` and `mock-registry` come
from a separate repository (`nammayatri/beckn-gateway`, a stack extra-dep) and
were not among the executables in `/opt/app`. Running them meant building them,
which was the same Haskell build that blocked the `+91` phone-number change. One
build unblocked both — see `.github/scripts/algeria/README.md`.

### Four more things had to be true

Getting the gateway running was necessary and not sufficient. Each of these
fails in exactly the same way from the passenger's side — a route, no price —
so none of them is diagnosable without reading the gateway and driver logs:

1. **The registry has to be seeded.** `atlas_registry` is not created by
   `mock-registry` itself; it comes from `sql-seed/mock-registry-seed.sql` plus
   the subscriber rows in `local-testing-data/mock-registry.sql`. Those rows
   already match this deployment exactly — BPP
   `JUSPAY.MOBILITY.PROVIDER.UAT.3` at
   `:8016/beckn/favorit0-0000-0000-0000-00000favorit` is precisely
   `atlas_driver_offer_bpp.merchant.subscriber_id` here — so nothing had to be
   written by hand.
2. **The driver side has its own geofences.** It shipped `{Karnataka}`, so it
   answered every Algerian search with
   `400 RIDE_NOT_SERVICEABLE — not serviceable due to georestrictions`, which
   the BAP has nowhere to display.
3. **The drivers were in Kochi.** No driver within the search radius means no
   offer. `setup.sh` now moves them to Algiers.
4. **`driver_location.point`, not `lat`/`lon`.** The pool query does its
   distance test on the PostGIS `point` column. Updating lat/lon looks entirely
   correct in psql and changes nothing — the pool stays empty and the search
   still returns no price.

All three signing parties use the same dev key, so the single public key in the
registry fixture is correct for all of them and signature auth works unmodified
(`disableSignatureAuth = False` throughout).

## The tariff — `./apply-tariff.sh`

```bash
./apply-tariff.sh          # applies algeria-tariff.sql AND clears the caches
```

Re-measured 2026-08-24, on both merchants, all four variants:

| App name | Variant | Start | Per km | Pickup | Max extra |
|---|---|---|---|---|---|
| Voiture | `SEDAN` | 150 | 45 | 70 | 300 |
| Scooter | `AUTO_RICKSHAW` | 100 | 35 | 50 | 300 |
| **Herbin** | `HATCHBACK` | **100** | **35** | **50** | 300 |
| Fourgon | `SUV` | 200 | 60 | 100 | 300 |

Set by the client on 2026-08-13, replacing the upstream Bangalore seed (10 / 12
/ 120) that made every vehicle cost the same 258 DZD. A 13.7 km trip is now
**629 / 836 / 1121** for hatchback / sedan / SUV.

**The names in the left column are the app's, and they are not the server's.**
The client replaced *Economy / Comfort / Premium* with four physical vehicle
types on 2026-08-21, and two of the four carry goods rather than people. The
enum is a compiled Haskell type with exactly four members, so each of ours is
pinned to one existing slot and the mapping is arbitrary — `HERBIN → HATCHBACK`
says nothing about hatchbacks. `Frontend`'s `lib/vehicle.ts` is the one place
that mapping lives. Renaming the enum properly is a rebuild.

**A herbin is priced exactly like a scooter**, because it inherited the row that
used to be *Economy*. A flatbed pickup and a two-wheeler on the same tariff is
not a decision anyone took; it is what the rename left behind. Raised with the
client 2026-08-24, along with the table above so he can name an increase rather
than guess at one. It is pure SQL — `fare_policy` is already one row per variant
and `apply-tariff.sh` applies it — so **no rebuild**.

**Both merchants carry identical rows**, checked the same day. That matters
because a tariff applied to one leaves half the fleet quoting the old price;
see the two-merchant note below.

The driver may add an extra, capped at roughly **half the fare** and growing
with distance — measured across three real searches:

| Trip | Economy | Comfort | Premium |
|---|---|---|---|
| 1.6 km | 205 → 280 (37%) | 291 → 366 (26%) | 394 → 469 (19%) |
| 7.4 km | 409 → 589 (44%) | 553 → 733 (33%) | 744 → 924 (24%) |
| 13.7 km | 629 → 914 (45%) | 836 → 1121 (34%) | 1121 → 1406 (25%) |

**The bands are identical for every category, and that is deliberate.**
Per-category caps were loaded first and the backend ignored them: three searches
with Economy/Comfort/Premium caps of 100/125/150, 180/245/330 and 250/335/450
came back `+125`, `+330` and `+450` — the *same* value for all three categories
in each search, taken from a different variant's row each time. The cap is
resolved once per search rather than per estimate.

So whatever cap is chosen applies to all three, which means it has to be sized
against the **cheapest** category or Economy goes over half. Each band is 50% of
the *Economy* fare at the band's lower bound; Comfort and Premium then sit
further under, which is the right way round.

**Never run the SQL on its own.** The driver service caches fare policies in
Redis and does not notice a row changing underneath it, so `psql -f` reports
success for every statement, the table holds the new numbers, and the app keeps
quoting the old ones. No error, nothing in any log. That is what
`apply-tariff.sh` exists to prevent.

There are **two** caches and they are not spelled alike:

```
driver-offer:CachedQueries:FarePolicy:*        the fares
driver-offer:CachedQueries:RestrictExtraFee:*  the cap on the driver's extra
```

The second is `RestrictExtraFee` while its table is `restricted_extra_fare` — a
scan for `*Fare*` misses it, so clearing only the first updates the prices and
silently leaves the driver's extra at its old value. Both are cleared by
pattern; never `FLUSHALL`, because the same Redis holds auth sessions and the
OTP lockout counters.

Two more things the table alone does not tell you:

- **There are two merchants** — `NAMMA_YATRI_PARTNER` and `OTHER_MERCHANT_2`,
  with 6 and 7 seeded drivers. Both dispatch, so a tariff applied to one leaves
  half the fleet quoting the old price. The SQL is deliberately not filtered by
  merchant.

  **And the rider sees every category twice.** The Beckn gateway multicasts each
  search to every BPP in the domain; both merchants live on the same driver-app
  instance, so both answer, and a search comes back with **eight** estimates —
  four variants at two different prices. Measured 2026-08-20.

  This is *not* the duplicated-seed bug of 9 August returning: `fare_policy`
  holds exactly one row per `(merchant, variant)`, so the unique indexes
  `dedupe-seed.sql` added are intact. It is two operators answering, which over
  Beckn is correct behaviour and is what the protocol is for. Checked with:

  ```sql
  SELECT merchant_id, vehicle_variant, count(*)
    FROM atlas_driver_offer_bpp.fare_policy GROUP BY 1,2;
  ```

  The passenger app keeps one row per tier, the cheaper of the two. If a single
  price list is wanted at the source instead, the fixture merchant can be taken
  out of the registry so it stops answering — but nothing depends on it being
  there, and nothing depends on it being gone either.
- **`base_distance_meters` is 0**, so the per-km charge runs from the first
  metre and the "starting price" is a flat charge on top. The seed had it
  covering the first 3 km. If the client meant the start to include some
  distance, that is the one value to change.

## The search radius — how "a car is near" is decided

The client's other question of 2026-08-24. Read out of the deployed Dhall
(`2023/Backend/dhall-configs/dev/dynamic-offer-driver-app.dhall`):

```dhall
{ minRadiusOfSearch = +5000      -- starts at 5 km
, maxRadiusOfSearch = +7000      -- grows to 7
, radiusStepSize    = +500       -- in 500 m steps, until it finds enough
, driverPositionInfoExpiry = Some +36000
}
{ driverBatchSize = +5 }
{ driverPoolBatchesCfg, singleBatchProcessTime = +60 }
```

So it is a plain radius that expands: 5 000 m, then 5 500, up to 7 000, taking
`driverBatchSize` drivers per round and giving each round
`singleBatchProcessTime` seconds to answer.

**There is no per-variant dimension, and no table to add one to.** The schema
carries `merchant_service_config`, `merchant_service_usage_config` and
`transporter_config` — and **no `driver_pool_config`**. The pool config is
selected by *trip distance*, not by vehicle variant. That settles the cost of
the client's request:

| | cost |
|---|---|
| Wider radius **for everyone** | edit the Dhall, restart — minutes, no rebuild |
| Wider radius **for herbin and fourgon only** | **a backend rebuild**, ~45 min |

**The display radius is wider than dispatch ever reaches.** `maps-shim/fleet.js`
answers `/fleet/nearby` with `DEFAULT_RADIUS = 8000`, a kilometre past the
7 000 m ceiling above, so a driver could appear in a list and sit outside the
range that would actually be asked. It is **unreachable today** — that list's
only caller was the passenger's driver picker, deleted with the prices screen on
2026-08-24 — but it is the shape that produces *"I chose him and nothing
happened"*, and it is one line in the shim if the picker ever comes back.

Careful with `singleBatchProcessTime`: it is the driver's answer window *and*
the batch pace. Raising it from 10 to 60 took three rounds of five drivers from
30 s to 180 s of the rider's 300-second search. A longer window spends the
**rider's** time.

## Driver freshness — `./drivers-keepalive.sh`

**The single most misleading failure in this stack.** The dispatch pool only
considers drivers whose recorded position is recent. Real drivers send one
constantly; the seeded ones are rows nobody updates. So a stack that worked
yesterday returns **zero estimates today, with no error anywhere** — empty
arrays, HTTP 200, nothing in any log — and it looks exactly like broken
dispatch.

Measured on 12 Aug: six drivers within 600 m of the pickup, every one invisible,
positions **1 day 21 hours** old. It has cost time twice.

```bash
./setup.sh drivers              # place them, once
./drivers-keepalive.sh install  # keep them visible, every 2 minutes
./drivers-keepalive.sh status   # is the timer up, how fresh are they
```

The timer only re-stamps rows `setup.sh drivers` already created — it does not
move anyone, so a driver placed by hand for a test stays where they were put.

**It is a demo prop, not a fix.** The real fix is a driver app sending real
positions, and on the day that exists this should be *deleted* rather than left
quietly keeping fictional cars alive next to real ones:

```bash
./drivers-keepalive.sh uninstall
```

Also worth knowing for anyone testing by hand: a rider only reaches drivers
whose **vehicle matches the variant they picked**, and the seeded fleet is 9
auto-rickshaws, 2 sedans, 1 hatchback and 1 SUV. "SUV" therefore reaches exactly
one driver, and dispatch will look unreliable for reasons that are nothing to do
with dispatch.

## The driver API — and why the source tree lies about it

Proven end to end against the running server on 13 Aug. Read this before writing
any driver code, because **the checked-out source describes a different system**.

### The trap, first

`Backend/dev/local-stack/bin/MANIFEST.txt` records the build ref the deployed
binaries came from: `03a7531`. That ref **is an ancestor of this branch's HEAD**
— the running backend is *older* than the tree you are reading, and on this code
path the two disagree completely.

Read the current source and you conclude driver positions come from the
**location-tracking service**, a separate Rust binary that is not in
`docker-compose.yml`. `Storage/Queries/DriverLocation/Internal.hs` calls
`LF.nearBy`, which unconditionally calls it, and there is no database fallback.
Taken at face value that means a whole extra service must be deployed before a
driver app is possible at all.

It is wrong. The running binary still has `POST /ui/driver/location`, which the
current source deleted, and it writes `atlas_driver_offer_bpp.driver_location`
in Postgres directly. Three independent confirmations:

```bash
# 1. the string is in the deployed binary and not in the tree
strings -n 6 bin/dynamic-offer-driver-app-exe | grep -i 'Domain.Action.UI.Location.UpdateLocation'

# 2. drivers-keepalive.sh measurably works, and all it does is UPDATE that table
# 3. a live POST moved a real driver's row within two seconds
```

**So: for the driver side, the binary is the authority, not the source tree.**
The binary publishes its own route list, which is the reference to use:

```bash
curl -s http://localhost:8017/openapi | python3 -m json.tool | less
```

### The routes, as they actually exist

All under `/ui`, on port **8017** (8016 inside the Docker VM).

```
POST /ui/auth                                  merchantId is the merchant UUID,
                                               NOT the short id — the rider side
                                               wants the short id, which is why
                                               this is so easy to get wrong.
                                               A number with no driver CREATES one.
POST /ui/auth/{authId}/verify                  otp 7891
POST /ui/driver/setActivity                    go online / offline
GET  /ui/driver/nearbyRideRequest              poll for incoming requests
POST /ui/driver/searchRequest/quote/offer      the driver's fare
POST /ui/driver/searchRequest/quote/respond    accept / decline
POST /ui/driver/ride/{rideId}/arrived/pickup
POST /ui/driver/ride/{rideId}/start            the rider's OTP
POST /ui/driver/ride/{rideId}/end
POST /ui/driver/ride/{rideId}/cancel
GET  /ui/driver/ride/list
POST /ui/driver/location                       position — see below
GET  /ui/driver/location/{rideId}              NO AUTH. This is what the rider
                                               app uses to track the driver.
```

### `POST /ui/driver/location`, and its one nasty property

Header `token`. Body is a **non-empty array**:

```json
[ { "pt": {"lat": 36.7574, "lon": 3.0588}, "ts": "2026-08-13T13:39:31Z", "acc": 8.0 } ]
```

Measured: batching works and the **last point wins**; the rate limit is 100/s,
so cadence is a client battery decision and not a server constraint.

**`ts` comes from the phone, and a point not newer than the stored one is
dropped — while still answering `200 Success`.** A driver whose clock is behind
reports healthily forever and never moves. Nothing distinguishes this from
working correctly except watching the row. Count fixes and successful posts
separately in any client; the two agreeing proves nothing.

### Reachable from a phone — `./enrol-driver.sh`

`/ui/` is published on 443 since 2026-08-18. It was not, for a long time, and the
two reasons are worth keeping because they are what the enrolment script exists
to answer.

Driver auth **creates a driver for any unknown number** — measured, not assumed:
one `POST /ui/auth` with a number nobody had ever seen produced a `person` row.
And the code is not merely guessable, it is *fixed*: `useFakeSms = Some 7891`, so
`0000` and `1234` are refused and `7891` is accepted, for everyone. Published as
it stood, anyone who knew a driver's phone number owned his shift and his
earnings.

There is no SMS gateway to turn the fake one off with, so the guard supplies the
missing half instead:

- **A number not enrolled is refused at `POST /ui/auth`**, before the backend
  hears about it, so no record is created for a stranger.
- **Each enrolled number has its own six-digit code.** The guard checks it
  against a salted hash and only then rewrites the body to `7891` before
  forwarding. The fixed code is dead from the internet — it spends an attempt
  and never reaches the backend.
- Three wrong codes lock the session for fifteen minutes; five sign-in starts per
  number per hour.

```bash
./enrol-driver.sh 0551234567 "Karim Benali"   # enrol, print a code once
./enrol-driver.sh --set 0551234567 482913     # set a chosen code
./enrol-driver.sh --list                      # who may sign in
./enrol-driver.sh --revoke 0551234567
```

The code is printed once and stored hashed — it cannot be read back. That fits
how the pilot onboards: the agency enrols a driver face to face and hands him the
number. When a real gateway exists, the guard generates and sends a code per
sign-in through the *same* substitution; only where the code comes from changes.

Three things that bite:

- **The trunk zero is part of the key.** The guard keys on
  `mobileCountryCode + mobileNumber` — `+2130551234567`, not `+213551234567`.
  The script normalises for you; hand-editing the file does not.
- **Enrolling is not enabling.** A freshly enrolled driver signs in and sees that
  he is waiting for approval. Enabling him and attaching a vehicle are
  `/dashboard/` operations, and `/dashboard/` is not published.
- **Six digits, not four.** The guard allows three attempts, so six digits makes
  guessing pointless rather than merely slow — but the driver sign-in screen must
  accept six where the passenger one accepts four. They are different screens.

`auth-guard/driver-codes.json` is **not in git** and is in the backup set. Losing
it means re-enrolling every driver.

**There is no working "resend".** `POST /ui/auth/otp/{id}/resend` answers 500 on
this stack — there is nothing to resend through. The guard refuses it outright on
`/ui/` rather than forwarding, because a personal code does not change. The
driver sign-in screen must not offer the button. (The passenger app *does* offer
it; it fails honestly with "Impossible d'envoyer le code pour le moment", which is
accurate, and it has never once succeeded.)

## Driver documents — why `register/*` is deliberately never called

**`POST /ui/driver/register/validateImage` sends the photo to India.** Not
figuratively: the route stores nothing itself, it forwards the image to
**Idfy**, an Indian document-verification service, and returns Idfy's verdict.
The only document types the binary knows are `ind_driving_license` and `ind_rc`
— *ind* as in India — so it could not read an Algerian licence even if we paid
for it.

Measured 2026-08-18. Left alone it answers:

```
500 IDFY_ERROR: ConnectionError … Connection refused
```

because `idfyCfg.url` is `http://localhost:6235` — a mock upstream expects for
local development and this stack has never run. So **nothing has ever left the
country**, and that is luck rather than design: real Idfy credentials in that
config would send every driver's licence to a third party abroad.

### The decision, 2026-08-19

The client's rule is that driver documents, vehicle model, colour and the rest
go to **our own admin website**, and that nothing touches an Indian service.

So the app does **not** call these routes at all:

| Route | Why not |
|---|---|
| `POST /ui/driver/register/validateImage` | forwards the image to Idfy |
| `POST /ui/driver/register/dl` | needs an `imageId` only Idfy can issue |
| `POST /ui/driver/register/rc` | same |
| `GET /ui/driver/register/status` | reports Idfy's verdict, so it will read `NO_DOC_AVAILABLE` for ever |

Documents will go to a service of ours, into our own storage, read by the admin
site when it exists. Until then the agency collects papers the way it already
does, and enables the driver from the office side — which it has to do anyway,
because **attaching a vehicle is an office operation** (`POST /ui/org/vehicle/…`
answers `403 ACCESS_DENIED` to a driver's own token).

The consequence to hold on to: **D7 must read our store, never
`register/status`.** The backend's verification fields stay empty by design, and
a screen that trusted them would tell every driver his file had not arrived.

### The contract, if it is ever needed again

Mapped from the binary rather than from Idfy's public documentation, which
describes a newer service. Kept because rediscovering it cost an evening.

```
POST /v3/tasks/sync/validate/document
headers   api-key, account-id
body      { task_id, group_id, data: { doc_type, document1: <base64> } }

reply     decodes as Idfy.Types.Response.IdfyResponse:
          action, task_id, group_id, request_id, status, type,
          created_at, completed_at,
          result: { detected_doc_type, readability { is_readable, confidence },
                    source_output, extraction_output }
```

`action` and `created_at` are the two whose absence produces
`DecodeFailure … key "…" not found` and a 500 that reads, from the app, as *the
service is unreachable* rather than *a field is missing*. The readability
verdict was never made to come back positive: `true`, `"yes"` and `1` all
produced `400 IMAGE_NOT_READABLE`, so the value it wants is still unknown. It
does not matter now, and it is written down in case it ever does.

There is also a webhook, `POST /service/idfy/verification`, for the asynchronous
path. Unused for the same reason.

## A ride from the driver's side — measured, and where `/openapi` is wrong

Everything below was read on 19 August from the deployed binary and from the 164
real search requests, 66 quotes and 41 rides sitting in
`atlas_driver_offer_bpp`. It is written down because the driver app is being
built against it, and because **one part of it contradicts the server's own
published schema**.

### The timings

| What | Measured | Sample |
|---|---|---|
| Time to answer a request | **a config value** — see below | 164 requests |
| Quote validity once offered | **60 s**, no exception | 66 quotes |
| Rider's time to choose | median **3 s**, p90 18 s, max 50 s | 41 bookings |
| Ride visible after the rider picks | **0–1 s** | 41 rides |
| Winning the ride → passenger aboard | median **85 s**, p90 111 s | 32 rides |
| Distance to the pickup | avg **1 588 m**, max 4 391 m | accepted requests |

**No rider has ever chosen after the 60 s quote expiry** — that deadline is
real, so the app may call an offer lost on its own clock, which it has to,
because losing is silent (see below).

### A bare search already reaches drivers — measured 2026-08-24

**`POST /v2/rideSearch` puts a request on drivers' phones by itself.** Nothing
needs to be selected afterwards. With 19 drivers online carrying fresh
positions, one search created three `search_request_for_driver` rows **0.49 s
later**; across five searches the first row landed at 0.25, 0.30, 0.31, 0.49 and
2.72 seconds. Dispatch happens at *search* time, not at *select* time.

That is worth knowing before any client is made to search more often than a
person asks it to. The passenger app's pickup screen prices itself now instead
of waiting for a tap, and because panning the pin invalidates the price, it
would re-search on every pan — so it waits 900 ms for the map to settle and
ignores a pin that moved less than 30 m. Without those two guards one
indecisive rider notifies every driver in range once per nudge: invisible in
testing with one driver, unbearable with forty.

Proving this took three attempts and the first two were unreadable, which is the
lesson worth keeping: a before/after row count is only evidence if nothing else
touched the table in the window, and two probe runs a minute apart both did.
**Take a `now()` watermark immediately before the request** and count rows after
it. Also check that drivers were actually online first — `driver_information`
and a fresh `driver_location` — or a zero measures an empty stack rather than a
quiet route.

### The pickup threshold the server keeps for itself

`transporter_config.pickup_loc_threshold` is **500 m**, alongside
`drop_loc_threshold` 500. That is the distance the backend still treats as being
at the pickup. There is **no arrival threshold and no `TOO_FAR` error code
anywhere in the driver binary**, so how close a driver must be before *Je suis
arrivé* lights up is the app's own choice — it was 100 m, raised to 300 on
2026-08-24 after the client watched it stay dead at 114 m. Anything past 500
would start arguing with the server.

### The answer window — and how this line was wrong twice

This row said **16,3 s** on 18 August and **10 s, no exception** on 19 August.
Both were reading the same rows from opposite ends, and neither is the window.

It is a **Dhall setting**, `singleBatchProcessTime`, not a measurement:

```haskell
-- SendSearchRequestToDrivers/Handle/Internal.hs:101
searchRequestValidTill = singleBatchProcessTime `addUTCTime` now
```

`now` there is when the *dispatcher wrote this driver's row*. `startTime` is
when the **rider** searched, seconds earlier — and a whole batch-length earlier
again for the second batch of drivers. So the two anchors answer different
questions:

| Anchored on | Gives | Which is |
|---|---|---|
| `startTime` | 12–40 s, spread | the setting **plus** dispatch latency **plus** the batch offset |
| the row's own `createdAt` | exactly the setting | the window the driver actually has |

The full spread over every request this database has recorded, on `startTime`:

```
12s x4   13s x10  14s x29  15s x38  16s x23  17s x6
18s x16  19s x12  22s x1   23s x3   24s x4   25s x1
28s x3   30s x3   33s x4   36s x1   38s x3   40s x3
```

So "16,3 s" was one batch-one row and "10 s" was the setting — both true about
what they measured, and both wrong as a statement about the driver's window.
**A client must read `searchRequestValidTill` against its own clock** and derive
nothing from `startTime`, which is what the driver app now does.

### Changing it — `./apply-search-window.sh`

```bash
./apply-search-window.sh --show     # what it is now
./apply-search-window.sh 60         # give the driver a minute
./probe-search-window.py            # prove it took, on a real request
```

Raised from 10 s to **60 s on 2026-08-20**, on the client's instruction after
driving the app: ten seconds at the wheel is the time for two glances, not for
a decision.

**The same value paces the batches, and that is the cost.** The request goes to
`driverBatchSize` drivers at a time for `maxNumberOfBatches` rounds, one
`singleBatchProcessTime` apart — seeded here at 5 and 3:

```
at 10 s   batch 1 at 0s, batch 2 at 10s, batch 3 at 20s   -- all asked within 30s
at 60 s   batch 1 at 0s, batch 2 at 60s, batch 3 at 120s  -- all asked within 180s
```

So a longer window buys the driver time and spends the **rider's**: if the first
five drivers ignore the request, nobody else is asked for a full minute. The
rider's own search lives 300 s, so 3 × 60 still fits with room — but 60 is about
the largest value that comfortably does, and the script refuses anything over
100. If the rider's wait becomes the louder complaint, 30 is the middle setting.

> **It is a script because `2023/` is gitignored.** That tree is fetched by
> `setup.sh`, so an edit made by hand on the server is silently undone the next
> time it is refreshed and the window drops back to 10 s with nothing to show
> why. Re-run it after any `setup.sh` that refetches. Same reason
> `apply-tariff.sh` and `apply-fcm.sh` exist.

### `driverMaxExtraFee` must be read, never computed

`fare_policy` says a flat `driver_max_extra_fee = 300` for all four variants.
The requests actually sent to drivers say otherwise:

```
   10 DZD ×1     110 DZD ×27     300 DZD ×1
   20 DZD ×9     145 DZD ×8      335 DZD ×8   ← above the policy's own ceiling
   75 DZD ×15    285 DZD ×95
```

`offeredFare` is the **supplement**, not the total — sending the total answers
`EXTRA_FEE_NOT_ALLOWED`.

~~`driverMinExtraFee` is **0 on all 164**~~ — **not quite, corrected
2026-08-20.** It is 0 on **159 of 169**, and **10 DZD on the other ten**, all
issued between 9 and 13 August, i.e. under the tariff that
`apply-tariff.sh` replaced. The current Algerian policy declares
`driver_min_extra_fee = 0` for all four variants and both merchants, so the
floor is zero *today*. Read it off the request anyway: a step computed below it
is refused, and the field is one `apply-tariff.sh` away from being non-zero
again.

**And the supplement path is not unexercised.** The D11 design page said
`offeredFare` "has never been sent to this server" and recommended testing it
before building the screen. `fare_parameters` disagrees:

```sql
SELECT driver_selected_fare, count(*) FROM atlas_driver_offer_bpp.fare_parameters
 GROUP BY 1;   -->   0 x65,  120 x2
```

Two rides from 16 August carry a 120 DZD supplement — sent, accepted, and
carried through fare calculation into the ride. Not the fleet simulator, which
omits the field entirely; a manual test. The path works.

`./probe-driver-offers.sql` is where all of this is read now, and it is a file
rather than a shell one-liner because these figures have been quoted into design
documents twice and been wrong twice, both times from the query being retyped
slightly differently.

### The trap: `/start` needs a code that `/openapi` does not mention

```
/openapi says          StartRideReq { point }
simulate-driver sends  { "rideOtp": "4821", "point": {…} }   → the ride starts

in the binary          rideOtp · RideOtp · IncorrectOTP · INCORRECT_OTP
in the database        ride.otp — 45 rides, 45 distinct codes, 4 digits each
                       all numeric, and the lowest is 0677 — A LEADING ZERO
```

The passenger reads a four-digit code off his phone and the driver types it.
**A client written from the published schema builds a start button with no code
field and every ride fails**, with the driver standing in front of the
passenger. This is the "ask the server, not the tree" rule again — except here
even the schema *generated by* the server is incomplete.

**The code is four characters, not a number.** `0677` is in the data. Held as a
number, trimmed, or reformatted it reaches the server as three digits and is
refused — roughly one ride in ten, while the driver reads the right digits aloud
off the passenger's screen. Same family as the trunk zero on `+213` numbers.

**There is no attempt limit on it.** `IncorrectOTP` and `INCORRECT_OTP` are in
the binary, but the only attempts counter it carries is
`RegistrationTokenAttempts`, which belongs to sign-in — the one that locks on the
third try. Nothing equivalent guards `ride/start` and the ride carries no counter
column, so a client may let the driver retry as often as he needs.

Two shapes that differ across the three calls of that leg, with nothing
announcing it:

| call | body |
|---|---|
| `POST .../arrived/pickup` | `{lat, lon}` — bare, at the top level |
| `POST .../start` | `{rideOtp, point: {lat, lon}}` |
| `POST .../end` | `{point: {lat, lon}}` |

`arrived/pickup` is advisory: it only writes `ride.driver_arrival_time`, absent
on 11 of 45 rides, so nothing should block on it.

**Cancellation reasons are a product decision.** `CancellationReasonCode` is a
bare string with no enum and the server stores what it is sent;
`additionalInfo` round-trips. But of 12 cancellations **8 are `ByUser` with a
null `reason_code`** and only 4 are `ByDriver` — so a reason list, however good,
only ever explains the smaller half of the failures.

### What the driver is not given

`DriverRideRes` carries `riderName` and nothing else about the person. There is
**no phone number on any `/ui/` route**. `customerPhoneNo` exists in the binary
only inside `RideInfoRes` and `RideListItem` — dashboard types, behind
`/dashboard/`, which is deliberately not published.

So a driver at an empty address cannot call anyone. His only move is to cancel,
which is why cancellation matters more than it looks: **8 of 41 rides were
cancelled**, four by the driver and four by the rider.

`CancellationReasonCode` is declared as a bare string with no enum — the server
stores whatever is sent, and all four driver cancellations so far say `OTHER`.
The list of reasons is therefore a product decision, not a technical constraint,
and it is the only data the agency will ever have on why rides fail.

**Seeded 2026-08-24.** `GET /ui/cancellationReason/list` had never returned a
row; it now returns six, applied with
`./apply-migration.sh cancellation-reasons.sql`:

| priority | code | what the driver reads |
|---|---|---|
| 1 | `PASSENGER_NO_SHOW` | Le passager n'est pas venu |
| 2 | `ADDRESS_NOT_FOUND` | Adresse introuvable |
| 3 | `PASSENGER_CANCELLED` | Le passager a annulé sur place |
| 4 | `VEHICLE_PROBLEM` | Problème de véhicule |
| 5 | `TOO_FAR` | Le passager est trop loin |
| 9 | `OTHER` | Autre — opens a free-text box in the app |

These words are now the vocabulary of every report the agency will ever run, and
changing them later cuts the history in two. `enabled = false` retires one
without losing the rows already recorded against it.

The app still ships five of its own, used only when this route answers `[]`.
That fallback is now dead weight worth keeping — and note its third code is
`PASSENGER_CANCELLED_ON_SITE` where the table says `PASSENGER_CANCELLED`. No
history splits on it, because no driver cancellation has ever used either: all
four on record say `OTHER`.

### Losing an offer — recorded by the server, exposed by nothing

**This section said losing was silent, and that the server sends nothing. Both
halves were wrong, and only the practical conclusion survives.** Corrected 20
August against the live database and the published route list; the numbers come
from `./probe-driver-wait.sql`.

The server records a loss precisely. `Domain/Action/Beckn/Confirm.hs` sets every
non-winning driver's `search_request_for_driver.response` to **`Pulled`** and
sends each of them `notifyDriverClearedFare` — FCM `CLEARED_FARE`. In the data:

| the driver's row, after the passenger decided | count |
|---|---|
| won — `response=Accept`, request `Inactive`, quote `Inactive` | 43 |
| lost to another driver — `response=Pulled` | 22 |
| lost some other way (search cancelled or expired) | 3 |

So **26 of 69 offers (38 %) never became a ride**, and 22 of the 25 concluded
losses are literally "another driver was chosen".

What is true is that **no `/ui/` route exposes any of it.** All twenty driver
routes the binary publishes were enumerated from `/openapi`; none returns a
driver's own quotes, and none reports `Pulled`. So a client still cannot ask
"did I lose".

**But it can observe that the search ended.** Confirming sets the whole search
inactive in one transaction, so the request leaves `nearbyRideRequest` at the
instant the passenger decides — for the winner and every loser alike. That says
the search is over, not which way it went; one
`GET /ui/driver/ride/list?onlyActive=true` says which. Two traps sit in that,
both of which produced a wrong verdict in testing:

- **The ride row lags the booking**, 0 s on 31 assignments, 1 s on 11 and 3 s on
  one. Inside that gap the request is gone and no ride exists yet, which is
  indistinguishable from losing. The app waits 10 s before concluding.
- **A row leaving the list is not always a decision.** `nearbyRideRequest`
  selects on `searchRequestValidTill > now`, so it also ages out at the end of
  the *answer* window — while the quote has its own fresh 60 s from the press
  and the passenger's search runs 300 s. Only a disappearance *before* that
  deadline counts as a verdict.

### The offer's own life, which is not the answer window

`driver_quote.valid_till - created_at` is **60 s on all 69 quotes**, and no
booking in 43 has ever landed after it. The driver's *answer* window is also 60 s
today, and the two are different settings that merely agree: the answer window is
`singleBatchProcessTime`, moved from 10 s on 20 August, and the three quotes
issued since that change are still exactly 60 s. Anything deriving one from the
other is right today and wrong after the next `./apply-search-window.sh`.

How long the passenger takes to choose, over the 43 assignments: fastest 0 s,
**median 4 s**, mean 7 s, nine times in ten under 18 s, slowest 50 s.

### Driver push is configured — this was recorded as unverified

`atlas_driver_offer_bpp.transporter_config` carries `fcm_url`,
`fcm_service_account` and `fcm_token_key_prefix` (note: **not** on `merchant`,
which is where the rider side keeps them). It points at
`https://fcm.googleapis.com/v1/projects/movin-dz/messages:send` with a real
3 152-character service account — `./apply-fcm.sh` did both sides on purpose,
and its header says why. The binary carries `NEW_RIDE_AVAILABLE`,
`DRIVER_QUOTE_INCOMING` and `CLEARED_FARE`, and **26 of 33 driver rows already
hold a device token**.

What is still unproven is delivery to a real driver handset, and the driver app
does not yet register its token. So push is an enhancement on top of the polling
above, not a prerequisite — the wait screen must be correct without it.

## Two test drivers, at the two ends of the journey

Kept as a pair because one account cannot show both paths: the duty screen sends
a driver with no vehicle back to his file, correctly, so the working loop is
unreachable from an unapproved account.

| Number | State | Vehicle |
|---|---|---|
| `0555000001` | not approved — lands on the file screen, must file papers | none |
| `0555000002` | approved, `Yacine` — lands on the duty screen, can go online | SEDAN · Hyundai Accent Blanc · `06182 118 16` |

Their personal codes are **not written here**: the guard keeps a salted hash and
prints a code once, and this file is in git. `./enrol-driver.sh --list` shows
who is enrolled; `--set <number> <code>` sets a new one.

To approve a driver the way the agency does — both switches, plus the vehicle
that dispatch actually matches on:

```sql
UPDATE atlas_driver_offer_bpp.driver_information
   SET enabled = true, verified = true, blocked = false WHERE driver_id = '…';

INSERT INTO atlas_driver_offer_bpp.vehicle
  (driver_id, capacity, make, model, variant, color, registration_no,
   merchant_id, vehicle_class, created_at, updated_at)
VALUES ('…', 4, 'Hyundai', 'Accent', 'SEDAN', 'Blanc', '06182 118 16',
        (SELECT merchant_id FROM atlas_driver_offer_bpp.vehicle LIMIT 1),
        '3WT', now(), now());
```

~~`enabled` and `verified` are separate switches and the pool skips a driver
missing either.~~ **Not true, and worth knowing exactly which of the three
matters where.** Read from `Storage/Queries/Person.hs` and
`Domain/Action/UI/Driver.hs` at `03a7531`, the ref these binaries were built
from:

| Column | Read by | Effect |
|---|---|---|
| `blocked` | `setActivity` **and** `getNearestDrivers` | cannot go online, and skipped by the pool |
| `enabled` | `setActivity` only | cannot go online — `DRIVER_ACCOUNT_DISABLED` |
| `verified` | **nothing at all** | none |

`getNearestDrivers` filters on role, merchant, `active`, not-`blocked`, position
freshness and vehicle variant. It never looks at `enabled` or `verified`.

Three consequences.

**A driver created by `POST /ui/auth` starts `enabled = false`**, which is why
enrolling is not enabling. Watch out for the near-miss here: there are **two
functions called `createDriverDetails`**, and they disagree. `Registration.hs`
— the self-signup path `/ui/auth` uses — writes `enabled = False`. `Driver.hs`
— the office path — writes `enabled = True`. Reading the wrong one produces the
confident and wrong conclusion that a fresh driver can work immediately.

**`verified` is not what makes an account work.** Nothing in the backend reads
it. The app uses it as *"the agency has checked the papers"*, which is a product
convention this stack invented; the approve SQL above must therefore keep
setting it, or D7 holds a driver who could actually work.

**Disabling by SQL does not put a driver offline.** The dashboard route does
(`changeDriverEnableState` calls `updateActivity … False` when disabling), but
`/dashboard/` is not published here, so accounts are switched off with raw SQL —
and that leaves `active = true`. `setActivity` is a gate, not a leash: nothing
revokes a flag already set, so he keeps receiving work until he next toggles.
**Set `active = false` in the same statement.**

`registration_no` is unique — reusing a plate fails the insert.
`vehicle_class = '3WT'` is copied from the fleet rows known to dispatch
correctly; it reads wrong for a sedan and is an upstream artefact.

## Playing a driver — `./simulate-driver.py`

```bash
./simulate-driver.py seed      # one Algerian driver per row the app sells
./simulate-driver.py status    # who exists, who is online, how fresh
./simulate-driver.py once      # take the next request, drive it, finish
./simulate-driver.py daemon    # all three online, keep accepting
```

Runs **on the server** — `/ui/` is loopback-only, for the reason above.

There is no driver app, and screens 10–13 of the passenger app cannot be built
or demonstrated without something on the other side. This drives the real
endpoints against the real backend, so what the app sees is what it will see in
production. It also **drives the actual OSRM route**, which is the part that
makes a moving car on the passenger's map testable rather than imagined.

```
11:51:28 HATCHBACK driver taking a request
11:51:28   accepting 8003ac71 -- 14.0 km, base 641 DZD
11:51:32     ride h36FJtMGuf assigned
11:51:32     to the pickup: 35 points, 4.1 min of real driving
11:51:36     started with the passenger's code 2240
11:51:36     to the destination: 329 points, 19.3 min of real driving
11:51:56     finished -- 641 DZD
```

`--speed` is a multiplier on real driving time: `1` is real time (19 minutes for
the standard 14 km test trip), `0` teleports, and the default `8` is roughly
demo pace. `--decline N` turns down the first N requests so that path can be
built too. `--variant` restricts `once` to one row.

### The one shortcut, kept visible

It reads the ride OTP out of Postgres. A real driver is told the code by the
passenger, and `/ui/driver/ride/list` deliberately does not carry it. That is
the entire difference between this and a real driver, and it is better stated
than hidden behind something that looks complete.

### Why `seed` exists — dispatch matches on vehicle variant

**A search only ever reaches drivers whose vehicle variant matches the estimate
the rider picked.** Before this, the only Algerian driver was a `SEDAN`, so of
the three rows the app sells:

| Row | Variant | Before | After `seed` |
|---|---|---|---|
| Economy | `HATCHBACK` | nobody | `0551234568` |
| Comfort | `SEDAN` | `0551234567` | unchanged |
| Premium | `SUV` | nobody | `0551234569` |

Two of the three rows spun for the full 300 s and returned nothing, **with no
error on either side** — it presents exactly like broken dispatch. The remaining
seeded drivers are upstream's, with `+91`/`+94` numbers that driver auth rejects
outright (`mobileCountryCode matches regex /^\+213$/`), so nothing can log in as
them.

`0551234567` is left as a `SEDAN` on purpose: he is the driver every earlier
probe was proven against, and `setup.sh`'s smoke test recreates him on login.

### Keep it running — `./fleet-service.sh`

```bash
./fleet-service.sh install     # run the fleet, and keep it running
./fleet-service.sh status      # up? and what has it done lately
./fleet-service.sh uninstall   # stop and remove
```

**Cars on the map and no offers is this, every time.** The two are produced by
completely different things and only one of them was ever automated:

| The rider sees | Needs |
|---|---|
| Estimates, and cars drawn on screen 10 | fresh rows in `driver_location` — the `movin-drivers` timer does this every 2 min |
| An actual **offer** | a *process* polling the driver API and answering — `simulate-driver.py daemon` |

So with the timer running and the simulator not, a search succeeds, prices come
back, screen 10 draws three cars near the rider — and then nobody ever bids. It
looks exactly like broken dispatch, and it is not: there is simply no driver.

That state persisted for hours at a time because the simulator had only ever
been started by hand, usually wrapped in `timeout`, so it always died later.
`fleet-service.sh install` makes it a systemd unit with `Restart=always`, so it
survives a reboot, a crash, and a stack restart.

Stopping it is clean: the script turns `SIGTERM` into the interrupt its own
cleanup handles, so the drivers go **offline** rather than being left online
with positions that then go stale.

### Two behaviours worth knowing before changing this

**A declined request keeps appearing.** After `respond` with `Reject`, the same
search stays in `nearbyRideRequest`. Poll, decline, poll again and you will be
handed the one you just refused; accepting it then fails with
`QUOTE_ALREADY_REJECTED`. The simulator remembers what it declined.

**Killing it leaves the drivers online.** `finally` does not run on `SIGTERM`,
so `timeout`, `docker stop` or a systemd restart used to leave the fleet
marked online whose positions then went stale — which is the silent
zero-estimates failure in [Driver freshness](#driver-freshness--drivers-keepalivesh)
all over again. `SIGTERM` and `SIGHUP` are now turned into the interrupt the
cleanup already handles.

While it is running it also heartbeats its own drivers' positions every 30 s,
so for those six it does `drivers-keepalive.sh`'s job.

### An unfinished ride locks that rider out — `./simulate-driver.py finish`

This is the most expensive trap in the whole stack, because the symptom points
squarely at the wrong thing.

**One ride left open ends every future booking for that account.** Confirming
any new quote while a booking is still open answers

```
E400 INVALID_REQUEST: ACTIVE_BOOKING_PRESENT
```

and nothing else about the flow changes. The search runs. Estimates come back.
Drivers offer. Cars appear on the map. Every single tap is refused.

Measured 2026-08-18: a test booking from **10 August** sat in `TRIP_ASSIGNED`
for eight days. On the 18th a tester tapped five different drivers, watched
nothing happen five times, and reported the app's button as dead. The server had
said exactly what was wrong on all five, in the log, at the time.

```bash
./simulate-driver.py finish --speed 0     # close out everything hanging
```

It completes rather than cancels: the real end-of-ride path runs, the rider gets
a finished trip in their history, and screen 14 has something to rate. `--speed 0`
teleports, so a fossil costs about two seconds.

The daemon will never do this for you. `my_active_ride` ignores anything older
than 30 minutes, deliberately, so a fossil cannot hijack a live session — which
is correct there and the reason `finish` is separate.

**Two things this trap taught, both now fixed in the script:**

*Logging in as a driver revokes that driver's other session.* One session per
user applies to drivers exactly as it does to riders. So running `finish` while
`movin-fleet` is up used to pull the daemon's tokens out from under it — and the
daemon could not tell, because a 401 made `poll()` return `None`, which is what
it also returns when there is simply no work. It looped silently for sixteen
minutes, `systemctl status` said `active` the whole time, and searches came back
with cars on the map and no offers. It now recognises a revoked session and
signs back in.

*`run_ride` could not resume an `INPROGRESS` ride.* It called `arrived/pickup`
and `start` unconditionally, and `start` on an already-started ride is not a 200
— so it gave up and returned `False`. That is exactly what a daemon restart
mid-trip produces, which means the fix for ghost rides was also quietly creating
them.

**The daemon is single-threaded.** `run_ride` blocks the entire loop, so while
one driver is driving, *no* driver answers anything. At `--speed 3` a 16-minute
trip is five and a half real minutes of a fleet that offers nothing. On one
phone that is invisible; it is worth knowing before blaming dispatch again.

## Push notifications — `./apply-fcm.sh`

Live since 2026-08-18, Firebase project **`movin-dz`**. Worth reading before
touching, because almost everything written down about this was wrong.

**Push was never missing.** `Kernel.External.FCM.Flow` is compiled into both
binaries, eleven message types exist, and the rider app has been collecting
device tokens since it shipped. The only broken thing was the key: upstream's
placeholder ships with `project_id: jp-beckn-dev` and a private key that is
literally `xxxxxxx`, so every send died at JWT signing with

```
[FCM] |> error while sending message to person with id … : "Bad RSA key!"
```

Three columns and a restart fixed it. **No rebuild, no new container** — the same
trick as maps and routing.

```bash
./apply-fcm.sh /path/to/service-account.json
```

It writes **both** sides: `atlas_app.merchant` for the rider and
`atlas_driver_offer_bpp.transporter_config` for the driver, which carried the
same dead placeholder under different column names. One Firebase service account
is scoped to the *project*, not to an app, so the key installed today already
serves the driver app the day it exists.

### `fcm_url` must be the whole endpoint

Neither binary contains a `projects` or `messages:send` string, so nothing is
assembled at runtime — the column holds the complete URL, project id included:

```
https://fcm.googleapis.com/v1/projects/movin-dz/messages:send
```

That was a guess until the first real send put the path in the log. Override with
`FCM_URL=` if it ever changes.

### Reading the result

The three outcomes are easy to tell apart and only one is a problem:

| In the log | Means |
|---|---|
| `Bad RSA key!` | the key did not take |
| `404` on the URL | `fcm_url` is wrong |
| `INVALID_ARGUMENT` on `message.token` | **everything is right** — that device token is not a real FCM token |

The last one is what a probe will always produce, because probes invent their
device tokens. It is a pass, not a failure.

### The text is English in the binary, and it does not matter

The notification wording is compiled in — `"Driver assigned!"`, `"Karim will be
your driver for this trip."` — with **no template table and no merchant column**
to override it. Checked by searching the executable and by listing every table in
both schemas. The client wants French only, which looks like a rebuild.

It is not, because of one detail in the payload:

```json
{"message":{"token":"…","apns":{…},"android":{"data":{…}}}}
```

The Android half carries **`data` and no `notification` block**. Android renders
a `notification` message itself, with the server's words, and cannot be stopped.
A **data-only** message it does not render at all — it wakes the app and hands
the payload over. So the server says *what happened* (`notification_type`) and
the app chooses every word. The French text lives in the app, in
`src/lib/notifications.ts`.

### The eleven types

`QUOTE_RECEIVED`, `DRIVER_QUOTE_INCOMING`, `DRIVER_ASSIGNMENT`,
`DRIVER_ON_THE_WAY`, `DRIVER_HAS_REACHED`, `TRIP_STARTED`, `TRIP_FINISHED`,
`CANCELLED_PRODUCT`, `REALLOCATE_PRODUCT`, `EXPIRED_CASE`,
`REGISTRATION_APPROVED`.

Four are shown, on the client's instruction: a driver answered, ride confirmed,
driver on the way, driver arrived. The rest are translated and silent.

### The device tokens already in the table are useless

44 of 55 riders have a `device_token` and **not one is an FCM token** — the app
minted them with `Math.random` as a stand-in while push was believed impossible.
Firebase rejects every one. No existing rider receives anything until they open a
build that registers a real token and posts it to `/v2/profile`.

## The rider API — what the app uses, and what is sitting there unused

The driver-API section above exists because the source tree lies about the
backend. This one exists for the opposite reason: the backend can do more than
anyone remembers, and "what should we build next" kept getting answered from
what other ride apps have rather than from what this binary can serve.

```bash
./probe-unused-routes.py     # on the VPS — what exists vs what screens 1-14 call
./probe-rider-extras.py      # through the public edge — do the good ones work?
```

**41 rider-facing routes. 20 used. 21 unused.** Four of the unused ones are
worth real screens, and the results are recorded in each script's header so
planning does not require re-running them.

| Route | Verdict |
|---|---|
| `/v2/frontend/flowStatus` | Works, 0.27 s. Says whether a rider is mid-ride. |
| `/v2/savedLocation` | Stores Home/Work — but **discards the address text**. |
| `/v2/serviceability/destination` | Works. We only ever check the origin today. |
| `/v2/auth/logout` | Works. There is no sign-out in the app. |
| `/v2/support/sendIssue` | Present but broken. Complaints reach nobody. |

Three of those are traps rather than features:

**`flowStatus` is the fix for the worst hole in the app.** Close the app
mid-ride today and the ride is gone from the rider's side. That happened to a
real tester, and the ride had to be cancelled from the server by hand. The
server knew where he was the whole time — nothing asked it. This is a launch
check, not a screen.

**`savedLocation` keeps the address — but only if it is sent FLAT.**
`CreateSavedReqLocationReq` declares `area`, `city`, `street`, `building`,
`door`, `state` and `country` at the **top level**. Sent nested inside an
`address` object — the shape `rideSearch` uses — Servant drops the unknown key,
answers `200`, and the place saves with no address and no complaint.

This page previously said the backend discarded them. **It does not**; the probe
that produced that finding was sending them in the wrong shape. Measured
2026-08-17: all seven come back exactly as they went in, and the same is true of
`fromLocation`/`toLocation` on a booking when the search actually sends an
address (both of the app's searches already do).

The tag is free text and is the identity: saving an existing one is refused with
`400 · Location with this tag already exists`, so an edit is delete-then-create.

**`serviceability/destination` answers on the national border**, like the origin
check — so `true` means "inside Algeria", not "a car will come". Tamanrasset is
`true` and 1,900 km from any driver. Useful for catching a destination abroad,
useless as a promise.

### Push: no route, because none is needed

There is no push/notification route on the rider API — only an
`FCMConfigUpdateReq` schema with no endpoint behind it. **That was read as "this
backend cannot send push", and that was wrong.**

Push is **fully implemented in the running binary and has been failing silently
since deployment.** `Kernel.External.FCM.Flow` is compiled in, with JWT
service-account auth and the `firebase.messaging` scope, and the rider log says
so on every ride:

```
ERROR [FCM] |> error while sending message to person with id 851790f2… : "Bad RSA key!"
```

The configuration is a **row on `atlas_app.merchant`** — `fcm_url`,
`fcm_service_account`, `fcm_redis_token_key_prefix` — exactly like
`Maps_Google`'s `googleMapsUrl`. Ours points at `http://localhost:4545/`, the
upstream *mock*, with a placeholder service account. Device tokens are already
collected (35 of 45 riders), and nine message types already exist including
`QUOTE_RECEIVED` and `DRIVER_HAS_REACHED`.

Turning it on is a free Firebase project, its service-account JSON, and one SQL
update. No rebuild.

**Approved by the client on 2026-08-17**, with two constraints worth keeping
here rather than in a chat log:

- The Firebase project belongs to the **company** Google account
  (`movindz2026@gmail.com`, which already holds the backups). Not a personal
  one — the same reasoning as the APK signing key: if the account is lost,
  notifications stop and there is no way back into the project.
- **Only four of the nine messages are to be sent:** `QUOTE_RECEIVED`,
  `DRIVER_ASSIGNMENT`, `DRIVER_ON_THE_WAY`, `DRIVER_HAS_REACHED`. The other
  five — trip started, trip finished, driver cancelled, search expired,
  registration approved — exist in the binary and are deliberately unwanted.
  Whatever switches these on has to be selective; sending all nine because the
  binary can is not the agreed product.

FCM costs nothing: no quota, no card, the free Spark plan is enough. Billing
only starts if this project adopts *other* Firebase products (database, storage,
hosting), and this stack has its own.

iOS, when it exists, needs no second integration — FCM delivers to APNs itself.

### Switching off a driver who has not paid

Drivers pay us a monthly subscription; passengers pay drivers cash. So the
system has to be able to stop an unpaid driver receiving work, and the client
asked for it to be automatic. `./probe-subscription.sql` asked the database, and
the two halves have opposite answers:

- **The switch exists.** `driver_information.enabled` / `blocked` — one boolean,
  and dispatch stops immediately.
- **The record does not exist at all.** Nothing in the schema is about plans,
  subscriptions, fees or invoices. Upstream's driver-subscription subsystem is
  not in this binary; every `%subscri%` hit is the BECKN registry or pg_catalog.

So the automatic half is a nightly job, and the expensive half is the one nobody
asks about: a `paid_until` per driver, and somewhere to set it. Marking a payment
stays manual as long as drivers pay in cash or by CIB, outside the app. That
belongs to the admin website, not to either mobile app.

## Ratings — `./apply-ratings.sh`

Run once per server. After that there is nothing to schedule.

```bash
./apply-ratings.sh      # install the trigger, backfill, and prove it fires
```

**Riders could rate from the day screen 14 shipped, and nobody ever saw a
star.** Every rating landed correctly in `atlas_driver_offer_bpp.rating`.
Nothing read them back: `person.rating` — the column the driver's offer carries
to the rider over Beckn — was written by no one, so `driverRatings` and the
offer's `rating` arrived `null` on every ride and the app could not draw
anything. Upstream has a subsystem that maintains it; our binary predates that,
the same story as [subscriptions](#switching-off-a-driver-who-has-not-paid).

So `ratings-average.sql` keeps the column correct with a **trigger**, not a
timer. `backup.sh` runs on a systemd timer because a backup is a periodic thing;
this is not. `person.rating` is *derived*, and the only moment it can change is
when a row in `rating` changes — so a trigger is immediate, cannot drift, and
needs no service enabling on a rebuilt server.

Measured on the way in, and worth keeping:

- **The averages that existed were wrong.** Karim carried 3.67 from hand-testing
  while his three real ratings (2, 2, 5) average 3.00. The backfill corrects
  values as well as filling empty ones, and clears any rating with no ratings
  behind it.
- **There is no `Person` cache to bust.** The obvious fear is that this has
  `apply-tariff.sh`'s trap, where SQL alone changes nothing because Redis holds
  the old value. Scanned: there is a `CachedQueries:DriverInformation`, a
  `Merchant`, a `TransporterConfig` and a `FarePolicy`, and **no
  `CachedQueries:Person`**. The rating is read from Postgres when the offer is
  built.
- **A driver nobody has rated stays `null`**, never `0` — the API's own scale is
  1–5, so zero is not a rating, and the app shows "Nouveau" instead of no stars.

Proven end to end afterwards: a booking made through the rider API came back
with `driverRatings=3` on the ride, which is the number the app draws.

## Choosing a driver — the fleet, the car, and the shortlist

Until August the passenger compared a first name, a star and a price. He could
not see what car was coming, and he could not say which drivers he wanted. Both
are now possible, and the three pieces got there by three different routes —
worth reading in that order, because the cheapest one did most of the work.

### 1. Who is nearby, and what they drive — no rebuild at all

`GET /fleet/nearby?lat=&lon=&variant=` on **maps-shim** (`maps-shim/fleet.js`).

The rider API does not have this and never did. `EstimateAPIEntity.driversLatLong`
is `[{lat, lon}]` and nothing else, and the provider's own dispatch pool —
`DriverPoolResult` — carries `driverId, variant, lat, lon` and **no vehicle**.
So the model and the colour were not being withheld from the app; they were
never put anywhere the app could reach.

Rather than widen the pool, the BECKN payload and the rider entity, this reads
the fleet out of the same database the shim already connects to, with dispatch's
own three filters — `active AND NOT blocked AND NOT on_ride` — and a 300-second
freshness window on the position.

It is a **display** list, not the pool. The two agree because they read the same
table, not because one drives the other. Nothing in the app should claim
otherwise.

Two deliberate choices:

- **It returns no plate.** A signed-in rider could otherwise walk the map and
  enumerate the fleet. The plate belongs to the screen after a driver has
  accepted, which is also where you can actually read it off a car.
- **It does return the driver's person id**, which is what makes a row
  *choosable* rather than merely countable (see 3). Safe in a way the plate is
  not: a UUID identifies nobody who does not already have it, and every driver
  endpoint still wants that driver's own token.

It refuses a caller who is not a signed-in passenger by asking the rider app —
a token that can read its own profile belongs to a real account.

```bash
python3 probe-fleet-nearby.py     # 401 without a token, real cars with one
```

### 2. The car on each offer — two builds, one existing field

An offer carried `driverName`, `rating`, `distanceToPickup`, `durationToPickup`
and `validTill`. Nothing about the vehicle: `DriverQuote` on the provider side
has a `vehicleVariant` and no model, and the provider never looked the vehicle
up when building `on_select`.

**The provider now writes `"make|model|colour"` into `OS.ItemDescriptor.name`.**
That field already exists, upstream sets it to `""`, and the rider never read
it — so using it means **no change to the shared BECKN types**, which are
compiled into the gateway and the registry as well as both apps. A new field
would have been cleaner and far riskier.

The second half is the part that is easy to miss: the rider **already receives
it**. `ItemDescriptor` is `{ name, code }` and `buildQuoteInfo` reads only
`code`, so upstream has been parsing the name and dropping it on the floor since
2023. Four small patches stop the drop, and they are small because upstream uses
RecordWildCards everywhere that matters — naming the field on four records makes
`buildDriverOffer`, `fromTType`, `toTType` and `Quote.hs`'s API-entity
conversion carry it with no further changes.

Pipe-separated rather than JSON so the parse on the far side cannot throw: worst
case a field is empty and the passenger reads one word less.

`driver_offer.driver_name` was the tempting place to hide this without a
migration. It is narrowly safe — `ride.driver_name` comes from `on_update`'s
`fulfillment.agent.name`, a different path entirely — and it was rejected
anyway. A column reading `Ahmed|Renault|Clio|Grey` is a trap for whoever next
opens that table.

```bash
./apply-migration.sh driver-offer-vehicle.sql   # atlas_app.driver_offer.vehicle_desc
```

**It carried three fields until 24 August and now carries five:**

    make|model|colour|registrationNo|driverId

**The plate is there for the year.** The client asked for the propositions
screen to show each car's year in place of the word *Voiture*, and there is no
year column anywhere in `atlas_driver_offer_bpp.vehicle` — but an Algerian plate
keeps the year in its middle group. `04217 118 16` is a 2018 car, `02456 122 16`
a 2022 one, and the fleet's plates are real and correctly shaped. The lookup is
by **driver**, not by variant, so a scooter and a fourgon carry it exactly as a
voiture does.

**The driver id is there for his photograph.** It is what lets the passenger's
app find his avatar with no extra route, no extra column and no extra lookup:
`maps-shim` serves the picture under that id, and the offer now names it. That
is the whole of the backend's involvement in the photograph — it carries a
string and never learns what an image is.

The rider binary needed no change for either. It stores whatever arrives in a
`varchar(255)`, and five fields fit comfortably.

### 2b. Drivers can rate passengers — `./apply-migration.sh passenger-rating.sql`

> **Applied and deployed 2026-08-24.** Build #5, image
> `ghcr.io/mohagnpro/ny-backend:latest`, digest `41cbe406…`.

This was refused three times before it was built, and the refusals were honest:
**the backend could not do it.** The only rating route in the entire driver API
is `/beckn/{merchantId}/rating` — the provider *receiving* a rating from the
rider app over BECKN — and `rider_details` had five columns with nowhere to put
one. That is why the driver's history screen ships a star that points one way
and deliberately no *Noter* pill.

What exists now, all on the provider side, so nothing crosses BECKN and neither
the gateway nor the rider binary is involved:

| | |
|---|---|
| `rider_details.rating` | the average, 1–5, NULL until somebody rates |
| `rider_details.total_ratings` | how many drivers have |
| `rider_details.total_rating_score` | their sum |
| `POST /ui/driver/ride/{rideId}/rateCustomer` | `{ "ratingValue": 1..5 }` |
| `DriverRideRes.riderRating` | what comes back out, on the list the app polls |

**Why three columns and not one.** A driver's own average is rebuilt by reading
every row of the `rating` table (`calculateAverageRating`). Passengers have no
such table and are not getting one, so there would be nothing to recompute an
average *from* — keeping the count and the running sum makes the next average
one addition, and the average can never drift from the ratings that produced it.

**The ride he drove is the authorisation.** The action checks the ride was his
and that it is `COMPLETED` before writing anything; the token only proves who is
asking.

**One stated limitation.** There is no per-ride record of a passenger rating, so
a second POST for the same ride counts twice. A driver's own rating is protected
by a `rating` row keyed on the ride; giving passengers the same means a second
table. The app disables the control after use. If it is ever abused, that table
is the fix — not a flag on the ride.

**The trap this cost 34 minutes to learn.** Adding fields to a record means
patching **every place the record is constructed**, and this backend compiles
with `-Werror=missing-fields`. Build #4 died on one such site —
`Confirm.hs:213`, where a passenger first becomes known at confirm — with every
other patched module already compiled. `grep` for the constructor before
widening a record.

### 3. The passenger picks who gets the request — one build, one line

> **Status, 24 August: built, deployed, and no longer used by the app.** The
> client removed the prices screen — the vehicle is chosen on the map already,
> so a second list asked a question he had answered — and the driver picker was
> the other half of that screen. Everything below is still on the box and still
> correct: the column exists, the tag is parsed, the filter runs. The passenger
> app simply sends no shortlist, which the provider reads as *ask everyone* —
> the behaviour that existed before 22 August. Turning it back on is a caller
> change in one file and no rebuild.

Dispatch asks every driver the pool finds, in batches, and the first to answer
wins. The client asked for the other thing: the passenger sees the cars near him
and sends the request to the one, two or three he wants.

**The channel already existed.** `select` has always carried a rider decision to
the provider — `auto_assign_enabled`, a `Bool` riding in
`order.fulfillment.tags`, which the provider stores on `search_request` and the
allocator reads back when it builds batches. The shortlist rides in the same
tags, into the same row, read at the same moment.

The filter is one line, in `prepareDriverPoolBatch`:

```haskell
allNearbyDrivers <- onlyChosen searchReq <$> calcDriverPool radiusStep
```

Everything below it — batching, sorting, the fill, the radius expansion — works
off that list, so filtering there filters all of it at once.

`Maybe Text`, comma-separated, identical at every hop: request body → BECKN tag
→ database column. Exactly one place splits it. The `Maybe` is what lets the two
binaries deploy in either order — an old provider ignores a JSON key it does not
know, and a new provider reading an old rider's payload gets `Nothing`, which
means *ask everyone* and is the behaviour that existed before.

`Select.Tags` goes from `newtype` to `data`. It is only used by these two apps:
`select` goes BAP → BPP directly and the gateway never sees it.

The app posts to **`/v2/estimate/{id}/select2`**, not `/select`. `/select` takes
no request body at all, which is precisely why the driver rows on the prices
screen were not selectable before.

```bash
./apply-migration.sh search-request-chosen-drivers.sql   # ...search_request.chosen_drivers
```

#### What deliberately does not happen

**There is no fallback to the full pool.** If the two drivers he chose never
answer, he gets no offers. Widening the search quietly would put a driver he
specifically did not pick at his door, which is the opposite of the feature.

That is the trap to remember if this is ever switched back on, and it is the
reason it needs more than a caller change to be safe: the waiting screen, which
runs its own clock already, is where an "ask everyone instead" escape hatch
belongs, and **that escape hatch was never built**. A passenger who picked one
driver who ignored him waited out the whole search with no way out but
cancelling. Nobody is exposed to it today — no shortlist is sent — but it comes
back with the feature.

### Deploying these — the order matters, and it is the safe order

Both migrations add a **nullable** column, which is what makes this reversible:

1. **Run the SQL first.** The deployed binary does not know the column and does
   not care — Postgres fills `NULL` for a column nobody mentions — so every
   insert keeps working and the box is in a valid state on its own.
2. **Then swap the images.** Rider *and* provider: the two halves of the vehicle
   chain live one in each.
3. **Restart `maps-shim`** for the driver id. No build — it is Node behind a
   bind mount.

Rollback is then a plain image swap with nothing to undo. **Do not drop the
columns on rollback** — the old binary tolerates them exactly as it did in step
1, and dropping them is the only way to turn a reversible deploy into an
irreversible one.

The app side ships in the same APK as the backend that honours it, so there is
no feature flag to forget: an APK without the picking screen cannot send a
shortlist, and one with it is only handed out after the swap.

#### The workflow builds two binaries, not twenty — check the one you changed

The image carries every executable in the upstream repo, but our workflow only
builds **`rider-app`** and **`dynamic-offer-driver-app`**. Everything else in
`/opt/app` is the 2023 binary that came with the base image.

Caught on 2026-08-23, and worth the paranoia that caught it: the build reported
success in **9 minutes** after a change to a type in `beckn-spec` that both apps
depend on, which should force a wide recompile. `strings` on the binaries
settled it:

| binary | | |
|---|---|---|
| `rider-app-exe` | rebuilt 15:26 | has `chosen_drivers`, `vehicle_desc` |
| `dynamic-offer-driver-app-exe` | rebuilt 15:26 | has `chosen_drivers` |
| `driver-offer-allocator-exe` | **dated 2023-03-02** | byte-identical to the old image |

So the fast build was a genuinely warm stack cache, *and* the allocator was
never ours to begin with.

**Why that did not matter, and when it would.** `driver-offer-allocator-exe`
runs the scheduled `SendSearchRequestToDriver` job — batches 2 and later.
**There is no allocator container in this compose.** `ny-driver` runs
`dynamic-offer-driver-app-exe` and nothing else, so those scheduled jobs are
written to the database and never picked up:

> **Dispatch in this deployment is one batch only.** The first batch runs
> inline inside the select handler; `createAllocatorSendSearchRequestToDriverJob`
> then queues a job nothing executes. `driverBatchSize` is therefore the total
> number of drivers a search ever reaches, not the size of the first wave.

That is why the shortlist cannot leak: there is no later batch to leak into. If
an allocator container is ever added, **it must be built by the workflow first**
— otherwise a 2023 binary would run the old `prepareDriverPoolBatch`, batch 1
would honour the passenger's choice and every batch after it would ask
everyone. Silently.

`beckn-gateway-exe` is stale for the same reason (step 16 takes 0 seconds) and
is harmless for a different one: `select` goes BAP → BPP directly, so the
gateway never deserialises the tags the shortlist rides in.

```bash
python3 probe-shortlist.py   # two searches, one shortlisted; reads who was
                             # actually asked out of search_request_for_driver
```

Measured 2026-08-23 against the live stack: control asked 4 drivers, a
shortlist of one asked exactly that one.

## Backups — `./backup.sh`

```bash
./backup.sh              # take one now
./backup.sh restore F    # restore F into a scratch database and check it
./backup.sh list         # what we hold, and how old the newest is
./backup.sh install      # install the nightly systemd timer (02:30)
```

Nightly, encrypted with GPG AES-256, uploaded off the server with `rclone`.
**1.8 MB** per backup out of a 183 MB database, which is the whole point of the
next two sections.

### It is not `pg_dump atlas_dev`, for two reasons

**The encryption keys are in a different container.** `atlas_app` stores rider
phone numbers encrypted and the keys live in `ny-passetto-db`. A dump of
`ny-postgres` alone restores perfectly and leaves every number permanently
unreadable — a backup that looks complete and is not. Both databases go into the
archive, and a restore is only meaningful with both.

**Most of the database is rebuildable, and skipping it is 183 MB → 1.8 MB:**

| Schema | Size | In the backup? |
|---|---|---|
| `geo` | 155 MB | no — `./geocoder-prepare.sh` rebuilds it in ~5 min |
| `public` | 7 MB | no — PostGIS `spatial_ref_sys`, ships with the extension |
| `tiger`, `tiger_data`, `topology` | 2 MB | no — PostGIS reference data |
| `atlas_app` | 3.9 MB | **yes** — riders, bookings, rides |
| `atlas_driver_offer_bpp` | 3.6 MB | **yes** — drivers, fares, the BPP side |
| `atlas_registry` | 40 kB | **yes** |

An include list has one failure mode: a schema added later is left out silently.
So the script **refuses to run if the schema set has changed**, and says which
one is new. That guard earned itself on its first run by catching `tiger_data`.

### Setting it up

```bash
openssl rand -base64 32 > /root/.movin-backup-pass
chmod 600 /root/.movin-backup-pass

rclone config                                        # once, interactively
RCLONE_REMOTE=movin-drive:movin-backups ./backup.sh install
```

**The passphrase must not live only on this server.** It is the one thing
standing between the uploaded archive and every rider's phone number, and the
backups exist for the case where this machine is gone. Keep it wherever the
Android signing key is kept. Changing it later does **not** re-encrypt existing
backups, so the old value has to be kept too.

With `RCLONE_REMOTE` unset the backup still runs and says loudly that it stayed
on this server. That is deliberate — a local-only backup is worth something, and
a script that implied it went offsite when it did not would be worth less than
nothing.

### Verifying, rather than assuming

`./backup.sh restore` decrypts into a **scratch database**, never the live one,
and checks the row counts against the manifest inside the archive. Do it against
a copy pulled back down from the remote, not the local file — that is the copy
that will actually be used.

Two traps met while building this, both silent:

- `docker exec -i` inside `ssh host "bash -s" <<EOF` **consumes the rest of the
  script** from stdin; the first query runs and nothing after it does.
- A unit that works when you run it can still fail under systemd. Fire
  `systemctl start movin-backup.service` and read the journal, the same way the
  certbot timer had to be checked.

## Gotchas

**Use `/swagger`, not `/swagger/`.** With a trailing slash the page's relative
asset paths resolve to `/swagger/swagger-ui.css` and 404, leaving a blank page.
The static files are served from the root, and `swagger-initializer.js` derives
the spec URL via `window.location.href.replace("/swagger", "/openapi")`.

**Port 8014, not 8013.** rider-app runs with `network_mode: host` (the dhall
configs hardcode `localhost` for Postgres/Redis/Kafka/passetto). Host-network
ports aren't forwarded out of the Docker Desktop VM, so a small `socat` proxy
re-exposes the API on `localhost:8014`.

**Migration ordering.** `setup.sh` applies only the base schema; rider-app runs
`dev/migrations/rider-app` itself on every startup. Applying them beforehand
causes `column ... already exists` failures. Test data is loaded *after*
rider-app has migrated.

---

## Known limitations

- **This is the 2023 baseline, not current `main`.** Running today's backend
  would need a full Haskell build *and* merchant config we don't have.
- ~~`GET /v2/profile` returns 500~~ — **no longer true, and probably has not
  been for a while.** Measured 2026-08-17: it answers `200` with the name and a
  *masked* number (`055...188`). Screen 16 reads it. Left here struck through
  rather than deleted, because this line was believed for weeks and nearly had a
  screen designed around its absence.
- **A rider cannot change their phone number, and has no profile photo.** Not a
  policy choice — there is nowhere to put either. Read from the server's own
  OpenAPI on 2026-08-18, so this is the schema and not an inference from
  behaviour:

  | | |
  |---|---|
  | `POST /v2/profile` accepts | `firstName`, `middleName`, `lastName`, `email`, `deviceToken` |
  | `GET /v2/profile` returns | those, plus `id`, `maskedEmail`, `maskedMobileNumber`, `maskedDeviceToken`, `whatsappNotificationEnrollStatus` |

  There is **no `mobileNumber` field to write to** and **no image field
  anywhere**, in either direction. Nor is there an upload route: all 60 rider
  routes were listed and `/v2/profile` is the only one that touches a profile.
  The number is also the identity — auth is by phone — so "changing" it means a
  different account, not an edited field. Screen 16's `Non modifiable` is the
  honest rendering of this, and the endpoint answers `200` to anything it is
  sent, so a number field would show a tick and change nothing.
- Kafka connection warnings in the logs are harmless.
- The gateway logs a 404 against `localhost:8014/v1/e1f37274-…` and a refused
  connection to `localhost:8000` on every search. Both are stale fixture rows in
  the registry (`another-test-cabs`, `metro-bpp`) that point at services this
  deployment does not run. The gateway multicasts to every BPP in the domain and
  ignores the ones that fail, so this is noise, not breakage — the real BPP on
  `:8016` answers.
- The binaries in `bin/` are gitignored. `MANIFEST.txt` alongside them records
  which build produced them; `setup.sh` refuses to start without them.

## Layout

```
setup.sh               one-shot bring-up / verify / algeria / drivers / down / clean
docker-compose.yml     the stack
Dockerfile.rider       librdkafka fix
Dockerfile.maps-shim   the Google-Maps-shaped shim

  data and config
algeria-geofences.sql  service area — the national border
algeria-tariff.sql     the Algerian fares;  apply-tariff.sh applies them
osrm-config.sql        points the backend's routing at our OSRM
dedupe-seed.sql        removes the duplicated upstream seed rows

  prepare steps, each run once and slow
osrm-prepare.sh        builds the routing graph from algeria-latest.osm.pbf
tiles-prepare.sh       builds the map tiles from the same extract
geocoder-prepare.sh    builds the 111.5k-row place index (output gitignored)

  keeping it alive
drivers-keepalive.sh   driver positions go stale silently — this is the fix
simulate-driver.py     plays a fleet of six, so the app can be finished on one phone
fleet-service.sh       keeps that fleet running;  without it: cars but no offers
backup.sh              nightly encrypted backup, offsite;  also restore / list
ratings-average.sql    trigger keeping person.rating true;  apply-ratings.sh installs it
apply-fcm.sh           installs a real Firebase key — push, rider AND driver, no rebuild
enrol-driver.sh        who may sign in on /ui/, and with which code;  the code is
                       printed once and cannot be read back
apply-search-window.sh how long a driver has to answer.  A Dhall value, so it is
                       lost on any setup.sh that refetches 2023/ -- re-run it

  measuring, not running — each records its results in its own header
probe-booking-flow.py     a whole ride from both sides
probe-booking-timeouts.py how long a search really lives
probe-unused-routes.py    what the rider API can serve that we don't use
probe-rider-extras.py     do the useful unused routes actually work?
probe-trip-history.py     what a past-trips list can show — and one wrong finding
probe-subscription.sql    can we switch off a driver who hasn't paid?
probe-push.py             one push to a real phone, no ride needed;  isolates app vs server
probe-search-window.py    how long a driver really gets, read off a real request.
                          Signs in as a RIDER only -- never as a fleet driver
probe-driver-offers.sql   the bounds, the window and what drivers do with it.
                          Read-only;  the source for every figure D10/D11 use
probe-driver-wait.sql     what happens AFTER the driver presses -- how long an
                          offer lives, how it ends, how often it ends in
                          nothing.  Read-only;  the source for D12
probe-driver-pickup.sql   reaching the passenger:  the ride OTP (four digits,
                          and they can start with a zero), how long the leg
                          takes, and how it is cancelled.  Read-only;  D13

  services fronting the stack
edge/                  nginx + TLS, the public face
auth-guard/            the OTP lock, in front of both backends: brute-force limits
                       on /v2/, and on /ui/ the per-driver code that retires 7891
  driver-codes.json    salted hashes, gitignored, in the backup set
maps-shim/             Google Places/geocoding, answered from Postgres
geocoder/              place-index build;  places.csv gitignored
demo-map/              the map on :8025 (nginx conf + page)
  site/areas.geojson   exported from the DB by setup.sh (gitignored)

bin/                   backend binaries (gitignored;  MANIFEST.txt records the build)
2023/                  pinned upstream tree (fetched by setup.sh, gitignored)
```
