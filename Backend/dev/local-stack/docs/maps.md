# Maps — routing, tiles and place search

Our three replacements for Google: OSRM for routes, tileserver-gl for the map picture, and the place index behind `maps-shim`.

> Moved here from the local-stack README on 2026-10-08 (phase 7), word for word: dates and measurements are as they were taken. Commands written `./x.sh` run from `stack/`. Back to the [README](../README.md).

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

### The map in Arabic — `tiles-arabic.sh`, since 2026-09-14

The image's bundled **"Noto Sans Regular" has no Arabic glyphs** — its
`1536-1791.pbf` range is 32 bytes — so every Arabic label drew *nothing*,
silently, although the tiles always carried `name:ar` (place, poi,
transportation_name, water_name). `tiles-arabic.sh` switches the server from
`--file` to `--config /config/config.json` (`./tiles-config`):

| | |
|---|---|
| Fonts | OpenMapTiles font pack v2.0 — same family names, **96 kB** of Arabic |
| `basic-preview` | the bundled style, unchanged ids and URLs — older APKs see no difference |
| `movin-ar` | the same style, its 7 label layers `coalesce(name:ar, name)` — what the app loads when it is in Arabic (`tileStyleUrl()`) |

It proves the new setup on a throwaway container on `127.0.0.1:8036` before the
live server is touched, edits the compose file **in place** with a backup, and
rolls back on its own if the public check fails. `bash tiles-arabic.sh
rollback` returns to `--file`. The mbtiles name goes into `config.json` from
`MAP_COUNTRY` — **after changing `MAP_COUNTRY`, run it again.**

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

### Arabic place names, and why they needed a fourth route

`geo.place.name_ar` since 2026-09-10: **3,324 of 10,005 rows — every street
but three (939/942) and every transport stop (96/96)**.

It came in three passes. **3,080 were already in the data**: `extract.py`
collects `name:ar` into `alt_names`, so nothing there was translated or
invented — 2,835 rows offered exactly one clean candidate, 152 more had theirs
trapped inside a mixed-script string ("Centre de santé النقطة الصحية بأم لحياظ",
where the LONGEST Arabic run is the name, because OSM holds some doubled and
truncated), and 93 disagreed with themselves and went to a human.

**The remaining 247 streets and stops had no Arabic in OSM at all** and were
COMPOSED, not translated. Every token came from geo.place itself where possible
— `Route Rosso - Boghé` is built from the روصو and بوغي already in the table,
which also stops a town being spelled one way as a locality and another inside a
road name — then from a table of the elements these names are built from (`Ould`
appears 72 times in 247 rows, `Cheikh` 23, `Sidi` 14), then from a hand-written
list of the 257 words neither covered.

Those 257 are written as NAMES, not as letter sequences, and the difference is
the whole point. A letter-substitution pass was written first and thrown away:
it produced هابا for Haiba, whose name is هيبة, and لي for Ely. French
romanisation is lossy exactly where Arabic distinguishes — `h` is ه or ح, `s` is
س or ص, `t` is ت or ط — so rules cannot get there. Written by hand, `Melainine`
is ماء العينين, `Hamahoullah` is حماه الله, `Med` is the administration's
abbreviation of محمد, and French words are translated rather than transcribed:
`Pêcheurs` is الصيادين.

All 247 went to a reviewer pre-filled, each row labelled with where its
proposal came from. Three are deliberately still empty: their edits read like a
deletion that was never finished, and a half-typed name is worse than the
French.

Getting it *out* is the interesting part. **The rider app cannot tell the index
which language it wants**, and this was measured against the deployed binary
rather than read from the tree:

| | |
|---|---|
| `language: "ARABIC"` | **400** — a five-value enum: `ENGLISH HINDI KANNADA TAMIL MALAYALAM` |
| the Google client's parameters | `sessiontoken place components fields latlng geocode distancematrix alternatives directions autocomplete` |

**There is no `language` in that table.** The backend accepts the field,
validates it, and drops it — so hijacking an unused enum value (nobody in
Mauritania will ever legitimately send `KANNADA`) fails on the second wall even
though it clears the first. Adding it is a rebuild: 45 minutes, new binaries,
and every measurement in this project re-proved against them, for a displayed
name.

So the app asks us directly, with the placeIds the backend just gave it:

    GET /place/labels/json?lang=ar&ids=n1505634228,n3794447023

    {"status":"OK","labels":{"n1505634228":"أكجوجت","n3794447023":"اوجفت"}}

The edge exposes it as **`location = /place/labels/json`** — an exact match, not
the `/place/` prefix. `autocomplete` and `details` answer the *backend* and stay
404 from a phone; widening that location would publish the whole geocoder.
Verified from the public host, because a probe against `127.0.0.1:8030` is
exactly the check that passed while every phone got a 404 on the wallet.

Three traps the filling exposed, all of which would have written wrong names:

- **Two separators.** `|` is ours; `;` is OSM's own way of stuffing several
  names into one tag. Rosso reads `القوارب;Roco;Rusu;Rosso`, and splitting on
  `|` alone returns that entire string as one "Arabic name".
- **A Latin letter means it is not the Arabic name.** Aleg's best candidate was
  `Aleg ألاك`, a `name` tag carrying both scripts.
- **Arabic presentation forms** (U+FB50–U+FEFF) render identically to ordinary
  letters and match nothing typed on a keyboard. One reviewed answer came back
  in them. **NFKC anything that came from a human or from OSM.**

### Typing Arabic found nothing, for as long as the index has existed

`geo.normalise` — one function, applied to the stored text and to the rider's
query so they cannot drift — read:

    regexp_replace(lower(unaccent(t)), '[^a-z0-9]+', ' ', 'g')

**Every Arabic character is outside `a-z0-9`**, so Arabic normalised to a string
of spaces. `geo.normalise('شارع عبد الناصر')` returned `''`. It happened on the
way in, when `search_norm` was built, and on the way out, to what the rider
typed. Nothing errored; the screen showed no suggestions, which reads as "there
is no such place".

The column's own comment said *"Includes the Arabic names, so typing Arabic
works even though we display French."* **It was never true.** A comment is not a
test, and this one described an intention that the code four lines above it
undid.

Fixed in `geocoder/arabic-search.sql`, which does three things. The character
class admits `ء-ي`. **Harakat and tatweel are stripped** — `الحلـــــه` is a real
row, elongated with U+0640 for display, and would never have matched `الحله`.
And **أ إ آ ٱ fold to ا, ى to ي, ة to ه** — our own index holds both أنواذيبو and
انواذيبو for Nouadhibou, so without folding, typing one misses the other.

It also rebuilds `search_norm`, for two reasons: the column is **stored**, so
redefining the function changes nothing already on disk, and `name_ar` did not
exist when the index was built, so the reviewed Arabic names were not in there
at all. Reindex after, or Postgres keeps using entries built by the old
definition of an `immutable` function.

Measured after: `شارع` 363 rows, `نواكشوط` 53, `تفرغ` 3 — and through the rider
API, 8 suggestions where there had been none.

⚠ `geocoder-prepare.sh` drops and rebuilds `geo.place`, and **`name_ar` does not
survive that** until `extract.py` and `index.sql` carry `name:ar` in a column of
their own. Not done, because it cannot be proved without a full rebuild.
`geocoder/arabic-names.sql` refills the deployed index without one.

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
