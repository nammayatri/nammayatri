# Fares and dispatch

What a ride costs in each country, how "a car is near" is decided, and how a driver is chosen.

> Moved here from the local-stack README on 2026-10-08 (phase 7), word for word: dates and measurements are as they were taken. Commands written `./x.sh` run from `stack/`. Back to the [README](../README.md).

## The tariff — `./apply-tariff.sh`

```bash
./apply-tariff.sh          # applies algeria-tariff.sql AND clears the caches
```

Re-measured 2026-08-24, on both merchants, all four variants:

| App name | Variant | Start | Per km | Pickup | Max extra |
|---|---|---|---|---|---|
| Voiture | `SEDAN` | 150 | 45 | 70 | 300 |
| Scooter | `AUTO_RICKSHAW` | 100 | 35 | 50 | 300 |
| **Waw** | `HATCHBACK` | **100** | **35** | **50** | 300 |
| Fourgon | `SUV` | 200 | 60 | 100 | 300 |

Set by the client on 2026-08-13, replacing the upstream Bangalore seed (10 / 12
/ 120) that made every vehicle cost the same 258 DZD. A 13.7 km trip is now
**629 / 836 / 1121** for hatchback / sedan / SUV.

**The names in the left column are the app's, and they are not the server's.**
The client replaced *Economy / Comfort / Premium* with four physical vehicle
types on 2026-08-21, and two of the four carry goods rather than people. The
enum is a compiled Haskell type with exactly four members, so each of ours is
pinned to one existing slot and the mapping is arbitrary — `WAW → HATCHBACK`
says nothing about hatchbacks. `Frontend`'s `lib/vehicle.ts` is the one place
that mapping lives. Renaming the enum properly is a rebuild.

**A waw is priced exactly like a scooter**, because it inherited the row that
used to be *Economy*. A flatbed pickup and a two-wheeler on the same tariff is
not a decision anyone took; it is what the rename left behind. Raised with the
client 2026-08-24, along with the table above so he can name an increase rather
than guess at one. It is pure SQL — `fare_policy` is already one row per variant
and `apply-tariff.sh` applies it — so **no rebuild**.

### What the rider is shown, and what he can be charged

The estimate is **the floor, not the price**, and the app shows only the floor.
Measured on `atlas_app.estimate`, twelve real quotes:

| Variant | Estimate | Ceiling | Gap |
|---|---|---|---|
| `SEDAN` | 761 | 1046 | 285 |
| `SEDAN` | 768 | 1053 | 285 |
| `SUV` | 1026 | 1311 | 285 |
| `HATCHBACK` | 617 | 902 | 285 |
| `AUTO_RICKSHAW` | 574 | 859 | 285 |

**The gap is NOT flat, and it was written up here as if it were.** It comes
from `restricted_extra_fare`, which is keyed on `min_trip_distance` and steps —
75 at 0 km up to 600 at 30 km on the Algerian tariff. All twelve quotes above
were similar-length trips and landed in the same 12 000 m band, so the same
number came back twelve times and a constant was read into it. Twelve samples
that agree are not twelve independent samples if nothing varied the thing being
measured.

`fare_policy.driver_max_extra_fee` is a fallback the bands override, which is
why 300 never appeared: the 12 000 m band is 285. There was no 15 DZD rounding
artefact to trace.

Nobody is surprised by a bill — the driver's actual offer reaches the passenger
*before* he accepts, and he can refuse it. But between pressing *Commander* and
that offer arriving he used to see one number and a sentence that did not say
how much could be added. The range had left with the deleted prices screen on
2026-08-24, when the ceiling was 20 DZD.

**Raised with the client 2026-09-02, and he chose the range.** The app now
shows `761–1046 DZD` on the vehicle card, on the order button and on the recap
while the search runs, with the sentence naming the higher figure as a ceiling.
It is deliberately *not* shown on the offers header, which is the baseline each
driver's offer is subtracted from. Frontend `a18bcc0`.

So the 285/300 discrepancy above is now visible to passengers and worth
tracing: the app prints the server's own `totalFareRange`, so if 285 is a
rounding artefact rather than the real bound, riders are being quoted a ceiling
15 DZD below the one the policy allows.

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
| Wider radius **for waw and fourgon only** | **a backend rebuild**, ~45 min |

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
