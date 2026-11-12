# Riders — the rider API and ratings

What the rider app uses and what sits unused; rider → driver; ratings in both directions.

> Moved here from the local-stack README on 2026-10-08 (phase 7), word for word: dates and measurements are as they were taken. Commands written `./x.sh` run from `stack/`. Back to the [README](../README.md).

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

### The office's own push — `maps-shim/driver-push.js`, since 2026-09-28

Every push before this was the backend's; the relay only forwarded or
rewrote it. **Nothing the office does reached a phone**: upstream's
`POST /message/send` pushes to nobody (measured 2026-09-24), and enabling a
driver sends nothing. So a driver accepted in the console heard nothing — the
owner found out by being that driver.

    admin-api  --POST /internal/driver-push {driverId, type}-->  maps-shim
      iPhone token   push-relay's appleNotify (same key, same words)
      FCM token      FCM v1, data-only, signed with our own Google token
                     (JWT from transporter_config.fcm_service_account, cached 1 h)

Two types only, both ours: `REGISTRATION_APPROVED` (« Dossier accepté », the
words the app always had) and `REGISTRATION_REFUSED` (« Dossier refusé —
ouvrez l'application pour voir pourquoi »; the reason is in the app, never on
the lock screen). Best effort: the decision is made and recorded first.

**Reachable from admin-api's container, and nowhere else.** The route accepts
loopback and 172.16.0.0/12 only, and the edge routes nothing under
`/internal/` to the shim (probed: 404 through `api.movinapp.net`). That needed
one firewall rule, the same shape as the three already there for 5000, 8013
and 8016 — **none of which was written down anywhere until now**:

    sudo ufw allow from 172.16.0.0/12 to 172.17.0.1 port 8030 proto tcp \
      comment 'docker bridge -> host service (maps-shim, admin-api driver push)'

Without it the container's fetch times out and the push is silently skipped
(admin-api logs `driver push: shim unreachable`). Port 8030 stays closed from
the internet (probed from outside: no answer). `tests/driver-push.test.js`
checks the JWT against the key, the FCM message shape and the token cache.

**A refused driver can now send his file again.** The app's « Votre dossier »
shows the reason and the agent's note (`GET /driver/validation` on admin-api)
and a « Renvoyer mon dossier » button (`POST /driver/validation/resubmit`),
which unblocks him — back in the Validation queue, still not enabled — and
records `resubmitted` in `movin.driver_validation` (website migration 020). Only
while he has no vehicle linked: the dispatch pool honours `blocked` and ignores
`enabled`. Both routes are exact `location =` blocks in `edge/nginx.conf`. The
bot announces it as « dossier renvoyé après refus ».

**Proved on the owner's own phone the same day**, end to end: refused in the
console → « Dossier refusé » push and the reason on screen; « Renvoyer mon
dossier » → back in the queue as « Renvoyé par le chauffeur »; accepted →
« Dossier accepté » push and the screen turned to « Vous êtes validé » by
itself. The same day, also on his phone: a Chargily top-up put the driver back
into dispatch at once (`wallet.js` now republishes `movin:unpaid` on credit).
That fix covers **Moosyl in Mauritania too**, and was proved rather than
assumed: both gateways credit through the one `creditIfPaid`, and
`tests/wallet-dispatch.test.js` runs the webhook once per gateway (Chargily's
`/checkouts/`, Moosyl's `/checkout-session/public/`). On the wallet.js before
the fix both fail with 0 writes to the key; after it, both pass.

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

~~iOS, when it exists, needs no second integration — FCM delivers to APNs itself.~~
Wrong, found 2026-09-16: FCM would deliver the backend's English `apns.alert`
for every type, and the app's iOS token is not an FCM token anyway. iPhones go
through the push relay — see *iPhones — the push relay* above.

### Switching off a driver who has not paid

Drivers pay us from a wallet they top up (since 2026-09-07; a monthly
subscription before that); passengers pay drivers cash. So the system has to be
able to stop an unpaid driver receiving work, and the client asked for it to be
automatic. `./probe-subscription.sql` asked the database, and
the two halves have opposite answers:

- **The switch exists.** `driver_information.enabled` / `blocked` — one boolean,
  and dispatch stops immediately.
- **The record does not exist at all.** Nothing in the schema is about plans,
  subscriptions, fees or invoices. Upstream's driver-subscription subsystem is
  not in this binary; every `%subscri%` hit is the BECKN registry or pg_catalog.

So the record had to be ours: the driver wallet, below, and the dispatch list
built from it (*[Dispatch](wallet.md#dispatch--maps-shimrestrictedjs-and-two-lines-of-haskell)*, at the end of that section).

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

### Showing somebody their own rating — `maps-shim/rating.js`

The client asked on 2026-08-25 for both apps to show the user *their own* stars,
with the number of people behind the average beside it. Neither number is
reachable from the binary that ought to have it, and both refusals were
measured rather than assumed:

| Wanted | Asked of | Answer |
|---|---|---|
| the passenger's average | `GET /v2/profile` | eight fields — three name parts, `id`, two masked contacts, a masked device token, a WhatsApp flag. **No rating of any kind.** |
| the driver's rater count | the driver binary | the string `totalRatings` **does not appear in the executable**. `grep -a` finds `totalEarnings` and nothing else of that shape. |

And the passenger's rating could not be on the rider binary anyway: a driver
rates her through the route *our own patch* added to the **provider**, which
writes `atlas_driver_offer_bpp.rider_details` — a table the rider binary has
never heard of. Bridging the two schemas inside Haskell is a field on a response
type, which is a rebuild and new binaries. Bridging them in the shim is one
query.

```
GET /rating/phone/{number}     -> {"rating": 4.5, "total": 3}
GET /rating/driver/{driverId}  -> {"rating": 4.2, "total": 6, "rides": 148}
```

`rides` was added on 2026-08-25, when the client asked that a passenger
choosing between offers see how much each driver has driven. Nothing on the
offer carries it and a sixth field on `vehicleDesc` would be a rebuild for one
integer, so it is a `count(*)` over his COMPLETED rides.

**Counted, never inferred from `total`.** Most rides are never rated — measured
here, Yacine has 15 rides and Karim 14, against 6 and 5 ratings — so showing the
rating count as experience would undersell every driver by roughly half. And
`rating: null` does **not** imply `rides: 0`: the first version of that handler
returned early on a null rating, which would have hidden the ride count of every
driver nobody has rated yet, who are exactly the drivers a passenger needs a
second number for.

`probe-driver-rides.py` checks every driver against the database. It also
found, by accident, that **33 rapid requests trip `limit_req burst=20`** on this
location — worth knowing and not worth changing: the offer screen makes one
request per offer and there are never more than five.

The passenger route uses the join `avatars.js` already trusts: the two schemas
agree on the phone-number hash, so her `atlas_app.person` row finds her
`rider_details` row, matched on the **last nine digits** of the number — the app
holds a bare NSN and the database writes the trunk zero, and an equality test
finds nothing, silently. The driver route is a `count(*)` over the `rating`
table, because he has no running count the way she now does.

**Neither ever 404s and neither ever throws.** An unrated person is the normal
state — the route that rates passengers went live the day before — so "nobody
has rated you" and "the network failed" answer identically, and both apps draw
*Nouveau*. That also means the apps ship safely **before** this route does.

### Driver → passenger: the guard reports each one, since 2026-09-27

`rateCustomer` adds the stars to a running total on
`atlas_driver_offer_bpp.rider_details` and **writes no row**: which driver,
which ride and how many stars are gone once it returns. The console's Notes
needed them, so the auth guard catches them on the way through —
`noteDriverRating` in `auth-guard/driver-rules.js`: after the driver backend answers
2xx, it tells admin-api `POST /internal/driver-rating {rideId, stars}` on
loopback, fire and forget. admin-api reads who drove and who rode from the ride
and keeps the first rating per ride (`movin.driver_rider_rating`).
**Anyone editing the guard must keep that call after the forward**, or the
console silently stops receiving them.

Ratings from before that day were recovered from `docker logs ny-edge` —
ride and time, not stars (the log keeps no body). That log also showed the
backend **counting every repeat**: six rides rated eleven times, all eleven in
one passenger's average.
