# The driver API — what the deployed binary really does

The routes the driver app calls, measured against the running binary rather than read from the source tree; driver documents; a ride from the driver's side.

> Moved here from the local-stack README on 2026-10-08 (phase 7), word for word: dates and measurements are as they were taken. Commands written `./x.sh` run from `stack/`. Back to the [README](../README.md).

## The driver API — and why the source tree lies about it

Proven end to end against the running server on 13 Aug. Read this before writing
any driver code, because **the checked-out source describes a different system**.

### The trap, first

The deployed binaries are built from upstream ref `03a7531` plus our patches.
That ref **is an ancestor of this branch's HEAD**
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

Turning the fake one off is not the fix, because the gateway the binary would
then look for is a dead port on 4343 and changing which gateway it calls is a
rebuild. So the guard supplies the missing half in front instead:

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
number.

**Since 2026-09-06 a code is also texted per sign-in** (see *[The SMS gateway](sign-in.md#the-sms-gateway--moorsyl-since-2026-09-06)*
below), through the same substitution — only where the code comes from changed,
which is what this paragraph used to predict. The personal code still works
alongside it, deliberately: it is the one credential that does not depend on a
third party being up, and an outage at the gateway must not ground the fleet.

Three things that bite:

- **The trunk zero is part of the key.** The guard keys on
  `mobileCountryCode + mobileNumber` — `+2130551234567`, not `+213551234567`.
  The script normalises for you; hand-editing the file does not.
- **Enrolling is not enabling.** A freshly enrolled driver signs in and sees that
  he is waiting for approval. Enabling him and attaching a vehicle are
  `/dashboard/` operations, and `/dashboard/` is not published.
- **Six digits, not four.** The guard allows three attempts, so six digits makes
  guessing pointless rather than merely slow. The passenger screen accepted four
  until 2026-09-06 and now accepts six as well, because Moorsyl's Verify codes
  are exactly six characters — a four-character check is refused outright with
  `too_small`. `CODE_LENGTH` in the app's `config.ts` and `codeDigits` on the
  guard's routes must agree, or the symptom is a code that cannot be typed in
  full, which on screen looks like the SMS itself was wrong.

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
