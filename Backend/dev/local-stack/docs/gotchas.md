# Gotchas and known limitations

Traps that each cost an afternoon, and what this stack does not do.

> Moved here from the local-stack README on 2026-10-08 (phase 7), word for word: dates and measurements are as they were taken. Commands written `./x.sh` run from `stack/`. Back to the [README](../README.md).

## Gotchas

**The SMS gateway can be dead for two weeks and nobody notices, because every
number that ever signs in here is exempt.** Measured 2026-09-20: Moorsyl
answers our key with `401 Unauthorized`, and `/healthz` had `sent: 0` with that
error sitting in `lastError`. The guard log held **exactly one** real attempt
since it started — the client's own number, 2026-09-17 17:32 — because
`SMS_BYPASS` covers every test number the team uses and those send nothing.

Two consequences worth carrying:

- **Check the gateway itself, not the sign-ins.** `/healthz` reports
  `gateway.configured`, `sent` and `lastError`. `sent: 0` after a day of work
  means nothing has been tested that a real user does.
- **The free key test is `POST /verify/check` with a made-up id**: a good key
  answers 404 "does not belong to this organization", a dead one answers 401.
  It sends no message and costs nothing. Both header styles (`x-api-key` and
  `authorization: Bearer`) answered 401 that day, which is how we know it is
  the key and not the header.

**A new `maps-shim` route is unreachable until the edge has a `location` for
it.** The shim listens on `127.0.0.1:8030` and is published nowhere; the only
way in from outside the box is an explicit block in
`/opt/ny/local-stack/edge/nginx.conf`. Without one the route answers
**correctly on 8030 and 404 from every phone**, which reads as a broken app
rather than as missing routing. The whole wallet server shipped that way on
2026-09-07 and was found by a probe calling the *public* host — one calling
`127.0.0.1:8030` would have passed and proved nothing.

**The repo's `edge/nginx.conf` is 11 KB behind the deployed one.** The website
session's admin-console and movinapp.net blocks only ever existed on the box.
Copying the repo version over would delete the admin console's routing. **Edit
the deployed file in place and insert the same block into the repo copy
separately.**

**After `nginx -s reload`, the first request can still hit the old config.** A
verification loop run immediately reported one 404 while the three paths after
it passed. A race, not a fault — do not diagnose a reload from its first
response.

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

## Known limitations

- **This is the 2023 baseline, not current `main`.** Running today's backend
  would need a full Haskell build *and* merchant config we don't have.
- **The rider's cancellation reasons are still in English.** Measured
  2026-08-30: `atlas_driver_offer_bpp.cancellation_reason` carries six French
  reasons, and `atlas_app.cancellation_reason` carries seven of upstream's —
  *"My fare was too high."*, *"ETA was too long."* A passenger picking a reason
  reads them. One SQL statement, nothing else.
- **Two cancellation codes are stored that no list contains.**
  `CHANGE_OF_PLANS` (3 rows, rider side) and `PASSENGER_CANCELLED_ON_SITE` (1
  row, driver side). The app sends them; the seeded tables do not know them.
  Anything joining reasons to their labels renders an empty cell for those, and
  an empty cell reads as *no reason given* when one was.
- **A driver rating a passenger stores no row and no comment.**
  `rateCustomer(rideId, stars)` writes only an average, a count and a running
  sum onto `rider_details` — so there is no history to list, nothing to read
  back per ride, and **a second POST for the same ride counts twice**. The app
  disables its control the moment one succeeds; anything else calling that
  route must do the same. Passenger→driver is the opposite: a row each, with
  the tags and a written comment below three stars.
- **The agency cannot write to a driver, and if it could he would not see it.**
  Two separate failures, and only the second is ours to fix.
  `atlas_driver_offer_bpp.message`, `message_translation` and `message_report`
  all exist and have been **empty since this stack existed** — upstream fills
  them from a dashboard we do not run, so nothing writes them today. That half
  is straightforward: a message is one row in `message` plus **one row per
  driver** in `message_report`, and the per-driver row is what makes "who read
  it" answerable at all. The other half is not: **`GET /ui/message/list`
  answers `500` the moment it has a row to return.** Empty it answers
  `200 []`. Measured 2026-08-24 both ways round, with a French translation,
  with an English one, with the driver's `language` set and null, and with no
  attachment — always 500, with no detail on the wire; the reason exists only
  in the container log. `probe-agency-messages.py` walks the row up one field
  at a time to find out which column, and **has never been run**. Until it is,
  an admin site can record a message and the driver will never be shown it.
  Related: a "new message" push would be **silent**, because the app writes the
  French words itself and drops a notification type it does not recognise —
  one line in the app, not a backend change.
- **Nothing uploads a driver's papers.** `DOCUMENT_UPLOAD_URL` in the app is
  `null`, so the licence and the carte grise stay on the phone that
  photographed them and the enrolment screens say so plainly. The admin site is
  the missing half — see `movin.deletion_request` for the shape that half takes
  when it is built.
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
  which build produced **them** (5 August); `setup.sh` refuses to start without
  them. **On the live server the rider and driver apps do not run from `bin/`**
  but from the image `ghcr.io/nammayatri-algeria/ny-backend:latest` (`ny-rider:patched`),
  CI run #10 of 2026-09-14 (tag `03a7531-10`, digest `108eca6c…`), which carries
  the two-country patches. Hashed inside the containers on 2026-10-05: the
  gateway and the registry match `bin/`, `rider-app-exe` and
  `dynamic-offer-driver-app-exe` do not. Ask the containers, not the folder.
