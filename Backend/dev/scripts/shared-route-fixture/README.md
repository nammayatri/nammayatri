# Shared-route (corporate shuttle) dev fixture

The company, route, fleet, drivers, bus and riders every shuttle module tests against. LOCAL ONLY. Three pieces:

| Piece | Where | How it is applied |
|---|---|---|
| Config: feed rows, knobs (incl. the feeds the engine reads), fares, run fare product, the company | `dev/local-testing-data/shared-route-extra-setup.sql` | automatically, with every other `local-testing-data/*.sql`, by the dev stack's context API; or by hand with psql |
| People and vehicle: fleet owner, two drivers, the bus on the route, three riders | `apply-fixture-data.sh` + `fixture-data.sql` (here) | run the script once the stack is up; it needs the local passetto for the encrypted columns |
| Timetable: the GTFS feed `corp_chennai` | `corp_chennai.gtfs/` (here) | load into the local GIMS, see below |

## Loading the GTFS feed

The feed follows the shared-route GTFS convention (Plan tab of the shared-mobility doc):

| Thing | Value |
|---|---|
| feed id (`feed_info.feed_id`, GIMS `gtfs_id`, `integrated_bpp_config.feed_key`) | `corp_chennai` |
| agency id (= `integrated_bpp_config.agency_key`, same in both DBs) | `corp_chennai` |
| route | `CORP-ACME-1`, Adyar Depot to the ACME campus at Sholinganallur |
| stops, in order | `CORP_ACME_S1` … `CORP_ACME_S5`, 10 minutes apart |
| trips | `CORP-ACME-1-0830` (direction 0, S1 to S5) and `CORP-ACME-1-1800` (direction 1, S5 to S1) |
| calendar | `corp_acme_weekday`, Monday to Friday, 2026-11-01 to 2027-12-31 |

Once module C's publish tool exists it writes this same shape; until then load the feed by hand, either way:

1. **GIMS editor import** (preferred, no OTP rebuild): zip the folder and post it as the body.
   ```bash
   cd Backend/dev/scripts/shared-route-fixture && zip -r corp_chennai.gtfs.zip corp_chennai.gtfs
   curl -X POST "$GIMS/internal/gtfs-editor/feeds/corp_chennai/import" --data-binary @corp_chennai.gtfs.zip
   ```
   `$GIMS` is the local gtfs-inmemory server the rider-app's `MultiModalStaticData_OTPTransit` config points at.
   The import needs an admin sign-in on the editor; `?dry_run=true` first reports what it would write.
2. **Through OTP**: copy `corp_chennai.gtfs` into the `nandi` repo's `assets/`, zip it there (`zip -r corp_chennai.gtfs.zip corp_chennai.gtfs`),
   add it to the OTP build config and `docker-compose up`; GIMS picks it up on its next poll.

Changing a stop, trip or time means re-importing (GIMS `POST /feeds/corp_chennai/reload`) and re-applying
`shared-route-extra-setup.sql`, whose fares are keyed on the stop codes.

## Materialising and driving runs

Runs come from the driver platform's shared-run engine, a daily allocator job, not from the apps. Once the feed is in GIMS
and the config half is applied (`shared_run_config.feeds` names `corp_chennai`), arm the job once for the city from the
dashboard's scheduler trigger (`POST /scheduler/trigger` with job name `MaterialiseSharedRunsTrigger`, the merchant and city
ids in `jobData`, and `scheduledAt` now). The job runs in the allocator, so it must be up (`, run-mobility-stack-dev` starts
it); its log line `shared runs materialised for city ...` counts, per service day, the trips read, the runs created, the
ones that already existed and the ones handed to the bus bound to the route. It re-arms itself a day later and a second
run creates nothing. The bus `TN01SH0001` is bound to `CORP-ACME-1`, so both of
the day's runs land on driver 1 as UPCOMING rides. The driver then uses the ordinary ride API to start and end the run
and to arrive at and depart its stops (a stop where nobody boards or alights is departed by the arrive call itself), and the
shared cab's `GET /sharedCab/session` and `POST /sharedCab/booking/{bookingId}/operations` for the passenger list, boarding,
no-shows and drops: on a run those answer from the ride's bookings instead of a cab session.

## Who is who

Phones are `99999001xx`, dev OTP `7891`, login tokens `e2e-corp-<who>-token`, ids `md5('e2e-corp-…')::uuid`.

| Who | Phone | Notes |
|---|---|---|
| fleet owner "ACME shuttle fleet" | 9999900110 | the Namma Yatri-owned entity |
| driver 1 | 9999900111 | owned driver; the bus `TN01SH0001` (BUS_NON_AC, 20 seats) is on him and bound to `CORP-ACME-1` |
| driver 2 | 9999900112 | curated (DCO) driver in the same fleet, tag `DCO#true` |
| riders 1, 2 | 9999900121, 9999900122 | ACME staff (work-email verification lands with module C) |
| rider 3 | 9999900123 | the walk-up who should be refused a subscription |
