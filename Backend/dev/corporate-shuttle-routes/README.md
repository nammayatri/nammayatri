# Corporate shuttle routes (module C)

These tools take a company's interest forms, turn them into routes, and publish the routes as the city's GTFS feed
`corp_<city>`. Rider-app then reads the routes and stops from GIMS, the same way it reads bus and shared-cab routes.

The tools use plain Python 3 with no dependencies. Run them from this folder. They're modelled on the shared cab's feed
builder, `nandi/scripts/shillong-shared-cab/`.

## Naming convention

| What | Id | Notes |
|---|---|---|
| feed, agency | `corp_<city>` | one feed per city, shared by every partner company |
| route | `CORP-<ORG>-<n>` | `<ORG>` = the company code (upper-case letters and digits) |
| stop | `CORP_<ORG>_S<n>` | numbered per company, in order of first use; `stop_code` = `stop_id` |
| trip | `<route>-<HHMM>` | the departure time from the route's first stop |
| service | `MON_TUE_..._<start>_<end>` | one per distinct set of days and dates |

The org code inside every id keeps companies apart in the shared feed. It's all defined in `naming.py`.

## From interest forms to a live route

1. **Export the interest forms**, or let step 2 read them itself:
   - `python3 plan_routes.py <city> --from-api` reads every form of the city from the local dashboard API
     (`shuttleInterest/list`, page by page) and saves them as `out/<city>_interests.json`.
   - Or save the responses of `GET /{merchant}/{city}/shuttleInterest/list?limit=100&offset=0` yourself, one file per
     page (`offset=100`, …), and pass the files to step 2.
2. **Plan the routes** for every company in the city:
   ```
   python3 plan_routes.py chennai --from-api [--start 20261201] [--end 20271130] [--fare 40]
   python3 plan_routes.py chennai interests.json [page2.json ...]      # from saved exports instead
   ```
   This writes `plans/chennai/<ORG>.json` (one per company) and `out/chennai_plan_report.txt`.
   - **Company codes**: from `"orgs"` in `cities/<city>.json`, else the company's initials. Two companies that would
     share a code stop the run before any plan is written; give them their own codes under `"orgs"`.
   - **Pickup points**: employees within 500 m of each other share one point, whatever their time.
   - **Routes**: points within 3 km of each other are one route, planned once per company and direction (morning to
     the office, evening home), so a route has the same stops every day (`planner.py`).
   - **Shuttles**: employees on a route with the same time share a shuttle (a departure): 8 seats, 5–8 riders
     preferred. A full time is split into shuttles 5 minutes apart; a light one joins the nearest time within 30 minutes.
   - **Stop names**: the public road the stop is on (OSRM), else "Pickup point". An employee's address is never used.
     Rename a stop after a public landmark if you like.
3. **Review the plans** in `plans/<city>/<ORG>.json`. The format is described at the top of `plan.py`.
4. **Set the environment's ids.** The committed `cities/<city>.json` holds the local fixture's ids. Before building
   for another environment, put that environment's ids in it, since step 5 writes them into the fare SQL.
5. **Build and validate the feed:**
   ```
   python3 build_feed.py chennai
   python3 validate.py chennai     # inside the nix shell, gtfstidy also runs
   ```
   This writes three things under `out/`:
   - `chennai.corp_shuttle.gtfs.zip`: the feed, with an inner `chennai.corp_shuttle.gtfs/` folder (the `nandi/assets`
     layout)
   - `chennai.corp_shuttle.gtfs/`: the same feed, flat
   - `chennai_fares.sql`: a seat fare for every forward stop pair, in the shape of section 3 of
     `Backend/dev/local-testing-data/shared-route-extra-setup.sql`

   Every company plan in `plans/<city>/` goes into the one city feed. A rebuild with no plan change gives a
   byte-identical zip. If you change `cities/<city>.json` after this step, build and validate again.
6. **Release.** This is the same path as the shared cab's feed (`nandi/docs/RELEASE.md`):
   1. Copy the zip into `nandi/assets/` as `chennai.corp_shuttle.gtfs.zip`.
   2. Run the Nandi job with `buildGimsData=true`. This builds the GTFS data image.
   3. Run the GIMS job on that data image, then release GIMS in system-control-centre.
   4. Apply `out/chennai_fares.sql` (built in step 5 with this environment's ids) on that environment's rider DB.

   The route is live once GIMS serves the feed and the fares exist.

## Local test, end to end

All of this runs against local services only.

1. **Stack**: run `, run-mobility-stack-dev`, then apply
   `Backend/dev/local-testing-data/shared-route-extra-setup.sql`. It creates the ACME company with email domain
   `acme.example`, the `corp_chennai` integrated BPP configs, and the local OTP/GIMS endpoints.
2. **Company**: in the dashboard, call `POST /{merchant}/{city}/passOrganization/update/{personId}` with
   `{"name": "ACME Corp", "passEnum": "RegularPass", "emailDomains": ["acme.example"]}`.
   - The same call with `"emailDomains": ["gmail.com"]` is rejected.
   - Leaving `emailDomains` out keeps the current domains.
3. **Interest test data.** For each test rider (dev `/auth` OTP), using rider-app at `http://localhost:8013/v2`:
   1. `POST /profile` with `{"businessEmail": "<name>@acme.example"}`.
   2. `POST /auth/business-email/send-verification`.
   3. Read the OTP:
      `select auth_value_hash from atlas_app.registration_token where entity_id = '<personId>' and info = 'businessEmailVerification' order by created_at desc limit 1;`
   4. `POST /auth/business-email/verify` with `{"otp": "<otp>"}`.
   5. `GET /shuttle/eligibility` → `eligible: true`, `companyName: "ACME Corp"`, and `officeAddress`.
   6. `POST /shuttle/interest` with the pickup, drop, trip type, times and days.

   A rider with `@gmail.com` gets `COMPANY_NOT_ONBOARDED`. Removing `acme.example` from the company makes eligibility
   false again.
4. **Routes from that data**: run steps 1–5 above.
   - `plans/chennai/ACME.json` is the fixture's route `CORP-ACME-1`.
5. **Publish locally**: `python3 publish_local.py chennai --start-gims`. It builds and validates the feed, runs Nandi's
   GTFS preprocessor on it (used as it is), starts the local GIMS (built in `gtfs-inmemory-server-rust`) on 8090, and
   applies the feed config, fares and published timetable to the local rider DB, then clears the related caches.
   Restart rider-app afterwards, since it keeps GIMS answers in memory.
6. **Check the routes and stops in GIMS:**
   - `curl localhost:8090/routes/corp_chennai` → `CORP-ACME-1`
   - `curl localhost:8090/route-stop-mapping/corp_chennai/route/CORP-ACME-1` → `CORP_ACME_S1` … `CORP_ACME_S5`, in order
7. **Check them through rider-app**:
   - Dashboard `GET /{merchant}/{city}/shuttleInterest/publishedRoutes?day=monday` → the routes, stops and that day's
     departures and riders.
   - The fare from S1 to S5 is 40 INR.

## Tests

```
python3 -m unittest discover -s tests
python3 validate.py --selftest      # breaks each validation rule on purpose and checks that it fires
```

## Files

| File | Purpose |
|---|---|
| `naming.py` | the id convention |
| `plan.py` | loads and checks plans, numbers the stops, gives the fare of a stop pair |
| `interests.py` | loads the dashboard's interest export |
| `geo.py` | distances and draft driving times |
| `plan_routes.py` | interest forms → `plans/<city>/<ORG>.json` + planning report |
| `planner.py` | one company's routes for one direction (pickup points, roads, each day's shuttles) |
| `osrm.py` | OSRM road snapping and routing |
| `build_feed.py` | plans → GTFS zip + fare SQL |
| `validate.py` | rules on the built feed and fares; `--selftest` |
| `publish_local.py` | local only: build, Nandi preprocess, local GIMS, local DB rows |
| `cities/<city>.json` | the environment's ids and API/GIMS settings (local fixture ids committed) |
| `plans/<city>/<ORG>.json` | one reviewed plan per company |
| `out/` | generated, not committed |
