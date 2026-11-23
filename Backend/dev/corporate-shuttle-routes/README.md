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

1. **Export the interest forms.** In the control centre, call the dashboard API
   `GET /{merchant}/{city}/shuttleInterest/list?passOrganizationId=<company>&limit=100&offset=0`. Save the response as
   `interests.json`. Save each further page (`offset=100`, …) as another file.
2. **Draft the routes:**
   ```
   python3 suggest_stops.py chennai ACME interests.json [page2.json ...] --start 20261201 --end 20271130 --fare 40
   ```
   This writes `out/chennai_ACME_suggested.json`.
   - **Stops**: placed so as many employees as possible can walk to one within their own walking distance.
   - **Office**: added as the last stop (morning route) or the first stop (evening route).
   - **Departures**: 15-minute slots that fit the most employees' times, within their flexibility.

   The stop names are placeholders. Rename each one after a public landmark. Never use an employee's address.
3. **Check the draft against the demand:**
   ```
   python3 demand_check.py chennai ACME interests.json --plan out/chennai_ACME_suggested.json
   ```
   - It prints how many employees are covered (walk, time and days), which stop each one uses, and why the rest aren't covered.
   - It writes `out/chennai_ACME_demand.geojson`. Open it in https://geojson.io: green dots are covered employees, red
     dots aren't, the circles are their walking distances, and the lines are the routes.
   - Edit the plan and run the check again until the coverage is acceptable.
4. **Save the plan** as `plans/<city>/<ORG>.json`. The format is described at the top of `plan.py`.
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
   byte-identical zip.
6. **Release.** This is the same path as the shared cab's feed (`nandi/docs/RELEASE.md`):
   1. Copy the zip into `nandi/assets/` as `chennai.corp_shuttle.gtfs.zip`.
   2. Run the Nandi job with `buildGimsData=true`. This builds the GTFS data image.
   3. Run the GIMS job on that data image, then release GIMS in system-control-centre.
   4. Apply `chennai_fares.sql` on that environment's rider DB. First put the environment's own ids in
      `cities/chennai.json`; the committed ids are the local fixture's.

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
   6. `POST /shuttle/interest` with the pickup, drop, times, days, walking distance and flexibility.

   A rider with `@gmail.com` gets `COMPANY_NOT_ONBOARDED`. Removing `acme.example` from the company makes eligibility
   false again.
4. **Routes from that data**: run steps 1–5 above.
   - `examples/chennai_acme_interests.json` is a ready-made export of 12 test employees, if you want to try the tools
     without step 3.
   - `plans/chennai/ACME.json` is the fixture's route `CORP-ACME-1`.
5. **Load the feed locally**, the way the shared cab does it (its local setup points the city at OTP on 8089 and GIMS on
   8090):
   1. Make a scratch directory containing:
      - `out/chennai.corp_shuttle.gtfs/` (the flat feed)
      - a copy of `nandi/assets/chennai.osm.pbf`
      - copies of `nandi/otp-configs/*.json`, with only the chennai entry in `build-config.json`'s `osm` list
   2. Run `java -Xmx4G -jar otp-2.5.0-shaded.jar --build --serve --port 8089 <dir>`.
   3. Start GIMS on 8090 with its dev config, pointed at that OTP. It reads routes and stops from it at boot.
6. **Check the routes and stops in GIMS:**
   - `curl localhost:8090/routes/corp_chennai` → `CORP-ACME-1`
   - `curl localhost:8090/route-stop-mapping/corp_chennai/route/CORP-ACME-1` → `CORP_ACME_S1` … `CORP_ACME_S5`, in order
   - `curl localhost:8090/stops/corp_chennai` → the 5 stops
7. **Check them through rider-app**, the way bus and shared cab read them:
   1. Apply `out/chennai_fares.sql`, clear the Redis config caches, and restart rider-app.
   2. `GET /frfs/route/CORP-ACME-1?vehicleType=BUS&integratedBppConfigId=5190da6e-3400-8d10-9f5a-62af9dbf7dff` → the route
      and its stops.
   3. `GET /frfs/stations?vehicleType=BUS&routeCode=CORP-ACME-1` → the 5 stops.
   4. The fare from S1 to S5 is 40 INR.

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
| `geo.py` | distances, draft driving times, walking circles |
| `suggest_stops.py` | interest forms → draft plan |
| `demand_check.py` | plan + interest forms → coverage report and GeoJSON map |
| `build_feed.py` | plans → GTFS zip + fare SQL |
| `validate.py` | rules on the built feed and fares; `--selftest` |
| `cities/<city>.json` | the environment's ids for the fare SQL (local fixture ids in `chennai.json`) |
| `plans/<city>/<ORG>.json` | one reviewed plan per company |
| `examples/` | a synthetic interest export for trying the tools |
| `out/` | generated, not committed |
