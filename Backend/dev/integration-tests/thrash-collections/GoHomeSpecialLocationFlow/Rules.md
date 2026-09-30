# GoHomeSpecialLocationFlow

E2E test for the Go-Home **blocked special location** feature: a driver cannot set a home
location inside a special location listed in
`go_home_config.blocked_home_special_location_ids`. The backend **silently drops** the write
(returns `Success`, persists nothing) — no error is surfaced.

## Run

```bash
cd Backend/dev/integration-tests
./run-tests.sh gohome                 # Bangalore
./run-tests.sh gohome NY_Bangalore    # explicit city
```

`run-tests.sh gohome` auto-runs `setup-gohome-special-location.sql` before the suite.

## What it does

1. Driver auth → OTP → Switch City → Add Vehicle → Enable Driver (driver must be enabled to
   add a home location).
2. **Add home INSIDE** the airport special location (`sz_origin` = 77.7087, 13.1997) → asserts
   `200 Success`.
3. **Add home OUTSIDE** (Koramangala, `12.9352, 77.6245`) → asserts `200 Success`.
4. **Get home locations** → asserts the inside/airport home was **NOT** persisted and the
   outside home **WAS** persisted.

## Data dependency (important)

- `setup-gohome-special-location.sql` blocks **whichever enabled special location covers the
  airport test point**, resolved dynamically from `special_location.geom_geo_json` (the same
  column the app reads). No special-location id is hardcoded.
- It relies on an **existing** enabled airport special location at `sz_origin` in local data
  (the same point the special-zone ride suites use). If none exists, the seed emits a
  `WARNING` and the "blocked" assertion will fail — check that local `special_location` data
  is imported and `geom_geo_json` is backfilled.

## Cache notes

- Special locations live in a **per-process in-memory cache** (1h TTL). This suite reuses an
  existing special location, so **no app restart is needed**. If you instead add a *new*
  special location, the driver-app must be restarted for it to be visible.
- `go_home_config` is CAC-cached and re-read after the harness `FLUSHALL` (which `run_single`
  performs before each collection), so the seeded blocked list takes effect on the next request.

## Idempotency

Random driver + vehicle per run. The seed is idempotent and leaves the Bangalore
`blocked_home_special_location_ids` set to the airport special location (harmless — only
affects home-location setting).
