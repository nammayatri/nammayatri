# TollConfigFlow — Integration Test Rules

Provider-dashboard toll CRUD with **GeoJSON LineString and Polygon** geometry objects only. Legacy `{start,end}` LineSegment gates are rejected on create but may still exist in DB from older rows.

## Prerequisites

1. **Services running** (from `Backend/`, nix shell): mobility stack + `provider-dashboard` (default port 8018), `dynamic-offer-driver-app` (8016).
2. **Postgres** on `localhost:5434`, database `atlas_dev`.
3. **Dashboard auth** (one-time or after DB reset):
   - **Auto (recommended):** `./run-tests.sh toll-config` seeds access before Newman runs.
   - **Manual access_matrix:**
     ```bash
     psql -h localhost -p 5434 -U atlas_superuser -d atlas_dev \
       -f Backend/dev/local-testing-data/toll-dashboard-access.sql
     ```
   - **Manual person + token + cross-merchant `merchant_access`** (if toll create returns 403):
     ```bash
     psql -h localhost -p 5434 -U atlas_superuser -d atlas_dev \
       -f Backend/dev/local-testing-data/provider-dashboard.sql
     ```
4. **Environment files:** `Local/Local_NY_Bangalore.postman_environment.json`, `Local/Local_BT_Delhi.postman_environment.json` — use `dashboard_token` `local-admin-token-bangalore-namma-yatri` (JUSPAY_ADMIN).

## Running

```bash
cd Backend/dev/integration-tests
./run-tests.sh toll-config              # all toll config cities
./run-tests.sh toll-config NY_Bangalore # Bangalore only
./run-tests.sh toll-config BT_Delhi     # Delhi only
./run-tests.sh toll                     # config + ride suites
```

Skip automatic SQL seed: `NY_TEST_SKIP_TOLL_SEED=1 ./run-tests.sh toll-config`

Seed only: `./run-tests.sh --setup` (runs toll dashboard SQL + `provider-dashboard.sql`)

## Collections

| File | What it tests |
|------|----------------|
| `01-TollDashboardCrud.json` | Reject legacy LineSegment create; LineString + Polygon create/list/update/delete; CSV upsert |

First request is **Switch City** (`POST .../user/switchMerchantAndCity`), then toll CRUD on the internal dashboard API. Same pattern as `RideBookingFlow/01-AutoRideFlow.json`.

## Related

- **TollRideFlow** — toll on estimate during auto ride; shares the same setup SQL. See `../TollRideFlow/Rules.md`.
- **Source migrations:** `Backend/dev/migrations-read-only/provider-dashboard/Local_API_Management_Merchant.sql` (toll rows at end of file).
