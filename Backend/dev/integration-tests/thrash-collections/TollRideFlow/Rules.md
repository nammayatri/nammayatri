# TollRideFlow — Integration Test Rules

End-to-end: create a toll with **Polygon start + LineString end** gates on the test corridor → verify in dashboard list → cab (SEDAN) ride search → assert `tollChargesInfo` → complete ride → delete toll. Legacy LineSegment gates cannot be created via dashboard API.

## Prerequisites

Same as [TollConfigFlow/Rules.md](../TollConfigFlow/Rules.md):

- Mobility stack + provider-dashboard + driver-app + rider-app
- **OSRM on port 5001** (rider `POST /rideSearch` uses OSRM for routes; do not kill `:5001` for this flow)
- Toll dashboard `access_matrix` (auto-seeded by test dashboard, `./run-tests.sh toll-ride`, or `local-testing-data/toll-dashboard-access.sql`)
- `provider-dashboard.sql` if the admin token lacks `merchant_access` for the target merchant/city

## Toll detection vs rider API

- BPP detects tolls from the **search route polyline** (OSRM road geometry) crossing line gates.
- Rider `tollChargesInfo` is only returned for **toll-applicable** vehicle tiers (cab/SEDAN/TAXI, etc.). **AUTO_RICKSHAW** estimates do not get `tollChargesInfo` even when a toll is detected — this flow asserts on `vehicle_variant` (default **SEDAN**).

## Gate geometry (env)

TollRide uses **Polygon start gate** (box around origin / early corridor) + **GeoJSON LineString end gate** near destination. BPP requires the search route to cross **both** start and end gates.

Env keys: `toll_polygon_coordinates`, `toll_end_lat1/lon1/lat2/lon2` in `Local/Local_*_*.postman_environment.json`.

## Running

```bash
cd Backend/dev/integration-tests

# Ensure OSRM is up (mobility stack / local dev)
# lsof -i :5001   # should show osrm or similar listener

./run-tests.sh toll-ride
./run-tests.sh toll-ride NY_Bangalore
./run-tests.sh toll-ride BT_Delhi
```

Each run creates a **new** toll name (`INT_TEST_TOLL_RIDE_<suffix>`). Re-run after changing gate env values so the upsert uses fresh coordinates.

## Collections

| File | What it tests |
|------|----------------|
| `01-TollRideFlow.json` | Toll upsert + list verify + cab onboarding + search/results toll assertion + ride completion + cleanup |

Dashboard flow: `POST .../user/switchMerchantAndCity` → `POST .../merchant/toll/upsert` → `GET .../merchant/config/toll/list` → driver onboarding → ride steps.

## If `tollChargesInfo` is still null

1. Confirm toll appears in dashboard list step (gates non-empty).
2. Confirm OSRM is reachable (`FAILED_TO_CALL_OSRM_ROUTE_API` means `:5001` is down).
3. Confirm **List tolls** step passed (toll in DB + Redis cache warmed for `merchantOperatingCityId`).
4. Check BPP logs on Beckn search for `getTollInfoOnRoute`; widen `toll_polygon_coordinates` or end-gate lon span if OSRM polyline misses gates.
5. Inspect **cab** tiers (`SEDAN`, `TAXI`, …) — AUTO never gets `tollChargesInfo` on the rider API.
