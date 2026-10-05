# Driver Pool Debugging: "Driver never got NEW_RIDE_AVAILABLE"

Use this doc when a rider searched for a ride but a specific driver never received the
`NEW_RIDE_AVAILABLE` push notification. There are multiple filtering gates between a
`search_request` being created and a driver's device buzzing — this doc maps every gate
to the exact code and the exact DB/Redis state you can inspect to find where a driver
was dropped.

## Pipeline overview

```
Rider search → search_request row created → search_try row created
   → SendSearchRequestToDriver scheduler job (recurring, per batch)
      → gate: is search_try still valid? are quotes already maxed?
      → getNextDriverPoolBatch → calculateDriverPoolWithActualDist
           → fetchSortedLTSCandidates      (location fetch, radius filter)
           → parallelRequestsFilterForDriver (in-flight request cap)
           → buildDriverResult             (the big eligibility gate)
           → filterByWalletBalance         (prepaid/cash/airport liability)
      → GoHome / blocklist filtering
      → search_request_for_driver rows created (QSRD.createMany)
      → Notify.sendSearchRequestToDriverNotification → FCM push (NEW_RIDE_AVAILABLE)
```

All code below lives under
`Backend/app/provider-platform/dynamic-offer-driver-app/Main/src/`.

## Stage-by-stage gates

### 1. Job trigger gate
`SharedLogic/Allocator/Jobs/SendSearchRequestToDrivers/Handle.hs:60-96` (`handler`, `processRequestSending`)

- `isSearchTryValid` — reads Postgres `search_try`; requires `status = 'ACTIVE'` and
  `valid_till > now`. If the try already expired or was cancelled/assigned, the job
  never runs again for it.
  Log: `"Search request is either assigned, cancelled or expired."`
- `isReceivedMaxDriverQuotes` — counts rows in `driver_quote` for the search try; once
  count ≥ `maxDriverQuotesRequired` (from driver pool config), **no more batches are
  sent at all**, even to drivers who never saw the request.
  Log: `"Received enough quotes from drivers."`
- `isBatchNumExceedLimit` — Redis batch counter; past the configured limit the job
  stops and either reschedules (scheduled rides) or expires the search.
  Log: `"No driver accepted"`

### 2. Nearby-candidate fetch
`Storage/Queries/Person/GetNearestDrivers.hs:134` (`fetchSortedLTSCandidates`)

- Calls the external **Location Tracking Service** (`SharedLogic/External/LocationTrackingService/Flow.hs:85`, `nearBy`) — **not a Postgres table**. Filters by `merchantId`,
  search radius, and vehicle variant. Excludes drivers already sent this search
  (`excludeDriverIds`), sorts previously-attempted drivers to the tail.
- Log tag to grep: `"DriverPool[1-LTS] N drivers within Xm (excluded=…, notPrev=…, prevAtTail=…)"`

### 3. Parallel-request cap
`Storage/Queries/Person/GetNearestDrivers.hs:318` (`parallelRequestsFilterForDriver`)

- Redis ZSET `driver-offer:DriverPool:Search-Req-Validity-Map-:<merchantId><driverId>`.
  If the driver already has ≥ `maxParallelSearchRequests` concurrent in-flight
  requests, dropped.

### 4. Driver pool data build
`SharedLogic/DriverPool/DriverPoolDataBuilder.hs`, `DriverPoolData.hs`

- Redis key `driver-pool-data:<driverId>` — cached composite record rebuilt from
  Postgres `driver_information`, `vehicle`, `person`, `driver_bank_account`
  (fetched in `SendSearchRequestToDrivers.hs:83-94`).

### 5. The big eligibility gate (most common cause — silent, no per-driver log)
`Storage/Queries/Person/GetNearestDrivers.hs:217` (`buildDriverResult`)

| Guard | Field | Table |
|---|---|---|
| not blocked | `blocked` | `driver_information` |
| enabled | `enabled` | `driver_information` |
| subscribed | `subscribed` | `driver_information` |
| online/mode | `mode`, `active` — must be `ONLINE`/`SILENT`, or (`mode` null) `active = true` | `driver_information` |
| trip-type eligible | `can_switch_to_rental` / `can_switch_to_inter_city` / `can_switch_to_intra_city` | `driver_information` |
| airport check (airport pickup only) | `enable_for_airport = 'ENABLED'` | `driver_information` |
| on-ride forward batching (if already on a ride) | `forward_batching_enabled`, `has_ride_started`, `on_ride_trip_category` | `driver_information` / active `ride` |
| online-payment check (if merchant requires) | `charges_enabled`, bank payment mode | `driver_information` |
| **service tier match** | `vehicle.selected_service_tiers ∩ availableCityTiers ∩ requested tiers`, minus `driver_information.soft_block_stiers` | `vehicle`, `driver_information` |

If the intersected tier list ends up empty (e.g. driver hasn't selected the tier the
rider searched for, or the vehicle's `variant` doesn't map to that tier), the driver
is dropped here with **zero log output**. This is the single most common "driver never
got notified" cause.

### 6. Wallet/liability filter
`Storage/Queries/Person/GetNearestDrivers.hs:338` (`filterByWalletBalance`)

- If the merchant has prepaid-wallet enabled, checks driver's (or fleet owner's)
  wallet balance ≥ fare + threshold. Also gates on minimum cash-wallet balance and
  airport entry-fee balance. Silent drop, no log.

### 7. GoHome / blocklist
`SharedLogic/Allocator/Jobs/SendSearchRequestToDrivers/Handle/Internal/DriverPoolUnified.hs:180-190`

- Active go-home request (`driver_go_home_request.status = 'ACTIVE'`) removes the
  driver from the normal batch.
- Redis lists `Block-Listed-Drivers-Key:SearchRequestId-<searchRequestId>` and
  `Block-Listed-Drivers-Key:RiderId-<riderId>` — populated when the driver previously
  cancelled this search or this rider.

### 8. Notification creation & send
`SharedLogic/Allocator/Jobs/SendSearchRequestToDrivers/SendSearchRequestToDrivers.hs:163-217`

- A `search_request_for_driver` row is created per surviving driver
  (`QSRD.createMany`). **This table is ground truth** — a row here means the driver
  passed every pool filter and a notification was attempted.
- `Notify.sendSearchRequestToDriverNotification`
  (`Tools/Notifications.hs`, tag `NEW_RIDE_AVAILABLE`) looks up the driver's push
  config via `person.device_token` and sends FCM. A missing/stale token means the
  push silently fails downstream of the pool logic entirely.

## Schema reference (verified against `Backend/dev/migrations-read-only/dynamic-offer-driver-app/`)

All tables below are in schema `atlas_driver_offer_bpp`.

| Table | Purpose | Key columns |
|---|---|---|
| `search_request` | The rider's search | `id`, `rider_id`, `from_location_id`, `to_location_id`, `estimated_distance` |
| `search_try` | One pooling attempt for a search | `id`, `request_id`, `status`, `valid_till`, `vehicle_variant`, `service_tier_array` |
| `driver_quote` | Quotes drivers have submitted | `driver_id`, `search_try_id`, `status` |
| `search_request_for_driver` | One row per driver a search was sent to — **ground truth for "was this driver notified"** | `id`, `driver_id`, `search_try_id`, `status`, `batch_number`, `vehicle_service_tier`, `response`, `created_at`, `responded_at`, `rendered_at` |
| `driver_information` | Driver eligibility flags | `driver_id`, `active`, `mode`, `blocked`, `enabled`, `subscribed`, `can_switch_to_*`, `enable_for_airport`, `on_ride`, `soft_block_stiers` |
| `vehicle` | Vehicle + selected tiers | `driver_id`, `variant`, `category`, `selected_service_tiers` |
| `driver_go_home_request` | Go-home mode state | `driver_id`, `status`, `reached_home` |
| `person` | Push notification target | `id`, `device_token` |

**Not a Postgres table:** driver GPS location is served by the external Location
Tracking Service (`SharedLogic/External/LocationTrackingService/Flow.hs`, Redis-geo
backed, HTTP API) — it cannot be queried from Metabase/SQL. If everything below checks
out and the driver is still not getting requests, suspect stale/missing GPS pings and
check LTS directly (its own debug endpoint or Redis), not SQL.

## Debug queries

Replace `<driverId>`, `<searchTryId>`, `<searchRequestId>`, `<riderId>` as needed.

```sql
-- 1. Driver's own eligibility state — start here
SELECT driver_id, active, mode, blocked, blocked_reason, enabled, subscribed,
       can_switch_to_rental, can_switch_to_inter_city, can_switch_to_intra_city,
       enable_for_airport, on_ride, on_ride_trip_category, forward_batching_enabled,
       has_ride_started, soft_block_stiers, soft_block_expiry_time,
       merchant_id, merchant_operating_city_id, updated_at
FROM atlas_driver_offer_bpp.driver_information
WHERE driver_id = '<driverId>';
```

```sql
-- 2. Vehicle + selected service tiers (tier-match gate)
SELECT driver_id, variant, category, registration_category, selected_service_tiers
FROM atlas_driver_offer_bpp.vehicle
WHERE driver_id = '<driverId>';
```

```sql
-- 3. Did the pool ever create a notification row for this driver?
-- Row exists  => driver passed pool filtering, notification was attempted (check FCM/device_token next)
-- No row      => driver was filtered out somewhere in stages 2-7 above
SELECT id, search_try_id, request_id, status, batch_number, vehicle_variant,
       vehicle_service_tier, response, is_part_of_intelligent_pool,
       actual_distance_to_pickup, straight_line_distance_to_pickup,
       created_at, start_time, search_request_valid_till, responded_at, rendered_at
FROM atlas_driver_offer_bpp.search_request_for_driver
WHERE driver_id = '<driverId>'
ORDER BY created_at DESC
LIMIT 20;
```

```sql
-- 4. The search_try + search_request behind a given attempt
SELECT st.id AS search_try_id, st.status, st.valid_till, st.vehicle_variant,
       st.service_tier_array, st.trip_category, st.created_at,
       sr.id AS search_request_id, sr.rider_id, sr.estimated_distance,
       sr.from_location_id, sr.to_location_id, sr.merchant_operating_city_id
FROM atlas_driver_offer_bpp.search_try st
JOIN atlas_driver_offer_bpp.search_request sr ON sr.id = st.request_id
WHERE st.id = '<searchTryId>';
```

```sql
-- 5. Were quotes already maxed out before this driver's batch ran?
SELECT driver_id, search_try_id, status, created_at
FROM atlas_driver_offer_bpp.driver_quote
WHERE search_try_id = '<searchTryId>'
ORDER BY created_at;
```

```sql
-- 6. Is the driver in an active go-home state (removed from normal batch)?
SELECT id, status, lat, lon, reached_home, num_cancellation, created_at, updated_at
FROM atlas_driver_offer_bpp.driver_go_home_request
WHERE driver_id = '<driverId>'
ORDER BY created_at DESC
LIMIT 5;
```

```sql
-- 7. FCM delivery prerequisite: does the driver have a device token?
SELECT id, device_token, merchant_id, merchant_operating_city_id
FROM atlas_driver_offer_bpp.person
WHERE id = '<driverId>';
```

## Redis keys to inspect

| Key pattern | Purpose |
|---|---|
| `driver-pool-data:<driverId>` | Cached snapshot the eligibility gate (`buildDriverResult`) actually evaluated |
| `driver-offer:DriverPool:Search-Req-Validity-Map-:<merchantId><driverId>` (ZSET) | Parallel in-flight search-request count for the driver |
| `Block-Listed-Drivers-Key:SearchRequestId-<searchRequestId>` (list) | Drivers blocklisted for this specific search request |
| `Block-Listed-Drivers-Key:RiderId-<riderId>` (list) | Drivers blocklisted for this rider (e.g. past cancellations) |

## Debug checklist

1. Run query 3 for the driver. **This is the fastest fork in the road.**
   - Row exists → the pool logic worked; go to step 5 (notification delivery).
   - No row → the driver was filtered before notification creation; go to step 2.
2. Run query 1 — check `blocked = false`, `enabled = true`, `subscribed = true`,
   `mode` is `ONLINE`/`SILENT` (or `active = true` if `mode` is null).
3. Run query 2 — confirm `selected_service_tiers` on the vehicle includes the tier the
   rider searched for (cross-check against `search_try.service_tier_array` from
   query 4).
4. Run query 6 — confirm the driver isn't in an `ACTIVE` go-home state. Also check the
   two `Block-Listed-Drivers-Key:*` Redis lists for the driverId.
5. Run query 5 against the relevant `search_try_id` — if `driver_quote` rows already
   hit the configured max before this driver's batch ran, no notification was ever
   attempted for them, regardless of eligibility.
6. If query 3 showed a row but nothing arrived on-device, run query 7 — a missing or
   stale `device_token` means the push fails after pool logic succeeded. Cross-check
   FCM/push-provider logs for that token.
7. If everything above is clean, grep application logs for `"DriverPool[1-LTS]"` and
   `"calculateDriverPool"` around the search's `search_try_id`/transaction id to see
   raw candidate counts before the deep filters, then suspect the Location Tracking
   Service (driver's live GPS ping) since that layer isn't visible in Postgres.

## Related docs

- Ride lifecycle: `06-ride-flow.md`
- BECKN protocol flow: `05-beckn-protocol-flow.md`
- General debugging patterns: `14-testing-and-debugging.md`
- Database/query patterns: `08-database-patterns.md`
