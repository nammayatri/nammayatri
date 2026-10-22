# Driver analytics tests: sessionizer and driver performance

`atlas_kafka.driver_analytics_5m` has one row per 5-minute bucket × city ×
driver × distinct driver state (SummingMergeTree). Two consumers write it:

- **SESSIONIZER** writes the online-time columns (`online_ms`,
  `online_on_ride_ms`, `pings`, `pings_unknown_state`), from LTS pings and the
  DRIVER_POOL_STATE writer's state. Tested by `test_sessionizer.py`.
- **DRIVER_PERFORMANCE** writes the performance_analytics columns (pop-ups,
  rides, cancellations, gross earnings, driver addition, tip, ratings), from
  the CDC topics the driver drainer publishes. Tested by
  `test_driver_performance.py`.

Idle, unavailable and no-state time are not columns: split `online_ms` by the
state columns (`sumIf(online_ms, NOT onRide AND enabled AND NOT blocked)`,
`sumIf(online_ms, state_known = 0)`). Average response time is
`response_secs_sum / (popups_accepted + popups_rejected)`.

The views `driver_performance_5m` and `driver_performance_hourly` present them
under the performance_analytics column names, one row per driver and bucket /
IST hour.

## State columns: one row per distinct state

The driver's DriverPoolData state is part of the row key: one plain column per
key of the DRIVER_POOL_STATE writer's hash, named exactly like the key (`mode`,
`active`, `onRide`, `enabled`, `blocked`, `selectedServiceTiers`, `variant`,
`vehicleRating`, `lastUpdatedAt`, ... 39 in all, listed in
`Processor/StateColumns.hs`; `deviceToken`, `registrationNo`, the row-key
fields and the keys not wanted (`driverTripEndLocation`, `clientDevice`,
`schemaVersion`, the client versions, `enableCashRide`, `safetyPlusEnabled`,
`bankAccountPaymentMode`, `language`, `gender`, `totalRides`,
`acRestrictionLiftCount`, `luggageCapacity`, `lastRideEndedAt`,
`mYManufacturing`) are left out). A bucket gets one row per distinct state the driver was
in, each with its own online time, pings and counts:

- SESSIONIZER: each piece of online time goes to the row of the state in force
  during it (a state change inside a gap splits the gap across two rows), each
  ping to the row of the state at its `st`.
- DRIVER_PERFORMANCE: each pop-up and ride goes to the row of the driver's
  state at the event's own time (SRD / ride `created_at`), looked up once on
  first sight and reused for every later version, so an answer or a
  retraction lands in the same row even if the state has changed since.
  Ratings and other joined facts go with their ride.

`state_known` is 0 when the writer had no state for that time (all state
columns NULL / `[]`); `state_at` is the epoch ms the row's state took effect
(the writer's history entry time), 0 when unknown. Types: bools
`Nullable(Bool)`, text `LowCardinality(Nullable(String))`, lists
`Array(LowCardinality(String))`, numbers `Nullable(Int64)` / `Nullable(Float64)`,
times `Nullable(DateTime64(3, 'UTC'))`.
`ORDER BY` holds all of them; `PRIMARY KEY` stays (city, bucket, driver).

Always `sum()` the measures. Group by the state columns to split by state, or
leave them out for per-driver totals:

```sql
SELECT driver_id, mode, onRide,
       sum(online_ms) / 60000 AS online_min,
       sum(pings)             AS pings
FROM atlas_kafka.driver_analytics_5m
WHERE state_known = 1
GROUP BY driver_id, mode, onRide
```

The views sum over the states and expose each state column as the latest
state in the bucket / hour (`argMax` by `state_at`), plus `state_at`
(DateTime64, NULL when no state is known) and `states_in_bucket` /
`states_in_hour` (distinct state rows). Example queries 13 to 15 in
`dev/clickhouse/driver-analytics-queries.sql` use the state columns.

Adding a state field later: add it to `StateColumns.hs`, then

```sql
ALTER TABLE atlas_kafka.driver_analytics_5m
    ADD COLUMN newField Nullable(String),
    MODIFY ORDER BY (merchant_operating_city_id, bucket, driver_id, state_known,
                     <the current 39 state columns>, newField);
```

ClickHouse appends a column added in the same `ALTER` to the sorting key
without rewriting data; old rows read it as NULL. Then add its `argMax` line to
both views. Removing a field: drop it from `StateColumns.hs` only; the column
stays and new rows write its default (NULL / `[]`).

```bash
# from Backend/dev/integration-tests, with the stack running
uv run sessionizer/test_sessionizer.py                  # ~11 min: ~5.5 min timeline + flush wait
uv run sessionizer/test_sessionizer.py --e2e            # right after a ride run (see below)
uv run sessionizer/test_driver_performance.py           # ~5 min: four phases, a flush after each
uv run sessionizer/test_driver_performance.py --fast    # no flush between phases (~1.5 min)
uv run sessionizer/test_driver_performance.py --e2e     # right after a ride run
uv run sessionizer/test_driver_performance.py --selfcheck   # reference model only, no stack needed
```

# `test_sessionizer.py`

It tests the DRIVER_POOL_STATE writer and the SESSIONIZER together. It plays
scripted timelines onto their two input topics (`driver-pool-state`,
`location-updates`) in real time, waits for the sessionizer's flush, then
checks the activity columns of `driver_analytics_5m` and the writer's Redis.

Each run uses new driver ids and its own city id, so runs don't collide and
don't need a clean database. Don't run it straight after `run-tests.sh`: that
script flushes every Redis DB, including the writer's.

## What it checks

Every activity scenario is checked twice: against hand-written totals, and
against a reference model of the rules (in the script) bucket by bucket
(summed over the driver's states).

| Scenario | Proves |
|---|---|
| `idle_steady` | Gaps between ONLINE pings count as online, in the idle state's row. Malformed input aimed at this driver is skipped: a ping without a Kafka key, plus invalid JSON on both topics. |
| `gap_300s_counted` / `gap_301s_dropped` | The 300 s gap limit is inclusive. |
| `bucket_edge_split` | An interval crossing a 5-minute boundary is split between the two buckets. |
| `offline_ping` | The gap after an OFFLINE ping isn't online. The OFFLINE ping is still counted. |
| `silent_mode` | SILENT counts as online. |
| `on_ride`, `disabled`, `blocked` | On-ride time counts in `online_on_ride_ms`; a disabled or blocked driver's time is online, in an `enabled = false` / `blocked = true` state row. |
| `unknown_state` | A driver with no state published: online time and unknown pings in the `state_known = 0` row. |
| `change_inside_gap` | A ride assigned inside a gap splits it into an idle row and an on-ride row. |
| `many_changes_inside_gap` | Three state changes inside one gap: four pieces, in three state rows (the idle state before and after the ride is one state). |
| `out_of_order_ping` | A late, older ping counts as a ping but adds no time. |
| `duplicate_ping` | A ping delivered twice counts twice as a ping, but adds no time. |
| `city_change` | Time goes to the city of the earlier ping. |
| `ping_mode_decides_online` | The ping's mode decides "online", not the state's mode or active flag. |
| `state_columns_stamped` | A change to a state column outside the seven the sessionizer tags with (`vehicleRating`) adds a history entry and splits the gap into a 4.5 row and a 4.8 row. |

State rows: grouped by bucket, city, driver, `state_known`, `mode`, `active`,
`onRide`, `enabled`, `blocked`, `subscribed`, `selectedServiceTiers` and
`vehicleRating`, every row matches the reference model (online pieces by the
state in force, pings by the state at their `st`, `state_at` = the history
entry time). Explicitly: `change_inside_gap` splits into an idle row (55 s) and
an onRide row (60 s, `state_at` t+60 s); `many_changes_inside_gap` has three
states (idle 75 s, on ride 40 s, blocked 60 s); `state_columns_stamped` has a
4.5 and a 4.8 row of 10 s each; `edge_first_state_inside_gap` has an unknown
and an idle row; `unknown_state` rows have `state_known = 0`; a column never
published (`vehicleRating` of `idle_steady`) is NULL.

Writer checks:
- history has an entry only when a state column changes, `lastUpdatedAt` aside (`non_state_field_change`, `same_state_again`), and each entry holds its state columns;
- the current-state hash holds every merged field;
- both keys expire in 30 days;
- history is trimmed back to the state in force at the sessionizer's watermark (`trim_target`).

## `--e2e`

Run it after `uv run driver-pool-state/check_stream.py -- ./run-tests.sh auto NY_Bangalore 01-AutoRideFlow`.
It checks the newest real driver in the writer's Redis:
- seeded from the DB;
- history goes online → on ride → off ride;
- no unknown-state time (`online_ms` with `state_known = 0`, unknown pings);
- on ride ≤ online.

## Behaviour these tests pin down (decide if any should change)

- **Duplicate pings count twice** in `pings`. LTS redelivery would inflate ping counts, but not time.
- **The ping's mode decides "online".** A driver whose state is OFFLINE but whose pings say ONLINE is counted as online.
- **Not covered automatically: restarts.** After a sessionizer restart, each driver's first gap is lost, because the last ping per driver is kept in memory only. A crash between the ClickHouse insert and the offset commit is safe: the replay sends the same dedup token. To check both by hand, stop `kafka-sessionizer-exe` mid-run, restart it, and compare.

## Comparing with cosmos.performance_analytics (once both run in prod)

```sql
-- Hourly IST online / on-ride hours per driver: sessionizer vs performance_analytics.
WITH s AS (
    SELECT driver_id,
           formatDateTime(toStartOfHour(toTimeZone(bucket, 'Asia/Kolkata')), '%Y-%m-%d %H:00:00') AS local_time,
           sum(online_ms) / 3.6e6 AS s_online_h,
           sum(online_on_ride_ms) / 3.6e6 AS s_on_ride_h
    FROM atlas_kafka.driver_analytics_5m
    WHERE bucket >= toDateTime('2026-10-19 18:30:00') AND bucket < toDateTime('2026-10-20 18:30:00')
    GROUP BY driver_id, local_time
)
SELECT count() AS driver_hours,
       sum(p.online_time) AS pa_online_h, sum(s.s_online_h) AS s_online_h,
       sum(p.driver_on_ride_time) AS pa_on_ride_h, sum(s.s_on_ride_h) AS s_on_ride_h
FROM cosmos.performance_analytics AS p
INNER JOIN s ON s.driver_id = p.driver_id AND s.local_time = p.local_time
WHERE p.local_time LIKE '2026-10-20%'
```

Expect the sessionizer's online time to be **lower**, for three reasons:
- `driver_supply_snapshot` gives a full 5 minutes to every driver seen in the last 300 s, whatever their mode, including OFFLINE pings.
- The sessionizer counts only the actual time between ONLINE or SILENT pings.
- `performance_analytics` takes on-ride from the ping's `on_ride` flag, while the sessionizer takes it from the writer's state.

# `test_driver_performance.py`

It produces synthetic CDC rows onto the six topics DRIVER_PERFORMANCE
consumes (group `driver-performance`), in the drainer's exact format:

- one flat JSON object per message: the full row, snake_case columns, every column present (null when empty), top-level keys sorted;
- timestamps as integer epoch seconds; bools as `"True"`/`"False"`; enums as bare constructor names;
- Money as integers; HighPrecMoney as aeson prints it (`0.0001` is `1.0e-4`);
- drainer Kafka keys (`driver-db-sync-stream{shard-N}`; SRD uses the app's `searchRequestForDriver_id_<id>{shard-N}` key).

The SRD rows copy a real message from the local app. The other tables carry
every column of their Beam definition.

Each run uses fresh ids and its own city (`perf-test-<run>`), so runs don't
collide. All timestamps sit inside the last hour, so no state TTL interferes.
Messages go out in four phases. By default the script waits for a consumer
flush after each phase (the consumer sets the Redis key `perf:watermark`, in
DB 2, to the flush time in ms). So a fact that arrives "before" or "after" its
ride really is joined through the consumer's Redis state, not inside one poll
batch.

Before sending, the script checks that the `driver-performance` group has all
six topics assigned. It creates missing topics, then waits up to 330 s. A
running consumer only sees a new topic at its next metadata refresh, and would
start it at the latest offset. If the wait times out, restart
`kafka-driver-performance-exe`. Once all six are assigned, it waits 5 s more:
a partition the group never committed gets its latest offset only after the
assignment, and messages sent before that are skipped.

Every scenario is checked twice: against hand-written totals, and against a
reference model of the metric rules (in the script), row by row. The model
applies the definitions to the final state of every entity: the last version
per id, and for rides the last version of the highest status rank. It was
written from the spec, not from the Haskell code. `--selfcheck` runs only the
model and proves it agrees with the hand-written totals at many alignments
against the 5-minute edge. It also checks the message format and that the
result doesn't depend on how the topics interleave.

| Scenario | Proves |
|---|---|
| `malformed` | `{}`, invalid JSON, `[]`, `null` and partial rows on all six topics are skipped without stalling (every later scenario still lands). |
| `srd_accept` / `srd_reject` / `srd_neglected` | Pop-up counts and response time (responded − rendered). An Active create counted as neglected is replaced by its Accept. |
| `srd_pulled_retraction` | Pulled counts nothing. Pop-ups already flushed as neglected or rejected, with response time, are retracted when they become Pulled. |
| `srd_auto_accept` | `auto_accept_search_requests` needs Accept and `is_auto_accepted = "True"`. |
| `srd_response_time_rules` | Response time only when rendered and responded are both set and responded > rendered. |
| `srd_sentinel_skip` | `drivers-not-found` marker rows are skipped. |
| `srd_duplicates` | One message delivered 3× and replayed after two flushes counts once. |
| `srd_bucket_edge` | `created_at` on a 5-minute edge goes in the new bucket; 1 s before goes in the old one. |
| `srd_no_city` | A null city lands under city `''`. |
| `ride_lifecycle` | NEW → INPROGRESS → COMPLETED across a flush: 1 total, 1 finished, 0 in progress. The driver booking arrives a flush before the ride. |
| `ride_inprogress` | INPROGRESS counted. |
| `cancel_by_driver_bcr_before` / `_after` | The ByDriver cancellation reason joins whether it arrives before the ride or two flushes after it. |
| `cancel_by_user_auto` | ByUser reason with a null `ride_id`, a flush after the ride, joins by booking id; auto-accept booking counts. |
| `cancel_by_user_bcr_before` | ByUser reason with a null `ride_id`, a flush before the ride, joins by booking id. |
| `cancel_by_application` | Other sources count as cancelled only. |
| `auto_accept_completed` | The booking's auto-accept flag arriving after the ride. |
| `auto_accept_cancel_by_driver` | The auto-accept cancelled-by-driver columns. |
| `ride_versions` | Lower-rank versions after COMPLETED are ignored, in the same batch and after a flush. A later COMPLETED replaces the fare. Duplicates count once. |
| `toll_and_parking_ignored` | The ride's toll and its own fare params' parking (a flush before the ride) change nothing: gross = fare. |
| `driver_addition_tip` | Addition and tip come from the booking's (quote's) fare params, before or after the ride, whatever the ride status. |
| `quote_fare_params_updated` | Quote fare params joined from pending, then updated a flush later, keep the update when the booking's next version arrives (phase 3); the old pending copy is not attached again. |
| `ratings` | A rating before the ride, one after it, and one updated 5 → 3. |
| `ride_row_key_change` | A same-rank version that changes the driver retracts the old driver's row. |
| `driver_state_stamped` | Two `driver-pool-state` events before the pop-up (the newer: on a ride, two tiers), a flush before it. The pop-up arrives Active (neglected), then the driver goes OFFLINE (after `created_at`, same bucket), then the answer arrives. One state row: the state at `created_at` (`mode`, `onRide`, `selectedServiceTiers`), `state_at` = that entry's time, accepted 1, neglected 0; nothing in an OFFLINE row. Needs the DRIVER_POOL_STATE writer running. |

Plus: every (bucket, city, driver, state) row matches the reference model
(each pop-up and ride keyed by the state at its event time), no rows for
other drivers in the test city, rows of drivers without writer history have
`state_known = 0`, and every `perf:*` state key has a TTL of at most 8 days.

## `--e2e`

Run it after a real ride run (`./run-tests.sh auto NY_Bangalore 01-AutoRideFlow`),
with `PUSH_TO_KAFKA=True` on the driver drainer. It takes the driver of the newest
ride in local Postgres (or `--driver`) and everything of theirs from the start
of the 5-minute bucket `--hours` (default 2) before that ride. Both sides use
that same bucket bound. It computes the metrics from Postgres with the same
reference model: SRDs, rides, bookings, cancellation reasons, the bookings'
fare params and ratings (Postgres `localhost:5434`, db `atlas_dev`, user
`atlas_driver_offer_bpp_user`). It then waits for a consumer flush and
compares them with the totals in `driver_performance_5m`. Response-time sums
and rating sums come from `driver_analytics_5m`. Money is
compared to 0.001. The window must lie inside the time the consumer has been
running.

## Behaviour these tests pin down

- **Facts are joined, not dated.** Addition, tip and rating all land in the ride's bucket (ride `created_at`), whenever they arrive. Ratings later than the ride state's TTL (8 days) are dropped.
- **Fare params wait for their ride only when they matter.** A fare_parameters row that arrives before its ride is stored only if it carries a non-zero driver_selected_fare or customer_extra_fee, or if it replaces such a stored row. A later all-zero version therefore zeroes the earlier values. Any other all-zero row is dropped until its ride is known. A version that arrives once its ride is known also replaces a stored row, so later booking versions don't bring back the old values (`quote_fare_params_updated`).
- **Not covered automatically: crashes.** A crash between the ClickHouse insert and the Redis state write replays with the same dedup token. To check it by hand, stop `kafka-driver-performance-exe` mid-run, restart it, and rerun the comparison.
