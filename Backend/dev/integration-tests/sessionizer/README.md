# Driver analytics tests: sessionizer

`atlas_kafka.driver_analytics_5m` has one row per 5-minute bucket × city ×
driver × distinct driver state (SummingMergeTree, keyed by `state_id`). The
SESSIONIZER (kafka-consumers, `CONSUMER_TYPE = SESSIONIZER`) writes every
column, through the `driver-analytics-5m` topic and a ClickHouse Kafka engine
table:

- **presence** from LTS pings (`location-updates`): every ping whose mode is
  not OFFLINE adds to `pings` and sets `online = 1` on the row of its bucket
  and of the state in force at its `st`. A point the phone recorded on a ride
  and sent later (`st - ts` over 60 s, `onRide` true at `ts`) counts at its
  `ts` and the state then. Other batched points count at `st`, because LTS
  gives a batch one `st`. Online time is not a column: it is
  the number of distinct online buckets × 5 minutes,
  `uniqExactIf(bucket, online = 1) * 5` minutes per driver. Count buckets,
  never rows: a bucket with a state change has a row per state.
- **events** from `driver-analytics-events`: the driver app publishes one per
  pop-up sent / answered / pulled, ride assigned / started / completed /
  cancelled and rating (`SharedLogic/DriverPool/DriverAnalyticsEvent.hs`),
  keyed by driver id. Each adds its counts (`popups_sent`, `popups_accepted`,
  `popups_rejected`, `popups_pulled`, `response_secs_sum`,
  `auto_accept_search_requests`, `total_bookings`, `rides_started`,
  `finished_rides`, `cancelled_rides`, `rides_cancelled_by_driver` / `_by_user`,
  the `auto_accept_*` ride columns, `rides_with_rating`, `rating_sum`,
  `driver_addition`, `customer_tip`, `gross_earnings`) to the row of its own
  time and the state in force then. A row can have events and `online = 0`.

Every count is at its event's time, so a ride's booking, start, end and
cancellation can sit in different buckets. An accepted pop-up's time is just
before the accept runs, so it counts in the state the driver accepted from, not
the `onRide` state the accept can put them in. The views `driver_performance_5m`
and `driver_performance_hourly` present the rows under performance_analytics
names: pop-ups sent = `popups_sent - popups_pulled`, neglected = sent −
accepted − rejected (exact over a day, approximate per bucket), average
response time = `response_secs_sum / (popups_accepted + popups_rejected)`.

`location-updates` and `driver-analytics-events` must have the same partition
count and the same key hash (both keyed by driver id): the sessionizer's
group uses the range assignor, so one pod gets the same partition number of
both and holds all of a driver's records.

## State columns: one row per distinct state

The driver's DriverPoolData state is part of the row: one plain column per key
of the DRIVER_POOL_STATE writer's hash, named exactly like the key (`mode`,
`active`, `onRide`, `enabled`, `blocked`, `selectedServiceTiers`, `variant`,
`vehicleRating`, `registrationNo`, `lastUpdatedAt`, ..., listed in
`Processor/StateColumns.hs`), and `state_id`, a hash of them, in the key.
`registrationNo` is the vehicle linked to the driver at that time;
`registrationNoChangedAt` is when it last changed (the writer and the
sessionizer stamp it with the event time whenever the vehicle number changes
from a known one; `""` and null both mean no vehicle).

`state_known` is 0 when the writer had no state for that time (all state
columns NULL / `[]`); `state_at` is when the row's state took effect (the
writer's history entry time), `1970-01-01 00:00:00` when unknown.

The SESSIONIZER reads every partition of `driver-pool-state` itself, on a
second consumer that commits nothing, so every pod sees every state event. It
keeps each driver's state in memory for good (history trimmed at each flush)
and reads Redis only for a driver not in memory, once the writer has applied
that driver's events. A record waits, with no timeout, until every state event
published up to 30 s after its time has been read; waiting never blocks
polling. An event or buffered ping older than the history kept in memory takes
its state from the writer's Redis history (`sessionizer_history_fallbacks`).

Always `sum()` the measures, and group by the state columns to split by state:

```sql
SELECT driver_id, mode, onRide,
       uniqExactIf(bucket, online = 1) * 5 AS online_min,
       sum(pings)                          AS pings,
       sum(finished_rides)                 AS finished
FROM atlas_kafka.driver_analytics_5m
WHERE state_known = 1
GROUP BY driver_id, mode, onRide
```

```sql
-- Which vehicle each driver had, and when it was linked.
SELECT driver_id, registrationNo, min(registrationNoChangedAt) AS linked_at
FROM atlas_kafka.driver_analytics_5m
WHERE state_known = 1
GROUP BY driver_id, registrationNo
ORDER BY driver_id, linked_at
```

## Running

```bash
# from Backend/dev/integration-tests, with the stack running
uv run sessionizer/test_sessionizer.py                  # ~11 min: ~6 min timeline + flush wait
uv run sessionizer/test_sessionizer.py --e2e            # right after a ride run (see below)
uv run sessionizer/load_test.py                         # 4000 drivers x 10 min, pings + state + events
uv run sessionizer/load_test.py --drivers 1000 --minutes 5
```

Don't run them straight after `run-tests.sh`: that script flushes every Redis
DB, including the writer's.

# `compare_flows.sh`

Runs the same ride suites on the `origin/main` merge-base (a worktree at
`../nammayatri-base`) and on this tree, one suite at a time. `diff.txt` shows what
each suite wrote on each side (Postgres rows, Redis keys, Kafka messages), with the
differences the branch means to make listed apart from the rest. `truth-<suite>.txt`
compares this tree's `driver_analytics_5m` with Postgres and `location-updates`, per
driver. Results go to `dev/integration-tests/data/compare/`.

```bash
dev/integration-tests/sessionizer/compare_flows.sh                  # from Backend/, no stack running
NAMES="01-AutoRideFlow" dev/integration-tests/sessionizer/compare_flows.sh
SKIP_BASE=1 dev/integration-tests/sessionizer/compare_flows.sh      # reuse the base snapshots
```

The first run does a full build of the worktree. A database with no merchants gets
the config bundle this tree last synced from, and `prepare` adds registry rows for
each local merchant's key (without them every Beckn search fails with
`SIGNATURE_VERIFICATION_FAILURE`). Check a single stack with
`compare_flows.py mark`, one `run-tests.sh` suite, then `compare_flows.py truth`: one
suite per mark, because `run-tests.sh` flushes Redis before each suite and that drops
writes the drainer hasn't reached yet.

# `test_sessionizer.py`

It tests the DRIVER_POOL_STATE writer and the SESSIONIZER together. It plays
scripted timelines onto `driver-pool-state`, `location-updates` and
`driver-analytics-events` in real time, waits for the sessionizer's flush, then
checks `driver_analytics_5m` and the writer's Redis. Each run uses new driver
ids and its own city id, so runs don't collide.

Every scenario is checked twice: against hand-written totals, and against a
reference model of the rules (in the script), per bucket and city and per
state row.

| Scenario | Proves |
|---|---|
| `idle_steady` | ONLINE pings mark the idle state's row online. Malformed input is skipped: a ping without a Kafka key, invalid JSON on all three topics. |
| `two_buckets`, `bucket_edge`, `edge_ping_on_bucket_edge` | A ping marks its own bucket; a ping exactly on an edge belongs to the later bucket. |
| `offline_ping`, `only_offline`, `edge_rapid_toggles` | OFFLINE pings count nowhere (no ping, no row). |
| `silent_mode`, `edge_null_mode` | SILENT and a null mode count as online. |
| `on_ride`, `disabled`, `blocked` | The ping lands in an `onRide` / `enabled = false` / `blocked = true` row. |
| `unknown_state`, `edge_first_state_after_ping` | No state yet: the `state_known = 0` row. |
| `state_change`, `many_changes`, `edge_change_at_ping_time`, `edge_fifty_changes` | Each ping takes the state in force at its `st` (a change stamped at the ping's time applies to it). |
| `out_of_order_ping`, `duplicate_ping` | Late and duplicate pings each count. |
| `city_change` | Each ping goes to its own city's row. |
| `ping_mode_decides_online` | The ping's mode decides "online", not the state's. |
| `state_columns_stamped` | A `vehicleRating` change adds a history entry and a row. |
| `vehicle_change` | A row per vehicle; `registrationNoChangedAt` = the change time, none for the first vehicle. |
| `events_stamped` | Events count at their own time in the row of the state then: pop-ups in the idle row, booking and finished ride in the onRide row. |
| `event_without_ping` | An event while not pinging makes a row with `online = 0`. |
| `event_unknown_state` | An event with no state: the `state_known = 0` row. |
| `event_unknown_column` | A key that isn't a table column is ignored. |
| `event_rating_changed` | A changed rating adds only its difference. |
| `event_old_state` | An event an hour old takes the state in force an hour ago. |
| `batched_on_ride`, `batched_ride_ended_in_outage`, `batched_older_than_memory` | A point recorded on a ride counts at its `ts`, split by the state then; the state can come from Redis. |
| `batched_not_on_ride`, `batched_under_gap`, `batched_clock_behind`, `batched_clock_ahead`, `batched_before_any_state` | Everything else stays at `st`. |
| `edge_st_missing_uses_ts`, `edge_future_*`, `edge_ancient_pings_skipped` | No `st` falls back to `ts`; implausible times are skipped without blocking. |
| `edge_burst_200_pings`, `edge_weird_ids` | 200 pings; quotes, unicode and 300-char ids. |

Writer checks: history has an entry only when a state column changes
(`lastUpdatedAt` aside), each entry holds its state columns, the current hash
holds every merged field and `registrationNoChangedAt`, keys expire in 30 days,
history is capped at the newest 100 entries (`trim_target`).

## `--e2e`

Run it after `uv run driver-pool-state/check_stream.py -- ./run-tests.sh auto NY_Bangalore 01-AutoRideFlow`.
It checks the newest real driver in the writer's Redis: seeded from the DB;
history goes online → on ride → off ride; no unknown-state rows; online
buckets on a ride ≤ online buckets; the ride's events arrived (pop-up sent,
booking, start, end or cancel).

## Behaviour these tests pin down (decide if any should change)

- **Duplicate pings and events count twice.** The sessionizer skips Kafka
  records it already flushed (`sessionizer:flushed:<topic>:<partition>`), but
  an event the app publishes twice counts twice.
- **The ping's mode decides "online".** A driver whose state is OFFLINE but
  whose pings say ONLINE is counted as online.
- **A session's first and last bucket count in full** (5 minutes each).
- **Network outages.** While idle, an outage shows as offline: the batch lands in
  the bucket it reached LTS. On a ride, it counts at `ts`, but a change only the
  phone can make (ending the ride) takes effect when the request reaches the server.
- **Restarts.** Records up to the last flushed offset are skipped on replay. A
  crash between the deliveries and the Redis write of the flushed offsets
  counts that window again. To check by hand, stop `kafka-sessionizer-exe`
  mid-run, restart it, and compare.

## Alerts (metrics on the sessionizer's metrics port)

| Metric | Alert when |
|---|---|
| `sessionizer_state_read_lag_ms` | above a few minutes: the state consumer is behind, records are held |
| `sessionizer_oldest_held_age_ms` | above a few minutes: records wait (state topic or writer behind) |
| `sessionizer_writer_lag_ms` | above a few minutes: the DRIVER_POOL_STATE writer is behind |
| `rate(sessionizer_history_fallbacks[10m])` | rises: many late events or buffered pings (taking their state from Redis) |
| `rate(sessionizer_flush_failures[10m])` | above 0: producing to `driver-analytics-5m` fails |
| Kafka engine lag | consumer group `ch-atlas_kafka-driver_analytics_5m` on `driver-analytics-5m` lags (`system.kafka_consumers` in ClickHouse, or the Kafka exporter) |

## Comparing with cosmos.performance_analytics (once both run in prod)

Example query 12 in `dev/clickhouse/driver-analytics-queries.sql`. Expected
gaps: online time (the stream counts every 5-minute bucket with an online
ping in full, so about +5 min per session), events bucketed at their own time
(performance_analytics buckets rides by search time), and the batch's known
tip bug (00:00–05:30 IST).
