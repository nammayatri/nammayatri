# Driver Online Duration from ClickHouse History — Design

Date: 2026-08-18
Status: Approved for planning

## Problem

Ops needs to answer "how many minutes was this driver online on a given day?" for an
arbitrary historical day, from the BPP dashboard.

The data exists in ClickHouse. The Metabase question titled *Driver Information Debug*
is a view over `atlas_driver_offer_bpp.driver_information`, which is fed from Kafka topic
`adob-sessionizer-driver_information` — the drainer publishes a row for every write to the
Postgres `driver_information` table
(`Backend/app/provider-platform/dynamic-offer-driver-drainer/src/Utils/Utils.hs:188`).
Each row is a point-in-time snapshot, so the sequence of rows for one driver is a
changelog of that driver's state, including the transitions into and out of `ONLINE`.

There is no API over it. Today the only way to get this number is to open Metabase and
write SQL by hand.

## Goal

A Management-group BPP dashboard endpoint taking a driver and an inclusive merchant-local
date range, returning online duration per day, derived from the ClickHouse changelog.

## Non-goals

- Changing how `daily_stats.online_duration` is computed. This is a read-only,
  parallel derivation.
- Bulk (multi-driver) lookup. One driver per call, by decision. The ClickHouse query is
  nonetheless written so a `GROUP BY driver_id` variant is a small follow-up.
- Fleet or Operator surfaces. Management only, by decision.
- Changing the existing `Storage.Clickhouse.DriverInformation` module or its callers.

## Decisions and rationale

### ClickHouse changelog, not `daily_stats.online_duration`

The repo already computes this number. `Domain/Action/Internal/ProcessingChangeOnline.hs`
banks elapsed online time into `daily_stats.online_duration` on every
`ONLINE -> not-ONLINE` mode transition (`:37`), keyed by `(driverId, merchantLocalDate)`.
Reading that column would be a trivial endpoint.

We are deriving from ClickHouse instead, deliberately. The stored value is gated by
`transporterConfig.analyticsConfig.onlineDurationCalculateFrom` — null means the feature
is off, and any date before that timestamp has no value at all. It is also capped by
`maxOnlineDurationDays`. The ClickHouse changelog is subject to neither, so it answers for
any day for which Kafka data was retained, and it can be used to audit the stored value.

The cost of that choice is that the derivation must be written and maintained, and it can
disagree with what the driver sees in the app. Where they disagree the stored value is
authoritative for the driver; this endpoint is an ops/debugging view.

### "Online" means `mode == ONLINE`

The table carries several overlapping signals: `active :: Bool`, `mode :: DriverMode`
(`ONLINE | OFFLINE | SILENT`, `Domain/Types/Common.hs:42`), `driver_flow_status`
(`ONLINE | OFFLINE | SILENT | ON_PICKUP | ON_RIDE | ACTIVE | INACTIVE`,
`Domain/Types/DriverFlowStatus.hs:11`), and `on_ride :: Bool`.

We count only `mode == ONLINE`, matching `ProcessingChangeOnline.hs:37` exactly. `SILENT`
accrues nothing. This makes the derived number directly comparable to
`daily_stats.online_duration`, which is the main thing that makes the endpoint useful as
an audit tool. Any other definition would produce numbers that reconcile with nothing.

### Per-query `NO_SELECT_MODIFIER` override, not a new module

`Storage/Clickhouse/DriverInformation.hs:38` binds the table as:

```haskell
$(TH.mkClickhouseInstances ''DriverInformationT 'SELECT_FINAL_MODIFIER)
```

`SELECT ... FINAL` forces `ReplacingMergeTree` deduplication at query time, returning
exactly one row per driver — the latest. That is correct for its four existing callers,
which want current state. It is fatal here: a history query through that binding would see
one row per driver and return approximately zero minutes for everyone, with no error.

The obvious fix — a second module binding the same table with `NO_SELECT_MODIFIER` — does
not work. `mkClickhouseInstances` derives the physical table name from the *type* name
(`SearchRequestForDriverT` -> `search_request_for_driver`, `FleetRcDailyStatsT` ->
`fleet_rc_daily_stats`), and no call site in the codebase overrides it. A
`DriverInformationHistoryT` would query a non-existent `driver_information_history`.

The mechanism that does work is `CH.selectModifierOverride`, which overrides the modifier
per query. `rider-app/Storage/Clickhouse/Booking.hs:144` is the precedent: `BookingT` is
bound `SELECT_FINAL_MODIFIER` and individual queries override to `NO_SELECT_MODIFIER`.
`SearchRequestForDriver.hs:259` does the same in driver-app.

So the history queries are added to the existing `Storage/Clickhouse/DriverInformation.hs`,
each wrapping its source in `CH.selectModifierOverride CH.NO_SELECT_MODIFIER`, and the
record gains the two columns they need.

Extending the shared record is safe because every existing query in that module projects
its columns explicitly through `CH.select_` — the SELECT list is built from the projection,
not from the record — so no existing query starts requesting `mode` or `updated_at`. This
does mean a future whole-row select through this record would fail in any environment
missing those columns; the queries in this module must stay explicitly projected.

### The fold is in Haskell, not in ClickHouse

The ClickhouseV2 DSL in `mobility-core` exposes `sum_`, `argMax`
(`Storage/Clickhouse/SearchRequestForDriver.hs:288`), `groupBy`, `orderBy_`
(`Storage/Clickhouse/DriverFee.hs:156`), and `limit_`. It has no window functions, so
`lagInFrame` / `neighbor` / `runningDifference` — the natural way to pair consecutive rows
in SQL — are not expressible.

The rows are therefore fetched ordered and folded in Haskell. This is not merely a
workaround: row counts are tens to hundreds per driver-month, so the transfer is trivial,
and it puts every edge case in a pure function that can be unit-tested without a
ClickHouse instance.

## Design

### Route

```
GET /api/bpp/driver-offer/{merchantId}/{city}/driver/onlineDuration
      ?driverId=<uuid>&from=2026-08-11&to=2026-08-11
```

Modelled on `GET /driver/earnings`
(`CommonAPIs/spec/ProviderPlatform/Management/API/Driver.yaml:610-631`), the closest
existing shape: mandatory driver plus mandatory date range, Management group, with a
`helperApi` variant carrying `requestorId`.

Spec addition:

```yaml
  - GET: # DriverOnlineDurationAPI
      endpoint: /onlineDuration
      auth: ApiAuthV2
      mandatoryQuery:
        - driverId: Id Driver
        - from: Day
        - to: Day
      response:
        type: DriverOnlineDurationRes
      helperApi:
        - GET:
            endpoint: /onlineDuration
            auth: ApiAuthV2
            mandatoryQuery:
              - driverId: Id Driver
              - from: Day
              - to: Day
              - requestorId: Text
            response:
              type: DriverOnlineDurationRes
      migrate:
        capability: city-operations.driver.read
```

The `capability:` field is mandatory for any new endpoint.
`Backend/dev/dsl-capability-baseline.txt` whitelists only the endpoints that already had a
`capability_endpoint` row when the capability framework landed; codegen fails for anything
not listed and not declaring one. `city-operations.driver.read` is the capability that
already gates `GET_DRIVER_STATS`
(`dev/seed-migrations/dashboard-unification/0009-capability-seed.sql:382`). Declaring it
causes the generator to emit the `capability_endpoint` INSERT into
`dev/migrations-read-only/provider-dashboard/API_Management_Driver.sql`, following
`API_Management_DriverRegistration.sql:98`. No hand-written migration.

### Response

```yaml
  DriverOnlineDurationRes:
    - driverId: Id Driver
    - totalOnlineDuration: Seconds
    - dataComplete: Bool
    - days: [DriverOnlineDurationDay]
  DriverOnlineDurationDay:
    - date: Day
    - onlineDuration: Seconds
```

`Seconds` rather than raw minutes, so the values are directly comparable to
`daily_stats.online_duration` and `DriverStatsRes.onlineDuration`. Every duration in this
codebase is `Seconds`; the UI divides.

`days` always has one element per day in `[from, to]`, including days with no activity,
which report `0`. Absent keys would be ambiguous between "no data" and "zero minutes" —
`dataComplete` carries that distinction instead (see below).

### ClickHouse read model

`Storage/Clickhouse/DriverInformation.hs` gains two fields on `DriverInformationT` and the
matching entries in `driverInformationTTable`:

```haskell
    mode      :: C f (Maybe DriverInfo.DriverMode),   -- "mode"
    updatedAt :: C f UTCTime                          -- "updated_at"
```

No orphan instance is required: `instance CH.ClickhouseValue DriverMode` already exists at
`Domain/Types/Common.hs:45`. Redeclaring it would be a duplicate-instance error, and the
project builds with `-Werror`.

Two new queries in the same module, both against `CH.APP_SERVICE_CLICKHOUSE` (the env used
by the existing queries and reachable from dashboard handlers — see
`Domain/Action/Dashboard/Fleet/Driver.hs:5175`), and both wrapping their source in
`CH.selectModifierOverride CH.NO_SELECT_MODIFIER`:

```
windowStart = UTCTime(from, 00:00)   - timeDiffFromUtc      -- merchant-local -> UTC
windowEnd   = UTCTime(to + 1, 00:00) - timeDiffFromUtc

A. lookback : driver_id = X AND updated_at <  windowStart
              orderBy_ desc updated_at, limit_ 1
              -> the mode the driver entered the window in

B. window   : driver_id = X AND updated_at >= windowStart AND updated_at < windowEnd
              orderBy_ asc updated_at
```

Query A is what makes a single-day answer correct. A driver who went `ONLINE` at 23:00 on
the 10th and `OFFLINE` at 01:00 on the 11th has 60 online minutes on the 11th with no row
inside the 11th marking the start.

### The fold

Pure, in `SharedLogic/DriverOnlineDuration.hs`, with no `Flow` and no ClickHouse types in
its signature:

```haskell
data OnlineDurationResult = OnlineDurationResult
  { perDay :: [(Day, Seconds)],
    dataComplete :: Bool
  }

foldOnlineIntervals ::
  Seconds ->            -- timeDiffFromUtc
  Day -> Day ->         -- from, to (merchant-local, inclusive)
  UTCTime ->            -- now, to clamp an open interval
  Maybe Bool ->         -- online at window start; Nothing = no lookback row
  [(Bool, UTCTime)] ->  -- state changes in the window, ascending
  OnlineDurationResult
```

It takes `Bool`, not `DriverMode`: the caller applies `(== Just ONLINE)` before calling.
That keeps the enum decision in one place at the boundary and leaves the arithmetic with no
domain dependency at all, which is what makes it cheap to test exhaustively. `Nothing` for
the initial state encodes "no lookback row" and is what drives `dataComplete`.

It walks the sequence, opening an interval on entry into `ONLINE` and closing it on exit,
then splits every interval at merchant-local midnight and sums per local day.

Four behaviours the fold must get right:

- **Repeated identical modes.** The drainer emits a row for *any* column change — `on_ride`
  flipping, a referral code being set — not only mode changes. Consecutive rows with the
  same mode must neither open nor close an interval. This is the most likely source of a
  silently wrong number, because such rows are the majority.
- **Still online at window end.** Clamp the open interval to `min(windowEnd, now)`. Never
  to a future time.
- **Midnight spanning.** Split, so a 23:00–01:00 session credits both days. This mirrors
  `ProcessingChangeOnline.hs:109-117`.
- **No lookback row.** Treat as `OFFLINE` at window start and return `dataComplete = False`.
  This happens when the driver was created inside the window (harmless) or when Kafka
  retention has truncated the history (not harmless). The flag exists so the second case
  does not present as a confident low number.

"Day" throughout means merchant-local day, derived from `transporterConfig.timeDiffFromUtc`
exactly as `ProcessingChangeOnline.hs:82` derives `merchantLocalDate`. A UTC-day boundary
would start "August 11" at 05:30 IST and reconcile with nothing else in the dashboard.

### Driver-app handler

In `Domain/Action/Dashboard/Management/Driver.hs`, following `getDriverEarnings`
(`:1451-1470`):

1. `findMerchantByShortId` / `CQMOC.getMerchantOpCityId`.
2. Reject `from > to`, and a span over 31 days, with `InvalidRequest`. The ClickHouse scan
   is proportional to the span.
3. Authorization, copied from `getDriverEarnings`:

```haskell
entities <- QPerson.findAllByPersonIdsAndMerchantOpsCityId [Id requestorId, driverId] merchantOpCityId
driver   <- find (\e -> e.id == driverId) entities & fromMaybeM (PersonDoesNotExist driverId.getId)
whenJust (find (\e -> e.id == Id requestorId) entities) $ \requestor -> do
  isValid <- isAssociationWithDriver requestor driver
  unless isValid $ throwError AccessDenied
```

This is not optional. Without it any Management caller could read any driver's history by
UUID. The same lookup also scopes the driver to the merchant operating city, which
ClickHouse cannot do — the columns we read carry no city.

4. Run queries A and B, fold, respond.

### Dashboard proxy

`provider-dashboard/src/Domain/Action/ProviderPlatform/Management/Driver.hs`, four lines,
identical in shape to `getDriverEarnings` (`:374-377`):

```haskell
getDriverOnlineDuration merchantShortId opCity apiTokenInfo driverId from to = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  Client.callManagementAPI checkedMerchantId opCity (.driverDSL.getDriverOnlineDuration) driverId from to apiTokenInfo.personId.getId
```

No transaction storing: this is a read.

## Files affected

| File | Change |
|---|---|
| `Backend/app/dashboard/CommonAPIs/spec/ProviderPlatform/Management/API/Driver.yaml` | Endpoint + `DriverOnlineDurationRes` / `DriverOnlineDurationDay` + capability |
| `Backend/app/provider-platform/dynamic-offer-driver-app/Main/src/Storage/Clickhouse/DriverInformation.hs` | Two fields on the record; queries A and B with `selectModifierOverride` |
| `Backend/dev/clickhouse/sql-seed/atlas-driver-offer-bpp-seed.sql` | Add `mode` and `updated_at` to the dev-local table, queue, and MV |
| `Backend/app/provider-platform/dynamic-offer-driver-app/Main/src/SharedLogic/DriverOnlineDuration.hs` (new) | Pure `foldOnlineIntervals` |
| `Backend/app/provider-platform/dynamic-offer-driver-app/Main/src/Domain/Action/Dashboard/Management/Driver.hs` | `getDriverOnlineDuration` handler |
| `Backend/app/dashboard/provider-dashboard/src/Domain/Action/ProviderPlatform/Management/Driver.hs` | Proxy |

Generated by `, run-generator` (do not hand-edit): `src-read-only/` on all three sides,
and `Backend/dev/migrations-read-only/provider-dashboard/API_Management_Driver.sql`.
Then `, hpack` for the two new modules and `cabal build all`.

## Testing

Unit, against `foldOnlineIntervals` — this is where the risk is:

- Single `ONLINE -> OFFLINE` pair inside one day gives the exact elapsed seconds.
- Session spanning local midnight is credited to both days, and the two parts sum to the
  session length.
- Still `ONLINE` at window end clamps to `now`, never beyond.
- Missing lookback row yields `dataComplete = False`.
- Lookback says `ONLINE` with no change until mid-window: the leading segment counts.
- `ONLINE -> SILENT -> ONLINE`: the `SILENT` gap is excluded.
- Consecutive rows with the same mode neither open nor close an interval.
- `from == to` yields exactly one element in `days`.
- Empty range or no rows yields an all-zero array of the correct length, not `[]`.

Integration:

- A driver whose day has a known `daily_stats.online_duration`: the two agree within a
  small tolerance. A large gap is a real finding about one of the two systems.
- Caller with no association to the driver gets `AccessDenied`, and no ClickHouse query is
  issued.
- Driver in another merchant operating city gets `PersonDoesNotExist`.
- `from > to` and a 40-day span both give `InvalidRequest`.

## Open questions

Both must be resolved before the ClickHouse module is written. Neither affects the other
layers.

1. **Does prod's `driver_information` carry a `mode` column, and does it retain history?**
   To be answered by `SHOW CREATE TABLE atlas_driver_offer_bpp.driver_information`.

   The local seed (`Backend/dev/clickhouse/sql-seed/atlas-driver-offer-bpp-seed.sql:53`)
   declares only five columns — `driver_id`, `driver_flow_status`, `enabled`, `enabled_at`,
   `version` — with `ENGINE = ReplacingMergeTree(version) ORDER BY (driver_id)`. Prod's
   table is visibly wider (the Metabase grid shows `active`, `on_ride`, `created_at`,
   `updated_at`, `blocked`, `last_enabled_on` and more), so its DDL is managed outside this
   repo and the seed file is a dev-local subset.

   Two things follow if prod matches the seed's *engine*: `ORDER BY (driver_id)` under
   `ReplacingMergeTree` means background merges collapse each driver to a single row, so
   the history visible in Metabase today is transient unmerged parts. A derivation over it
   would be correct today and silently near-zero later. If that is the case, retaining
   history is a schema prerequisite — a sort key including `updated_at` — and it belongs to
   the infra team, not to this change.

   If `mode` is absent, the fallback is `driver_flow_status`, whose `ONLINE` is a different
   enum from `DriverMode`'s and would need its own reconciliation against
   `daily_stats.online_duration`.

2. **`Seconds` versus minutes on the wire.** Specified as `Seconds` above for
   comparability. A one-word change if ops would rather have minutes.

## Performance

One driver, one date range, two ClickHouse queries — one of them `LIMIT 1`. The scan is
bounded by the 31-day span cap. The fold is linear in row count, which is tens to hundreds.

The known upgrade path, if a list screen ever needs this: query B becomes
`... WHERE driver_id IN (...)` with the fold applied per driver, which is one round trip
for a whole fleet rather than N. The read model is written to make that a small change,
but it is not built now.
