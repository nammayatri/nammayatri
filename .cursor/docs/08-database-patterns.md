# Database Patterns

## Overview

Namma Yatri uses PostgreSQL as the primary database, Redis for caching, and a KV connector layer that abstracts both.

## Query Modules

### Generated Queries (`src-read-only/Storage/Queries/`)

Auto-generated from `spec/Storage/*.yaml`. Functions include:
- `findByPrimaryKey` — Find by primary key
- `findOneWithKV` — Single record query with KV support
- `findAllWithOptionsKV` — Multiple records with sorting/pagination
- `updateWithKV` — Update records
- `deleteWithKV` — Delete records
- `createWithKV` — Insert record (calls create under the hood)

Example generated query:
```haskell
findByQuoteId :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Id Quote -> m (Maybe Booking)
findByQuoteId quoteId = findOneWithKV [Se.Is BeamB.quoteId $ Se.Eq (getId quoteId)]
```

### Extra Queries (`src/Storage/Queries/`)

Hand-written queries for complex logic. Created when `EXTRA_QUERY_FILE` is in YAML spec's `extraOperations`.

### Common Query Patterns

```haskell
-- Find with error handling
booking <- QBooking.findById bookingId >>= fromMaybeM (BookingNotFound bookingId.getId)

-- Simple create (no transaction wrapper needed for single creates)
QBooking.create booking

-- Multiple creates in transaction
runInTransaction $ do
  QBooking.create booking
  QRide.create ride

-- Update
QBooking.updateStatus CONFIRMED bookingId

-- Find all with options
bookings <- QBooking.findAllWithOptionsKV
  [Se.Is BeamB.riderId $ Se.Eq (getId riderId)]
  (Se.Desc BeamB.createdAt)
  (Just limit)
  (Just offset)
```

## Cached Queries

### Generated Cached Queries (`src-read-only/Storage/CachedQueries/`)

Auto-generated from `cachedQueries` in YAML spec. Pattern:
1. Check Redis cache
2. On miss → query DB → cache result
3. Cache invalidation via delete functions

```haskell
-- Pattern: findX checks Redis first, falls back to DB
findByPersonId personId = do
  Hedis.safeGet (makeKey personId) >>= \case
    Just result -> pure result
    Nothing -> do
      result <- QSavedReqLocation.findByPersonId personId
      Hedis.setExp (makeKey personId) result 3600  -- TTL in seconds
      pure result
```

### Hand-Written Cached Queries (`src/Storage/CachedQueries/`)

Created when `EXTRA_CACHED_QUERY_FILE` is in YAML spec.

### Key Cached Query Locations

| Service | Path |
|---------|------|
| rider-app | `app/rider-platform/rider-app/Main/src/Storage/CachedQueries/` |
| rider-app (generated) | `app/rider-platform/rider-app/Main/src-read-only/Storage/CachedQueries/` |
| driver-app | `app/provider-platform/dynamic-offer-driver-app/Main/src/Storage/CachedQueries/` |
| driver-app (Cac) | `app/provider-platform/dynamic-offer-driver-app/Main/src/Storage/Cac/` |

## Beam ORM Types (`src-read-only/Storage/Beam/`)

Auto-generated Beam table definitions mapping Haskell types to DB columns.

### FromTType / ToTType Transformations

When Haskell domain types differ from DB column types, use `fromTType` and `toTType` in the YAML spec:

```yaml
# In Storage YAML
beamFields:
  amount:
    amount: Maybe HighPrecMoney
    amountCurrency: Maybe Currency

fromTType:
  amount: Kernel.Prelude.fromMaybe 0 amount
  currency: Kernel.Prelude.fromMaybe INR amountCurrency

toTType:
  amount: Kernel.Prelude.Just amount
  amountCurrency: Kernel.Prelude.Just currency
```

## Migrations

### Location
| Service | Path |
|---------|------|
| rider-app | `dev/migrations/rider-app/` |
| driver-app | `dev/migrations/dynamic-offer-driver-app/` |
| rider-dashboard | `dev/migrations/rider-dashboard/` |
| provider-dashboard | `dev/migrations/provider-dashboard/` |
| safety-dashboard | `dev/migrations/safety-dashboard/` |
| scheduler | `dev/migrations/scheduler/` |
| public-transport | `dev/migrations/public-transport-rider-platform/` |
| special-zone | `dev/migrations/special-zone/` |
| Generated migrations | `dev/migrations-read-only/` |

### Migration Format
SQL files numbered sequentially:
```
dev/migrations/rider-app/1021-saved-locations.sql
dev/migrations/rider-app/1128-juspay-payments-integration.sql
dev/migrations/dynamic-offer-driver-app/0181-juspay-payments-integration.sql
```

## Transaction Handling

```haskell
-- Single create: NO transaction wrapper needed
QBooking.create booking

-- Multiple operations: wrap in runInTransaction
runInTransaction $ do
  QBooking.create booking
  QRide.create ride
  QPayment.updateStatus PAID paymentId
```

## Database Schemas

| Schema | Service | Used For |
|--------|---------|----------|
| `atlas_app` | rider-app | Customer bookings, searches, payments |
| `atlas_driver_offer_bpp` | driver-app | Driver data, rides, allocations |

## ClickHouse Analytics

ClickHouse is used for analytics and event tracking:
- Query module: `app/provider-platform/dynamic-offer-driver-app/Main/src/Storage/Clickhouse/BppTransactionJoin.hs`

## Related Docs

- Code generation: `07-namma-dsl.md`
- Multi-cloud / KV connector: `12-multi-cloud.md`
- Conventions: `15-conventions.md`
