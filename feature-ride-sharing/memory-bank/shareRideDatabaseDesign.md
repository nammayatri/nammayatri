# Shared Ride Database Design

This document defines the **optimized, consolidated schema** for the shared ride feature that eliminates redundancy by using a single `SharedEntity` table approach. This design simplifies relationships and reduces table proliferation while maintaining all necessary functionality.

**UPDATE:** This document now includes the recovered and optimized database design featuring `SharedEntity` with `TrackedEntity` status tracking, replacing the deprecated multi-table approach.

---

## Table of Contents
1. [Design Philosophy](#design-philosophy)
2. [Core Entity: SharedEntity](#core-entity-sharedentity)
3. [rider-app Schema](#rider-app-schema)
4. [driver-app Schema](#driver-app-schema)
5. [Integration with Existing Tables](#integration-with-existing-tables)
6. [Migration Strategy](#migration-strategy)
7. [Type Definitions](#type-definitions)
8. [Old Multi-Table Design (DEPRECATED)](#old-multi-table-design-deprecated)

---

## Design Philosophy

### **Key Principles:**
1. **Single Source of Truth**: One `SharedEntity` table tracks all shared ride states
2. **Array-Based Relationships**: Use `TrackedEntity` arrays to link to existing entities with status tracking
3. **State-Driven Design**: Entity lifecycle managed through status transitions
4. **Minimal Redundancy**: Avoid duplicating data that exists in linked entities
5. **Cross-Platform Consistency**: Same structure for both rider-app and driver-app

### **Benefits:**
- **Reduced Complexity**: Single table vs. 4 separate shared tables
- **Simplified Queries**: No complex joins between shared entities
- **Easy State Management**: All shared ride state in one place
- **Flexible Linking**: Can link to any combination of existing entities with individual status tracking
- **Future-Proof**: Easy to add new entity types without schema changes
- **Enhanced Status Tracking**: TrackedEntity allows marking individual entities as active/cancelled

---

## Core Entity: SharedEntity

The `SharedEntity` tables manage all shared ride state and relationships for each app independently.

### **State Flow:**
```
SEARCHING → MATCHED → ESTIMATED → BOOKED → ONGOING → COMPLETED/CANCELLED
```

### **Common Domain Fields:**
- `id`: `Id SharedEntity` (Primary Key)
- `status`: `SharedEntityStatus` (State machine driver)
- `entityType`: `SharedEntityType` (OVERLAPPING, FIXED_ROUTE)
- `searchRequestIds`: `[TrackedEntity]` (Core relationship with status tracking)
- `estimateIds`: `[TrackedEntity]` (Linked estimates with status tracking)
- `bookingIds`: `[TrackedEntity]` (Linked bookings with status tracking)
- `rideIds`: `[TrackedEntity]` (Linked rides with status tracking) 
- `merchantId`: `Id Merchant`
- `merchantOperatingCityId`: `Id MerchantOperatingCity`
- `vehicleCategory`: `VehicleCategory`
- `tripCategory`: `TripCategory` (Always RideShare)
- `driverId`: `Maybe (Id Person)` (Assigned driver)
- `waypoints`: `Value` (JSON array of pickup/drop points)
- `totalSeats`: `Int` (Sum of all passenger seats)
- `pairingTime`: `Maybe UTCTime`
- `createdAt`: `UTCTime`
- `updatedAt`: `UTCTime`

### **Driver-App Additional Fields:**
- `bapSharedEntityId`: `Maybe Text` (References rider-app SharedEntity.id)

---

## rider-app Schema

### Table: `SharedEntity`

**YAML Reference:** `Backend/app/rider-platform/rider-app/Main/spec/Storage/SharedEntity.yaml`

**Database Schema (Beam):**
```sql
CREATE TABLE atlas_app.shared_entity (
    id UUID PRIMARY KEY,
    status VARCHAR(255) NOT NULL,
    entity_type VARCHAR(255) NOT NULL,
    search_request_ids TEXT[] NOT NULL,
    estimate_ids TEXT[],
    booking_ids TEXT[],
    ride_ids TEXT[],
    merchant_id VARCHAR(36) NOT NULL,
    merchant_operating_city_id VARCHAR(36) NOT NULL,
    vehicle_category VARCHAR(255) NOT NULL,
    trip_category VARCHAR(255) DEFAULT 'RideShare',
    driver_id VARCHAR(36),
    waypoints JSONB NOT NULL,
    total_seats INT NOT NULL,
    pairing_time TIMESTAMPTZ,
    valid_till TIMESTAMPTZ NOT NULL,
    created_at TIMESTAMPTZ NOT NULL,
    updated_at TIMESTAMPTZ NOT NULL
);
```

**Beam Transformations:**
- `searchRequestIds`: `[TrackedEntity]` ↔ `[Text]` (via Show/Read instances: `"id:ACTIVE"` or `"id:CANCELLED"`)
- `estimateIds`: `[TrackedEntity]` ↔ `[Text]` (via Show/Read instances)  
- `bookingIds`: `[TrackedEntity]` ↔ `[Text]` (via Show/Read instances)
- `rideIds`: `[TrackedEntity]` ↔ `[Text]` (via Show/Read instances)

**Note:** The database automatically handles serialization/deserialization using the `FromField` and `HasSqlValueSyntax` instances, which internally use the custom `Show`/`Read` instances for `TrackedEntity`.

**Core Queries:**
```haskell
-- Find by individual search request
findBySearchRequestId :: Id SearchRequest -> Flow [SharedEntity]

-- Find by any linked entity
findByEstimateId :: Id Estimate -> Flow (Maybe SharedEntity)
findByBookingId :: Id Booking -> Flow (Maybe SharedEntity) 
findByRideId :: Id Ride -> Flow (Maybe SharedEntity)

-- Status-based queries
findByStatus :: SharedEntityStatus -> Flow [SharedEntity]
findActiveEntities :: Flow [SharedEntity]

-- Driver queries
findByDriverId :: Id Person -> Flow [SharedEntity]
findActiveRidesByDriver :: Id Person -> Flow [SharedEntity]

-- Merchant queries  
findByMerchantOperatingCityId :: Id MerchantOperatingCity -> Flow [SharedEntity]

-- Transaction sync
findByTransactionId :: Text -> Flow (Maybe SharedEntity)

-- Update operations
updateStatus :: SharedEntityStatus -> UTCTime -> Id SharedEntity -> Flow ()
updateDriverAssignment :: Id Person -> UTCTime -> Id SharedEntity -> Flow ()
updateCounterAppEntityId :: Text -> UTCTime -> Id SharedEntity -> Flow ()

-- Tracked entity operations
addEstimate :: Id Estimate -> UTCTime -> Id SharedEntity -> Flow ()
addBooking :: Id Booking -> UTCTime -> Id SharedEntity -> Flow ()
addRide :: Id Ride -> UTCTime -> Id SharedEntity -> Flow ()

-- Entity status updates
markEstimateCancelled :: Id Estimate -> UTCTime -> Id SharedEntity -> Flow ()
markBookingCancelled :: Id Booking -> UTCTime -> Id SharedEntity -> Flow ()
markRideCancelled :: Id Ride -> UTCTime -> Id SharedEntity -> Flow ()
markSearchRequestCancelled :: Id SearchRequest -> UTCTime -> Id SharedEntity -> Flow ()

-- Entity status queries
getActiveEstimates :: SharedEntity -> [Id Estimate]
getActiveBookings :: SharedEntity -> [Id Booking] 
getActiveRides :: SharedEntity -> [Id Ride]
getActiveSearchRequests :: SharedEntity -> [Id SearchRequest]

getCancelledEstimates :: SharedEntity -> [Id Estimate]
getCancelledBookings :: SharedEntity -> [Id Booking]
getCancelledRides :: SharedEntity -> [Id Ride]
getCancelledSearchRequests :: SharedEntity -> [Id SearchRequest]
```

**Example: Tracked Entity Usage**
```haskell
-- Sample SharedEntity with mixed active/cancelled entities
exampleSharedEntity = SharedEntity
  { searchRequestIds = 
      [ TrackedEntity "search-uuid-1" True    -- Active
      , TrackedEntity "search-uuid-2" False   -- Cancelled
      , TrackedEntity "search-uuid-3" True    -- Active
      ]
  , estimateIds = 
      [ TrackedEntity "est-uuid-1" True       -- Active estimate
      , TrackedEntity "est-uuid-2" False      -- Cancelled estimate
      ]
  , bookingIds = 
      [ TrackedEntity "booking-uuid-1" True   -- Active booking
      ]
  , rideIds = []                              -- No rides yet
  , ...
  }

-- Database storage as TEXT[] arrays (automatically serialized via Show instances):
-- search_request_ids = {"search-uuid-1:ACTIVE", "search-uuid-2:CANCELLED", "search-uuid-3:ACTIVE"}
-- estimate_ids = {"est-uuid-1:ACTIVE", "est-uuid-2:CANCELLED"}
-- booking_ids = {"booking-uuid-1:ACTIVE"}
-- ride_ids = {}

-- When reading from database, the FromField [TrackedEntity] instance 
-- automatically deserializes the TEXT[] back to [TrackedEntity] using Read instances

-- Query active entities
activeSearchRequests = getActiveSearchRequests exampleSharedEntity 
-- Result: ["search-uuid-1", "search-uuid-3"]

cancelledEstimates = getCancelledEstimates exampleSharedEntity
-- Result: ["est-uuid-2"]

-- Example domain-level operations:
markSearchRequestCancelled :: Id SearchRequest -> UTCTime -> Id SharedEntity -> Flow ()
markSearchRequestCancelled searchId now sharedEntityId = do
  sharedEntity <- QSharedEntity.findById sharedEntityId >>= fromMaybeM (SharedEntityNotFound sharedEntityId.getId)
  let updatedSearchRequests = updateTrackedEntityStatus (searchId.getId) False (searchRequestIds sharedEntity)
  QSharedEntity.updateSearchRequestIds updatedSearchRequests now sharedEntityId

updateTrackedEntityStatus :: Text -> Bool -> [TrackedEntity] -> [TrackedEntity]
updateTrackedEntityStatus targetId newStatus = 
  map (\te -> if entityId te == targetId then te { isActive = newStatus } else te)
```

**Sample waypoints JSON:**
```json
[
  {
    "type": "PICKUP",
    "search_request_id": "search-uuid-1",
    "customer_id": "customer-uuid-1",
    "lat": 12.9716,
    "lon": 77.5946,
    "address": "Pickup Address 1"
  },
  {
    "type": "PICKUP", 
    "search_request_id": "search-uuid-2",
    "customer_id": "customer-uuid-2",
    "lat": 12.9726,
    "lon": 77.5956,
    "address": "Pickup Address 2"
  },
  {
    "type": "DROPOFF",
    "search_request_id": "search-uuid-1", 
    "customer_id": "customer-uuid-1",
    "lat": 12.9816,
    "lon": 77.6046,
    "address": "Drop Address 1"
  },
  {
    "type": "DROPOFF",
    "search_request_id": "search-uuid-2",
    "customer_id": "customer-uuid-2", 
    "lat": 12.9826,
    "lon": 77.6056,
    "address": "Drop Address 2"
  }
]
```

### Configuration Table: `SharedRideConfigs`

Configuration parameters for shared ride feature in the rider app.

**YAML Reference:** `Backend/app/rider-platform/rider-app/Main/spec/Storage/SharedRideConfigs.yaml`

**Domain Fields:**
- `id`: `Id SharedRideConfigs` (Primary Key)
- `merchantId`: `Id Merchant`
- `merchantOperatingCityId`: `Id MerchantOperatingCity` (Secondary Key)
- `vehicleCategory`: `VehicleCategory`
- `pickupLocationSearchRadius`: `Meters`
- `searchThresholdForSharedEstimate`: `Int`
- `searchRequestExpirySeconds`: `Seconds`
- `searchExpiryBufferSeconds`: `Seconds`
- `customerRemainingThresholdForFlowContinuation`: `Int`
- `dropLocationSearchRadius`: `Meters`
- `actualPickupDistanceThreshold`: `Meters`
- `actualDropDistanceThreshold`: `Meters`
- `routeMatchingThreshold`: `Double` (stored as percentage/100, e.g., 0.85 for 85%)
- `geoHashPrecisionForRouteMatching`: `Int`
- `routeOverlapThreshold`: `Double` (stored as percentage/100, e.g., 0.70 for 70%)
- `createdAt`: `UTCTime`
- `updatedAt`: `UTCTime`

**Database Schema (Beam):**
```sql
CREATE TABLE atlas_app.shared_ride_configs (
    id UUID PRIMARY KEY,
    merchant_id VARCHAR(36) NOT NULL,
    merchant_operating_city_id VARCHAR(36) NOT NULL,
    vehicle_category VARCHAR(255) NOT NULL,
    pickup_location_search_radius DOUBLE PRECISION NOT NULL,
    search_threshold_for_shared_estimate INT NOT NULL,
    search_request_expiry_seconds INT NOT NULL,
    search_expiry_buffer_seconds INT NOT NULL,
    customer_remaining_threshold_for_flow_continuation INT NOT NULL,
    drop_location_search_radius DOUBLE PRECISION NOT NULL,
    actual_pickup_distance_threshold DOUBLE PRECISION NOT NULL,
    actual_drop_distance_threshold DOUBLE PRECISION NOT NULL,
    route_matching_threshold DOUBLE PRECISION NOT NULL,
    geo_hash_precision_for_route_matching INT NOT NULL,
    route_overlap_threshold DOUBLE PRECISION NOT NULL,
    created_at TIMESTAMPTZ NOT NULL,
    updated_at TIMESTAMPTZ NOT NULL
);

-- Indexes for performance
CREATE INDEX idx_shared_ride_configs_merchant_city_vehicle ON atlas_app.shared_ride_configs(merchant_operating_city_id, vehicle_category);
CREATE INDEX idx_shared_ride_configs_merchant_city ON atlas_app.shared_ride_configs(merchant_operating_city_id);
```

**Core Queries:**
```haskell
-- Find config by city and vehicle
findByMerchantOperatingCityIdAndVehicleCategory :: Id MerchantOperatingCity -> VehicleCategory -> Flow (Maybe SharedRideConfigs)

-- Find all configs for a city
findByMerchantOperatingCityId :: Id MerchantOperatingCity -> Flow [SharedRideConfigs]

-- Update configuration values
updateConfigValues :: SharedRideConfigs -> UTCTime -> Id SharedRideConfigs -> Flow ()
```

---

## driver-app Schema

### Table: `SharedEntity`

**YAML Reference:** `Backend/app/provider-platform/dynamic-offer-driver-app/Main/spec/Storage/SharedEntity.yaml`

**Database Schema (Beam):**
```sql
CREATE TABLE atlas_driver_offer_bpp.shared_entity (
    id UUID PRIMARY KEY,
    status VARCHAR(255) NOT NULL,
    entity_type VARCHAR(255) NOT NULL,
    search_request_ids TEXT[] NOT NULL,
    estimate_ids TEXT[],
    booking_ids TEXT[],
    ride_ids TEXT[],
    merchant_id VARCHAR(36) NOT NULL,
    merchant_operating_city_id VARCHAR(36) NOT NULL,
    vehicle_category VARCHAR(255) NOT NULL,
    trip_category VARCHAR(255) DEFAULT 'RideShare',
    driver_id VARCHAR(36),
    bap_shared_entity_id VARCHAR(255),
    waypoints JSONB NOT NULL,
    total_seats INT NOT NULL,
    pairing_time TIMESTAMPTZ,
    valid_till TIMESTAMPTZ NOT NULL,
    created_at TIMESTAMPTZ NOT NULL,
    updated_at TIMESTAMPTZ NOT NULL
);
```

**Additional Driver-App Queries:**
```haskell
-- BAP sync queries
findByBapSharedEntityId :: Text -> Flow (Maybe SharedEntity)
updateBapSharedEntityId :: Text -> UTCTime -> Id SharedEntity -> Flow ()

-- All other queries same as rider-app
```

---

## Integration with Existing Tables

### Linking Strategy

Instead of adding shared ride IDs to existing tables, we **reverse the relationship**:

**Before (Redundant Approach):**
```haskell
-- Every table needs shared ride fields
data SearchRequest = SearchRequest 
  { sharedEntityId :: Maybe (Id SharedEntity) -- ❌ Redundant
  , ... 
  }

data Estimate = Estimate
  { sharedEntityId :: Maybe (Id SharedEntity) -- ❌ Redundant  
  , ...
  }
```

**After (Array-Based Approach with Status Tracking):**
```haskell
-- Only SharedEntity tracks relationships with status
data SharedEntity = SharedEntity
  { searchRequestIds :: [TrackedEntity] -- ✅ Single source of truth with status
  , estimateIds :: [TrackedEntity]      -- ✅ No redundancy, tracks active/cancelled
  , bookingIds :: [TrackedEntity]       -- ✅ Can mark individual bookings as cancelled
  , rideIds :: [TrackedEntity]          -- ✅ Can track ride status individually
  , ...
  }
```

### Query Patterns

**Find shared entity for any linked entity:**
```haskell
findSharedEntityBySearchRequest :: Id SearchRequest -> Flow (Maybe SharedEntity)
findSharedEntityBySearchRequest searchId = do
  entities <- runInReplica $ QSharedEntity.findBySearchRequestId searchId
  return $ listToMaybe entities

findSharedEntityByEstimate :: Id Estimate -> Flow (Maybe SharedEntity)  
findSharedEntityByEstimate estimateId = do
  runInReplica $ QSharedEntity.findByEstimateId estimateId
```

**Check if entity is part of shared ride:**
```haskell
isSharedRide :: Id SearchRequest -> Flow Bool
isSharedRide searchId = do
  mbSharedEntity <- findSharedEntityBySearchRequest searchId
  return $ isJust mbSharedEntity
```

### Minimal Changes to Existing Tables

**Only add these fields where absolutely necessary:**

**DriverQuote.yaml** (only if needed for driver-app queries):
```yaml
# Optional - only if direct query access needed
isSharedRide: Bool  # Default: False
```

**SearchRequestForDriver.yaml** (only if needed for driver-app queries):
```yaml  
# Optional - only if direct query access needed
isSharedRide: Bool  # Default: False
```

**All other tables remain unchanged.**

---

## Migration Strategy

### Phase 1: Create SharedEntity Table
1. Create `shared_entity` table in both rider-app and driver-app
2. Add indexes and constraints
3. Deploy schema changes

### Phase 2: Migrate Existing Data (if any)
```sql
-- Example migration from old shared tables to new SharedEntity
INSERT INTO atlas_app.shared_entity (
    id, status, entity_type, search_request_ids, 
    merchant_id, merchant_operating_city_id, 
    vehicle_category, waypoints, total_seats,
    valid_till, created_at, updated_at
)
SELECT 
    shared_search_request.id,
    'SEARCHING'::varchar,
    'SEARCH_GROUP'::varchar,
    shared_search_request.search_request_ids,
    shared_search_request.merchant_id,
    shared_search_request.merchant_operating_city_id,
    shared_search_request.vehicle_category,
    shared_search_request.waypoints,
    calculate_total_seats(shared_search_request.search_request_ids),
    shared_search_request.valid_till,
    shared_search_request.created_at,
    shared_search_request.updated_at
FROM atlas_app.shared_search_request;
```

### Phase 3: Update Application Code
1. Replace queries to old shared tables with SharedEntity queries
2. Update business logic to use new status-driven model
3. Remove dependencies on old shared tables

### Phase 4: Cleanup (Optional)
1. Drop old shared tables once migration is verified
2. Remove unused indexes and constraints

---

## Type Definitions

### New Enums and Types for Consolidated Design

```yaml
SharedEntityStatus:
  enum: "SEARCHING, MATCHED, ESTIMATED, BOOKED, DRIVER_ASSIGNED, ONGOING, COMPLETED, CANCELLED, EXPIRED"
  derive: "HttpInstance"

SharedEntityType:  
  enum: "OVERLAPPING, FIXED_ROUTE"
  derive: "HttpInstance"
  
WaypointType:
  enum: "PICKUP, DROPOFF, INTERMEDIATE"
  derive: "HttpInstance"

TrackedEntityStatus:
  enum: "ACTIVE, CANCELLED"
  derive: "HttpInstance"
```

### TrackedEntity Data Type

```haskell
data TrackedEntity = TrackedEntity
  { entityId :: Text
  , isActive :: Bool
  } deriving (Eq, Generic, ToJSON, FromJSON, ToSchema)

-- Custom Show/Read instances for database serialization  
instance Show TrackedEntity where
  show (TrackedEntity entityId True) = T.unpack $ entityId <> ":ACTIVE"
  show (TrackedEntity entityId False) = T.unpack $ entityId <> ":CANCELLED"

instance Read TrackedEntity where
  readsPrec _ input = 
    case T.splitOn ":" (T.pack input) of
      [entityId, "ACTIVE"] -> [(TrackedEntity entityId True, "")]
      [entityId, "CANCELLED"] -> [(TrackedEntity entityId False, "")]
      _ -> []

-- Database instances (following existing patterns)
instance HasSqlValueSyntax be Value => HasSqlValueSyntax be TrackedEntity where
  sqlValueSyntax = sqlValueSyntax . show

instance FromField TrackedEntity where
  fromField = fromFieldEnum

instance FromField [TrackedEntity] where
  fromField f mbValue = V.toList <$> fromField f mbValue

instance (HasSqlValueSyntax be (V.Vector Text)) => HasSqlValueSyntax be [TrackedEntity] where
  sqlValueSyntax trackedList =
    let x = (T.pack . show <$> trackedList :: [Text])
     in sqlValueSyntax (V.fromList x)

instance BeamSqlBackend be => B.HasSqlEqualityCheck be [TrackedEntity]

instance FromBackendRow Postgres [TrackedEntity]

instance ToSQLObject TrackedEntity where
  convertToSQLObject = SQLObjectValue . T.pack . show

-- Helper functions
mkActiveEntity :: Text -> TrackedEntity
mkActiveEntity entityId = TrackedEntity entityId True

mkCancelledEntity :: Text -> TrackedEntity  
mkCancelledEntity entityId = TrackedEntity entityId False

isEntityActive :: TrackedEntity -> Bool
isEntityActive = isActive

isEntityCancelled :: TrackedEntity -> Bool
isEntityCancelled = not . isActive

-- Example usage in domain operations:
updateTrackedEntityStatus :: Text -> Bool -> [TrackedEntity] -> [TrackedEntity]
updateTrackedEntityStatus targetId newStatus = 
  map (\te -> if entityId te == targetId then te { isActive = newStatus } else te)

-- Query helpers for extracting active/cancelled entities
getActiveEntities :: [TrackedEntity] -> [Text]
getActiveEntities = map entityId . filter isActive

getCancelledEntities :: [TrackedEntity] -> [Text]  
getCancelledEntities = map entityId . filter (not . isActive)

-- Required imports for the above instances:
-- import qualified Data.Vector as V
-- import qualified Data.Text as T
-- import Database.Beam.Backend
-- import Database.Beam.Postgres  
-- import Database.PostgreSQL.Simple.FromField (FromField, fromField)
-- import Kernel.Types.FromField (fromFieldEnum)
-- import Sequelize.SQLObject (SQLObject(..), ToSQLObject(..))
```

### Status Transition Rules

```haskell
validStatusTransitions :: SharedEntityStatus -> [SharedEntityStatus]
validStatusTransitions = \case
  SEARCHING -> [MATCHED, EXPIRED, CANCELLED]
  MATCHED -> [ESTIMATED, CANCELLED]  
  ESTIMATED -> [BOOKED, CANCELLED]
  BOOKED -> [DRIVER_ASSIGNED, CANCELLED]
  DRIVER_ASSIGNED -> [ONGOING, CANCELLED]
  ONGOING -> [COMPLETED, CANCELLED]
  _ -> [CANCELLED] -- Terminal states can only be cancelled
```

---

## Benefits of Optimized Design

### **Reduced Complexity:**
- **1 table** instead of 8 shared tables (4 per app)
- **Single query** to get all shared ride state
- **No complex joins** between shared entities
- **Unified status management** 

### **Better Performance:**
- **Array-based lookups** using GIN indexes
- **Fewer table joins** in complex queries
- **Single transaction** for shared ride updates
- **Reduced storage overhead**

### **Improved Maintainability:**
- **Single schema** to maintain
- **Centralized business logic** 
- **Easier debugging** with all state in one place
- **Simplified testing** with fewer moving parts

### **Enhanced Flexibility:**
- **Easy to add new fields** without schema proliferation
- **Support for complex ride scenarios** (multi-stop, etc.)
- **Future-proof design** for new shared ride features
- **Cross-platform consistency** guaranteed

---

## Comparison: Common vs Separate Tables

| Aspect | Common Table (Alternative) | Separate Tables (Current) |
|--------|---------------------------|---------------------------|
| **Type Dependencies** | Requires shared-kernel types | App-specific types only |
| **Schema Flexibility** | Single schema for both apps | Independent app schemas |
| **Deployment** | Must deploy both apps together | Independent app deployments |
| **Cross-App Queries** | Direct foreign key joins | API calls between apps |
| **Data Consistency** | Single transaction scope | Eventual consistency via APIs |
| **Complexity** | Lower initial complexity | Higher integration complexity |
| **Maintenance** | Single schema maintenance | Separate schema maintenance |
| **Performance** | Fewer API calls | More API calls for cross-app data |

**Decision: Separate Tables**
- Avoids shared-kernel type dependencies
- Enables independent app evolution
- Provides app-specific schema flexibility
- Aligns with microservice architecture

---

## Comparison: Old vs New Design

| Aspect | Old Design (Multiple Tables) | New Design (SharedEntity) |
|--------|------------------------------|---------------------------|
| **Tables** | 8 shared tables (4 × 2 apps) | 2 SharedEntity tables |
| **Relationships** | Complex foreign key chains | Simple array-based links |
| **Queries** | Multiple joins required | Single table queries |
| **State Management** | Scattered across tables | Centralized status machine |
| **Code Complexity** | High (multiple table ops) | Low (single entity ops) |
| **Data Consistency** | Risk of inconsistency | Single source of truth |
| **Migration Effort** | Complex schema changes | Simple array updates |
| **Performance** | Multiple table scans | Single table with indexes |

---

## Old Multi-Table Design (DEPRECATED)

⚠️ **DEPRECATED**: The following multi-table approach has been replaced by the optimized single-table design above. This section is kept for historical reference and migration purposes.

---

### rider-app Schema (OLD)

#### Table: `SharedSearchRequest` (DEPRECATED)

Wraps multiple `SearchRequest` entities to represent a pool of customers waiting to be matched.

**YAML Reference:** `Backend/app/rider-platform/rider-app/Main/spec/Storage/SharedSearchRequest.yaml` (REMOVED)

**Domain Fields:**
- `id`: `Id SharedSearchRequest` (Primary Key)
- `status`: `SharedSearchRequestStatus`
- `searchRequestIds`: `[Id SearchRequest]`
- `merchantId`: `Id Merchant`
- `merchantOperatingCityId`: `Id MerchantOperatingCity`
- `vehicleCategory`: `VehicleCategory`
- `waypoints`: `Value`
- `maxDistance`: `Maybe Distance`
- `totalCustomerExtraFee`: `Maybe Price`
- `validTill`: `UTCTime`
- `createdAt`: `UTCTime`
- `updatedAt`: `UTCTime`

**Database Schema (Beam):**
- `id`: `uuid` (Primary Key)
- `status`: `character varying(255)`
- `search_request_ids`: `uuid[]`
- `merchant_id`: `character varying(36)`
- `merchant_operating_city_id`: `character varying(36)`
- `vehicle_category`: `character varying(255)`
- `waypoints`: `jsonb`
- `max_distance`: `double precision` (via beam transformation)
- `max_distance_value`: `double precision` (via beam transformation)
- `distance_unit`: `character varying(255)` (via beam transformation)
- `total_customer_extra_fee`: `numeric(30,2)` (via beam transformation)
- `total_customer_extra_fee_amount`: `numeric(30,10)` (via beam transformation)
- `currency`: `character varying(255)` (via beam transformation)
- `valid_till`: `timestamptz`
- `created_at`: `timestamptz`
- `updated_at`: `timestamptz`

**Beam Transformations:**
- `maxDistance` splits into: `maxDistance` (Centesimal), `maxDistanceValue` (HighPrecDistance), `distanceUnit` (DistanceUnit)
- `totalCustomerExtraFee` splits into: `totalCustomerExtraFee` (Money), `totalCustomerExtraFeeAmount` (HighPrecMoney), `currency` (Currency)

**Queries:**
- `updateStatus(status, updatedAt)` WHERE `id`
- `findActiveRequests()` WHERE `status`

#### Table: `SharedEstimate` (DEPRECATED)

Wraps multiple `Estimate` entities. Represents the combined fare estimate for a matched group.

**YAML Reference:** `Backend/app/rider-platform/rider-app/Main/spec/Storage/SharedEstimate.yaml` (REMOVED)

**Domain Fields:**
- `id`: `Id SharedEstimate` (Primary Key)
- `sharedSearchRequestId`: `Id SharedSearchRequest` (Secondary Key)
- `estimateIds`: `[Id Estimate]`
- `merchantId`: `Id Merchant`
- `merchantOperatingCityId`: `Id MerchantOperatingCity`
- `bppSharedEstimateId`: `Text`
- `providerId`: `Text`
- `providerName`: `Text`
- `providerUrl`: `BaseUrl`
- `serviceTierName`: `Maybe Text`
- `estimatedTotalFare`: `HighPrecMoney`
- `totalFareRangeMin`: `HighPrecMoney`
- `totalFareRangeMax`: `HighPrecMoney`
- `estimatedDuration`: `Maybe Seconds`
- `estimatedDistance`: `Maybe Distance`
- `vehicleServiceTierType`: `ServiceTierType|NoRelation`
- `vehicleServiceTierSeatingCapacity`: `Maybe Int`
- `nightShiftCharge`: `Maybe Money`
- `nightShiftChargeAmount`: `Maybe HighPrecMoney`
- `oldNightShiftCharge`: `Maybe Centesimal`
- `nightShiftStart`: `Maybe TimeOfDay`
- `nightShiftEnd`: `Maybe TimeOfDay`
- `status`: `EstimateStatus`
- `tripCategory`: `Maybe TripCategory`
- `validTill`: `UTCTime`
- `createdAt`: `UTCTime`
- `updatedAt`: `UTCTime`

**Database Schema (Beam):**
- `id`: `uuid` (Primary Key)
- `shared_search_request_id`: `uuid` (Foreign Key, Secondary Key)
- `estimate_ids`: `uuid[]`
- `merchant_id`: `character varying(36)`
- `merchant_operating_city_id`: `character varying(36)`
- `bpp_shared_estimate_id`: `text`
- `provider_id`: `character varying(255)`
- `provider_name`: `character varying(255)`
- `provider_url`: `character varying(255)` (via beam transformation)
- `service_tier_name`: `text`
- `estimated_total_fare`: `numeric(30,10)`
- `total_fare_range_min`: `numeric(30,10)`
- `total_fare_range_max`: `numeric(30,10)`
- `estimated_duration`: `integer`
- `estimated_distance`: `double precision` (via beam transformation)
- `estimated_distance_value`: `double precision` (via beam transformation)
- `distance_unit`: `character varying(255)` (via beam transformation)
- `vehicle_variant`: `character varying(255)` (via beam transformation)
- `vehicle_service_tier_seating_capacity`: `integer`
- `night_shift_charge`: `numeric(30,2)`
- `night_shift_charge_amount`: `numeric(30,10)`
- `old_night_shift_charge`: `numeric(10,2)`
- `night_shift_start`: `time`
- `night_shift_end`: `time`
- `status`: `character varying(255)`
- `trip_category`: `character varying(255)`
- `valid_till`: `timestamptz`
- `created_at`: `timestamptz`
- `updated_at`: `timestamptz`

**Beam Transformations:**
- `providerUrl`: `BaseUrl` ↔ `Text` (showBaseUrl/parseBaseUrl)
- `estimatedDistance` splits into: `estimatedDistance` (HighPrecMeters), `estimatedDistanceValue` (HighPrecDistance), `distanceUnit` (DistanceUnit)
- `vehicleServiceTierType` mapped to `vehicleVariant` in beam

**Queries:**
- `findAllBySharedSearchRequestId(sharedSearchRequestId)`
- `updateStatus(status)` WHERE `id`
- `findByStatus(status)`

#### Table: `SharedBooking` (DEPRECATED)

Wraps multiple `Booking` entities. Represents the confirmed booking for a matched group before a driver is assigned.

**YAML Reference:** `Backend/app/rider-platform/rider-app/Main/spec/Storage/SharedBooking.yaml` (REMOVED)

**Domain Fields:**
- `id`: `Id SharedBooking` (Primary Key)
- `sharedEstimateId`: `Id SharedEstimate` (Secondary Key)
- `bookingIds`: `[Id Booking]`
- `transactionId`: `Text`
- `bppSharedBookingId`: `Text`
- `status`: `BookingStatus`
- `merchantId`: `Id Merchant`
- `merchantOperatingCityId`: `Id MerchantOperatingCity`
- `driverId`: `Maybe (Id Person)`
- `providerId`: `Text`
- `providerUrl`: `BaseUrl`
- `vehicleServiceTierType`: `ServiceTierType|NoRelation`
- `estimatedTotalFare`: `Price`
- `estimatedDuration`: `Maybe Seconds`
- `estimatedDistance`: `Maybe Distance`
- `pairingTime`: `UTCTime`
- `distanceUnit`: `DistanceUnit`
- `createdAt`: `UTCTime`
- `updatedAt`: `UTCTime`

**Database Schema (Beam):**
- `id`: `uuid` (Primary Key)
- `shared_estimate_id`: `uuid` (Foreign Key, Secondary Key)
- `booking_ids`: `uuid[]`
- `transaction_id`: `character varying(36)`
- `bpp_shared_booking_id`: `text`
- `status`: `character varying(255)`
- `merchant_id`: `character varying(36)`
- `merchant_operating_city_id`: `character varying(36)`
- `driver_id`: `character varying(36)`
- `provider_id`: `character varying(255)`
- `provider_url`: `character varying(255)` (via beam transformation)
- `vehicle_service_tier_type`: `character varying(255)`
- `estimated_total_fare`: `numeric(30,10)` (via beam transformation)
- `currency`: `character varying(255)` (via beam transformation)
- `estimated_duration`: `integer`
- `estimated_distance`: `double precision` (via beam transformation)
- `estimated_distance_value`: `double precision` (via beam transformation)
- `distance_unit`: `character varying(255)`
- `pairing_time`: `timestamptz`
- `created_at`: `timestamptz`
- `updated_at`: `timestamptz`

**Beam Transformations:**
- `providerUrl`: `BaseUrl` ↔ `Text` (showBaseUrl/parseBaseUrl)
- `estimatedTotalFare` splits into: `estimatedTotalFare` (HighPrecMoney), `currency` (Currency)
- `estimatedDistance` splits into: `estimatedDistance` (HighPrecMeters), `estimatedDistanceValue` (HighPrecDistance)

**Queries:**
- `findBySharedEstimateId(sharedEstimateId)`
- `updateStatus(status, updatedAt)` WHERE `id`
- `updateDriverId(driverId, updatedAt)` WHERE `id`
- `findByStatus(status)`

#### Table: `SharedRide` (DEPRECATED)

Wraps multiple `Ride` entities. Represents the active, in-progress shared ride after a driver has been assigned.

**YAML Reference:** `Backend/app/rider-platform/rider-app/Main/spec/Storage/SharedRide.yaml` (REMOVED)

**Domain Fields:**
- `id`: `Id SharedRide` (Primary Key)
- `sharedBookingId`: `Id SharedBooking` (Secondary Key)
- `rideIds`: `[Id Ride]`
- `bppSharedRideId`: `Text`
- `status`: `SharedRideStatus`
- `merchantId`: `Id Merchant`
- `merchantOperatingCityId`: `Id MerchantOperatingCity`
- `driverId`: `Id Person`
- `vehicleNumber`: `Text`
- `vehicleModel`: `Text`
- `vehicleVariant`: `VehicleVariant|NoRelation`
- `vehicleServiceTierType`: `Maybe ServiceTierType|NoRelation`
- `waypoints`: `Value`
- `trackingUrl`: `Maybe BaseUrl`
- `totalFare`: `Maybe HighPrecMoney`
- `chargeableDistanceValue`: `Maybe HighPrecDistance`
- `traveledDistanceValue`: `Maybe HighPrecDistance`
- `rideStartTime`: `Maybe UTCTime`
- `rideEndTime`: `Maybe UTCTime`
- `createdAt`: `UTCTime`
- `updatedAt`: `UTCTime`

**Database Schema (Beam):**
- `id`: `uuid` (Primary Key)
- `shared_booking_id`: `uuid` (Foreign Key, Secondary Key)
- `ride_ids`: `uuid[]`
- `bpp_shared_ride_id`: `text`
- `status`: `character varying(255)`
- `merchant_id`: `character varying(36)`
- `merchant_operating_city_id`: `character varying(36)`
- `driver_id`: `character varying(36)`
- `vehicle_number`: `character varying(255)`
- `vehicle_model`: `character varying(255)`
- `vehicle_variant`: `character varying(60)`
- `vehicle_service_tier_type`: `character varying(255)`
- `waypoints`: `jsonb`
- `tracking_url`: `character varying(255)` (via beam transformation)
- `total_fare`: `numeric(30,10)`
- `chargeable_distance_value`: `double precision`
- `traveled_distance_value`: `double precision`
- `ride_start_time`: `timestamptz`
- `ride_end_time`: `timestamptz`
- `created_at`: `timestamptz`
- `updated_at`: `timestamptz`

**Beam Transformations:**
- `trackingUrl`: `Maybe BaseUrl` ↔ `Maybe Text` (showBaseUrl/parseBaseUrl)

**Queries:**
- `findBySharedBookingId(sharedBookingId)`
- `updateStatus(status, updatedAt)` WHERE `id`
- `updateRideStartTime(rideStartTime, updatedAt)` WHERE `id`
- `updateRideEndTime(rideEndTime, updatedAt)` WHERE `id`
- `updateTotalFare(totalFare, updatedAt)` WHERE `id`
- `findByDriverId(driverId)`
- `findByStatus(status)`

### dynamic-offer-driver-app Schema (OLD)

#### Table: `SharedSearchRequest` (DEPRECATED)

Wraps multiple `SearchRequest` entities for the driver app. This table holds aggregated data for a potential shared ride offer.

**YAML Reference:** `Backend/app/provider-platform/dynamic-offer-driver-app/Main/spec/Storage/SharedSearchRequest.yaml` (REMOVED)

**Domain Fields:**
- `id`: `Id SharedSearchRequest` (Primary Key)
- `transactionId`: `Text` (References `SharedSearchRequest.id` in rider-app)
- `status`: `SearchRequestStatus`
- `merchantId`: `Id Merchant`
- `merchantOperatingCityId`: `Id MerchantOperatingCity`
- `fromLocationIds`: `[Id Location]`
- `toLocationIds`: `[Id Location]`
- `estimatedDistance`: `Maybe Meters`
- `estimatedDuration`: `Maybe Seconds`
- `vehicleCategory`: `Maybe VehicleCategory`
- `tollCharges`: `Maybe HighPrecMoney`
- `tollNames`: `Maybe [Text]`
- `tripCategory`: `Maybe TripCategory`
- `validTill`: `UTCTime`
- `createdAt`: `UTCTime`
- `updatedAt`: `UTCTime`

**Database Schema (Beam):**
- `id`: `uuid` (Primary Key)
- `transaction_id`: `character varying(36)`
- `status`: `character varying(255)`
- `merchant_id`: `character varying(36)`
- `merchant_operating_city_id`: `character varying(36)`
- `from_location_ids`: `uuid[]`
- `to_location_ids`: `uuid[]`
- `estimated_distance`: `double precision`
- `estimated_duration`: `integer`
- `vehicle_category`: `character varying(255)`
- `toll_charges`: `double precision`
- `toll_names`: `text[]`
- `trip_category`: `character varying(255)`
- `valid_till`: `timestamptz`
- `created_at`: `timestamptz`
- `updated_at`: `timestamptz`

**Queries:**
- `findByTransactionId(transactionId)`
- `updateStatus(status, updatedAt)` WHERE `id`
- `findByStatus(status)`
- `findActiveRequests()` WHERE `status`

#### Table: `SharedEstimate` (DEPRECATED)

Wraps multiple `Estimate` entities for the driver app.

**YAML Reference:** `Backend/app/provider-platform/dynamic-offer-driver-app/Main/spec/Storage/SharedEstimate.yaml` (REMOVED)

**Domain Fields:**
- `id`: `Id SharedEstimate` (Primary Key)
- `transactionId`: `Text` (References `SharedSearchRequest.id` in rider-app)
- `status`: `EstimateStatus`
- `merchantId`: `Id Merchant`
- `merchantOperatingCityId`: `Id MerchantOperatingCity`
- `totalMinFare`: `HighPrecMoney`
- `totalMaxFare`: `HighPrecMoney`
- `currency`: `Currency`
- `estimatedDistance`: `Maybe Meters`
- `estimatedDuration`: `Maybe Seconds`
- `distanceUnit`: `DistanceUnit`
- `vehicleServiceTier`: `ServiceTierType|NoRelation`
- `tripCategory`: `TripCategory|NoRelation`
- `tollNames`: `Maybe [Text]`
- `validTill`: `UTCTime`
- `createdAt`: `UTCTime`
- `updatedAt`: `UTCTime`

**Database Schema (Beam):**
- `id`: `uuid` (Primary Key)
- `transaction_id`: `character varying(36)`
- `status`: `character varying(255)`
- `merchant_id`: `character varying(36)`
- `merchant_operating_city_id`: `character varying(36)`
- `total_min_fare`: `numeric(30,2)`
- `total_max_fare`: `numeric(30,2)`
- `currency`: `character varying(255)`
- `estimated_distance`: `double precision`
- `estimated_duration`: `integer`
- `distance_unit`: `character varying(255)`
- `vehicle_service_tier`: `character varying(255)`
- `trip_category`: `character varying(255)`
- `toll_names`: `text[]`
- `valid_till`: `timestamptz`
- `created_at`: `timestamptz`
- `updated_at`: `timestamptz`

**Queries:**
- `findByTransactionId(transactionId)`
- `updateStatus(status, updatedAt)` WHERE `id`
- `findByStatus(status)`
- `findByVehicleServiceTier(vehicleServiceTier)`
- `findActiveEstimates()` WHERE `status`

#### Table: `SharedBooking` (DEPRECATED)

Wraps multiple `Booking` entities for the driver app.

**YAML Reference:** `Backend/app/provider-platform/dynamic-offer-driver-app/Main/spec/Storage/SharedBooking.yaml` (REMOVED)

**Domain Fields:**
- `id`: `Id SharedBooking` (Primary Key)
- `transactionId`: `Text` (References `SharedSearchRequest.id` in rider-app)
- `sharedEstimateId`: `Id SharedEstimate` (Secondary Key)
- `bookingIds`: `[Id Booking]`
- `status`: `BookingStatus`
- `merchantId`: `Id Merchant`
- `merchantOperatingCityId`: `Id MerchantOperatingCity`
- `providerId`: `Text`
- `providerUrl`: `BaseUrl`
- `driverId`: `Maybe (Id Person)`
- `fromLocationIds`: `[Id Location]`
- `toLocationIds`: `[Id Location]`
- `vehicleServiceTier`: `ServiceTierType|NoRelation`
- `estimatedTotalFare`: `HighPrecMoney`
- `estimatedDuration`: `Maybe Seconds`
- `estimatedDistance`: `Maybe Meters`
- `tollNames`: `Maybe [Text]`
- `distanceUnit`: `DistanceUnit`
- `pairingTime`: `UTCTime`
- `createdAt`: `UTCTime`
- `updatedAt`: `UTCTime`

**Database Schema (Beam):**
- `id`: `uuid` (Primary Key)
- `transaction_id`: `character varying(36)`
- `shared_estimate_id`: `uuid` (Foreign Key, Secondary Key)
- `booking_ids`: `uuid[]`
- `status`: `character varying(255)`
- `merchant_id`: `character varying(36)`
- `merchant_operating_city_id`: `character varying(36)`
- `provider_id`: `character varying(255)`
- `provider_url`: `character varying(255)` (via beam transformation)
- `driver_id`: `character varying(36)`
- `from_location_ids`: `uuid[]`
- `to_location_ids`: `uuid[]`
- `vehicle_service_tier`: `character varying(255)`
- `estimated_total_fare`: `numeric(30,10)`
- `estimated_duration`: `integer`
- `estimated_distance`: `double precision`
- `toll_names`: `text[]`
- `pairing_time`: `timestamptz`
- `created_at`: `timestamptz`
- `updated_at`: `timestamptz`

**Beam Transformations:**
- `providerUrl`: `BaseUrl` ↔ `Text` (showBaseUrl/parseBaseUrl)

**Queries:**
- `findByTransactionId(transactionId)`
- `findBySharedEstimateId(sharedEstimateId)`
- `updateStatus(status, updatedAt)` WHERE `id`
- `updateDriverId(driverId, updatedAt)` WHERE `id`
- `findByDriverId(driverId)`
- `findByStatus(status)`

#### Table: `SharedRide` (DEPRECATED)

Wraps multiple `Ride` entities for the driver app.

**YAML Reference:** `Backend/app/provider-platform/dynamic-offer-driver-app/Main/spec/Storage/SharedRide.yaml` (REMOVED)

**Domain Fields:**
- `id`: `Id SharedRide` (Primary Key)
- `transactionId`: `Text` (References `SharedSearchRequest.id` in rider-app)
- `sharedBookingId`: `Id SharedBooking` (Secondary Key)
- `rideIds`: `[Id Ride]`
- `status`: `SharedRideStatus`
- `merchantId`: `Id Merchant`
- `merchantOperatingCityId`: `Id MerchantOperatingCity`
- `driverId`: `Id Person`
- `vehicleNumber`: `Text`
- `vehicleModel`: `Text`
- `vehicleVariant`: `VehicleVariant|NoRelation`
- `vehicleServiceTierType`: `Maybe ServiceTierType|NoRelation`
- `waypoints`: `Value`
- `trackingUrl`: `Maybe BaseUrl`
- `totalFare`: `Maybe HighPrecMoney`
- `chargeableDistance`: `Maybe Meters`
- `traveledDistance`: `HighPrecMeters`
- `rideStartTime`: `Maybe UTCTime`
- `rideEndTime`: `Maybe UTCTime`
- `tripCategory`: `TripCategory|NoRelation`
- `numberOfSnapToRoadCalls`: `Maybe Int`
- `numberOfOsrmSnapToRoadCalls`: `Maybe Int`
- `numberOfSelfTuned`: `Maybe Int`
- `numberOfDeviation`: `Maybe Bool`
- `estimatedTollCharges`: `Maybe HighPrecMoney`
- `estimatedTollNames`: `Maybe [Text]`
- `tollCharges`: `Maybe HighPrecMoney`
- `tollNames`: `Maybe [Text]`
- `rideEndedBy`: `Maybe RideEndedBy`
- `passedThroughDestination`: `Maybe Bool`
- `isPickupOrDestinationEdited`: `Maybe Bool`
- `createdAt`: `UTCTime`
- `updatedAt`: `UTCTime`

**Database Schema (Beam):**
- `id`: `uuid` (Primary Key)
- `transaction_id`: `character varying(36)`
- `shared_booking_id`: `uuid` (Foreign Key, Secondary Key)
- `ride_ids`: `uuid[]`
- `status`: `character varying(255)`
- `merchant_id`: `character varying(36)`
- `merchant_operating_city_id`: `character varying(36)`
- `driver_id`: `character varying(36)`
- `vehicle_number`: `character varying(255)`
- `vehicle_model`: `character varying(255)`
- `vehicle_variant`: `character varying(60)`
- `vehicle_service_tier_type`: `character varying(255)`
- `waypoints`: `jsonb`
- `tracking_url`: `character varying(255)` (via beam transformation)
- `total_fare`: `numeric(30,10)`
- `chargeable_distance`: `double precision`
- `traveled_distance`: `double precision`
- `ride_start_time`: `timestamptz`
- `ride_end_time`: `timestamptz`
- `trip_category`: `character varying(255)`
- `number_of_snap_to_road_calls`: `integer`
- `number_of_osrm_snap_to_road_calls`: `integer`
- `number_of_self_tuned`: `integer`
- `number_of_deviation`: `boolean`
- `estimated_toll_charges`: `numeric(30,10)`
- `estimated_toll_names`: `text[]`
- `toll_charges`: `numeric(30,10)`
- `toll_names`: `text[]`
- `ride_ended_by`: `character varying(255)`
- `passed_through_destination`: `boolean`
- `is_pickup_or_destination_edited`: `boolean`
- `created_at`: `timestamptz`
- `updated_at`: `timestamptz`

**Beam Transformations:**
- `trackingUrl`: `Maybe BaseUrl` ↔ `Maybe Text` (showBaseUrl/parseBaseUrl)

**Queries:**
- `findByTransactionId(transactionId)`
- `findBySharedBookingId(sharedBookingId)`
- `updateStatus(status, updatedAt)` WHERE `id`
- `updateRideStartTime(rideStartTime, updatedAt)` WHERE `id`
- `updateRideEndTime(rideEndTime, updatedAt)` WHERE `id`
- `updateTotalFare(totalFare, updatedAt)` WHERE `id`
- `updateTollCharges(tollCharges, tollNames, updatedAt)` WHERE `id`
- `findByDriverId(driverId)`
- `findByStatus(status)`
- `findActiveRides()` WHERE `driverId AND status`

### YAML Reference Paths (OLD)

**Rider App YAML Files:** (All REMOVED)
- `Backend/app/rider-platform/rider-app/Main/spec/Storage/SharedSearchRequest.yaml`
- `Backend/app/rider-platform/rider-app/Main/spec/Storage/SharedEstimate.yaml`
- `Backend/app/rider-platform/rider-app/Main/spec/Storage/SharedBooking.yaml`
- `Backend/app/rider-platform/rider-app/Main/spec/Storage/SharedRide.yaml`

**Driver App YAML Files:** (All REMOVED)
- `Backend/app/provider-platform/dynamic-offer-driver-app/Main/spec/Storage/SharedSearchRequest.yaml`
- `Backend/app/provider-platform/dynamic-offer-driver-app/Main/spec/Storage/SharedEstimate.yaml`
- `Backend/app/provider-platform/dynamic-offer-driver-app/Main/spec/Storage/SharedBooking.yaml`
- `Backend/app/provider-platform/dynamic-offer-driver-app/Main/spec/Storage/SharedRide.yaml`

### Type Definitions (OLD)

```yaml
SharedSearchRequestStatus:
  enum: "POOLING, MATCHED, EXPIRED, CANCELLED"
  derive: "HttpInstance"

SearchRequestStatus:
  enum: "NEW, INPROGRESS, CONFIRMED, COMPLETED, CLOSED"
  derive: "HttpInstance"

EstimateStatus:
  enum: "NEW, DRIVER_QUOTE_REQUESTED, CANCELLED, GOT_DRIVER_QUOTE, DRIVER_QUOTE_CANCELLED, COMPLETED"
  derive: "HttpInstance"

BookingStatus:
  # Rider App
  enum: "NEW, CONFIRMED, AWAITING_REASSIGNMENT, REALLOCATED, COMPLETED, CANCELLED, TRIP_ASSIGNED"
  derive: "HttpInstance"
  
  # Driver App (different enum values)
  enum: "NEW, TRIP_ASSIGNED, COMPLETED, CANCELLED, REALLOCATED"
  derive: "HttpInstance"

SharedRideStatus:
  enum: "UPCOMING, NEW, INPROGRESS, COMPLETED, CANCELLED"
  derive: "HttpInstance"
```

---

**Last Updated:** 2025-01-30  
**Design Status:** Optimized Consolidated Approach  
**Compatibility:** Single SharedEntity table for both apps