# Chunk 2 Implementation Plan: Shared Ride Selection and Customer Pooling

## Overview
This document outlines the implementation plan for Chunk 2 of the shared ride feature, covering the customer journey from shared ride estimate selection through the pooling mechanism and seat allocation validation.

## Flow Summary
Based on the sharedRideFlowCore.md analysis, Chunk 2 implements the **"Pre-Pooling Validation and Rider Pooling Logic"** sequence:
1. **Seat Selection & Validation** - Ensure numSeats < vehicle capacity - 1
2. **Estimate Status Validation** - Verify estimate is not cancelled  
3. **Sync vs Async Flow Decision** - Route to appropriate pooling mechanism
4. **GSI Management** - Add customer to waiting pool for future matching
5. **Pooling Result Handling** - Process sync/async outcomes

**Note**: The actual **Rider Pooling Logic implementation** is deferred to **Chunks 5-7**, and **Async Cron Pooling** to **Chunk 9**.

## 🔴 IMPORTANT CLARIFICATIONS

### Estimate Flow Understanding (REVISED)
- **`/rideSearch/:searchId/results`** is a **GET API** that returns `GetQuotesRes` with estimates
- **Estimates come from Driver App** via BPP network and are stored in rider-app database
- **PRIMARY filtering happens in OnSearch.hs** before estimates are stored in database (following existing pattern)
- **SECONDARY validation happens in Select.hs** for user-specific prerequisites (KYC, etc.)
- **UX Strategy**: Show shared ride estimates to eligible customers, handle KYC at selection time for better conversion

### Domain Structure (from Chunk 1)
- **TripCategory**: `RideShare` identifies shared ride estimates
- **VehicleVariant**: `Auto Share`, `Cab Share`, `Cab Premium Share`, etc.
- **No additional flags needed**: Identification via `tripCategory == RideShare`

## ✅ Chunk 2 Implementation Summary

### What We've Implemented:
1. **Enhanced DSelectReq** - Added `numSeats` and `sharedEntityId` fields for shared ride selection
2. **Shared Ride Validation** - KYC verification and seat validation in select2 function
3. **Smart Sync/Async Flow Routing** - Config-based decision making with conditional GSI management
4. **Redis GSI Management** - Customer waiting pool with config-based TTL and estimateId tracking
5. **Progressive Disclosure UX** - Show estimates to eligible customers, handle prerequisites at selection
6. **Sync Pooling Handler Interface** - `handleSyncRiderPooling` function ready for Chunk 4 implementation
7. **Enhanced DSelectRes** - Added SharedEntity context for driver-app via Beckn protocol
8. **Smart Resource Management** - Only add to GSI when no immediate match found

### Key Design Decisions:
- **Leverage Chunk 1 filtering** - Use OnSearch.hs filtering instead of duplicating in Quote.hs
- **Progressive disclosure UX** - Show shared ride estimates, handle KYC at selection time for better conversion
- **Config-driven expiry** - Use `searchRequestExpirySeconds - searchExpiryBufferSeconds` for TTL
- **EstimateId tracking** - Use estimateId instead of searchRequestId in GSI for better tracking
- **Graceful degradation** - Clear error messages guide users through prerequisites
- **Smart GSI usage** - Only use waiting pool when sync pooling finds no immediate match
- **Beckn integration ready** - SharedEntity ID and numSeats available for driver-app communication

## API Mapping and Implementation Details

### 1. Leverage Chunk 1 Infrastructure (Already Implemented)
- **Status**: ✅ Chunk 1 Completed
- **Available Functions**:
  - Route caching in Search.hs
  - Geospatial indexing for hotspots 
  - `checkNearbyRiders` function in OnSearch.hs
  - `filterSharedRideEstimates` function in OnSearch.hs
- **Result**: Shared ride estimates are already filtered at OnSearch level based on hotspot eligibility and configuration

### 2. Shared Ride Estimate Selection with KYC and Seat Validation
- **Status**: 🔧 Requires Enhancement of Existing API
- **API**: Existing `/estimates/:estimateId/select2` endpoint
- **Location**: `Backend/app/rider-platform/rider-app/Main/src/Domain/Action/UI/Select.hs:120-132`
- **Implementation**:
  ```haskell
  -- Update existing DSelectReq to include shared ride fields
  data DSelectReq = DSelectReq
    { customerExtraFee :: Maybe Money,
      customerExtraFeeWithCurrency :: Maybe PriceAPIEntity,
      autoAssignEnabled :: Bool,
      autoAssignEnabledV2 :: Maybe Bool,
      isPetRide :: Maybe Bool,
      paymentMethodId :: Maybe Payment.PaymentMethodId,
      otherSelectedEstimates :: Maybe [Id DEstimate.Estimate],
      isAdvancedBookingEnabled :: Maybe Bool,
      deliveryDetails :: Maybe DTDD.DeliveryDetails,
      disabilityDisable :: Maybe Bool,
      preferSafetyPlus :: Maybe Bool,
      numSeats :: Maybe Int,  -- New field for shared ride seat selection
      sharedEntityId :: Maybe (Id SharedEntity)  -- Created during pooling (SEARCH_GROUP type)
    }
    deriving stock (Generic, Show)
    deriving anyclass (ToJSON, FromJSON, ToSchema)

  -- Update validation to include numSeats
  validateDSelectReq :: Validate DSelectReq
  validateDSelectReq DSelectReq {..} =
    sequenceA_
      [ validateField "customerExtraFee" customerExtraFee $ InMaybe $ InRange @Money 1 100000,
        -- ... existing validations
        validateField "numSeats" numSeats $ InMaybe $ InRange @Int 1 6  -- Max 6 seats for largest vehicles
      ]
  ```

### 3. Shared Ride Selection Validation and Smart Pooling in select2 Function
- **Status**: ✅ Implemented 
- **Location**: `Backend/app/rider-platform/rider-app/Main/src/Domain/Action/UI/Select.hs:277-314`
- **Implementation**: Added shared ride validation, sync/async flow routing, and smart pooling logic in `select2` function
  ```haskell
  -- Handle shared ride specific logic and capture result (implemented)
  (mbSharedEntityId, validatedNumSeats) <- case estimate.tripCategory of
    Just RideShare -> do
      -- Ensure numSeats is provided for shared rides (required field for shared rides)
      validatedSeats <- numSeats & fromMaybeM (InvalidRequest "Number of seats required for shared ride selection")
      
      -- Check KYC status (user-specific prerequisite)
      unless (fromMaybe False person.kycVerified) $ 
        throwError (InvalidRequest "Please complete KYC verification to book shared rides")
      
      -- Get shared ride config to determine sync vs async flow
      sharedRideConfig <- CQSharedRideConfig.findByMerchantOperatingCityIdAndVehicleCategory 
                            searchRequest.merchantOperatingCityId 
                            searchRequest.vehicleCategory
      
      case sharedRideConfig of
        Just config -> do
          if config.enableSyncPooling
            then do
              -- Sync pooling: Call rider pooling logic directly
              logInfo $ "Attempting sync pooling for searchRequest: " <> searchRequest.id.getId
              poolingResult <- handleSyncRiderPooling config searchRequest estimate validatedSeats
              case poolingResult of
                Nothing -> do
                  -- No immediate match found, add to GSI for future matching
                  logInfo $ "No immediate pool match, adding to waiting pool: " <> searchRequest.id.getId
                  addToSharedRideGSI config searchRequest estimate validatedSeats
                  return (Nothing, Just validatedSeats)
                Just sharedEntityId -> do
                  -- Match found, proceed with the shared entity
                  logInfo $ "Pool match found, using shared entity: " <> sharedEntityId.getId
                  return (Just sharedEntityId, Just validatedSeats)
            else do
              -- Async pooling: Just add to Redis GSI for cron processing
              logInfo $ "Adding to async pooling queue for searchRequest: " <> searchRequest.id.getId
              addToSharedRideGSI config searchRequest estimate validatedSeats
              return (Nothing, Just validatedSeats)
        Nothing -> throwError (InvalidRequest "Shared ride configuration not found for this location/vehicle")
    _ -> return (Nothing, numSeats) -- Non-shared ride
  ```

### 4. Pooling Interface and Helper Functions
- **Status**: 🔧 Requires Interface Definition (Logic in Chunks 5-7)
- **Location**: New module `Backend/app/rider-platform/rider-app/Main/src/Domain/Action/UI/SharedRide/Pooling.hs`
- **Implementation**:
  ```haskell
  module Domain.Action.UI.SharedRide.Pooling where

  import qualified Storage.CachedQueries.SharedRideConfig as QSharedRideConfig

  -- Result type for pooling operations
  data PoolingResult 
    = PoolingSuccess (Id SharedEntity)  -- Single unified SharedEntity ID
    | PoolingNoMatch
    | PoolingError Text
    deriving (Show, Eq)

  -- Main interface function (implementation in Chunk 4)
  invokeRiderPoolingLogic :: 
    (MonadFlow m, CacheFlow m r, EsqDBFlow m r, EsqDBReplicaFlow m r) => 
    SSR.SearchRequest -> 
    Int -> 
    m PoolingResult
  invokeRiderPoolingLogic searchRequest numSeats = do
    -- TODO: Implement in Chunks 5-7
    -- This will contain the full filtering cascade from sharedRideFlowCore.md:
    -- 1. Initial GSI query for nearby riders
    -- 2. Filter 1: Locked Searches
    -- 3. Filter 2: Expiry Time  
    -- 4. Filter 3: Seat Availability
    -- 5. Filter 4: Destination Proximity
    -- 6. Filter 5: Pickup Proximity
    -- 7. Filter 6: Drop-off Proximity (Advanced)
    -- 8. Filter 7: Route Overlap Analysis (Geo-hashing)
    -- 9. Final Grouping and Lock Acquisition
    -- 10. Vehicle Capacity Check
    
    -- For now, return no match to allow testing of other components
    return PoolingNoMatch

  -- Helper function for queue management (used by async flow)
  addToPoolingQueue :: SSR.SearchRequest -> Int -> Flow ()
  addToPoolingQueue searchRequest requestedSeats = do
    now <- getCurrentTime
    let queueKey = "shared_ride_pooling_queue"
    let memberData = PoolingQueueEntry
          { searchRequestId = searchRequest.id
          , requestedSeats = requestedSeats
          , merchantOperatingCityId = searchRequest.merchantOperatingCityId
          , vehicleCategory = searchRequest.vehicleCategory
          , queuedAt = now
          }
    Redis.lpush queueKey [encode memberData]

  -- Data type for pooling queue entries (used by Chunk 9 cron)
  data PoolingQueueEntry = PoolingQueueEntry
    { searchRequestId :: Id SSR.SearchRequest
    , requestedSeats :: Int
    , merchantOperatingCityId :: Id MerchantOperatingCity
    , vehicleCategory :: VehicleCategory
    , queuedAt :: UTCTime
    } deriving (Generic, Show, ToJSON, FromJSON)
  ```

### 5. Geospatial Index Management
- **Status**: ✅ Implemented
- **Location**: `Backend/app/rider-platform/rider-app/Main/src/Domain/Action/UI/Select.hs:550-574`
- **Implementation**: Added `addToSharedRideGSI` function with config-based TTL and estimateId tracking
  ```haskell
  -- Add customer to shared ride GSI for pooling (implemented)
  addToSharedRideGSI :: 
    (MonadFlow m, CacheFlow m r) => 
    SharedRideConfigs -> 
    DSearchReq.SearchRequest -> 
    DEstimate.Estimate -> 
    Int -> 
    m ()
  addToSharedRideGSI config searchRequest estimate numSeats = do
    let gsiKey = "ShareRideCustomerLoc"  -- GSI key from sharedRideFlowCore.md
    now <- getCurrentTime
    
    -- Calculate expiry time: config time - buffer time
    let expirySeconds = config.searchRequestExpirySeconds.getSeconds - config.searchExpiryBufferSeconds.getSeconds
    let validTill = addUTCTime (fromIntegral expirySeconds) now
    
    -- Use estimateId instead of searchRequestId for better tracking
    let memberKey = estimate.id.getId <> ":" <> show validTill <> ":" <> show numSeats
    let lat = searchRequest.fromLocation.lat  
    let lon = searchRequest.fromLocation.lon
    
    -- Add to GSI with format: estimateId:validTill:numSeats (updated format)
    Redis.geoAdd gsiKey [(lon, lat, memberKey)]
    
    -- Set TTL for auto-cleanup
    Redis.expireAt memberKey (round $ utcTimeToPOSIXSeconds validTill)
  ```

### 6. Sync Rider Pooling Handler
- **Status**: ✅ Interface Implemented (Logic for Chunks 5-7)
- **Location**: `Backend/app/rider-platform/rider-app/Main/src/Domain/Action/UI/Select.hs:584-602`
- **Implementation**: Smart pooling handler with conditional GSI management
  ```haskell
  -- Sync rider pooling handler (implemented interface, logic for Chunks 5-7)
  handleSyncRiderPooling :: 
    (MonadFlow m, CacheFlow m r, EsqDBFlow m r, EsqDBReplicaFlow m r) => 
    SharedRideConfigs -> 
    DSearchReq.SearchRequest -> 
    DEstimate.Estimate -> 
    Int -> 
    m (Maybe (Id DSE.SharedEntity))
  handleSyncRiderPooling config searchRequest estimate numSeats = do
    -- TODO: Implement in Chunks 5-7
    -- This will contain the full rider pooling logic:
    -- 1. Search for compatible riders in GSI
    -- 2. Apply filtering cascade (proximity, route overlap, etc.)
    -- 3. Create SharedEntity if match found
    -- 4. Return SharedEntity ID or Nothing
    
    -- For now, always return Nothing (no match found)
    logInfo $ "handleSyncRiderPooling called for estimate: " <> estimate.id.getId <> " with " <> show numSeats <> " seats"
    return Nothing
  ```

### 7. Enhanced DSelectRes for Driver-App Communication
- **Status**: ✅ Implemented
- **Location**: `Backend/app/rider-platform/rider-app/Main/src/Domain/Action/UI/Select.hs:166-191`
- **Implementation**: Added SharedEntity context for Beckn protocol
  ```haskell
  data DSelectRes = DSelectRes
    { -- ... existing fields
      sharedEntityId :: Maybe (Id DSE.SharedEntity),  -- New: SharedEntity ID for driver-app
      numSeats :: Maybe Int                           -- New: Seat selection for shared rides
    }
  
  -- DSelectRes construction includes shared ride context (lines 383-384)
  DSelectRes {
    sharedEntityId = mbSharedEntityId,     -- From pooling result
    numSeats = validatedNumSeats,          -- From customer selection
    -- ... other fields
  }
  ```

## Database Schema Updates

### SearchRequest Table (Link to Shared Request)
```sql
-- Add foreign key to link individual search requests to shared request
ALTER TABLE atlas_app.search_request 
ADD COLUMN shared_search_request_id UUID REFERENCES atlas_app.shared_search_request(id);

CREATE INDEX idx_search_request_shared_search_request_id 
ON atlas_app.search_request(shared_search_request_id);
```

### Person Table (KYC Field)
```sql
-- Add KYC verification field if not already added in Chunk 1
ALTER TABLE atlas_app.person 
ADD COLUMN kyc_verified BOOLEAN DEFAULT FALSE;
```

## API Response Updates

### Enhanced SelectListRes
```haskell
-- Add to existing SelectListRes type
data SelectListRes = SelectListRes
  { -- ... existing fields
    waitingForMoreCustomers :: Maybe Bool  -- New field for shared ride waiting status
  }
```

### Enhanced Existing API Endpoints
```haskell
-- Existing select2 endpoint now handles shared rides
POST /estimates/:estimateId/select2
Body: {
  -- ... existing fields
  "numSeats": Int?,  -- New field for shared ride seat selection
  "sharedSearchRequestId": UUID?,  -- Populated by pooling logic  
  "sharedEstimateId": UUID?  -- Populated by pooling logic
}

-- Future API endpoints (to be implemented in later chunks)
-- GET /sharedSearchRequests/:sharedSearchRequestId/status (Chunk 4)
-- GET /shared-rides/pooling-status/:searchId (Chunk 9 - async status)
```

## Configuration Requirements

### SharedRideConfig Fields (Updated YAML)
```yaml
# Core feature flags
enableSharedRide: Bool                              # Master feature flag
enableSyncPooling: Bool                             # True=sync flow, False=async flow

# Thresholds and search parameters  
pickupLocationSearchRadius: Meters                  # Radius for nearby customer search
searchThresholdForSharedEstimate: Int              # Min customers needed to show shared estimates  
dropLocationSearchRadius: Meters                   # Max distance between drop locations

# Advanced filtering (used by Chunk 4 pooling logic)
routeOverlapThreshold: Double                      # Min route overlap (0.7 = 70%)
geoHashPrecisionForRouteMatching: Int              # Geohash precision for route comparison
actualPickupDistanceThreshold: Meters             # Max pickup travel distance  
actualDropDistanceThreshold: Meters               # Max drop travel distance

# Flow control
searchRequestExpirySeconds: Seconds               # Search request validity
searchExpiryBufferSeconds: Seconds                # Buffer for pooling completion
customerRemainingThresholdForFlowContinuation: Int # Min customers to continue flow
```

## Redis Data Structures

### Shared Ride Customer Location GSI (per sharedRideFlowCore.md)
```
Key: "ShareRideCustomerLoc"
Type: Geospatial Index  
Members: "{searchId}:{validTill}:{numSeats}"
Coordinates: pickup location lat/lon
TTL: 5 minutes (as per core logic)
Purpose: Used by both sync and async pooling flows
```

### Async Pooling Queue (for Chunk 9)
```
Key: "shared_ride_pooling_queue"
Type: List
Values: JSON encoded PoolingQueueEntry objects
Purpose: Queue for cron-based async pooling
```

### Route Cache (from Chunk 1)
```
Key: "route_cache:{searchId}"
Value: JSON encoded RouteResponse  
TTL: 30 minutes
Purpose: Used by route overlap analysis in pooling logic
```

## Error Handling

### New Error Types
```haskell
data SharedRideError 
  = KYCNotCompleted
  | ExceedsMaxSeats Int Int           -- requested, max allowed
  | EstimateCancelled
  | EstimateNotSharedRide
  | NoCompatibleCustomers
  | SharedRideNotEnabled
  | InsufficientNearbyRiders Int Int  -- found, required
```

## Testing Strategy

### Unit Tests
1. **Seat validation** - Test boundary conditions for different vehicle types
2. **Estimate filtering** - Test shared ride estimate filtering based on conditions
3. **Pooling algorithm** - Test customer matching with various scenarios
4. **Route compatibility** - Test overlap calculation logic

### Integration Tests  
1. **End-to-end estimate flow** - From search to filtered estimates display
2. **Shared ride selection** - From selection to pooling to booking
3. **Multi-customer scenarios** - Test with 2-4 customers in different combinations
4. **GSI operations** - Test Redis geospatial index operations

### Load Tests
1. **Estimate filtering performance** - Test with large numbers of estimates
2. **Redis GSI scalability** - Test with many concurrent customers
3. **Pooling algorithm performance** - Test with many nearby customers

## Implementation Priority

1. **Phase 1**: Estimate filtering in getQuotes (most critical - from Chunk 1)
2. **Phase 2**: DSelectReq enhancement and seat validation 
3. **Phase 3**: Sync/async flow routing and GSI management
4. **Phase 4**: Interface definitions for pooling (actual logic in Chunk 4)

## Cross-Chunk Dependencies

- **Depends on Chunk 1**: Route caching, estimate filtering, GSI setup
- **Provides to Chunks 5-7**: Pooling interfaces, validated requests, GSI data structure
- **Provides to Chunk 9**: Async queue management, pooling queue data types

## Rollback Plan

- **Feature flag**: `enableSharedRide` in SharedRideConfig for instant disable
- **Estimate filtering**: Remove filtering to show all estimates
- **Graceful fallback**: Individual bookings if pooling fails
- **Redis cleanup**: Background job to clean stale GSI entries

## Future Work & Pending Tasks

### For Chunks 5-7 (Rider Pooling Logic Implementation):
1. **Complete `handleSyncRiderPooling` function**:
   - Implement GSI queries for compatible riders
   - Apply filtering cascade (proximity, route overlap, etc.)
   - Create SharedEntity when matches found
   - Handle complex pooling algorithms

### For Beckn Protocol Integration (Later Chunks):
1. **Modify Beckn select request** to include shared ride parameters:
   - Add `sharedEntityId` to select request payload
   - Add `numSeats` to select request payload
   - Update ACL layer to handle shared ride context
   - Ensure driver-app receives SharedEntity information

2. **Update rider-app to driver-app communication**:
   - Modify select ACL to include SharedEntity fields
   - Update driver-app to handle shared ride context
   - Ensure proper SharedEntity ID propagation

### For Chunk 9 (Async Pooling):
1. **Cron job implementation** for async pooling processing
2. **Notification system** for successful matches
3. **Queue management** for waiting customers

## Success Metrics

- **Estimate filtering accuracy**: Correct shared ride estimates shown based on eligibility
- **Pooling success rate**: Percentage of shared selections that find matches  
- **Customer wait time**: Time from selection to successful match or timeout
- **System performance**: Response times for estimate retrieval and selection
- **SharedEntity creation rate**: Successful pooling matches per hour
- **GSI efficiency**: Redis performance with concurrent shared ride requests

## Dependencies

- **Chunk 1**: Route caching, GSI setup, and configuration infrastructure ✅
- **SharedRideConfig**: Updated YAML with `enableSharedRide` and `enableSyncPooling` fields ✅
- **Person.kycVerified**: KYC status field in person table ✅
- **Redis Geospatial**: Redis version supporting geo commands ✅
- **SharedEntity**: Unified table design with proper status tracking ✅