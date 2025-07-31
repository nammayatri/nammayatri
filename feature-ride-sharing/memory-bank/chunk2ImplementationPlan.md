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

**Note**: The actual **Rider Pooling Logic implementation** is deferred to **Chunk 4**, and **Async Cron Pooling** to **Chunk 9**.

## 🔴 IMPORTANT CLARIFICATIONS

### Estimate Flow Understanding
- **`/rideSearch/:searchId/results`** is a **GET API** that returns `GetQuotesRes` with estimates
- **Estimates come from Driver App** via BPP network and are stored in rider-app database
- **Filtering happens in rider-app** at `getEstimates` function level in `Domain.Action.UI.Quote.hs:284`
- **No need to inform driver app** - filtering is done when serving estimates to frontend

### Domain Structure (from Chunk 1)
- **TripCategory**: `RideShare` identifies shared ride estimates
- **VehicleVariant**: `Auto Share`, `Cab Share`, `Cab Premium Share`, etc.
- **No additional flags needed**: Identification via `tripCategory == RideShare`

## API Mapping and Implementation Details

### 1. Shared Ride Estimate Filtering in GetQuotes
- **Status**: 🔧 Requires Implementation
- **API**: `/rideSearch/:searchId/results` (existing GET endpoint)
- **Location**: `Backend/app/rider-platform/rider-app/Main/src/Domain/Action/UI/Quote.hs:284-288`
- **Implementation**:
  ```haskell
  -- Update getEstimates function to filter shared ride estimates based on eligibility
  getEstimates :: (CacheFlow m r, EsqDBFlow m r, EsqDBReplicaFlow m r) => Id SSR.SearchRequest -> Bool -> m [UEstimate.EstimateAPIEntity]
  getEstimates searchRequestId isReferredRide = do
    estimateList <- runInReplica $ QEstimate.findAllBySRId searchRequestId
    estimates <- mapM (UEstimate.mkEstimateAPIEntity isReferredRide) (sortByEstimatedFare estimateList)
    
    -- Filter shared ride estimates based on eligibility
    filteredEstimates <- filterSharedRideEstimates searchRequestId estimates
    
    return . sortBy (compare `on` (.createdAt)) $ filteredEstimates

  filterSharedRideEstimates :: 
    (CacheFlow m r, EsqDBFlow m r, EsqDBReplicaFlow m r) => 
    Id SSR.SearchRequest -> 
    [UEstimate.EstimateAPIEntity] -> 
    m [UEstimate.EstimateAPIEntity]
  filterSharedRideEstimates searchRequestId estimates = do
    searchRequest <- runInReplica $ QSR.findById searchRequestId >>= fromMaybeM (SearchRequestDoesNotExist searchRequestId.getId)
    
    -- Check if shared ride is enabled for this merchant/city/vehicle category
    sharedRideConfig <- QSharedRideConfig.findByMerchantOperatingCityIdAndVehicleCategory 
                          searchRequest.merchantOperatingCityId 
                          searchRequest.vehicleCategory
    
    case sharedRideConfig of
      Just config | config.enableSharedRide -> do
        -- Check if customer meets minimum threshold for shared rides
        (hotspotEligible, nearbyCount) <- checkNearbyRiders searchRequest.fromLocation.gps 
                                            searchRequest.merchantOperatingCityId
                                            searchRequest.vehicleCategory
        
        -- Check KYC status
        person <- QP.findById searchRequest.riderId >>= fromMaybeM (PersonNotFound searchRequest.riderId.getId)
        let kycVerified = fromMaybe False person.kycVerified
        
        if hotspotEligible && kycVerified
          then return estimates  -- Keep all estimates including shared ride
          else return $ filter (\est -> est.tripCategory /= Just RideShare) estimates
      _ -> 
        -- Shared ride not enabled or no config, filter out shared ride estimates
        return $ filter (\est -> est.tripCategory /= Just RideShare) estimates

  checkNearbyRiders :: 
    (MonadFlow m, CacheFlow m r) => 
    LatLong -> 
    Id MerchantOperatingCity -> 
    VehicleCategory -> 
    m (Bool, Int)
  checkNearbyRiders sourceLocation merchantOperatingCityId vehicleCategory = do
    sharedRideConfig <- QSharedRideConfig.findByMerchantOperatingCityIdAndVehicleCategory 
                          merchantOperatingCityId vehicleCategory
    case sharedRideConfig of
      Just config -> do
        let gsiKey = "searchHotSpots"
        nearbySearches <- Redis.geoRadius gsiKey sourceLocation.lon sourceLocation.lat 
                           (fromIntegral config.pickupLocationSearchRadius.getMeters) "m"
        let count = length nearbySearches
        return (count >= config.searchThresholdForSharedEstimate, count)
      Nothing -> return (False, 0)
  ```

### 2. Shared Ride Estimate Selection with Seat Validation
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
      sharedSearchRequestId :: Maybe (Id SharedSearchRequest),  -- Created during pooling
      sharedEstimateId :: Maybe (Id SharedEstimate)  -- Created during pooling
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

### 3. Shared Ride Flow Detection and Pre-Processing
- **Status**: 🔧 Requires Pre-Processing Before select2 Function  
- **Location**: Add interceptor before `select2` in `Backend/app/rider-platform/rider-app/Main/src/API/UI/Select.hs:108`
- **Implementation**:
  ```haskell
  -- Update existing select2' function to handle shared ride pre-processing
  select2' :: DSelect.SelectFlow m r c => (Id DPerson.Person, Id Merchant.Merchant) -> Id DEstimate.Estimate -> DSelect.DSelectReq -> m DSelect.MultimodalSelectRes
  select2' (personId, merchantId) estimateId req = withPersonIdLogTag personId $ do
    estimate <- QEstimate.findById estimateId >>= fromMaybeM (EstimateNotFound estimateId.getId)
    
    -- Check if this is a shared ride estimate
    case estimate.tripCategory of
      Just RideShare -> do
        -- Handle shared ride flow with sync/async decision
        processedReq <- handleSharedRidePreProcessing personId estimateId req estimate
        dSelectReq <- DSelect.select2 personId estimateId processedReq
        return $ DSelect.buildMultimodalSelectRes dSelectReq
      _ -> do
        -- Regular flow - call select2 as usual
        dSelectReq <- DSelect.select2 personId estimateId req
        return $ DSelect.buildMultimodalSelectRes dSelectReq

  -- Shared ride pre-processing function
  handleSharedRidePreProcessing :: 
    (MonadFlow m, CacheFlow m r, EsqDBFlow m r, EsqDBReplicaFlow m r) =>
    Id DPerson.Person -> 
    Id DEstimate.Estimate -> 
    DSelectReq -> 
    DEstimate.Estimate -> 
    m DSelectReq
  handleSharedRidePreProcessing personId estimateId req estimate = do
    -- Validate numSeats is provided for shared rides
    requestedSeats <- case req.numSeats of
      Just seats -> return seats
      Nothing -> throwError $ InvalidRequest "Number of seats is required for shared ride selection"
    
    -- Validate seat count and estimate status
    validateSeatCount estimate requestedSeats
    validateEstimateStatus estimate
    
    -- Get shared ride config to determine sync vs async flow
    searchRequest <- QSR.findById estimate.requestId >>= fromMaybeM (SearchRequestNotFound estimate.requestId.getId)
    sharedRideConfig <- QSharedRideConfig.findByMerchantOperatingCityIdAndVehicleCategory 
                          searchRequest.merchantOperatingCityId 
                          searchRequest.vehicleCategory
    
    case sharedRideConfig of
      Just config -> do
        if config.enableSyncPooling  -- New config field
          then handleSyncPooling searchRequest requestedSeats req
          else handleAsyncPooling searchRequest requestedSeats req
      Nothing -> throwError $ InvalidRequest "Shared ride not configured for this city/vehicle"

  -- Sync pooling flow - invoke immediate pooling (implemented in Chunk 4)
  handleSyncPooling :: 
    SSR.SearchRequest -> 
    Int -> 
    DSelectReq -> 
    Flow DSelectReq
  handleSyncPooling searchRequest requestedSeats req = do
    -- Add to GSI for immediate pooling attempt
    addToSharedRideGSI searchRequest []
    
    -- Call Rider Pooling Logic (to be implemented in Chunk 4)
    poolingResult <- invokeRiderPoolingLogic searchRequest requestedSeats
    
    case poolingResult of
      PoolingSuccess (sharedSearchRequestId, sharedEstimateId) -> do
        -- Successful match found, continue with regular select2 flow
        return $ req { sharedSearchRequestId = Just sharedSearchRequestId
                     , sharedEstimateId = Just sharedEstimateId }
      PoolingNoMatch -> do
        -- No immediate match, customer added to waiting pool
        throwError $ InvalidRequest "No compatible riders found. You will be matched shortly."
      PoolingError errorMsg -> 
        throwError $ InvalidRequest errorMsg

  -- Async pooling flow - add to queue for cron processing (Chunk 9)
  handleAsyncPooling :: 
    SSR.SearchRequest -> 
    Int -> 
    DSelectReq -> 
    Flow DSelectReq
  handleAsyncPooling searchRequest requestedSeats req = do
    -- Add to GSI and pooling queue for cron to process
    addToSharedRideGSI searchRequest []
    addToPoolingQueue searchRequest requestedSeats
    
    -- Return success indication - customer will be notified via push notification
    throwError $ InvalidRequest "Your shared ride request has been queued. You will be notified when a match is found."

  -- Helper validation functions
  validateSeatCount :: DEstimate.Estimate -> Int -> Flow ()
  validateSeatCount estimate numSeats = do
    let vehicleCapacity = getVehicleCapacity estimate.vehicleVariant
    let maxAllowedSeats = vehicleCapacity - 1  -- Reserve 1 seat for driver
    when (numSeats >= maxAllowedSeats) $
      throwError $ InvalidRequest $ 
        "Seats requested (" <> show numSeats <> ") exceeds maximum allowed (" 
        <> show maxAllowedSeats <> ") for " <> show estimate.vehicleVariant

  getVehicleCapacity :: VehicleVariant -> Int
  getVehicleCapacity = \case
    AUTO_RICKSHAW -> 3    -- For Auto Share
    HATCHBACK -> 4        -- For Cab Share  
    SEDAN -> 4            -- For Cab Share
    SUV -> 6              -- For larger shared vehicles
    _ -> 4                -- Default fallback

  validateEstimateStatus :: DEstimate.Estimate -> Flow ()
  validateEstimateStatus estimate = do
    when (estimate.status == DEstimate.CANCELLED) $
      throwError $ InvalidRequest "Cannot select a cancelled estimate"
    
    when (estimate.status `notElem` [DEstimate.NEW, DEstimate.GOT_DRIVER_QUOTE]) $
      throwError $ InvalidRequest "Estimate is not available for selection"
  ```

### 4. Pooling Interface and Helper Functions
- **Status**: 🔧 Requires Interface Definition (Logic in Chunk 4)
- **Location**: New module `Backend/app/rider-platform/rider-app/Main/src/Domain/Action/UI/SharedRide/Pooling.hs`
- **Implementation**:
  ```haskell
  module Domain.Action.UI.SharedRide.Pooling where

  import qualified Storage.CachedQueries.SharedRideConfig as QSharedRideConfig

  -- Result type for pooling operations
  data PoolingResult 
    = PoolingSuccess (Id SharedSearchRequest, Id SharedEstimate)
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
    -- TODO: Implement in Chunk 4
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
- **Status**: 🔧 Requires Implementation
- **Location**: Update `Backend/app/rider-platform/rider-app/Main/src/Domain/Action/UI/Select.hs`
- **Implementation**:
  ```haskell
  -- Add customer to waiting pool in GSI (per sharedRideFlowCore.md Chunk 2 Step 4)
  addToSharedRideGSI :: 
    SSR.SearchRequest -> 
    [Id SSR.SearchRequest] -> 
    Flow ()
  addToSharedRideGSI searchRequest _pairableCustomers = do
    let gsiKey = "ShareRideCustomerLoc"  -- Key from sharedRideFlowCore.md
    now <- getCurrentTime
    let validTill = addUTCTime (5 * 60) now -- 5 minutes TTL as per core logic
    let numSeats = 1  -- Default, will be enhanced to use actual numSeats
    let memberKey = searchRequest.id.getId <> ":" <> show validTill <> ":" <> show numSeats
    let lat = searchRequest.fromLocation.lat  
    let lon = searchRequest.fromLocation.lon
    
    -- Add to GSI with format: searchId:validTill:numSeats (per core logic)
    Redis.geoAdd gsiKey [(lon, lat, memberKey)]
    
    -- Set TTL for auto-cleanup
    Redis.expireAt memberKey (round $ utcTimeToPOSIXSeconds validTill)

  -- Placeholder functions for shared entity creation (implemented by Chunk 4)
  createSharedSearchRequest :: 
    SSR.SearchRequest -> 
    [Id SSR.SearchRequest] -> 
    Int -> 
    Flow (Id SharedSearchRequest)
  createSharedSearchRequest primarySearch otherSearchIds numSeats = do
    -- TODO: Implement in Chunk 4 when actual pooling logic is ready
    -- This will create SharedSearchRequest with proper batch grouping
    error "createSharedSearchRequest: Implementation deferred to Chunk 4"

  createSharedEstimate :: 
    Id SharedSearchRequest -> 
    SSR.SearchRequest -> 
    [Id SSR.SearchRequest] -> 
    Flow (Id SharedEstimate)
  createSharedEstimate sharedSearchRequestId primarySearch otherSearchIds = do
    -- TODO: Implement in Chunk 4 when cumulative estimates are ready
    -- This will create SharedEstimate with combined fare calculations
    error "createSharedEstimate: Implementation deferred to Chunk 4"
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
- **Provides to Chunk 4**: Pooling interfaces, validated requests, GSI data structure
- **Provides to Chunk 9**: Async queue management, pooling queue data types

## Rollback Plan

- **Feature flag**: `enableSharedRide` in SharedRideConfig for instant disable
- **Estimate filtering**: Remove filtering to show all estimates
- **Graceful fallback**: Individual bookings if pooling fails
- **Redis cleanup**: Background job to clean stale GSI entries

## Success Metrics

- **Estimate filtering accuracy**: Correct shared ride estimates shown based on eligibility
- **Pooling success rate**: Percentage of shared selections that find matches  
- **Customer wait time**: Time from selection to successful match or timeout
- **System performance**: Response times for estimate retrieval and selection

## Dependencies

- **Chunk 1**: Route caching, GSI setup, and configuration infrastructure
- **SharedRideConfig**: Updated YAML with `enableSharedRide` field
- **Person.kycVerified**: KYC status field in person table
- **Redis Geospatial**: Redis version supporting geo commands