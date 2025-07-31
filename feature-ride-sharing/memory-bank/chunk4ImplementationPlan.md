# Chunk 4 Implementation Plan: Rider Pooling Logic and Batch Creation

## Overview
This document outlines the implementation plan for Chunk 4, which encompasses the **core Rider Pooling Logic** (Chunks 5-7 from sharedRideFlowCore.md) and **Batch Creation** (Chunk 3 from core flow). This is the heart of the shared ride matching system.

## Flow Summary
Based on sharedRideFlowCore.md, Chunk 4 implements the complete **Rider Pooling Logic** with sophisticated filtering cascade:

1. **Initial GSI Query** - Find nearby waiting customers
2. **Multi-Stage Filtering Cascade** - 7 progressive filters for compatibility
3. **Route Overlap Analysis** - Geo-hashing based route matching
4. **Lock Acquisition** - Prevent race conditions
5. **Batch Creation** - Create SharedSearchRequest and SharedEstimate
6. **Driver Pooling Integration** - Hand off to driver matching

## 🔴 CORE LOGIC MAPPING

### **GSI Structure (per sharedRideFlowCore.md)**
- **Key**: `ShareRideCustomerLoc`
- **Member Format**: `searchId:validTill:numSeats`
- **Coordinates**: Pickup location lat/lon

### **Filtering Cascade (7 Filters)**
1. **Locked Searches** - Remove already processing searches
2. **Expiry Time** - Remove searches expiring too soon
3. **Seat Availability** - Check vehicle capacity constraints
4. **Destination Proximity** - Radius-based drop location check
5. **Pickup Proximity** - Travel distance for pickup locations
6. **Drop-off Proximity** - Travel distance for drop locations
7. **Route Overlap** - Geo-hash based route analysis

## Implementation Details

### 1. Main Rider Pooling Logic Implementation
- **Status**: 🔧 Requires Complete Implementation
- **Location**: `Backend/app/rider-platform/rider-app/Main/src/Domain/Action/UI/SharedRide/Pooling.hs`
- **Implementation**:
  ```haskell
  module Domain.Action.UI.SharedRide.Pooling where

  import qualified Storage.CachedQueries.SharedRideConfig as QSharedRideConfig
  import qualified Storage.Queries.SearchRequest as QSR
  import qualified Storage.Queries.SharedSearchRequest as QSharedSR
  import qualified Storage.Queries.SharedEstimate as QSharedEst
  import qualified Data.HashSet as HS
  import qualified Data.HashMap.Strict as HM

  -- Main interface function (called from Chunk 2)
  invokeRiderPoolingLogic :: 
    (MonadFlow m, CacheFlow m r, EsqDBFlow m r, EsqDBReplicaFlow m r) => 
    SSR.SearchRequest -> 
    Int -> 
    m PoolingResult
  invokeRiderPoolingLogic searchRequest numSeats = do
    sharedRideConfig <- QSharedRideConfig.findByMerchantOperatingCityIdAndVehicleCategory 
                          searchRequest.merchantOperatingCityId 
                          searchRequest.vehicleCategory
    
    case sharedRideConfig of
      Nothing -> return $ PoolingError "Shared ride not configured"
      Just config -> do
        -- Step 1: Initial GSI Query
        nearbyCustomers <- queryNearbyCustomers searchRequest config
        
        -- Step 2: Apply filtering cascade
        filteredCustomers <- applyFilteringCascade searchRequest numSeats nearbyCustomers config
        
        case filteredCustomers of
          [] -> return PoolingNoMatch
          matchedSearchIds -> do
            -- Step 3: Create batch and shared entities
            (sharedSearchRequestId, sharedEstimateId) <- createSharedBatch searchRequest matchedSearchIds numSeats
            return $ PoolingSuccess (sharedSearchRequestId, sharedEstimateId)

  -- Step 1: Query nearby customers from GSI
  queryNearbyCustomers :: 
    SSR.SearchRequest -> 
    SharedRideConfig -> 
    Flow [NearbyCustomer]
  queryNearbyCustomers searchRequest config = do
    let gsiKey = "ShareRideCustomerLoc"
    let pickupLat = searchRequest.fromLocation.lat
    let pickupLon = searchRequest.fromLocation.lon
    let searchRadius = config.pickupLocationSearchRadius.getMeters
    
    -- Query Redis GSI for nearby customers
    nearbyMembers <- Redis.geoRadius gsiKey pickupLon pickupLat 
                       (fromIntegral searchRadius) "m"
    
    -- Parse member data and filter valid entries
    validCustomers <- catMaybes <$> mapM parseNearbyMember nearbyMembers
    
    -- Exclude current customer
    return $ filter (\nc -> nc.searchRequestId /= searchRequest.id) validCustomers

  parseNearbyMember :: Text -> Flow (Maybe NearbyCustomer)
  parseNearbyMember memberKey = do
    -- Format: "searchId:validTill:numSeats"
    case T.splitOn ":" memberKey of
      [searchIdText, validTillText, numSeatsText] -> do
        case (readMaybe (T.unpack validTillText), readMaybe (T.unpack numSeatsText)) of
          (Just validTill, Just numSeats) -> do
            let searchRequestId = Id searchIdText
            mbSearchRequest <- runInReplica $ QSR.findById searchRequestId
            case mbSearchRequest of
              Just sr -> return $ Just $ NearbyCustomer
                { searchRequestId = searchRequestId
                , searchRequest = sr
                , validTill = validTill
                , numSeats = numSeats
                }
              Nothing -> return Nothing
          _ -> return Nothing
      _ -> return Nothing

  data NearbyCustomer = NearbyCustomer
    { searchRequestId :: Id SSR.SearchRequest
    , searchRequest :: SSR.SearchRequest
    , validTill :: UTCTime
    , numSeats :: Int
    } deriving (Show, Eq)
  ```

### 2. Filtering Cascade Implementation
- **Status**: 🔧 Requires Implementation
- **Location**: Same module as above
- **Implementation**:
  ```haskell
  -- Step 2: Apply complete filtering cascade
  applyFilteringCascade :: 
    SSR.SearchRequest -> 
    Int -> 
    [NearbyCustomer] -> 
    SharedRideConfig -> 
    Flow [Id SSR.SearchRequest]
  applyFilteringCascade currentSearch currentNumSeats nearbyCustomers config = do
    -- Filter 1: Remove locked searches
    unlockedCustomers <- filterM (not <$> isSearchLocked . (.searchRequestId)) nearbyCustomers
    
    -- Filter 2: Remove searches expiring too soon
    now <- getCurrentTime
    let bufferTime = config.searchExpiryBufferSeconds.getSeconds
    let validCustomers = filter (\nc -> diffTime nc.validTill now > fromIntegral bufferTime) unlockedCustomers
    
    -- Filter 3: Seat availability check
    let seatCompatibleCustomers = filterSeatAvailability currentSearch currentNumSeats validCustomers
    
    -- Filter 4: Destination proximity
    destinationCompatible <- filterM (checkDestinationProximity currentSearch config) seatCompatibleCustomers
    
    -- Filter 5: Pickup proximity (travel distance)
    pickupCompatible <- filterM (checkPickupProximity currentSearch config) destinationCompatible
    
    -- Filter 6: Drop-off proximity (travel distance)
    dropoffCompatible <- filterM (checkDropoffProximity currentSearch config) pickupCompatible
    
    -- Filter 7: Route overlap analysis
    routeCompatible <- filterRouteOverlap currentSearch dropoffCompatible config
    
    -- Final grouping and lock acquisition
    finalMatches <- acquireLocksAndGroup currentSearch currentNumSeats routeCompatible config
    
    return $ map (.searchRequestId) finalMatches

  -- Filter 1: Check if search is locked
  isSearchLocked :: Id SSR.SearchRequest -> Flow Bool
  isSearchLocked searchId = do
    let lockKey = "search_lock:" <> searchId.getId
    Redis.exists lockKey

  -- Filter 3: Seat availability check
  filterSeatAvailability :: 
    SSR.SearchRequest -> 
    Int -> 
    [NearbyCustomer] -> 
    [NearbyCustomer]
  filterSeatAvailability currentSearch currentNumSeats customers = do
    let vehicleCapacity = getVehicleCapacity currentSearch.vehicleCategory
    let availableSeats = vehicleCapacity - 1 - currentNumSeats -- Subtract driver and current customer
    filter (\nc -> nc.numSeats <= availableSeats) customers

  getVehicleCapacity :: VehicleCategory -> Int
  getVehicleCapacity = \case
    AUTO_RICKSHAW -> 3    -- Total capacity for Auto
    HATCHBACK -> 4        -- Total capacity for Car
    SEDAN -> 4
    SUV -> 6
    _ -> 4

  -- Filter 4: Destination proximity check
  checkDestinationProximity :: 
    SSR.SearchRequest -> 
    SharedRideConfig -> 
    NearbyCustomer -> 
    Flow Bool
  checkDestinationProximity currentSearch config nearbyCustomer = do
    case (currentSearch.toLocation, nearbyCustomer.searchRequest.toLocation) of
      (Just currentDrop, Just otherDrop) -> do
        let distance = distanceBetweenInMeters currentDrop.gps otherDrop.gps
        return $ distance <= config.dropLocationSearchRadius.getMeters
      _ -> return False

  -- Filter 5: Pickup proximity (actual travel distance)
  checkPickupProximity :: 
    SSR.SearchRequest -> 
    SharedRideConfig -> 
    NearbyCustomer -> 
    Flow Bool
  checkPickupProximity currentSearch config nearbyCustomer = do
    -- Get actual travel distance between pickup points
    travelDistance <- calculateTravelDistance 
                        currentSearch.fromLocation.gps 
                        nearbyCustomer.searchRequest.fromLocation.gps
    return $ travelDistance <= config.actualPickupDistanceThreshold.getMeters

  -- Filter 6: Drop-off proximity (actual travel distance)
  checkDropoffProximity :: 
    SSR.SearchRequest -> 
    SharedRideConfig -> 
    NearbyCustomer -> 
    Flow Bool
  checkDropoffProximity currentSearch config nearbyCustomer = do
    case (currentSearch.toLocation, nearbyCustomer.searchRequest.toLocation) of
      (Just currentDrop, Just otherDrop) -> do
        travelDistance <- calculateTravelDistance currentDrop.gps otherDrop.gps
        return $ travelDistance <= config.actualDropDistanceThreshold.getMeters
      _ -> return False

  calculateTravelDistance :: LatLong -> LatLong -> Flow Double
  calculateTravelDistance from to = do
    -- TODO: Integrate with routing service for actual travel distance
    -- For now, use straight-line distance as approximation
    return $ distanceBetweenInMeters from to
  ```

### 3. Route Overlap Analysis (Geo-hashing)
- **Status**: 🔧 Requires Implementation
- **Location**: Same module as above
- **Implementation**:
  ```haskell
  -- Filter 7: Route overlap analysis using geo-hashing
  filterRouteOverlap :: 
    SSR.SearchRequest -> 
    [NearbyCustomer] -> 
    SharedRideConfig -> 
    Flow [NearbyCustomer]
  filterRouteOverlap currentSearch customers config = do
    -- Get cached route for current search
    mbCurrentRoute <- getCachedRoute currentSearch.id
    case mbCurrentRoute of
      Nothing -> return customers -- No route available, skip overlap check
      Just currentRoute -> do
        -- Create geo-hash set for current route
        let precision = config.geoHashPrecisionForRouteMatching
        currentRouteHashes <- createRouteHashSet currentRoute precision
        
        -- Filter customers based on route overlap
        filterM (checkRouteOverlapWithCustomer currentRouteHashes config precision) customers

  createRouteHashSet :: RouteResponse -> Int -> Flow (HS.HashSet Text)
  createRouteHashSet route precision = do
    let routePoints = extractRoutePoints route
    let hashes = map (geoHashEncode precision) routePoints
    return $ HS.fromList hashes

  checkRouteOverlapWithCustomer :: 
    HS.HashSet Text -> 
    SharedRideConfig -> 
    Int -> 
    NearbyCustomer -> 
    Flow Bool
  checkRouteOverlapWithCustomer currentHashes config precision customer = do
    mbCustomerRoute <- getCachedRoute customer.searchRequestId
    case mbCustomerRoute of
      Nothing -> return True -- No route available, allow match
      Just customerRoute -> do
        customerHashes <- createRouteHashSet customerRoute precision
        let overlapCount = HS.size $ HS.intersection currentHashes customerHashes
        let totalPoints = HS.size customerHashes
        let overlapPercentage = if totalPoints > 0 
                               then (fromIntegral overlapCount / fromIntegral totalPoints) * 100
                               else 0
        return $ overlapPercentage > config.routeOverlapThreshold

  getCachedRoute :: Id SSR.SearchRequest -> Flow (Maybe RouteResponse)
  getCachedRoute searchId = do
    let cacheKey = "route_cache:" <> searchId.getId
    cached <- Redis.get cacheKey
    return $ decode =<< cached

  extractRoutePoints :: RouteResponse -> [LatLong]
  extractRoutePoints route = do
    -- TODO: Extract actual coordinate points from route response
    -- This depends on the structure of RouteResponse
    []

  geoHashEncode :: Int -> LatLong -> Text
  geoHashEncode precision latLong = do
    -- TODO: Implement geo-hash encoding with specified precision
    -- Use existing geo-hash library
    ""
  ```

### 4. Lock Acquisition and Final Grouping
- **Status**: 🔧 Requires Implementation
- **Location**: Same module as above
- **Implementation**:
  ```haskell
  -- Final grouping with lock acquisition
  acquireLocksAndGroup :: 
    SSR.SearchRequest -> 
    Int -> 
    [NearbyCustomer] -> 
    SharedRideConfig -> 
    Flow [NearbyCustomer]
  acquireLocksAndGroup currentSearch currentNumSeats customers config = do
    -- Sort customers by compatibility score
    scoredCustomers <- mapM (calculateCompatibilityScore currentSearch) customers
    let sortedCustomers = map fst $ sortBy (comparing snd) scoredCustomers
    
    -- Attempt to acquire locks and form valid group
    acquireLocksForGroup currentSearch currentNumSeats sortedCustomers []

  acquireLocksForGroup :: 
    SSR.SearchRequest -> 
    Int -> 
    [NearbyCustomer] -> 
    [NearbyCustomer] -> 
    Flow [NearbyCustomer]
  acquireLocksForGroup currentSearch currentNumSeats [] acc = 
    return $ validateFinalGroup currentSearch currentNumSeats acc

  acquireLocksForGroup currentSearch currentNumSeats (customer:rest) acc = do
    lockAcquired <- tryAcquireLock customer.searchRequestId
    if lockAcquired
      then do
        let newAcc = customer : acc
        -- Check if we have a valid group size
        case validateGroupSize currentSearch currentNumSeats newAcc of
          Just validGroup -> return validGroup
          Nothing -> acquireLocksForGroup currentSearch currentNumSeats rest newAcc
      else acquireLocksForGroup currentSearch currentNumSeats rest acc

  tryAcquireLock :: Id SSR.SearchRequest -> Flow Bool
  tryAcquireLock searchId = do
    let lockKey = "search_lock:" <> searchId.getId
    Redis.setNx lockKey "locked" 300 -- 5 minute lock

  validateGroupSize :: 
    SSR.SearchRequest -> 
    Int -> 
    [NearbyCustomer] -> 
    Maybe [NearbyCustomer]
  validateGroupSize currentSearch currentNumSeats customers = do
    let totalCustomers = length customers + 1 -- Include current customer
    let totalSeats = currentNumSeats + sum (map (.numSeats) customers)
    let vehicleCapacity = getVehicleCapacity currentSearch.vehicleCategory
    
    case currentSearch.vehicleCategory of
      AUTO_RICKSHAW -> 
        if totalCustomers == 2 && totalSeats <= 2 -- Driver + 2 passengers max
          then Just customers
          else Nothing
      _ -> -- Cars (HATCHBACK, SEDAN, etc.)
        if totalCustomers >= 2 && totalCustomers <= 3 && totalSeats <= (vehicleCapacity - 1)
          then Just customers
          else Nothing

  validateFinalGroup :: 
    SSR.SearchRequest -> 
    Int -> 
    [NearbyCustomer] -> 
    [NearbyCustomer]
  validateFinalGroup currentSearch currentNumSeats customers = 
    case validateGroupSize currentSearch currentNumSeats customers of
      Just validGroup -> validGroup
      Nothing -> []

  calculateCompatibilityScore :: 
    SSR.SearchRequest -> 
    NearbyCustomer -> 
    Flow (NearbyCustomer, Double)
  calculateCompatibilityScore currentSearch customer = do
    let pickupDistance = distanceBetweenInMeters 
                          currentSearch.fromLocation.gps 
                          customer.searchRequest.fromLocation.gps
    
    dropDistance <- case (currentSearch.toLocation, customer.searchRequest.toLocation) of
      (Just currentDrop, Just customerDrop) -> 
        return $ distanceBetweenInMeters currentDrop.gps customerDrop.gps
      _ -> return 0
    
    let timeScore = fromIntegral $ abs $ diffTime currentSearch.createdAt customer.searchRequest.createdAt
    
    -- Lower score is better (weighted scoring)
    let score = (pickupDistance * 0.4) + (dropDistance * 0.4) + (timeScore * 0.2)
    return (customer, score)
  ```

### 5. Batch Creation (SharedSearchRequest and SharedEstimate)
- **Status**: 🔧 Requires Implementation
- **Location**: Same module as above
- **Implementation**:
  ```haskell
  -- Create shared entities after successful matching
  createSharedBatch :: 
    SSR.SearchRequest -> 
    [Id SSR.SearchRequest] -> 
    Int -> 
    Flow (Id SharedSearchRequest, Id SharedEstimate)
  createSharedBatch primarySearch matchedSearchIds numSeats = do
    -- Create SharedSearchRequest first
    sharedSearchRequestId <- createSharedSearchRequest primarySearch matchedSearchIds numSeats
    
    -- Create SharedEstimate
    sharedEstimateId <- createSharedEstimate sharedSearchRequestId primarySearch matchedSearchIds
    
    return (sharedSearchRequestId, sharedEstimateId)

  createSharedSearchRequest :: 
    SSR.SearchRequest -> 
    [Id SSR.SearchRequest] -> 
    Int -> 
    Flow (Id SharedSearchRequest)
  createSharedSearchRequest primarySearch matchedSearchIds numSeats = do
    now <- getCurrentTime
    sharedSearchRequestId <- generateGUID
    
    let allSearchRequestIds = primarySearch.id : matchedSearchIds
    let validTill = addUTCTime (30 * 60) now -- 30 minutes validity
    
    -- Get all search requests to generate waypoints
    allSearchRequests <- mapM (runInReplica . QSR.findById) allSearchRequestIds >>= 
                        mapM fromMaybeM . map (SearchRequestNotFound . (.getId))
    
    let waypoints = concatMap generateWaypoints allSearchRequests
    
    let sharedSearchRequest = SharedSearchRequest
          { id = sharedSearchRequestId
          , status = POOLING
          , searchRequestIds = allSearchRequestIds
          , merchantId = primarySearch.merchantId
          , merchantOperatingCityId = primarySearch.merchantOperatingCityId
          , vehicleCategory = primarySearch.vehicleCategory
          , waypoints = encode waypoints
          , maxDistance = Nothing -- Will be calculated
          , totalCustomerExtraFee = Nothing
          , validTill = validTill
          , createdAt = now
          , updatedAt = now
          }
    
    QSharedSR.create sharedSearchRequest
    return sharedSearchRequestId

  createSharedEstimate :: 
    Id SharedSearchRequest -> 
    SSR.SearchRequest -> 
    [Id SSR.SearchRequest] -> 
    Flow (Id SharedEstimate)
  createSharedEstimate sharedSearchRequestId primarySearch matchedSearchIds = do
    now <- getCurrentTime
    sharedEstimateId <- generateGUID
    
    -- Get all estimates for the matched searches
    allSearchIds <- return $ primarySearch.id : matchedSearchIds
    allEstimates <- concat <$> mapM (runInReplica . QEstimate.findAllBySRId) allSearchIds
    
    -- Calculate combined fare (simplified - needs proper algorithm)
    let estimatedTotalFare = sum $ map ((.amount) . (.estimatedFare)) allEstimates
    
    let sharedEstimate = SharedEstimate
          { id = sharedEstimateId
          , sharedSearchRequestId = sharedSearchRequestId
          , estimateIds = map (.id) allEstimates
          , merchantId = primarySearch.merchantId
          , merchantOperatingCityId = primarySearch.merchantOperatingCityId
          , bppSharedEstimateId = "shared_" <> sharedEstimateId.getId
          , providerId = "namma_yatri"
          , providerName = "Namma Yatri"
          , providerUrl = "https://nammayatri.in"
          , serviceTierName = Just "Shared"
          , estimatedTotalFare = estimatedTotalFare
          , totalFareRangeMin = estimatedTotalFare * 0.9
          , totalFareRangeMax = estimatedTotalFare * 1.1
          , estimatedDuration = Nothing -- Calculate from routes
          , estimatedDistance = Nothing -- Calculate from routes
          , vehicleServiceTierType = AUTO_RICKSHAW -- Based on vehicle category
          , vehicleServiceTierSeatingCapacity = Just 3
          , nightShiftCharge = Nothing
          , nightShiftChargeAmount = Nothing
          , oldNightShiftCharge = Nothing
          , nightShiftStart = Nothing
          , nightShiftEnd = Nothing
          , status = NEW
          , tripCategory = Just RideShare
          , validTill = addUTCTime (30 * 60) now
          , createdAt = now
          , updatedAt = now
          }
    
    QSharedEst.create sharedEstimate
    return sharedEstimateId

  generateWaypoints :: SSR.SearchRequest -> [Waypoint]
  generateWaypoints searchRequest = 
    let pickup = Waypoint 
                  { waypointType = PICKUP
                  , searchRequestId = searchRequest.id
                  , lat = searchRequest.fromLocation.lat
                  , lon = searchRequest.fromLocation.lon
                  }
        dropoffs = case searchRequest.toLocation of
                    Just toLoc -> [Waypoint 
                                    { waypointType = DROPOFF
                                    , searchRequestId = searchRequest.id  
                                    , lat = toLoc.lat
                                    , lon = toLoc.lon
                                    }]
                    Nothing -> []
    in pickup : dropoffs
  ```

## Database Schema Requirements

### Lock Management Table
```sql
-- Optional: Dedicated lock table for better tracking
CREATE TABLE IF NOT EXISTS atlas_app.search_locks (
    search_request_id UUID PRIMARY KEY,
    locked_at TIMESTAMPTZ NOT NULL,
    expires_at TIMESTAMPTZ NOT NULL,
    locked_by VARCHAR(255) -- Process identifier
);

CREATE INDEX idx_search_locks_expires_at ON atlas_app.search_locks(expires_at);
```

## Redis Data Structures

### Search Locks
```
Key: "search_lock:{searchId}"
Value: "locked"
TTL: 300 seconds (5 minutes)
Purpose: Prevent race conditions during pooling
```

### Route Overlap Cache
```
Key: "route_overlap:{searchId1}:{searchId2}"
Value: overlap percentage (double)
TTL: 15 minutes
Purpose: Cache expensive route overlap calculations
```

## Configuration Dependencies

### Required SharedRideConfig Fields
```yaml
# Advanced filtering thresholds
routeOverlapThreshold: Double              # x4 threshold (e.g., 70.0)
geoHashPrecisionForRouteMatching: Int      # Precision 9 for geo-hashing
actualPickupDistanceThreshold: Meters     # x2 travel distance
actualDropDistanceThreshold: Meters       # x6 travel distance
searchExpiryBufferSeconds: Seconds        # Y buffer time
dropLocationSearchRadius: Meters          # x1 destination radius
```

## Integration Points

### Driver Pooling Integration (Chunk 8)
```haskell
-- After successful batch creation, trigger driver pooling
initiateDriverPooling :: 
  Id SharedEstimate -> 
  Flow DriverPoolingResult
initiateDriverPooling sharedEstimateId = do
  -- TODO: Implement driver pooling logic from Chunk 8
  -- This will create cumulative estimates and find drivers
  error "Driver pooling: Implementation in separate chunk"
```

## Testing Strategy

### Unit Tests
1. **Individual filter functions** - Test each filter with edge cases
2. **Route overlap algorithm** - Test geo-hash based overlap calculation
3. **Lock acquisition** - Test race condition handling
4. **Batch creation** - Test shared entity creation

### Integration Tests
1. **Complete pooling flow** - End-to-end matching process
2. **Concurrent pooling** - Multiple customers pooling simultaneously
3. **Redis GSI operations** - Large-scale geospatial queries
4. **Lock contention** - Stress test lock acquisition

### Performance Tests
1. **Large GSI queries** - Performance with thousands of waiting customers
2. **Route overlap computation** - Expensive geo-hash calculations
3. **Filtering cascade** - Performance of 7-stage filtering
4. **Lock acquisition speed** - Redis lock performance

## Error Handling

### Pooling Errors
```haskell
data PoolingError 
  = NoNearbyCustomers
  | AllSearchesLocked
  | InsufficientSeats Int Int        -- required, available
  | RouteOverlapTooLow Double Double -- actual, required
  | BatchCreationFailed Text
  | LockAcquisitionTimeout
```

## Success Metrics

- **Pooling success rate**: Percentage of searches that find matches
- **Filter effectiveness**: Percentage filtered at each stage
- **Lock contention**: Lock acquisition failure rate
- **Route overlap accuracy**: Quality of geo-hash based matching
- **Batch completion rate**: Successful SharedSearchRequest creation

## Implementation Priority

1. **Phase 1**: Basic filtering cascade (Filters 1-6)
2. **Phase 2**: Route overlap analysis (Filter 7)
3. **Phase 3**: Lock acquisition and grouping
4. **Phase 4**: Batch creation and shared entity management

## Dependencies

- **From Chunk 2**: Pooling interfaces, GSI data structure
- **To Chunk 8**: Driver pooling integration
- **To Chunk 9**: Shared pooling logic for async flow
- **External**: Geo-hash library, routing service integration