# Chunk 5 Implementation Plan: GSI Queries and Initial Filtering

## Overview
This document outlines the implementation plan for Chunk 5 of the shared ride feature, focusing on the **initial GSI query and 6-step filtering cascade** that forms the core of the rider pooling logic.

## Flow Summary
Chunk 5 implements the **"Rider Pooling Logic - GSI Query and Basic Filters"** sequence:
1. **GSI Structure Setup** - Use existing `ShareRideCustomerLoc` GSI with `estimateId:validTill:numSeats` format
2. **Initial Radius Query** - Find nearby waiting customers within radius `R`
3. **6-Step Filtering Cascade** - Apply proximity, timing, and seat availability filters
4. **Return Filtered List** - Pass qualified estimateIds to Chunk 6 for advanced filtering

**Input**: Current customer's `estimateId`, `numSeats`, pickup/drop locations
**Output**: List of compatible `estimateIds` for advanced filtering in Chunk 6

## 🔴 IMPORTANT CLARIFICATIONS

### EstimateId-Based Implementation
- **GSI Members**: Use `estimateId:validTill:numSeats` format (already implemented in Chunk 2)
- **Lock Management**: All locks are acquired on `estimateIds`, not `searchIds` 
- **Result Tracking**: Return `estimateIds` for SharedEntity creation
- **Consistency**: Maintain estimateId usage throughout filtering cascade

### Configuration Dependencies
- **SharedRideConfigs**: All filtering parameters come from config table
- **Radius Parameters**: `pickupLocationSearchRadius`, `dropLocationSearchRadius`
- **Distance Thresholds**: `actualPickupDistanceThreshold`, `actualDropDistanceThreshold`
- **Time Buffers**: `searchExpiryBufferSeconds` for expiry validation

## ✅ Chunk 5 Implementation Summary

### What We've Implemented:
1. **Complete `handleSyncRiderPooling` Logic** - Full GSI query and filtering cascade with optimized distance calculations
2. **Modular Validation Pattern** - 4-step `ValidationStep` approach replacing nested if-else statements
3. **Optimized Distance Strategy** - Staged validation to minimize expensive `Tools.Maps.getDistance` API calls
4. **Robust Error Handling** - Proper `fromMaybeM` usage with specific error types following codebase patterns
5. **Performance-Aware Implementation** - Early exits, smart filtering order, and `Redis.safeGet` usage
6. **SharedEntity Creation** - OVERLAPPING type SharedEntity for matched riders with proper state tracking
7. **Concrete Type Usage** - `DLocation.Location` types instead of generic parameters for better performance

### Key Implementation Decisions:
- **Simplified to 4-step validation** - Combined proximity checks for better performance vs planned 6-step
- **Staged distance validation** - Straight-line → pickup compatibility → drop-to-drop (only when needed)
- **Tools.Maps integration** - Actual route distances for precision while minimizing API calls
- **Modular architecture** - Clean separation of concerns with individual validation functions
- **VaibhavD feedback addressed** - Lambda syntax, concrete types, staged validation, proper error handling

### Implementation Location:
- **File**: `Backend/app/rider-platform/rider-app/Main/src/SharedLogic/RiderPooling.hs`
- **Status**: ✅ **FULLY IMPLEMENTED AND TESTED**
- **Functions**: `handleSyncRiderPooling`, `isValidCandidate`, validation steps, `createSharedEntityForFinalMatch`
- **Integration**: Used in `Domain.Action.UI.Select.select2` via `SharedLogic.RiderPooling` module import

## Original Implementation Plan (Pre-Implementation)

### Target Module: Enhanced handleSyncRiderPooling Function
- **File**: `Backend/app/rider-platform/rider-app/Main/src/Domain/Action/UI/Select.hs:593-610`
- **Current Status**: Interface implemented, logic placeholder  
- **Enhancement Strategy**: Replace placeholder with full Chunk 5 implementation

```haskell
-- Current placeholder implementation
handleSyncRiderPooling :: 
  (MonadFlow m, CacheFlow m r, EsqDBFlow m r, EsqDBReplicaFlow m r) => 
  SharedRideConfigs -> 
  DSearchReq.SearchRequest -> 
  DEstimate.Estimate -> 
  Int -> 
  m (Maybe (Id DSE.SharedEntity))
handleSyncRiderPooling config searchRequest estimate numSeats = do
  -- TODO: Implement Chunks 5-7 logic here
  return Nothing
```

## Detailed Implementation Plan

### 1. GSI Query Implementation
```haskell
-- Query ShareRideCustomerLoc GSI for nearby riders
queryNearbyRiders :: 
  (CacheFlow m r) => 
  SharedRideConfigs -> 
  LatLong -> 
  m [GSIMember]
queryNearbyRiders config pickupLocation = do
  let gsiKey = "ShareRideCustomerLoc"
  let radiusMeters = config.pickupLocationSearchRadius.getMeters
  
  -- Query GSI with radius R
  nearbyMembers <- Redis.geoRadius gsiKey pickupLocation.lat pickupLocation.lon radiusMeters
  
  -- Parse GSI member format: estimateId:validTill:numSeats
  parsedMembers <- mapM parseGSIMember nearbyMembers
  return $ catMaybes parsedMembers

-- Parse GSI member data
data GSIMember = GSIMember
  { gsiEstimateId :: Id DEstimate.Estimate
  , gsiValidTill :: UTCTime  
  , gsiNumSeats :: Int
  , gsiCoordinates :: (Double, Double)
  } deriving (Generic, Show)

parseGSIMember :: Text -> m (Maybe GSIMember)
parseGSIMember memberText = do
  case T.splitOn ":" memberText of
    [estimateIdText, validTillText, numSeatsText] -> do
      estimateId <- pure $ Id estimateIdText
      validTill <- parseTimeM False defaultTimeLocale "%Y-%m-%d %H:%M:%S%Q %Z" validTillText
      numSeats <- readMaybe $ T.unpack numSeatsText
      return $ GSIMember estimateId validTill numSeats <$> numSeats
    _ -> return Nothing
```

### 2. Filter 1: Remove Locked Estimates
```haskell
-- Filter out estimates that are already locked (part of another batch)
filterUnlocked :: 
  (CacheFlow m r) => 
  [GSIMember] -> 
  m [GSIMember]
filterUnlocked gsiMembers = do
  unlockedMembers <- filterM checkNotLocked gsiMembers
  return unlockedMembers
  where
    checkNotLocked member = do
      lockKey <- generateLockKey member.gsiEstimateId
      isLocked <- Redis.exists lockKey
      return (not isLocked)

-- Generate consistent lock key for estimate
generateLockKey :: Id DEstimate.Estimate -> Text
generateLockKey estimateId = "shared_ride_lock:" <> estimateId.getId
```

### 3. Filter 2: Expiry Time Validation
```haskell
-- Filter out estimates that will expire within buffer time
filterByExpiry :: 
  SharedRideConfigs -> 
  [GSIMember] -> 
  UTCTime -> 
  [GSIMember]
filterByExpiry config gsiMembers currentTime = do
  let bufferSeconds = config.searchExpiryBufferSeconds.getSeconds
  let minimumValidTime = addUTCTime (fromIntegral bufferSeconds) currentTime
  
  filter (\member -> member.gsiValidTill > minimumValidTime) gsiMembers
```

### 4. Filter 3: Seat Availability Check
```haskell
-- Filter estimates where requested seats exceed vehicle capacity
filterBySeatAvailability :: 
  Int -> -- Current customer's numSeats
  VehicleCategory -> 
  [GSIMember] -> 
  [GSIMember]
filterBySeatAvailability currentSeats vehicleCategory gsiMembers = do
  let maxCapacity = getVehicleCapacity vehicleCategory
  let availableSeats = maxCapacity - 1 -- Reserve one seat for current customer
  
  filter (\member -> member.gsiNumSeats <= availableSeats - currentSeats) gsiMembers
  where
    getVehicleCapacity AUTO = 3
    getVehicleCapacity CAB = 4
    getVehicleCapacity _ = 4 -- Default to cab capacity
```

### 5. Filter 4: Destination Proximity Check
```haskell
-- Filter estimates with destinations outside radius x1
filterByDestinationProximity :: 
  (MonadFlow m, EsqDBReplicaFlow m r) =>
  SharedRideConfigs -> 
  LatLong -> -- Current customer's destination
  [GSIMember] -> 
  m [GSIMember]
filterByDestinationProximity config currentDestination gsiMembers = do
  let maxDistance = config.dropLocationSearchRadius
  
  filteredMembers <- filterM (checkDestinationDistance currentDestination maxDistance) gsiMembers
  return filteredMembers
  where
    checkDestinationDistance currentDest maxDist member = do
      -- Fetch estimate and get destination
      estimate <- QEstimate.findById member.gsiEstimateId
      case estimate >>= (.toLocation) of
        Just toLocation -> do
          distance <- calculateDistance currentDest (LatLong toLocation.lat toLocation.lon)
          return (distance <= maxDist)
        Nothing -> return False -- Reject estimates without destination
```

### 6. Filter 5: Pickup Proximity Check (Travel Distance)
```haskell
-- Filter estimates with pickup locations beyond x2 travel units
filterByPickupDistance :: 
  (MonadFlow m, EsqDBReplicaFlow m r) =>
  SharedRideConfigs -> 
  LatLong -> -- Current customer's pickup
  [GSIMember] -> 
  m [GSIMember]
filterByPickupDistance config currentPickup gsiMembers = do
  let maxTravelDistance = config.actualPickupDistanceThreshold
  
  filteredMembers <- filterM (checkPickupTravelDistance currentPickup maxTravelDistance) gsiMembers
  return filteredMembers
  where
    checkPickupTravelDistance currentPickup maxDist member = do
      -- Get pickup location from GSI coordinates or estimate
      let memberPickup = LatLong (fst member.gsiCoordinates) (snd member.gsiCoordinates)
      
      -- Calculate actual travel distance (not just radius)
      travelDistance <- calculateTravelDistance currentPickup memberPickup
      return (travelDistance <= maxDist)
```

### 7. Filter 6: Drop-off Proximity Check (Travel Distance)
```haskell
-- Filter estimates with drop-off locations beyond x6 travel units
filterByDropoffDistance :: 
  (MonadFlow m, EsqDBReplicaFlow m r) =>
  SharedRideConfigs -> 
  LatLong -> -- Current customer's drop-off
  [GSIMember] -> 
  m [GSIMember]
filterByDropoffDistance config currentDropoff gsiMembers = do
  let maxTravelDistance = config.actualDropDistanceThreshold
  
  filteredMembers <- filterM (checkDropoffTravelDistance currentDropoff maxTravelDistance) gsiMembers
  return filteredMembers
  where
    checkDropoffTravelDistance currentDropoff maxDist member = do
      estimate <- QEstimate.findById member.gsiEstimateId
      case estimate >>= (.toLocation) of
        Just toLocation -> do
          let memberDropoff = LatLong toLocation.lat toLocation.lon
          travelDistance <- calculateTravelDistance currentDropoff memberDropoff
          return (travelDistance <= maxDist)
        Nothing -> return False
```

### 8. Main Chunk 5 Implementation
```haskell
-- Enhanced handleSyncRiderPooling with Chunk 5 logic
handleSyncRiderPooling :: 
  (MonadFlow m, CacheFlow m r, EsqDBFlow m r, EsqDBReplicaFlow m r) => 
  SharedRideConfigs -> 
  DSearchReq.SearchRequest -> 
  DEstimate.Estimate -> 
  Int -> 
  m (Maybe (Id DSE.SharedEntity))
handleSyncRiderPooling config searchRequest estimate numSeats = do
  logInfo $ "Starting Chunk 5: GSI query and filtering for estimate: " <> estimate.id.getId
  
  -- Step 1: Query GSI for nearby riders
  let pickupLocation = LatLong searchRequest.fromLocation.lat searchRequest.fromLocation.lon
  nearbyRiders <- queryNearbyRiders config pickupLocation
  
  when (null nearbyRiders) $ do
    logInfo "No nearby riders found in GSI"
    return Nothing
  
  -- Step 2: Apply 6-step filtering cascade
  currentTime <- getCurrentTime
  
  -- Filter 1: Remove locked estimates
  unlockedRiders <- filterUnlocked nearbyRiders
  logInfo $ "After lock filter: " <> show (length unlockedRiders) <> " riders"
  
  -- Filter 2: Remove soon-to-expire estimates  
  let validRiders = filterByExpiry config unlockedRiders currentTime
  logInfo $ "After expiry filter: " <> show (length validRiders) <> " riders"
  
  -- Filter 3: Check seat availability
  let seatFilteredRiders = filterBySeatAvailability numSeats searchRequest.vehicleCategory validRiders
  logInfo $ "After seat filter: " <> show (length seatFilteredRiders) <> " riders"
  
  -- Filter 4: Destination proximity
  currentDestination <- case searchRequest.toLocation of
    Just toLoc -> return $ LatLong toLoc.lat toLoc.lon
    Nothing -> throwError (InvalidRequest "Destination required for shared rides")
  
  destFilteredRiders <- filterByDestinationProximity config currentDestination seatFilteredRiders
  logInfo $ "After destination filter: " <> show (length destFilteredRiders) <> " riders"
  
  -- Filter 5: Pickup distance
  pickupFilteredRiders <- filterByPickupDistance config pickupLocation destFilteredRiders
  logInfo $ "After pickup filter: " <> show (length pickupFilteredRiders) <> " riders"
  
  -- Filter 6: Drop-off distance  
  finalFilteredRiders <- filterByDropoffDistance config currentDestination pickupFilteredRiders
  logInfo $ "After dropoff filter: " <> show (length finalFilteredRiders) <> " riders"
  
  if null finalFilteredRiders
    then do
      logInfo "No compatible riders found after Chunk 5 filtering"
      return Nothing
    else do
      logInfo $ "Chunk 5 complete. Passing " <> show (length finalFilteredRiders) <> " riders to Chunk 6"
      -- TODO: Call Chunk 6 implementation
      -- For now return Nothing until Chunks 6-7 are implemented
      return Nothing
```

## Helper Functions

### Distance Calculation Utilities
```haskell
-- Calculate straight-line distance between two points
calculateDistance :: LatLong -> LatLong -> m Distance
calculateDistance point1 point2 = do
  -- Use Haversine formula or existing distance calculation
  -- Return distance in meters
  pure $ Distance (haversineDistance point1 point2) Meter

-- Calculate actual travel distance using route API
calculateTravelDistance :: LatLong -> LatLong -> m Distance  
calculateTravelDistance point1 point2 = do
  -- Use Google Maps or existing routing service
  -- Return actual travel distance in meters
  route <- getRouteDistance point1 point2
  return route.distance
```

## Configuration Requirements

### Enhanced SharedRideConfigs Usage
```haskell
-- All filtering parameters from config
data SharedRideConfigsUsage = SharedRideConfigsUsage
  { pickupLocationSearchRadius :: Meters    -- GSI query radius R
  , searchExpiryBufferSeconds :: Seconds    -- Filter 2: expiry buffer Y
  , dropLocationSearchRadius :: Meters      -- Filter 4: destination radius x1  
  , actualPickupDistanceThreshold :: Meters -- Filter 5: pickup travel distance x2
  , actualDropDistanceThreshold :: Meters   -- Filter 6: dropoff travel distance x6
  }
```

## Testing Strategy

### Unit Tests
1. **GSI Query Tests**: Mock Redis GSI with various rider distributions
2. **Filter Individual Tests**: Test each filter with edge cases
3. **Filter Cascade Tests**: Verify cumulative filtering behavior
4. **Lock State Tests**: Test concurrent access scenarios

### Integration Tests
1. **End-to-End Filtering**: Real GSI data with multiple riders
2. **Config Variation Tests**: Different parameter values
3. **Performance Tests**: Large numbers of nearby riders
4. **Edge Case Tests**: Empty GSI, all locked riders, expired estimates

## Error Handling

### Expected Scenarios
```haskell
data Chunk5Error 
  = GSIQueryFailed Text
  | InvalidEstimateData (Id DEstimate.Estimate)
  | MissingDestination (Id DSearchReq.SearchRequest)
  | ConfigurationMissing
  | DistanceCalculationFailed
```

### Graceful Degradation
- GSI query failures → return Nothing (no matches)
- Individual estimate errors → skip that estimate, continue filtering
- Distance calculation errors → reject that pair, continue
- Configuration issues → log error, use default values

## Performance Considerations

### Optimization Strategies
1. **Batch Distance Calculations**: Minimize external API calls
2. **Early Filtering**: Apply cheap filters first (time, seats)
3. **Parallel Processing**: Process distance calculations concurrently
4. **GSI Query Limits**: Reasonable radius to prevent large result sets

### Monitoring Metrics
- GSI query response times
- Filter cascade execution time
- Number of riders at each filter stage
- Distance calculation API usage

## Cross-Chunk Dependencies

### Input from Chunk 2
- `SharedRideConfigs` with all necessary parameters
- Validated `searchRequest` and `estimate`
- Customer's `numSeats` selection

### Output to Chunk 6
- List of `estimateIds` that passed basic filtering
- Current customer's route and location data
- Configuration context for advanced filtering

### Fallback Behavior
- Return `Nothing` if no matches found
- Chunk 2 will add customer to GSI waiting pool
- Async cron (Chunk 9) will retry matching later

## Implementation Priority

1. **Phase 1**: GSI query and basic filters (1-3)
2. **Phase 2**: Distance-based filters (4-6) 
3. **Phase 3**: Performance optimization and monitoring
4. **Phase 4**: Error handling and edge cases

---

**Status**: ✅ **FULLY IMPLEMENTED** (2025-08-05)
**Dependencies**: Chunk 2 GSI setup ✅, SharedRideConfigs table ✅  
**Implementation**: GSI query and filtering cascade completed in handleSyncRiderPooling function ✅
**Next Step**: Implement Chunks 6-7 for advanced route overlap analysis and geo-hashing