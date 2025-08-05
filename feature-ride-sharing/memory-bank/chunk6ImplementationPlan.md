# Chunk 6 Implementation Plan: Advanced Filtering and Route Overlap Analysis

## Overview
This document outlines the implementation plan for Chunk 6 of the shared ride feature, focusing on **advanced drop-off distance checks and preliminary route overlap analysis** using geo-hashing.

## Flow Summary
Chunk 6 implements the **"Advanced Filtering and Route Overlap Analysis"** sequence:
1. **Advanced Drop-off Distance Check** - Calculate deviation ratio between drop-off locations
2. **Route Overlap Analysis (Preliminary)** - Use geo-hashing precision 9 for route comparison
3. **Initial Lock Acquisition** - Acquire locks on compatible estimates
4. **Basic Vehicle Capacity Check** - Ensure list size constraints are met

**Input**: Filtered `estimateIds` from Chunk 5, current customer's route data
**Output**: Lock-acquired `estimateIds` ready for final validation in Chunk 7

## 🔴 IMPORTANT CLARIFICATIONS

### Route Overlap Strategy
- **Precision 9 Geo-hashing**: Convert all route points to geo-hash strings for efficient comparison
- **HashSet Operations**: Store primary customer's route hashes for O(1) lookup performance
- **Percentage Calculation**: `((matchCnt) / (total_route_points)) * 100 > x4`
- **Preliminary Check**: This is initial filtering; Chunk 7 will do detailed final analysis

### Advanced Distance Logic
- **Deviation Ratio**: `((distance_between_drops) / min_ride_distance) * 100 > x3`
- **Min Ride Distance**: Calculate for each pair and use the smaller distance as baseline
- **Pairwise Comparison**: Check current customer against each filtered estimate

### Lock Management
- **EstimateId Locking**: Acquire locks on `estimateIds` that pass all filters
- **Concurrent Protection**: Prevent other pooling attempts from using same estimates
- **Lock Cleanup**: Release locks if final capacity check fails

## ✅ Implementation Location

### Extension of handleSyncRiderPooling Function
- **File**: `Backend/app/rider-platform/rider-app/Main/src/Domain/Action/UI/Select.hs:593-610`
- **Integration Point**: Continue from Chunk 5 filtered results
- **Call Chain**: Chunk 5 → Chunk 6 → Chunk 7 → SharedEntity creation

```haskell
-- Extension point from Chunk 5
chunk6AdvancedFiltering :: 
  (MonadFlow m, CacheFlow m r, EsqDBFlow m r, EsqDBReplicaFlow m r) =>
  SharedRideConfigs ->
  DSearchReq.SearchRequest ->
  DEstimate.Estimate ->
  [GSIMember] -> -- From Chunk 5
  m [Id DEstimate.Estimate] -- Locked estimateIds for Chunk 7
```

## Detailed Implementation Plan

### 1. Advanced Drop-off Distance Check
```haskell
-- Filter estimates using drop-off deviation ratio
filterByDropoffDeviation :: 
  (MonadFlow m, EsqDBReplicaFlow m r) =>
  SharedRideConfigs -> 
  LatLong -> -- Current customer's drop-off
  [GSIMember] -> 
  m [GSIMember]
filterByDropoffDeviation config currentDropoff gsiMembers = do
  let deviationThreshold = config.routeMatchingThreshold -- x3 parameter
  
  -- Get current customer's route distance for baseline
  currentRouteDistance <- getCurrentCustomerRouteDistance
  
  filteredMembers <- filterM (checkDropoffDeviation currentDropoff currentRouteDistance deviationThreshold) gsiMembers
  return filteredMembers
  where
    checkDropoffDeviation currentDropoff currentRouteDist threshold member = do
      estimate <- QEstimate.findById member.gsiEstimateId
      case estimate >>= (.toLocation) of
        Just toLocation -> do
          let memberDropoff = LatLong toLocation.lat toLocation.lon
          
          -- Calculate distance between drop-offs
          dropoffDistance <- calculateDistance currentDropoff memberDropoff
          
          -- Get member's route distance
          memberRouteDistance <- getEstimateRouteDistance estimate
          
          -- Calculate deviation ratio using minimum ride distance
          let minRideDistance = min currentRouteDist memberRouteDistance
          let deviationRatio = (dropoffDistance.getMeters / minRideDistance.getMeters) * 100
          
          logInfo $ "Drop-off deviation ratio: " <> show deviationRatio <> " vs threshold: " <> show threshold
          return (deviationRatio <= threshold)
        Nothing -> return False

-- Helper to get route distance for estimate
getEstimateRouteDistance :: DEstimate.Estimate -> m Distance
getEstimateRouteDistance estimate = do
  -- Use cached route or calculate from pickup/dropoff
  case estimate.estimatedDistance of
    Just distance -> return distance
    Nothing -> calculateRouteDistance estimate.fromLocation estimate.toLocation
```

### 2. Route Overlap Analysis with Geo-hashing
```haskell
-- Primary route geo-hash generation
generatePrimaryRouteHashes :: 
  (MonadFlow m, EsqDBReplicaFlow m r) =>
  Id DSearchReq.SearchRequest -> 
  m (HashSet Text)
generatePrimaryRouteHashes searchRequestId = do
  -- Get cached route from Chunk 1 implementation
  routeData <- getRouteFromCache searchRequestId
  
  case routeData of
    Just route -> do
      -- Convert all route points to geo-hash precision 9
      let routePoints = extractRoutePoints route
      let geoHashes = map (geoHashPrecision9) routePoints
      return $ HashSet.fromList geoHashes
    Nothing -> do
      logError $ "No cached route found for searchRequestId: " <> searchRequestId.getId
      return HashSet.empty

-- Geo-hash utility function
geoHashPrecision9 :: LatLong -> Text
geoHashPrecision9 (LatLong lat lon) = do
  -- Use existing geo-hash library or implement
  geoHashEncode lat lon 9

-- Route overlap filtering
filterByRouteOverlap :: 
  (MonadFlow m, EsqDBReplicaFlow m r) =>
  SharedRideConfigs -> 
  HashSet Text -> -- Primary customer's route hashes
  [GSIMember] -> 
  m [GSIMember]
filterByRouteOverlap config primaryRouteHashes gsiMembers = do
  let overlapThreshold = config.routeOverlapThreshold -- x4 parameter
  
  filteredMembers <- filterM (checkRouteOverlap primaryRouteHashes overlapThreshold) gsiMembers
  return filteredMembers
  where
    checkRouteOverlap primaryHashes threshold member = do
      -- Get member's route data
      memberRouteHashes <- getMemberRouteHashes member.gsiEstimateId
      
      if HashSet.null memberRouteHashes
        then return False -- Reject if no route data
        else do
          -- Count overlapping geo-hashes
          let matchCount = HashSet.size $ HashSet.intersection primaryHashes memberRouteHashes
          let totalPoints = HashSet.size memberRouteHashes
          let overlapPercentage = (fromIntegral matchCount / fromIntegral totalPoints) * 100
          
          logInfo $ "Route overlap: " <> show overlapPercentage <> "% vs threshold: " <> show threshold
          return (overlapPercentage > threshold)

-- Get route hashes for member estimate
getMemberRouteHashes :: 
  (MonadFlow m, EsqDBReplicaFlow m r) =>
  Id DEstimate.Estimate -> 
  m (HashSet Text)
getMemberRouteHashes estimateId = do
  estimate <- QEstimate.findById estimateId
  case estimate of
    Just est -> do
      -- Get route from cache using estimate's searchRequestId
      memberRouteHashes <- generatePrimaryRouteHashes est.requestId
      return memberRouteHashes
    Nothing -> return HashSet.empty
```

### 3. Lock Acquisition Implementation
```haskell
-- Acquire locks on filtered estimates
acquireEstimateLocks :: 
  (CacheFlow m r) =>
  [GSIMember] -> 
  m [Id DEstimate.Estimate]
acquireEstimateLocks gsiMembers = do
  lockedEstimateIds <- catMaybes <$> mapM attemptLockAcquisition gsiMembers
  return lockedEstimateIds
  where
    attemptLockAcquisition member = do
      lockKey <- generateLockKey member.gsiEstimateId
      lockTtl <- return 300 -- 5 minutes in seconds
      
      -- Try to acquire lock with TTL
      lockAcquired <- Redis.setNX lockKey "locked" lockTtl
      
      if lockAcquired
        then do
          logInfo $ "Lock acquired for estimate: " <> member.gsiEstimateId.getId
          return $ Just member.gsiEstimateId
        else do
          logInfo $ "Lock acquisition failed for estimate: " <> member.gsiEstimateId.getId
          return Nothing

-- Lock key generation (consistent with Chunk 5)
generateLockKey :: Id DEstimate.Estimate -> Text
generateLockKey estimateId = "shared_ride_lock:" <> estimateId.getId
```

### 4. Preliminary Vehicle Capacity Check
```haskell
-- Basic capacity validation before passing to Chunk 7
validateBasicCapacity :: 
  VehicleCategory -> 
  Int -> -- Current customer's numSeats
  [Id DEstimate.Estimate] -> 
  m [Id DEstimate.Estimate]
validateBasicCapacity vehicleCategory currentSeats lockedEstimateIds = do
  case vehicleCategory of
    AUTO -> do
      -- For AUTO: can only pool with 1 other estimate
      let finalList = take 1 lockedEstimateIds
      when (length finalList < 1) $ 
        logInfo "No estimates available for AUTO pooling"
      return finalList
    
    CAB -> do
      -- For CAB: can pool with up to 2 other estimates
      -- Verify total seats don't exceed capacity
      validEstimates <- filterValidCabEstimates currentSeats lockedEstimateIds
      let finalList = take 2 validEstimates
      return finalList
    
    _ -> return [] -- Unsupported vehicle category

-- Validate cab estimates based on seat requirements
filterValidCabEstimates :: 
  (MonadFlow m, EsqDBReplicaFlow m r) =>
  Int -> 
  [Id DEstimate.Estimate] -> 
  m [Id DEstimate.Estimate]
filterValidCabEstimates currentSeats estimateIds = do
  let maxCabCapacity = 4
  let availableSeats = maxCabCapacity - currentSeats
  
  validEstimates <- filterM (checkEstimateSeats availableSeats) estimateIds
  return validEstimates
  where
    checkEstimateSeats availableSeats estimateId = do
      -- Extract seats from GSI or estimate data
      -- For now, assume we can get this from the cached GSI member data
      estimateSeats <- getEstimateSeatsFromCache estimateId
      return (estimateSeats <= availableSeats)
```

### 5. Main Chunk 6 Implementation
```haskell
-- Chunk 6 main function (called from Chunk 5)
chunk6AdvancedFiltering :: 
  (MonadFlow m, CacheFlow m r, EsqDBFlow m r, EsqDBReplicaFlow m r) =>
  SharedRideConfigs ->
  DSearchReq.SearchRequest ->
  DEstimate.Estimate ->
  Int ->
  [GSIMember] -> -- From Chunk 5
  m [Id DEstimate.Estimate] -- Locked estimateIds for Chunk 7
chunk6AdvancedFiltering config searchRequest estimate numSeats chunk5Results = do
  logInfo $ "Starting Chunk 6: Advanced filtering for " <> show (length chunk5Results) <> " estimates"
  
  -- Step 1: Advanced drop-off distance check
  currentDestination <- case searchRequest.toLocation of
    Just toLoc -> return $ LatLong toLoc.lat toLoc.lon
    Nothing -> throwError (InvalidRequest "Destination required for shared rides")
  
  dropoffFilteredResults <- filterByDropoffDeviation config currentDestination chunk5Results
  logInfo $ "After drop-off deviation filter: " <> show (length dropoffFilteredResults) <> " estimates"
  
  -- Step 2: Route overlap analysis
  primaryRouteHashes <- generatePrimaryRouteHashes searchRequest.id
  
  if HashSet.null primaryRouteHashes
    then do
      logError "No route data available for overlap analysis"
      return []
    else do
      routeFilteredResults <- filterByRouteOverlap config primaryRouteHashes dropoffFilteredResults
      logInfo $ "After route overlap filter: " <> show (length routeFilteredResults) <> " estimates"
      
      -- Step 3: Acquire locks on compatible estimates
      lockedEstimateIds <- acquireEstimateLocks routeFilteredResults
      logInfo $ "Successfully locked " <> show (length lockedEstimateIds) <> " estimates"
      
      -- Step 4: Basic capacity validation
      capacityValidatedIds <- validateBasicCapacity searchRequest.vehicleCategory numSeats lockedEstimateIds
      logInfo $ "After capacity validation: " <> show (length capacityValidatedIds) <> " estimates"
      
      if null capacityValidatedIds
        then do
          -- Release all acquired locks if capacity check fails
          mapM_ releaseLock lockedEstimateIds
          logInfo "Released all locks due to capacity validation failure"
          return []
        else do
          logInfo $ "Chunk 6 complete. Passing " <> show (length capacityValidatedIds) <> " estimates to Chunk 7"
          return capacityValidatedIds

-- Helper to release locks
releaseLock :: (CacheFlow m r) => Id DEstimate.Estimate -> m ()
releaseLock estimateId = do
  lockKey <- generateLockKey estimateId
  Redis.del [lockKey]
  logInfo $ "Released lock for estimate: " <> estimateId.getId
```

## Helper Functions and Utilities

### Route Data Management
```haskell
-- Get route from cache (leveraging Chunk 1 implementation)
getRouteFromCache :: Id DSearchReq.SearchRequest -> m (Maybe RouteResponse)
getRouteFromCache searchRequestId = do
  let cacheKey = "route_cache:" <> searchRequestId.getId
  routeData <- Redis.get cacheKey
  case routeData of
    Just json -> return $ decode json
    Nothing -> return Nothing

-- Extract coordinate points from route response
extractRoutePoints :: RouteResponse -> [LatLong]
extractRoutePoints routeResponse = do
  -- Parse route geometry and extract coordinate points
  -- Implementation depends on route response format
  parseRouteGeometry routeResponse.geometry
```

### Configuration Parameters
```haskell
-- Chunk 6 specific config parameters
data Chunk6Config = Chunk6Config
  { routeMatchingThreshold :: Double      -- x3: drop-off deviation ratio threshold
  , routeOverlapThreshold :: Double       -- x4: route overlap percentage threshold  
  , geoHashPrecisionForRouteMatching :: Int -- Precision 9 for geo-hashing
  }
```

## Testing Strategy

### Unit Tests
1. **Drop-off Deviation Tests**: Various distance combinations and thresholds
2. **Geo-hash Overlap Tests**: Route patterns with known overlap percentages
3. **Lock Acquisition Tests**: Concurrent access and failure scenarios
4. **Capacity Validation Tests**: Different vehicle types and seat combinations

### Integration Tests
1. **End-to-End Chunk 6**: Complete filtering pipeline
2. **Lock Lifecycle Tests**: Acquisition, usage, and cleanup
3. **Route Cache Integration**: With Chunk 1 route caching
4. **Multi-Customer Scenarios**: Complex pooling with multiple candidates

### Performance Tests
1. **Geo-hash Generation**: Large routes with many points
2. **HashSet Operations**: Overlap calculation efficiency
3. **Lock Contention**: High-concurrency lock acquisition
4. **Memory Usage**: Route hash storage for multiple customers

## Error Handling

### Expected Scenarios
```haskell
data Chunk6Error 
  = RouteDataMissing (Id DSearchReq.SearchRequest)
  | GeoHashGenerationFailed
  | LockAcquisitionTimeout (Id DEstimate.Estimate)
  | CapacityValidationFailed
  | RouteOverlapCalculationError
```

### Graceful Degradation
- Missing route data → skip route overlap check, continue with other filters
- Geo-hash failures → reject that estimate, continue with others
- Lock failures → exclude that estimate, continue with remaining
- Capacity failures → release all locks, return empty result

## Performance Optimizations

### Efficiency Strategies
1. **Batch Geo-hash Generation**: Process multiple routes in parallel
2. **HashSet Pre-sizing**: Optimize memory allocation for large routes
3. **Lock Batching**: Acquire multiple locks in single Redis transaction
4. **Early Termination**: Stop processing when capacity constraints are met

### Caching Optimizations
1. **Route Hash Caching**: Cache geo-hashes for frequently accessed routes
2. **Lock State Caching**: Track lock acquisition attempts
3. **Capacity Calculation Caching**: Pre-compute seat availability

## Cross-Chunk Dependencies

### Input from Chunk 5
- Filtered `GSIMember` list with valid estimates
- Customer's pickup/dropoff locations
- Configuration parameters and thresholds

### Output to Chunk 7
- Lock-acquired `estimateIds` ready for final validation
- Route overlap context for detailed analysis
- Lock keys for cleanup if final validation fails

### Fallback Behavior
- Empty result → Chunk 5 will handle GSI addition for async pooling
- Partial failures → Continue with successfully processed estimates
- Lock conflicts → Graceful degradation with available estimates

## Implementation Priority

1. **Phase 1**: Drop-off deviation filtering and basic route overlap
2. **Phase 2**: Lock acquisition and capacity validation
3. **Phase 3**: Performance optimization and error handling
4. **Phase 4**: Advanced geo-hashing and caching strategies

---

**Status**: Ready for Implementation  
**Dependencies**: Chunk 5 completed ✅, Route caching from Chunk 1 ✅  
**Next Step**: Implement advanced filtering logic extending handleSyncRiderPooling function