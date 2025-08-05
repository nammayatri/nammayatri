# Chunk 7 Implementation Plan: Final Grouping and SharedEntity Creation

## Overview
This document outlines the implementation plan for Chunk 7 of the shared ride feature, focusing on **final route overlap validation, capacity checks, and SharedEntity creation** for successful matches.

## Flow Summary
Chunk 7 implements the **"Final Validation and SharedEntity Creation"** sequence:
1. **Enhanced Route Overlap Analysis** - Detailed geo-hash analysis with precise overlap calculation
2. **Final Vehicle Capacity Validation** - Strict capacity rules based on vehicle type
3. **SharedEntity Creation** - Create unified entity linking all matched estimates
4. **Return Success** - Return SharedEntity ID for batch creation (Chunk 3)

**Input**: Lock-acquired `estimateIds` from Chunk 6, route overlap context
**Output**: `SharedEntity ID` for successful match OR `Nothing` for no valid grouping

## 🔴 IMPORTANT CLARIFICATIONS

### Final Route Overlap Analysis
- **Enhanced Precision**: More detailed geo-hash analysis than Chunk 6 preliminary check
- **Exact Calculation**: `((matchCnt) / (total number of points in estimate's route)) * 100 > x4`
- **Per-Estimate Validation**: Each estimate must individually meet overlap threshold
- **Rejection Handling**: Failed estimates are removed from consideration

### Vehicle Capacity Rules (From JSON Analysis)
- **AUTO**: Final list size must == 1 (only current + 1 other customer)
- **CAR**: Final list size must == 2 AND sum of `numSeats` ≤ remaining capacity
- **Break Logic**: If conditions met → create SharedEntity; else → continue processing
- **Exhaustive Search**: Try different combinations until valid group found

### SharedEntity Creation Strategy
- **OVERLAPPING Type**: Use existing SharedEntity with OVERLAPPING entity type
- **Waypoint Management**: Link all pickup/dropoff locations for route optimization
- **Status Tracking**: Set initial status to allow batch creation in Chunk 3

## ✅ Implementation Location

### Final Extension of handleSyncRiderPooling Function
- **File**: `Backend/app/rider-platform/rider-app/Main/src/Domain/Action/UI/Select.hs:593-610`
- **Integration Point**: Continue from Chunk 6 locked results
- **Success Path**: Return `Just sharedEntityId` → Chunk 3 batch creation
- **Failure Path**: Return `Nothing` → Chunk 2 GSI addition for async pooling

```haskell
-- Final extension completing the pooling logic
chunk7FinalValidation :: 
  (MonadFlow m, CacheFlow m r, EsqDBFlow m r, EsqDBReplicaFlow m r) =>
  SharedRideConfigs ->
  DSearchReq.SearchRequest ->
  DEstimate.Estimate ->
  Int ->
  [Id DEstimate.Estimate] -> -- From Chunk 6
  m (Maybe (Id DSE.SharedEntity))
```

## Detailed Implementation Plan

### 1. Enhanced Route Overlap Analysis
```haskell
-- Final detailed route overlap validation
validateDetailedRouteOverlap :: 
  (MonadFlow m, EsqDBReplicaFlow m r) =>
  SharedRideConfigs -> 
  HashSet Text -> -- Primary customer's route hashes
  [Id DEstimate.Estimate] -> 
  m [Id DEstimate.Estimate]
validateDetailedRouteOverlap config primaryRouteHashes estimateIds = do
  let overlapThreshold = config.routeOverlapThreshold -- x4 parameter
  
  validEstimates <- filterM (validateIndividualOverlap primaryRouteHashes overlapThreshold) estimateIds
  return validEstimates
  where
    validateIndividualOverlap primaryHashes threshold estimateId = do
      -- Get detailed route data for this estimate
      memberRouteHashes <- getMemberRouteHashesDetailed estimateId
      
      if HashSet.null memberRouteHashes
        then do
          logError $ "No route data for estimate: " <> estimateId.getId
          return False
        else do
          -- Calculate exact overlap percentage
          let matchCount = HashSet.size $ HashSet.intersection primaryHashes memberRouteHashes
          let totalPoints = HashSet.size memberRouteHashes
          let overlapPercentage = (fromIntegral matchCount / fromIntegral totalPoints) * 100
          
          logInfo $ "Final overlap check for " <> estimateId.getId <> ": " <> show overlapPercentage <> "% vs " <> show threshold
          
          if overlapPercentage > threshold
            then do
              logInfo $ "Estimate " <> estimateId.getId <> " passed final overlap check"
              return True
            else do
              logInfo $ "Estimate " <> estimateId.getId <> " rejected in final overlap check"
              return False

-- Enhanced route hash generation for final validation
getMemberRouteHashesDetailed :: 
  (MonadFlow m, EsqDBReplicaFlow m r) =>
  Id DEstimate.Estimate -> 
  m (HashSet Text)
getMemberRouteHashesDetailed estimateId = do
  estimate <- QEstimate.findById estimateId
  case estimate of
    Just est -> do
      -- Get high-precision route data
      routeData <- getDetailedRouteFromCache est.requestId
      case routeData of
        Just route -> do
          let routePoints = extractDetailedRoutePoints route
          let geoHashes = map (geoHashPrecision9) routePoints
          return $ HashSet.fromList geoHashes
        Nothing -> do
          logError $ "No detailed route data for estimate: " <> estimateId.getId
          return HashSet.empty
    Nothing -> return HashSet.empty
```

### 2. Final Vehicle Capacity Validation with Exhaustive Search
```haskell
-- Final capacity validation with combination testing
findValidGrouping :: 
  (MonadFlow m, EsqDBReplicaFlow m r) =>
  VehicleCategory -> 
  Int -> -- Current customer's numSeats
  [Id DEstimate.Estimate] -> 
  m (Maybe [Id DEstimate.Estimate])
findValidGrouping vehicleCategory currentSeats estimateIds = do
  case vehicleCategory of
    AUTO -> validateAutoGrouping estimateIds
    CAB -> validateCabGrouping currentSeats estimateIds
    _ -> do
      logError $ "Unsupported vehicle category: " <> show vehicleCategory
      return Nothing

-- AUTO validation: exactly 1 other customer
validateAutoGrouping :: 
  [Id DEstimate.Estimate] -> 
  m (Maybe [Id DEstimate.Estimate])
validateAutoGrouping estimateIds = do
  case estimateIds of
    [singleEstimate] -> do
      logInfo $ "AUTO grouping successful with estimate: " <> singleEstimate.getId
      return $ Just [singleEstimate]
    [] -> do
      logInfo "No estimates available for AUTO grouping"
      return Nothing
    multiple -> do
      logInfo $ "Too many estimates for AUTO: " <> show (length multiple) <> ", taking first one"
      return $ Just [head multiple]

-- CAB validation: up to 2 customers with seat constraints
validateCabGrouping :: 
  (MonadFlow m, EsqDBReplicaFlow m r) =>
  Int -> 
  [Id DEstimate.Estimate] -> 
  m (Maybe [Id DEstimate.Estimate])
validateCabGrouping currentSeats estimateIds = do
  let maxCabCapacity = 4
  let availableSeats = maxCabCapacity - currentSeats
  
  -- Try different combinations to find valid grouping
  validGrouping <- findValidCabCombination availableSeats estimateIds
  case validGrouping of
    Just group -> do
      logInfo $ "CAB grouping successful with " <> show (length group) <> " estimates"
      return $ Just group
    Nothing -> do
      logInfo "No valid CAB grouping found within capacity constraints"
      return Nothing

-- Find valid combination of estimates that fit in remaining capacity
findValidCabCombination :: 
  (MonadFlow m, EsqDBReplicaFlow m r) =>
  Int -> -- Available seats
  [Id DEstimate.Estimate] -> 
  m (Maybe [Id DEstimate.Estimate])
findValidCabCombination availableSeats estimateIds = do
  -- Get seat requirements for each estimate
  estimateSeats <- mapM getEstimateSeats estimateIds
  let estimatesWithSeats = zip estimateIds estimateSeats
  
  -- Try combinations: single estimate first, then pairs
  singleResult <- findSingleEstimateMatch availableSeats estimatesWithSeats
  case singleResult of
    Just match -> return $ Just match
    Nothing -> findPairEstimateMatch availableSeats estimatesWithSeats

-- Try to find single estimate that fits
findSingleEstimateMatch :: 
  Int -> 
  [(Id DEstimate.Estimate, Int)] -> 
  m (Maybe [Id DEstimate.Estimate])
findSingleEstimateMatch availableSeats estimatesWithSeats = do
  let validSingle = find (\(_, seats) -> seats <= availableSeats) estimatesWithSeats
  case validSingle of
    Just (estimateId, seats) -> do
      logInfo $ "Found single estimate match: " <> estimateId.getId <> " with " <> show seats <> " seats"
      return $ Just [estimateId]
    Nothing -> return Nothing

-- Try to find pair of estimates that fit together
findPairEstimateMatch :: 
  Int -> 
  [(Id DEstimate.Estimate, Int)] -> 
  m (Maybe [Id DEstimate.Estimate])
findPairEstimateMatch availableSeats estimatesWithSeats = do
  let pairs = [(e1, e2) | e1 <- estimatesWithSeats, e2 <- estimatesWithSeats, fst e1 /= fst e2]
  validPair <- find (\((_, s1), (_, s2)) -> s1 + s2 <= availableSeats) pairs
  case validPair of
    Just ((id1, s1), (id2, s2)) -> do
      logInfo $ "Found pair match: " <> id1.getId <> " (" <> show s1 <> ") + " <> id2.getId <> " (" <> show s2 <> ")"
      return $ Just [id1, id2]
    Nothing -> return Nothing

-- Get seat count for estimate (from GSI cache or estimate data)
getEstimateSeats :: 
  (MonadFlow m, EsqDBReplicaFlow m r) =>
  Id DEstimate.Estimate -> 
  m Int
getEstimateSeats estimateId = do
  -- Try to get from cached GSI data first
  cachedSeats <- getEstimateSeatsFromGSICache estimateId
  case cachedSeats of
    Just seats -> return seats
    Nothing -> do
      -- Fallback: assume 1 seat per estimate if no data available
      logWarning $ "No seat data found for estimate: " <> estimateId.getId <> ", assuming 1 seat"
      return 1
```

### 3. SharedEntity Creation
```haskell
-- Create SharedEntity for successful match
createSharedEntityForMatch :: 
  (MonadFlow m, EsqDBFlow m r) =>
  DSearchReq.SearchRequest -> 
  DEstimate.Estimate -> 
  [Id DEstimate.Estimate] -> 
  m (Id DSE.SharedEntity)
createSharedEntityForMatch searchRequest currentEstimate matchedEstimateIds = do
  now <- getCurrentTime
  sharedEntityId <- Id <$> generateGUID
  
  -- Collect all waypoints for route optimization
  allWaypoints <- collectWaypoints currentEstimate matchedEstimateIds
  
  -- Create SharedEntity with OVERLAPPING type
  let sharedEntity = DSE.SharedEntity
        { id = sharedEntityId
        , entityType = DSE.OVERLAPPING
        , waypoints = allWaypoints
        , status = DSE.ACTIVE
        , trackedEntities = [] -- Will be populated during batch creation
        , merchantId = searchRequest.merchantId
        , merchantOperatingCityId = searchRequest.merchantOperatingCityId
        , createdAt = now
        , updatedAt = now
        }
  
  -- Store SharedEntity in database
  QSharedEntity.create sharedEntity
  
  logInfo $ "Created SharedEntity: " <> sharedEntityId.getId <> " with " <> show (length matchedEstimateIds + 1) <> " estimates"
  return sharedEntityId

-- Collect waypoints from all estimates for route optimization
collectWaypoints :: 
  (MonadFlow m, EsqDBReplicaFlow m r) =>
  DEstimate.Estimate -> 
  [Id DEstimate.Estimate] -> 
  m [DSE.Waypoint]
collectWaypoints currentEstimate matchedEstimateIds = do
  -- Add current customer's waypoints
  currentWaypoints <- extractWaypointsFromEstimate currentEstimate
  
  -- Add matched customers' waypoints
  matchedWaypoints <- mapM (getEstimateWaypoints) matchedEstimateIds
  
  let allWaypoints = currentWaypoints ++ concat matchedWaypoints
  return allWaypoints

-- Extract waypoints from estimate
extractWaypointsFromEstimate :: DEstimate.Estimate -> m [DSE.Waypoint]
extractWaypointsFromEstimate estimate = do
  -- Create pickup waypoint
  let pickupWaypoint = DSE.Waypoint
        { location = estimate.fromLocation
        , waypointType = DSE.PICKUP
        , estimateId = Just estimate.id
        }
  
  -- Create dropoff waypoint if available
  dropoffWaypoints <- case estimate.toLocation of
    Just toLocation -> return [DSE.Waypoint
      { location = toLocation
      , waypointType = DSE.DROPOFF
      , estimateId = Just estimate.id
      }]
    Nothing -> return []
  
  return $ pickupWaypoint : dropoffWaypoints
```

### 4. Main Chunk 7 Implementation
```haskell
-- Complete Chunk 7 implementation
chunk7FinalValidation :: 
  (MonadFlow m, CacheFlow m r, EsqDBFlow m r, EsqDBReplicaFlow m r) =>
  SharedRideConfigs ->
  DSearchReq.SearchRequest ->
  DEstimate.Estimate ->
  Int ->
  [Id DEstimate.Estimate] -> -- From Chunk 6
  m (Maybe (Id DSE.SharedEntity))
chunk7FinalValidation config searchRequest estimate numSeats lockedEstimateIds = do
  logInfo $ "Starting Chunk 7: Final validation for " <> show (length lockedEstimateIds) <> " locked estimates"
  
  if null lockedEstimateIds
    then do
      logInfo "No locked estimates from Chunk 6, returning Nothing"
      return Nothing
    else do
      -- Step 1: Enhanced route overlap analysis
      primaryRouteHashes <- generatePrimaryRouteHashesDetailed searchRequest.id
      
      if HashSet.null primaryRouteHashes
        then do
          logError "No route data available for final overlap analysis"
          releaseAllLocks lockedEstimateIds
          return Nothing
        else do
          overlapValidatedIds <- validateDetailedRouteOverlap config primaryRouteHashes lockedEstimateIds
          logInfo $ "After final overlap check: " <> show (length overlapValidatedIds) <> " estimates"
          
          -- Step 2: Final vehicle capacity validation with exhaustive search
          validGrouping <- findValidGrouping searchRequest.vehicleCategory numSeats overlapValidatedIds
          
          case validGrouping of
            Nothing -> do
              logInfo "No valid grouping found, releasing all locks"
              releaseAllLocks lockedEstimateIds
              return Nothing
            
            Just finalEstimateIds -> do
              logInfo $ "Valid grouping found with " <> show (length finalEstimateIds) <> " estimates"
              
              -- Step 3: Create SharedEntity for successful match
              sharedEntityId <- createSharedEntityForMatch searchRequest estimate finalEstimateIds
              
              -- Step 4: Keep locks for batch creation, release unused locks
              let unusedLocks = lockedEstimateIds \\ finalEstimateIds
              mapM_ releaseLock unusedLocks
              
              logInfo $ "Chunk 7 complete. SharedEntity created: " <> sharedEntityId.getId
              return $ Just sharedEntityId

-- Release all acquired locks
releaseAllLocks :: (CacheFlow m r) => [Id DEstimate.Estimate] -> m ()
releaseAllLocks estimateIds = do
  mapM_ releaseLock estimateIds
  logInfo $ "Released " <> show (length estimateIds) <> " locks"

-- Enhanced primary route hash generation for final analysis
generatePrimaryRouteHashesDetailed :: 
  (MonadFlow m, EsqDBReplicaFlow m r) =>
  Id DSearchReq.SearchRequest -> 
  m (HashSet Text)
generatePrimaryRouteHashesDetailed searchRequestId = do
  routeData <- getDetailedRouteFromCache searchRequestId
  case routeData of
    Just route -> do
      let routePoints = extractDetailedRoutePoints route
      let geoHashes = map (geoHashPrecision9) routePoints
      logInfo $ "Generated " <> show (length geoHashes) <> " geo-hashes for primary route"
      return $ HashSet.fromList geoHashes
    Nothing -> do
      logError $ "No detailed route data for searchRequestId: " <> searchRequestId.getId
      return HashSet.empty

-- Enhanced route point extraction with higher precision
extractDetailedRoutePoints :: RouteResponse -> [LatLong]
extractDetailedRoutePoints routeResponse = do
  -- Parse route geometry with more granular point extraction
  -- Include intermediate points for better overlap analysis
  parseDetailedRouteGeometry routeResponse.geometry
```

### 5. Complete handleSyncRiderPooling Integration
```haskell
-- Final complete implementation of handleSyncRiderPooling
handleSyncRiderPooling :: 
  (MonadFlow m, CacheFlow m r, EsqDBFlow m r, EsqDBReplicaFlow m r) => 
  SharedRideConfigs -> 
  DSearchReq.SearchRequest -> 
  DEstimate.Estimate -> 
  Int -> 
  m (Maybe (Id DSE.SharedEntity))
handleSyncRiderPooling config searchRequest estimate numSeats = do
  logInfo $ "Starting complete rider pooling logic for estimate: " <> estimate.id.getId
  
  -- Chunk 5: GSI query and initial filtering
  chunk5Results <- chunk5InitialFiltering config searchRequest estimate numSeats
  
  if null chunk5Results
    then do
      logInfo "No compatible riders found in Chunk 5"
      return Nothing
    else do
      -- Chunk 6: Advanced filtering and lock acquisition
      chunk6Results <- chunk6AdvancedFiltering config searchRequest estimate numSeats chunk5Results
      
      if null chunk6Results
        then do
          logInfo "No compatible riders found in Chunk 6"
          return Nothing
        else do
          -- Chunk 7: Final validation and SharedEntity creation
          chunk7Result <- chunk7FinalValidation config searchRequest estimate numSeats chunk6Results
          
          case chunk7Result of
            Nothing -> do
              logInfo "No valid grouping found in Chunk 7"
              return Nothing
            Just sharedEntityId -> do
              logInfo $ "Rider pooling successful! SharedEntity: " <> sharedEntityId.getId
              return $ Just sharedEntityId
```

## Helper Functions and Utilities

### Enhanced Route Processing
```haskell
-- Get detailed route with higher precision
getDetailedRouteFromCache :: Id DSearchReq.SearchRequest -> m (Maybe RouteResponse)
getDetailedRouteFromCache searchRequestId = do
  -- Same as Chunk 6 but with enhanced error handling
  routeData <- getRouteFromCache searchRequestId
  case routeData of
    Just route -> return $ Just route
    Nothing -> do
      logError $ "Missing route data for final analysis: " <> searchRequestId.getId
      return Nothing

-- Extract more granular route points for final analysis
parseDetailedRouteGeometry :: RouteGeometry -> [LatLong]
parseDetailedRouteGeometry geometry = do
  -- Enhanced point extraction with intermediate waypoints
  -- Include points between major waypoints for better overlap detection
  extractGranularPoints geometry
```

## Testing Strategy

### Unit Tests
1. **Route Overlap Precision**: Exact percentage calculations
2. **Capacity Validation**: All vehicle types and seat combinations
3. **SharedEntity Creation**: Valid entity structure and waypoints
4. **Lock Management**: Cleanup and release scenarios

### Integration Tests
1. **Complete Pooling Flow**: Chunks 5→6→7 integration
2. **SharedEntity to Batch**: Chunk 7→3 handoff
3. **Error Recovery**: Failure at each stage
4. **Concurrent Pooling**: Multiple customers simultaneously

### Performance Tests
1. **Large Route Analysis**: Complex routes with many points
2. **Combination Search**: Multiple estimate groupings
3. **Lock Contention**: High-concurrency scenarios
4. **Memory Usage**: SharedEntity creation overhead

## Error Handling

### Expected Scenarios
```haskell
data Chunk7Error 
  = DetailedRouteAnalysisFailed (Id DSearchReq.SearchRequest)
  | CapacityValidationFailed
  | SharedEntityCreationFailed
  | LockManagementError
  | InvalidGroupingConfiguration
```

### Failure Recovery
- Route analysis failures → release locks, return Nothing
- Capacity validation failures → try different combinations
- SharedEntity creation failures → release locks, log error
- Any failures → ensure lock cleanup

## Cross-Chunk Dependencies

### Input from Chunks 5-6
- Lock-acquired `estimateIds` ready for final validation
- Route overlap context and configuration
- Customer location and vehicle category data

### Output to Chunk 3
- `SharedEntity ID` for successful batch creation
- Locked estimates remain locked for batch processing
- Waypoint data for route optimization

### Fallback Integration
- Failure → Return `Nothing` to Chunk 2
- Chunk 2 will add customer to GSI for async pooling
- Locks are cleaned up automatically

## Implementation Priority

1. **Phase 1**: Final route overlap and capacity validation
2. **Phase 2**: SharedEntity creation and waypoint management
3. **Phase 3**: Integration testing and error handling
4. **Phase 4**: Performance optimization and monitoring

---

**Status**: Ready for Implementation  
**Dependencies**: Chunks 5-6 completed ✅, SharedEntity table ✅, Lock management ✅  
**Next Step**: Complete the rider pooling logic implementation in handleSyncRiderPooling function