# Chunk 3 Implementation Plan: Batch Creation and Driver Pooling

## 🔥 IMPORTANT ARCHITECTURAL CHANGE

**Unified SharedEntity Design**: This implementation uses a **unified `shared_entity` table** instead of separate tables for shared ride components. The previous design with separate `shared_search_request`, `shared_estimate`, `shared_booking`, etc. tables has been **replaced** with a single unified entity approach.

### Current SharedEntity Schema:
```sql
CREATE TABLE shared_entity (
    id TEXT PRIMARY KEY,
    status TEXT NOT NULL, -- SharedEntityActive, SharedEntityCancelled, SharedEntityOfferedQuote
    entity_type TEXT NOT NULL, -- SEARCH_GROUP, ESTIMATE_GROUP, BOOKING_GROUP, RIDE_GROUP
    search_request_ids JSONB NOT NULL DEFAULT '[]', -- Array of TrackedEntity {entityId: Text, isActive: Bool}
    estimate_ids JSONB NOT NULL DEFAULT '[]', -- Array of TrackedEntity {entityId: Text, isActive: Bool}
    booking_ids JSONB NOT NULL DEFAULT '[]', -- Array of TrackedEntity {entityId: Text, isActive: Bool}
    ride_ids JSONB NOT NULL DEFAULT '[]', -- Array of TrackedEntity {entityId: Text, isActive: Bool}
    merchant_id TEXT NOT NULL,
    merchant_operating_city_id TEXT NOT NULL,
    vehicle_category TEXT NOT NULL,
    trip_category TEXT NOT NULL, -- RideShare
    driver_id TEXT, -- Assigned driver (nullable)
    pairing_time TIMESTAMP, -- When entities were grouped (nullable)
    created_at TIMESTAMP NOT NULL DEFAULT NOW(),
    updated_at TIMESTAMP NOT NULL DEFAULT NOW()
);
```

### Key Benefits:
- **Single Source of Truth**: All shared ride data in one unified table
- **Simplified Queries**: No complex joins across multiple shared tables
- **Flexible Entity Tracking**: TrackedEntity arrays can track any type of entity with active/inactive states
- **Consistent Status Management**: Single status field for the entire shared ride batch
- **Easy Lifecycle Management**: One entity to track from search → estimate → booking → ride

---

## Overview
This document outlines the implementation plan for Chunk 3 of the shared ride feature, focusing on batch creation and driver pooling logic within the `select2` API handler.

## 🔴 IMPORTANT TERMINOLOGY
- **sharedSearchRequestIds**: NOT batchIds - refers to the grouped search request IDs from rider pooling
- **sharedEstimate**: NOT cumulative_estimate - the combined estimate for all pooled riders
- **Sync Logic**: All batch creation and driver pooling happens synchronously in select2 API handler

## Implementation Flow in Select2 API

### 1. Pre-Batching Validation (Step 3.1)
- **Location**: `Backend/app/rider-platform/rider-app/Main/src/Domain/Action/UI/Select.hs` - in `select2` function
- **Timing**: After basic select validation, before creating sharedEstimate
- **Logic**: Validate estimate status and paired customers from rider pooling

### 2. Shared Estimate Creation (Step 3.2) 
- **Location**: Same `select2` function in `Select.hs`
- **Timing**: After pre-batching validation passes
- **Logic**: Create sharedEstimate record for all pooled riders

### 3. Driver Pooling Integration (Step 3.3)
- **Location**: Same `select2` function, after sharedEstimate creation
- **Logic**: Call provider-platform select API with sharedEstimate data

---

## Detailed Implementation

### 3.1 & 3.2 Combined: Pre-Batching Validation and Shared Records Creation

**Location**: `Backend/app/rider-platform/rider-app/Main/src/Domain/Action/UI/Select.hs:240-318`

**Implementation**:
```haskell
-- Add after basic estimate validation (around line 244)
-- Store original values with primes for immutability
let estimate' = estimate
    remainingEstimateBppIds' = remainingEstimateBppIds

-- Check if this is a shared ride estimate and config allows sync mode
sharedRideConfig <- QSharedRideConfig.findByMerchantOpCityIdAndType 
                      searchRequest.merchantOperatingCityId OverlappingRoute
let isAsyncEnabled = maybe True (.asyncEnabled) sharedRideConfig -- default to True if not configured

(estimate, remainingEstimateBppIds) <- 
  if (estimate'.tripCategory == Just (Trip.RideShare _) && not isAsyncEnabled)
    then do
      -- Shared ride sync mode logic
      pairedEstimateIds <- getPooledRiders personId estimateId
      
      -- Fetch all estimates once (including current estimate)
      allEstimateIds <- return $ estimateId : pairedEstimateIds
      allEstimates <- catMaybes <$> mapM QEstimate.findById allEstimateIds
      
      -- Pre-batching validation using fetched estimates
      let validEstimates = filter (\est -> est.status /= DEstimate.Cancelled) allEstimates
      let validCount = length validEstimates
      
      -- Validate threshold requirement
      let minThreshold = maybe 2 (.minRidersThreshold) sharedRideConfig
      
      when (validCount < minThreshold) $ do
        -- Release locks on paired estimate IDs
        releasePairedCustomerLocks pairedEstimateIds
        throwError $ InvalidRequest "Insufficient valid paired customers for shared ride"
      
      -- Pre-batching validation passed, create shared entity
      now <- getCurrentTime
      
      -- Create unified shared_entity record
      sharedEntityId <- Id <$> generateGUID
      let allSearchRequestIds = map (.requestId) validEstimates -- Get search request IDs from estimates
      let trackedSearchRequests = map (\srId -> TrackedEntity { entityId = srId.getId, isActive = True }) allSearchRequestIds
      let trackedEstimates = map (\est -> TrackedEntity { entityId = est.id.getId, isActive = True }) validEstimates
      
      let sharedEntity = SharedEntity
            { id = sharedEntityId,
              status = SharedEntityActive,
              entityType = ESTIMATE_GROUP,
              searchRequestIds = trackedSearchRequests,
              estimateIds = trackedEstimates,
              bookingIds = [],
              rideIds = [],
              merchantId = searchRequest.merchantId,
              merchantOperatingCityId = searchRequest.merchantOperatingCityId,
              vehicleCategory = estimate'.vehicleCategory,
              tripCategory = RideShare,
              driverId = Nothing,
              pairingTime = Just now,
              createdAt = now,
              updatedAt = now
            }
      
      -- Store shared entity
      QSharedEntity.create sharedEntity
      
      -- Update all estimates to reference the shared entity
      let allEstimateIds = map (.id) validEstimates
      QEstimate.updateAll allEstimateIds (\est -> est { sharedEntityId = Just sharedEntityId })
      
      -- Update all search requests to reference the shared entity
      QSearchRequest.updateAll allSearchRequestIds (\sr -> sr { sharedEntityId = Just sharedEntityId })
      
      -- Return values for shared ride case
      return (head validEstimates, map (.bppEstimateId) (tail validEstimates))
      
    else 
      -- Non-shared ride case - use original values
      return (estimate', remainingEstimateBppIds')

-- Continue with existing select2 logic - variables now set correctly for both cases

where
  getPickupLocation :: DEstimate.Estimate -> Location
  getDropoffLocation :: DEstimate.Estimate -> Location  
  calculateCombinedFare :: [DEstimate.Estimate] -> Money
```

**Dependencies**:
- Import `qualified Domain.Types.Trip as Trip`
- Import `Storage.CachedQueries.SharedRideConfig as QSharedRideConfig`
- New domain types: `Domain.Types.SharedSearchRequest`, `Domain.Types.SharedEstimate`
- New storage queries: `Storage.Queries.SharedSearchRequest as QSharedSearchRequest`, `Storage.Queries.SharedEstimate as QSharedEstimate`
- Helper functions: `getPooledRiders`, `releasePairedCustomerLocks`
- **Estimate table update**: Add `sharedEstimateId :: Maybe (Id SharedEstimate)` field to reference shared estimates
- **Storage query**: `QEstimate.updateSharedEstimateId :: Maybe (Id SharedEstimate) -> Id Estimate -> m ()`

### 3.3 Driver Pooling Integration

**Note**: Driver pooling is automatically handled by the existing `CallBPP.selectV2` call in the select2 API flow. The provider platform will detect shared rides based on the descriptor code in Beckn ACL.

### 3.4 Beckn ACL Select Modification

**Location**: `Backend/app/rider-platform/rider-app/Main/src/Beckn/ACL/Select.hs:194` - in `mkOtheEstimatesTagGroup` function

**Current Code**:
```haskell
-- Line 194 - needs modification
descriptorCode = Tags.OTHER_SELECT_ESTIMATES
```

**Modified Code**:
```haskell
-- Check if primary estimate has sharedEstimateId to determine descriptor
descriptorCode = case res.estimate.sharedEstimateId of
  Just _ -> Tags.SHAREDRIDE_SELECT_ESTIMATES  -- Shared ride
  Nothing -> Tags.OTHER_SELECT_ESTIMATES      -- Regular ride
```

**New Enum Addition**:
Add `SHAREDRIDE_SELECT_ESTIMATES` to the Tags enum type (wherever Tags is defined)

**Dependencies**:
- Add `SHAREDRIDE_SELECT_ESTIMATES` enum to Tags type definition
- Ensure `estimate` has `sharedEstimateId` field available in Beckn ACL context

### 3.5 Provider Platform - Driver Side Beckn ACL Select Modification

**Location**: `Backend/app/provider-platform/dynamic-offer-driver-app/Main/src/Beckn/ACL/Select.hs:68+`

**Implementation**:
```haskell
-- Add after line 68 (after existing getBookAnyEstimates call)
bookAnyEstimates <- getBookAnyEstimates item.itemTags
sharedRideRemEstimates <- getSharedRideEstimates item.itemTags

-- Mutual exclusion check - both cannot happen at the same time
let (finalBookAnyEstimates, finalSharedRideEstimates) = 
      if not (null sharedRideRemEstimates)
        then ([], sharedRideRemEstimates)  -- Shared ride case
        else (bookAnyEstimates, [])        -- Regular ride case

-- Update line 86 to append both estimate lists
let estimateIds = map (.estimateId) estimates <> finalBookAnyEstimates <> finalSharedRideEstimates
```

**New Function to Add**:
```haskell
-- Add this function (similar to getBookAnyEstimates but for shared rides)
getSharedRideEstimates :: (MonadFlow m, Log m) => [TG.TagGroup] -> m [Text]
getSharedRideEstimates itemTags = do
  let tagGroups = filter (\tag -> tag.tagGroupDescriptor.code == show Tags.SHAREDRIDE_SELECT_ESTIMATES) itemTags
  pure $ concatMap (map (.tagValue) . (.tagGroupList)) tagGroups
```

**Dependencies**:
- Ensure `Tags.SHAREDRIDE_SELECT_ESTIMATES` enum is defined and imported
- Function follows same pattern as existing `getBookAnyEstimates` function

### 3.6 DSelectReq Type Modification

**Location**: `Backend/app/provider-platform/dynamic-offer-driver-app/Main/src/Domain/Action/Beckn/Select.hs`

**Add field to DSelectReq**:
```haskell
data DSelectReq = DSelectReq
  { -- existing fields...
    isSharedRide :: Bool  -- NEW: flag to identify shared ride requests
  }
```

**Update Beckn ACL Select logic**:
```haskell
-- After the mutual exclusion check
let (finalBookAnyEstimates, finalSharedRideEstimates, isSharedRide) = 
      if not (null sharedRideRemEstimates)
        then ([], sharedRideRemEstimates, True)   -- Shared ride case
        else (bookAnyEstimates, [], False)       -- Regular ride case

-- Update DSelectReq creation to include the flag
let dSelectReq = DSelectReq 
      { -- other fields...
        isSharedRide = isSharedRide
      }
```

### 3.7 Provider Platform Select Handler - Shared Records Creation

**Location**: `Backend/app/provider-platform/dynamic-offer-driver-app/Main/src/Domain/Action/Beckn/Select.hs` - in select handler

**Implementation**:
```haskell
-- Add in select handler using the isSharedRide flag
when req.isSharedRide $ do
  now <- getCurrentTime
  
  -- Get first estimate for reference data (no need to fetch all)
  primaryEstimate <- QEstimate.findById (head estimateIds) >>= fromMaybeM (EstimateNotFound (head estimateIds).getId)
  primarySearchRequest <- QSearchRequest.findById primaryEstimate.requestId >>= fromMaybeM (SearchRequestNotFound primaryEstimate.requestId.getId)
  
  -- Get search request IDs from estimates
  allEstimates <- catMaybes <$> mapM QEstimate.findById estimateIds
  let searchRequestIds = map (.requestId) allEstimates
  
  -- Create unified shared_entity record
  sharedEntityId <- Id <$> generateGUID
  let trackedSearchRequests = map (\srId -> TrackedEntity { entityId = srId.getId, isActive = True }) searchRequestIds
  let trackedEstimates = map (\estId -> TrackedEntity { entityId = estId, isActive = True }) estimateIds
  
  let sharedEntity = SharedEntity
        { id = sharedEntityId,
          status = SharedEntityActive,
          entityType = ESTIMATE_GROUP,
          searchRequestIds = trackedSearchRequests,
          estimateIds = trackedEstimates,
          bookingIds = [],
          rideIds = [],
          merchantId = primarySearchRequest.merchantId,
          merchantOperatingCityId = primarySearchRequest.merchantOperatingCityId,
          vehicleCategory = primarySearchRequest.vehicleCategory,
          tripCategory = RideShare,
          driverId = Nothing,
          pairingTime = Just now,
          createdAt = now,
          updatedAt = now
        }
  
  -- Store shared entity
  QSharedEntity.create sharedEntity
  
  -- Update all estimates to reference the shared entity
  QEstimate.updateAll estimateIds (\est -> est { sharedEntityId = Just sharedEntityId })
  
  -- Update all search requests to reference the shared entity
  QSearchRequest.updateAll searchRequestIds (\sr -> sr { sharedEntityId = Just sharedEntityId })
```

**Dependencies**:
- Add `isSharedRide :: Bool` field to `DSelectReq` type
- Import domain types: `SharedEntity`, `TrackedEntity`  
- Import storage queries: `QSharedEntity`
- Use `estimateIds` directly instead of fetching all estimates

---

## Database Schema Requirements

### Domain Types Required
- **SharedSearchRequest**: Groups multiple search request IDs together
- **SharedEstimate**: Similar structure to regular Estimate but with references to multiple individual estimates
- Both tables need to be created with proper foreign key relationships and all required columns

**Note**: Refer to existing `Domain.Types.Estimate` structure to ensure all necessary fields are included in SharedEstimate domain type (estimatedFare, discount, estimatedTotalFare, totalFareRange, descriptions, validTill, etc.)

---

## Redis Data Structures for Rider Pooling

### Pooled Riders Cache
- **Key Pattern**: `pooled_riders:{estimateId}`
- **Value**: JSON array of paired search request IDs
- **TTL**: 5 minutes (configurable)

### Customer Locks
- **Key Pattern**: `customer_lock:{searchRequestId}`
- **Value**: Timestamp of lock acquisition
- **TTL**: 2 minutes (configurable)

---

## Integration Points

### Provider Platform Select API
- **Endpoint**: `/estimate/{sharedEstimateId}/select`
- **Payload**: SharedEstimate data with longest route information
- **Response**: Driver acceptance/rejection

### Fallback Mechanism
- If no driver found for longest route, try routes of other customers in batch
- Implement retry logic with different route combinations

---

## Error Handling

### Pre-Validation Failures
- Release all acquired locks
- Return appropriate error to customer

### Driver Pooling Failures  
- Release customer locks
- Add newest customer back to waiting pool
- Log failure for monitoring

### Post-Acceptance Cancellations
- Handle customer cancellations after driver assignment
- Re-validate remaining customer count against threshold
- Cancel entire batch if below threshold

---

## Implementation Priority

1. **Phase 1**: Pre-batching validation in select2
2. **Phase 2**: SharedEstimate creation and storage
3. **Phase 3**: Driver pooling integration
4. **Phase 4**: Error handling and lock management
5. **Phase 5**: Integration testing with provider platform

---

## Testing Strategy

1. **Unit Tests**: Each validation and creation step
2. **Integration Tests**: End-to-end shared ride selection flow
3. **Load Tests**: Concurrent shared ride requests
4. **Failure Tests**: Various failure scenarios and lock cleanup

---

## Monitoring and Observability

- Metrics for shared ride success rates
- Timing metrics for each phase
- Lock acquisition and release monitoring
- Driver pooling success/failure rates

---

## 3.8 Chunk 3 Point 4: Driver Response Handling

### 3.8.1 No Driver Found Case - Handle.hs Line 87+ Modification

**Location**: `Backend/app/provider-platform/dynamic-offer-driver-app/Main/src/SharedLogic/Allocator/Jobs/SendSearchRequestToDrivers/Handle.hs:87+`

**Current Flow**:
```haskell
-- Lines 87-94 in Handle.hs
metrics.incrementFailedTaskCounter
logInfo "No driver accepted"
appBackendBapInternal <- asks (.appBackendBapInternal)
let request = CallBAPInternal.RideSearchExpiredReq {transactionId = transactionId}
void $ CallBAPInternal.rideSearchExpired appBackendBapInternal.apiKey appBackendBapInternal.url request
cancelSearchTry
cancelBookingIfApplies
return (Complete, NormalPool, Nothing)
```

**Modified Implementation**:
```haskell
-- Lines 87-94 modified in Handle.hs
else do
  metrics.incrementFailedTaskCounter
  logInfo "No driver accepted"
  
  -- Check if this is a shared ride search request and handle cleanup
  mbSearchRequest <- QSearchRequest.findByTransactionId transactionId
  case mbSearchRequest of
    Just searchRequest | isJust searchRequest.sharedEntityId -> do
      -- This is a shared ride - handle cleanup before calling rideSearchExpired
      handleSharedRideNoDriverFound searchRequest.sharedEntityId transactionId
    _ -> return () -- Regular ride, no additional cleanup needed
  
  -- Continue with existing flow
  appBackendBapInternal <- asks (.appBackendBapInternal)
  let request = CallBAPInternal.RideSearchExpiredReq {transactionId = transactionId}
  void $ CallBAPInternal.rideSearchExpired appBackendBapInternal.apiKey appBackendBapInternal.url request
  cancelSearchTry
  cancelBookingIfApplies
  return (Complete, NormalPool, Nothing)

-- New helper function to add in Handle.hs
handleSharedRideNoDriverFound :: Maybe (Id SharedEntity) -> Text -> m ()
handleSharedRideNoDriverFound mbSharedEntityId transactionId = do
  whenJust mbSharedEntityId $ \sharedEntityId -> do
    -- Get the shared entity to find all involved estimates
    mbSharedEntity <- QSharedEntity.findById sharedEntityId
    case mbSharedEntity of
      Just sharedEntity -> do
        -- Check if async pooling is enabled first - if enabled, skip re-pooling logic
        sharedRideConfig <- QSharedRideConfig.findByMerchantOpCityIdAndType 
                              sharedEntity.merchantOperatingCityId OverlappingRoute
        let isAsyncEnabled = maybe True (.asyncEnabled) sharedRideConfig -- default to True if not configured
        
        -- Extract estimate IDs from TrackedEntity array
        let estimateIds = map (.entityId) $ filter (.isActive) sharedEntity.estimateIds
        
        -- Release locks on all estimate IDs in the batch
        releasePairedCustomerLocks estimateIds
        
        unless isAsyncEnabled $ do
          -- Get all estimates and sort by creation time to find newest
          allEstimates <- catMaybes <$> mapM QEstimate.findById (map (Id) estimateIds)
          case sortOn (Down . (.createdAt)) allEstimates of
            (newestEstimate:_) -> do
              let ttlSeconds = maybe 300 (.rePoolingTtlSeconds) sharedRideConfig -- default to 5 minutes if not configured
              -- Add newest estimate back to shared waiting pool using geospatial index
              addToSharedWaitingPool newestEstimate ttlSeconds
              logInfo $ "Added newest estimate back to shared waiting pool: " <> newestEstimate.id.getId <> " with TTL: " <> show ttlSeconds <> " seconds (async disabled)"
            [] -> return ()
        
        when isAsyncEnabled $ do
          logInfo $ "Skipping re-pooling for shared entity: " <> sharedEntityId.getId <> " (async enabled)"
        
        logInfo $ "Cleaned up failed shared ride batch before calling rideSearchExpired: " <> sharedEntityId.getId
      Nothing -> 
        logWarning $ "SharedEntity not found for sharedEntityId: " <> sharedEntityId.getId

-- Helper functions to add:
releasePairedCustomerLocks :: [Text] -> m ()
releasePairedCustomerLocks estimateIds = do
  mapM_ releaseSingleCustomerLock estimateIds
  where
    releaseSingleCustomerLock estimateId = do
      let lockKey = "customer_lock:" <> estimateId
      Redis.del lockKey
      logDebug $ "Released lock for estimate: " <> estimateId

addToSharedWaitingPool :: Estimate -> Int -> m ()
addToSharedWaitingPool estimate ttlSeconds = do
  -- Get the search request from estimate to access location and seating info
  searchRequest <- QSearchRequest.findById estimate.requestId >>= fromMaybeM (SearchRequestNotFound estimate.requestId.getId)
  
  -- Add estimate back to shared rider waiting pool using geospatial index
  -- Key: ShareRideCustomerLoc
  -- MemberKey: searchRequestId:validTill:numSeats (where searchRequestId is transactionId on driver side)
  -- Value: pickup lat,lon
  now <- getCurrentTime
  let validTill = addUTCTime (fromIntegral ttlSeconds) now
      numSeats = fromMaybe 1 searchRequest.estimatedSeatingCapacity -- default to 1 seat
      memberKey = searchRequest.transactionId <> ":" <> show (round $ utcTimeToPOSIXSeconds validTill) <> ":" <> show numSeats
      geoKey = "ShareRideCustomerLoc"
      pickupLat = searchRequest.fromLocation.lat
      pickupLon = searchRequest.fromLocation.lon
  
  -- Add to geospatial index (TTL is embedded in memberKey, don't expire the entire index)
  Redis.geoAdd geoKey [(pickupLon, pickupLat, T.encodeUtf8 memberKey)]
```

**Dependencies**:
- Import `qualified Storage.Queries.SearchRequest as QSearchRequest`
- Import `qualified Storage.Queries.SharedEntity as QSharedEntity`
- Import `qualified Storage.Queries.Estimate as QEstimate`  
- Import `Storage.CachedQueries.SharedRideConfig as QSharedRideConfig`
- Import `qualified Kernel.Storage.Hedis as Redis`
- Import `Data.List (sortOn)`
- Import `Data.Ord (Down(..))`
- Import `Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)`
- Import `qualified Data.Text.Encoding as T`
- Assume `searchRequest.sharedEntityId :: Maybe (Id SharedEntity)` field exists
- Assume `searchRequest.transactionId :: Text` field exists (this is the bapSearchRequestId on BAP side)
- Assume `estimate.createdAt :: UTCTime` field exists for sorting by latest estimate
- Assume `searchRequest.fromLocation :: Location` with `lat, lon` fields exists for geospatial index
- Assume `searchRequest.estimatedSeatingCapacity :: Maybe Int` field exists for numSeats
- Assume `sharedRideConfig.rePoolingTtlSeconds :: Maybe Int` field exists in config table
- Assume `sharedRideConfig.asyncEnabled :: Maybe Bool` field exists in config table

### 3.8.2 Driver Accept Case - respondQuote Function Modification

**Location**: `Backend/app/provider-platform/dynamic-offer-driver-app/Main/src/Domain/Action/UI/Driver.hs:1358+` - in respondQuote function

**Implementation**: Add shared ride detection using `searchReq.sharedEntityId` and update SharedEntity status to OFFERED_QUOTE within existing flow.

**Modified Code**:
```haskell
-- Add after line 1349 in respondQuote (after searchReq is fetched)
-- Continue with existing logic...
-- ... existing merchant, driver, driverInfo, transporterConfig fetching ...
-- ... existing validations ...

-- Route to appropriate accept function and add shared ride status update
driverFCMPulledList <- case DTC.tripCategoryToPricingPolicy searchTry.tripCategory of
  DTC.EstimateBased _ -> do
    result <- acceptDynamicOfferDriverRequest merchant searchTry searchReq driver sReqFD mbBundleVersion mbClientVersion mbConfigVersion mbDevice reqOfferedValue
    -- Update shared entity status if this is a shared ride
    when (isJust searchReq.sharedEntityId) $ do
      updateSharedEntityStatus searchReq.sharedEntityId
    return result
  DTC.QuoteBased _ -> do
    result <- acceptStaticOfferDriverRequest (Just searchTry) driver (fromMaybe searchTry.estimateId sReqFD.estimateId) reqOfferedValue merchant clientId
    -- Update shared entity status if this is a shared ride  
    when (isJust searchReq.sharedEntityId) $ do
      updateSharedEntityStatus searchReq.sharedEntityId
    return result

where
  updateSharedEntityStatus mbSharedEntityId = do
    whenJust mbSharedEntityId $ \sharedEntityId -> do
      now <- getCurrentTime
      QSharedEntity.updateStatus SharedEntityOfferedQuote sharedEntityId now
      logInfo $ "Shared ride accepted by driver: " <> driverId.getId <> " for sharedEntity: " <> sharedEntityId.getId
```

**Dependencies**:
- Import `qualified Storage.Queries.SharedEntity as QSharedEntity`
- Assume `searchReq.sharedEntityId :: Maybe (Id SharedEntity)` field exists

### 3.8.3 Key Benefits

1. **Configurable Re-pooling**: Uses `sharedRideConfig.rePoolingTtlSeconds` for flexible TTL management
2. **Minimal Code Changes**: Only adds shared ride detection and cleanup logic to existing flows
3. **Proper Lock Management**: Releases all customer locks when no driver found
4. **Customer Re-queuing**: Puts newest customer back in shared waiting pool for re-matching
5. **Backward Compatible**: Regular rides continue to work exactly as before
6. **Monitoring Ready**: Comprehensive logging for observability

---

## 3.9 Chunk 3 Point 5: Post-Acceptance Cancellation Check

### 3.9.1 OnSelect.hs Implementation - Post-Acceptance Validation

**Location**: `Backend/app/rider-platform/rider-app/Main/src/Domain/Action/Beckn/OnSelect.hs:113+`

**Requirement**: Add shared ride validation after line 113, before calling init in OnSelect stage.

**Implementation**:
```haskell
-- Add after line 113 (after void $ QEstimate.updateStatus DEstimate.GOT_DRIVER_QUOTE estimate.id)
-- Chunk 3 Point 5: Shared ride post-acceptance validation
when (isJust searchRequest.sharedEntityId) $ do
  -- Shared ride must have autoAssign enabled
  when (searchRequest.autoAssignEnabledV2 /= Just True) $
    throwError $ InvalidRequest "Shared ride requires autoAssign to be enabled"
  
  -- Post-acceptance cancellation check: validate shared entity status
  validateSharedRidePostAcceptance searchRequest.sharedEntityId searchRequest.merchantOperatingCityId

-- Continue with existing autoAssign logic...
if searchRequest.autoAssignEnabledV2 == Just True
```

### 3.9.2 Validation Function

**Add to OnSelect.hs**:
```haskell
-- Chunk 3 Point 5: Post-acceptance cancellation check for shared rides
validateSharedRidePostAcceptance :: 
  (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => 
  Maybe (Id SharedEntity.SharedEntity) -> 
  Id MerchantOperatingCity.MerchantOperatingCity -> 
  m ()
validateSharedRidePostAcceptance mbSharedEntityId _ = do
  whenJust mbSharedEntityId $ \sharedEntityId -> do
    -- Find the shared entity for this batch
    mbSharedEntity <- QSharedEntity.findById sharedEntityId
    case mbSharedEntity of
      Just sharedEntity -> do
        -- Check if shared entity is cancelled
        when (sharedEntity.status == SharedEntityCancelled) $ do
          throwError $ InvalidRequest "Shared ride batch has been cancelled"
      Nothing -> 
        throwError $ InternalError $ "SharedEntity not found for sharedEntityId: " <> sharedEntityId.getId
```

### 3.9.3 Required Imports

**Add to OnSelect.hs imports**:
```haskell
import qualified Domain.Types.SharedEntity as SharedEntity
import qualified Domain.Types.MerchantOperatingCity as MerchantOperatingCity
import qualified Storage.Queries.SharedEntity as QSharedEntity
```

### 3.9.4 Key Validation Logic

1. **AutoAssign Requirement**: Shared rides must have autoAssign enabled, otherwise throw error
2. **Shared Entity Status Check**: Validate that the shared entity hasn't been cancelled
3. **Simplified Validation**: Only check shared entity status, no individual customer or count validation (as requested)

### 3.9.5 Error Handling

- **AutoAssign Disabled**: Throws `InvalidRequest "Shared ride requires autoAssign to be enabled"`
- **Cancelled Shared Entity**: Throws `InvalidRequest "Shared ride batch has been cancelled"`
- **Missing Shared Entity**: Throws `InternalError` with sharedEntityId

### 3.9.6 Dependencies

- Assume `searchRequest.sharedEntityId :: Maybe (Id SharedEntity)` field exists
- Assume `searchRequest.merchantOperatingCityId :: Id MerchantOperatingCity` field exists
- Assume `sharedEntity.status :: SharedEntityStatus` with `SharedEntityCancelled` enum value
- Domain types: `SharedEntity` 
- Storage queries: `QSharedEntity.findById`

### 3.9.7 Integration Point

This validation happens at the critical OnSelect stage before any booking/init process begins, ensuring shared rides are properly validated before proceeding to the driver acceptance flow.

---

## 3.10 TripQuoteDetail Shared Ride Enhancement

### 3.10.1 TripQuoteDetail Type Modification

**Location**: `Backend/app/provider-platform/dynamic-offer-driver-app/Main/src/SharedLogic/DriverPool/Types.hs:234+`

**Add sharedEntityId field to TripQuoteDetail**:
```haskell
data TripQuoteDetail = TripQuoteDetail
  { tripCategory :: DTC.TripCategory,
    vehicleServiceTier :: DVST.ServiceTierType,
    vehicleServiceTierName :: Text,
    baseFare :: HighPrecMoney,
    driverMinFee :: Maybe HighPrecMoney,
    driverMaxFee :: Maybe HighPrecMoney,
    driverStepFee :: Maybe HighPrecMoney,
    driverDefaultStepFee :: Maybe HighPrecMoney,
    driverPickUpCharge :: Maybe HighPrecMoney,
    driverParkingCharge :: Maybe HighPrecMoney,
    conditionalCharges :: [DAC.ConditionalCharges],
    congestionCharges :: Maybe HighPrecMoney,
    petCharges :: Maybe HighPrecMoney,
    priorityCharges :: Maybe HighPrecMoney,
    estimateOrQuoteId :: Text,
    eligibleForUpgrade :: Bool,
    sharedEntityId :: Maybe (Id SharedEntity.SharedEntity)  -- NEW: shared entity reference
  }
```

### 3.10.2 buildTripQuoteDetail Function Signature Update

**Location**: `Backend/app/provider-platform/dynamic-offer-driver-app/Main/src/SharedLogic/SearchTry.hs:280+`

**Update function signature to include sharedEntityId parameter**:
```haskell
buildTripQuoteDetail ::
  ( CacheFlow m r,
    EsqDBFlow m r,
    EsqDBReplicaFlow m r
  ) =>
  DSR.SearchRequest ->
  DTC.TripCategory ->
  DVST.ServiceTierType ->
  Maybe Text ->
  HighPrecMoney ->
  Maybe Bool ->
  Maybe HighPrecMoney ->
  Maybe HighPrecMoney ->
  Maybe HighPrecMoney ->
  Maybe HighPrecMoney ->
  Maybe HighPrecMoney ->
  Maybe HighPrecMoney ->
  Text ->
  [DAC.ConditionalCharges] ->
  Bool ->
  Maybe HighPrecMoney ->
  Maybe HighPrecMoney ->
  Maybe HighPrecMoney ->
  Maybe (Id SharedEntity.SharedEntity) -> -- NEW: shared entity ID parameter
  m TripQuoteDetail
buildTripQuoteDetail searchReq tripCategory vehicleServiceTier mbVehicleServiceTierName baseFare isDashboardRequest mbDriverMinFee mbDriverMaxFee mbStepFee mbDefaultStepFee mDriverPickUpCharge mbDriverParkingCharge estimateOrQuoteId conditionalCharges eligibleForUpgrade congestionCharges petCharges priorityCharges sharedEntityId = do
  -- ... existing function body remains the same ...
  return $ TripQuoteDetail {..} -- The {..} syntax will automatically include sharedEntityId
```

### 3.10.3 Select.hs Call Site Update

**Location**: `Backend/app/provider-platform/dynamic-offer-driver-app/Main/src/Domain/Action/Beckn/Select.hs:105`

**Update buildTripQuoteDetail call to pass estimate.sharedEntityId**:
```haskell
-- Line 105 - add estimate.sharedEntityId as the last parameter
buildTripQuoteDetail searchReq estimate.tripCategory estimate.vehicleServiceTier estimate.vehicleServiceTierName (estimate.minFare + fromMaybe 0 sReq.customerExtraFee + fromMaybe 0 petCharges') Nothing (mbDriverExtraFeeBounds <&> (.minFee)) (mbDriverExtraFeeBounds <&> (.maxFee)) (mbDriverExtraFeeBounds <&> (.stepFee)) (mbDriverExtraFeeBounds <&> (.defaultStepFee)) driverPickUpCharge driverParkingCharge estimate.id.getId driverAdditionalCharges False ((.congestionCharge) =<< estimate.fareParams) petCharges' (estimate.fareParams >>= (.priorityCharges)) estimate.sharedEntityId
```

### 3.10.4 All Other Call Sites Update

**Required Updates**: All other call sites of buildTripQuoteDetail need to be updated to pass `Nothing` as the last parameter for non-shared rides.

**Locations to Update**:
1. `SharedLogic/Cancel.hs:161` - add `Nothing`
2. `SharedLogic/Cancel.hs:224` - add `Nothing`  
3. `SharedLogic/Allocator/Jobs/SendSearchRequestToDrivers.hs:111` - add `Nothing`
4. `SharedLogic/Allocator/Jobs/SendSearchRequestToDrivers.hs:144` - add `Nothing`
5. `Domain/Action/Beckn/Confirm.hs:173` - add `Nothing`

**Template for non-shared ride call sites**:
```haskell
-- Add Nothing as the last parameter for all non-shared ride calls
buildTripQuoteDetail [all existing parameters...] Nothing
```

### 3.10.5 Required Imports

**Add to files using SharedEntity type**:
```haskell
import qualified Domain.Types.SharedEntity as SharedEntity
```

### 3.10.6 Benefits

1. **Shared Ride Detection**: TripQuoteDetail can now identify if it's part of a shared ride
2. **Downstream Processing**: Driver allocation logic can differentiate shared vs regular rides
3. **Backward Compatibility**: Non-shared rides pass `Nothing` and work exactly as before
4. **Type Safety**: Uses Maybe type to safely handle both shared and non-shared cases
5. **Record Syntax Compatibility**: The `{..}` syntax automatically includes the new field

### 3.10.7 Dependencies

- Ensure `estimate.sharedEstimateId :: Maybe (Id SharedEstimate)` field exists in Estimate domain type
- Update all buildTripQuoteDetail call sites to include the new parameter
- Import SharedEstimate domain type in relevant modules

---

## 3.11 SearchTry Shared Ride Enhancement

### 3.11.1 SearchTry Domain Type Modification

**Location**: SearchTry domain type definition

**Add sharedEntityId field to SearchTry**:
```haskell
data SearchTry = SearchTry
  { -- existing fields...
    sharedEntityId :: Maybe (Id SharedEntity.SharedEntity)  -- NEW: shared entity reference
  }
```

### 3.11.2 buildSearchTry Function Enhancement

**Location**: `Backend/app/provider-platform/dynamic-offer-driver-app/Main/src/SharedLogic/SearchTry.hs:222+`

**Update buildSearchTry function signature**:
```haskell
buildSearchTry ::
  ( MonadFlow m,
    CacheFlow m r,
    Metrics.CoreMetrics m,
    EsqDBFlow m r
  ) =>
  Id DM.Merchant ->
  DSR.SearchRequest ->
  [Text] ->
  Text ->
  HighPrecMoney ->
  Int ->
  DST.SearchRepeatType ->
  DTC.TripCategory ->
  Maybe HighPrecMoney ->
  Maybe HighPrecMoney ->
  Text ->
  [Text] ->
  DVST.ServiceTierType ->
  Maybe (Id SharedEntity.SharedEntity) -> -- NEW: shared entity ID parameter
  m DST.SearchTry
buildSearchTry merchantId searchReq estimateOrQuoteIds estOrQuoteId baseFare searchRepeatCounter searchRepeatType tripCategory customerExtraFee petCharges messageId estimateOrQuoteServTierNames serviceTier sharedEntityId = do
  -- ... existing function body ...
  pure $
    DST.SearchTry
      { -- existing fields...
        sharedEntityId = sharedEntityId,  -- NEW: include shared entity ID
        ..
      }
```

### 3.11.3 createNewSearchTry Function Enhancement

**Location**: `Backend/app/provider-platform/dynamic-offer-driver-app/Main/src/SharedLogic/SearchTry.hs:182+` - in `createNewSearchTry` function

**Extract sharedEntityId from tripQuoteDetails and pass to buildSearchTry**:
```haskell
    createNewSearchTry = do
      mbLastSearchTry <- QST.findLastByRequestId searchReq.id
      case tripQuoteDetails of  -- Fix the missing pattern match
        [] -> throwError $ InternalError "No trip quote details found"
        (firstQuoteDetail : _) -> do
          let estimatedFare = firstQuoteDetail.baseFare
          let tripCategory = firstQuoteDetail.tripCategory -- for fallback case
          let serviceTier = firstQuoteDetail.vehicleServiceTier -- for fallback case
          let estOrQuoteId = firstQuoteDetail.estimateOrQuoteId -- for fallback case
          let estimateOrQuoteIds = tripQuoteDetails <&> (.estimateOrQuoteId)
          let estimateOrQuoteServiceTierNames = tripQuoteDetails <&> (.vehicleServiceTierName)
          -- NEW: Extract sharedEntityId from first quote detail
          let sharedEntityId = firstQuoteDetail.sharedEntityId
          searchTry <- case mbLastSearchTry of
            Nothing -> do
              searchTry <- buildSearchTry merchant.id searchReq estimateOrQuoteIds estOrQuoteId estimatedFare 0 DST.INITIAL tripCategory customerExtraFee firstQuoteDetail.petCharges messageId estimateOrQuoteServiceTierNames serviceTier sharedEntityId  -- NEW: pass sharedEntityId
              _ <- QST.create searchTry
              return searchTry
            Just oldSearchTry -> do
              let searchRepeatType
                    | isRepeatSearch = DST.REALLOCATION
                    | oldSearchTry.status == DST.ACTIVE = DST.CANCELLED_AND_RETRIED
                    | otherwise = DST.RETRIED
              searchTry <- buildSearchTry merchant.id searchReq estimateOrQuoteIds estOrQuoteId estimatedFare (oldSearchTry.searchRepeatCounter + 1) searchRepeatType tripCategory customerExtraFee firstQuoteDetail.petCharges messageId estimateOrQuoteServiceTierNames serviceTier sharedEntityId  -- NEW: pass sharedEntityId
              when (oldSearchTry.status == DST.ACTIVE) $ do
                QST.updateStatus DST.CANCELLED oldSearchTry.id
                void $ QDQ.setInactiveBySTId oldSearchTry.id
              _ <- QST.create searchTry
              return searchTry

          logDebug $
            "search try id=" <> show searchTry.id
              <> "; estimated distance = "
              <> show searchReq.estimatedDistance
              <> "; estimated base fare:"
              <> show estimatedFare
              <> "; sharedEntityId = " <> show sharedEntityId  -- NEW: log shared entity ID
          return searchTry
```

### 3.11.4 Key Benefits

1. **Shared Ride Tracking**: SearchTry can now track which shared entity it belongs to
2. **Driver Context**: Drivers will know if they're handling a shared ride request
3. **Downstream Processing**: Other components can identify shared rides using SearchTry
4. **Audit Trail**: Complete traceability from estimate to search try for shared rides
5. **Consistent Data Flow**: sharedEntityId flows from TripQuoteDetail → SearchTry

### 3.11.5 Dependencies

- `SearchTry` domain type must include `sharedEntityId :: Maybe (Id SharedEntity)` field
- `TripQuoteDetail.sharedEntityId` field must be available (from section 3.10)
- Storage queries must support the new field for create/update operations
- Import `qualified Domain.Types.SharedEntity as SharedEntity` in SearchTry.hs

### 3.11.6 Database Schema

- `search_try` table needs `shared_entity_id` column with foreign key to `shared_entity` table
- Column should be nullable since regular rides won't have shared entities

### 3.11.7 Logic Flow

1. **TripQuoteDetail Creation**: Contains `sharedEntityId` from estimate
2. **SearchTry Creation**: Extracts `sharedEntityId` from first TripQuoteDetail
3. **Propagation**: Shared entity ID flows through entire driver search process
4. **Identification**: Any component can check `searchTry.sharedEntityId` to determine if it's a shared ride

---

## 3.12 SearchRequestForDriver Shared Ride Enhancement

### 3.12.1 SearchRequestForDriver Domain Type Modification

**Location**: SearchRequestForDriver domain type definition

**Add sharedEntityId field to SearchRequestForDriver**:
```haskell
data SearchRequestForDriver = SearchRequestForDriver
  { -- existing fields...
    sharedEntityId :: Maybe (Id SharedEntity.SharedEntity)  -- NEW: shared entity reference
  }
```

### 3.12.2 buildSearchRequestForDriver Function Enhancement

**Location**: `Backend/app/provider-platform/dynamic-offer-driver-app/Main/src/SharedLogic/Allocator/Jobs/SendSearchRequestToDrivers/Handle/Internal/SendSearchRequestToDrivers.hs:265+`

**Add sharedEstimateId to SearchRequestForDriver record construction**:
```haskell
      let searchRequestForDriver =
            SearchRequestForDriver
              { id = guid,
                requestId = searchReq.id,
                searchTryId = searchTry.id,
                vehicleCategory = searchTry.vehicleCategory,
                estimateId = Just tripQuoteDetail.estimateOrQuoteId,
                startTime = searchTry.startTime,
                merchantId = Just searchReq.providerId,
                fromLocGeohash = searchReq.fromLocGeohash,
                tripEstimatedDistance = searchReq.estimatedDistance,
                tripEstimatedDuration = searchReq.estimatedDuration,
                vehicleAge = dpRes.vehicleAge,
                merchantOperatingCityId = searchReq.merchantOperatingCityId,
                searchRequestValidTill = if dpwRes.pickupZone then addUTCTime (fromIntegral dpwRes.keepHiddenForSeconds) defaultValidTill else defaultValidTill,
                driverId = cast dpRes.driverId,
                vehicleVariant = dpRes.variant,
                vehicleServiceTier = tripQuoteDetail.vehicleServiceTier,
                vehicleServiceTierName = Just tripQuoteDetail.vehicleServiceTierName,
                airConditioned = dpRes.isAirConditioned,
                actualDistanceToPickup = dpwRes.actualDistanceToPickup,
                straightLineDistanceToPickup = dpRes.distanceToPickup,
                durationToPickup = dpwRes.actualDurationToPickup,
                status = Active,
                lat = Just dpRes.lat,
                lon = Just dpRes.lon,
                createdAt = now,
                updatedAt = Just now,
                response = Nothing,
                driverMinExtraFee = tripQuoteDetail.driverMinFee,
                driverMaxExtraFee = tripQuoteDetail.driverMaxFee,
                driverStepFee = tripQuoteDetail.driverStepFee,
                driverDefaultStepFee = tripQuoteDetail.driverDefaultStepFee,
                rideRequestPopupDelayDuration = dpwRes.intelligentScores.rideRequestPopupDelayDuration,
                baseFare = Just baseFare,
                currency,
                distanceUnit = searchReq.distanceUnit,
                isPartOfIntelligentPool = dpwRes.isPartOfIntelligentPool,
                acceptanceRatio = dpwRes.intelligentScores.acceptanceRatio,
                cancellationRatio = dpwRes.intelligentScores.cancellationRatio,
                driverAvailableTime = dpwRes.intelligentScores.availableTime,
                driverSpeed = dpwRes.intelligentScores.driverSpeed,
                keepHiddenForSeconds = dpwRes.keepHiddenForSeconds,
                pickupZone = dpwRes.pickupZone,
                mode = dpRes.mode,
                goHomeRequestId = dpwRes.goHomeReqId,
                rideFrequencyScore = dpwRes.intelligentScores.rideFrequency,
                customerCancellationDues = fromMaybe 0 searchReq.customerCancellationDues,
                clientSdkVersion = dpwRes.driverPoolResult.clientSdkVersion,
                clientBundleVersion = dpwRes.driverPoolResult.clientBundleVersion,
                clientConfigVersion = dpwRes.driverPoolResult.clientConfigVersion,
                clientDevice = dpwRes.driverPoolResult.clientDevice,
                backendConfigVersion = dpwRes.driverPoolResult.backendConfigVersion,
                backendAppVersion = Just deploymentVersion.getDeploymentVersion,
                isForwardRequest = dpwRes.isForwardRequest,
                previousDropGeoHash = dpwRes.previousDropGeoHash,
                driverTags = Just dpRes.driverTags,
                customerTags = dpRes.customerTags,
                poolingLogicVersion = searchReq.poolingLogicVersion,
                poolingConfigVersion = searchReq.poolingConfigVersion,
                notificationSource = Nothing,
                totalRides = fromMaybe (-1) (driverStats <&> (.totalRides)),
                renderedAt = Nothing,
                respondedAt = Nothing,
                middleStopCount = Just $ length searchReq.stops,
                upgradeCabRequest = Just tripQuoteDetail.eligibleForUpgrade,
                isFavourite = isFavourite,
                parcelType = searchReq.parcelType,
                parcelQuantity = searchReq.parcelQuantity,
                driverTagScore = dpwRes.score,
                conditionalCharges = additionalChargesEligiblFor,
                isSafetyPlus = Just isEligibleForSafetyPlusCharge,
                coinsRewardedOnGoldTierRide = driverCoinsRewardedOnGoldTierRideRequest,
                sharedEntityId = searchTry.sharedEntityId,  -- NEW: pass shared entity ID from SearchTry
                ..
              }
```

### 3.12.3 Data Source

**Extract sharedEntityId from SearchTry**:
- The `buildSearchRequestForDriver` function already has access to `searchTry` object
- Use `searchTry.sharedEntityId` to populate the new field
- This ensures the shared entity ID flows from SearchTry → SearchRequestForDriver

### 3.12.4 Key Benefits

1. **Driver Context**: Drivers receive shared ride context in their search requests
2. **FCM Notifications**: Can customize notifications for shared ride requests
3. **Driver UI**: Driver app can show shared ride specific UI elements
4. **Analytics**: Track driver responses to shared vs regular ride requests
5. **Business Logic**: Enable shared ride specific driver logic

### 3.12.5 Dependencies

- `SearchRequestForDriver` domain type must include `sharedEntityId :: Maybe (Id SharedEntity)` field
- `searchTry.sharedEntityId` field must be available (from section 3.11)
- Storage queries must support the new field for create/update operations
- Import `qualified Domain.Types.SharedEntity as SharedEntity`

### 3.12.6 Database Schema

- `search_request_for_driver` table needs `shared_entity_id` column with foreign key to `shared_entity` table
- Column should be nullable since regular rides won't have shared entities

### 3.12.7 Logic Flow Extension

1. **TripQuoteDetail**: Contains `sharedEntityId` from estimate
2. **SearchTry**: Contains `sharedEntityId` from TripQuoteDetail
3. **SearchRequestForDriver**: Contains `sharedEntityId` from SearchTry
4. **Driver Context**: Drivers receive shared ride context for proper handling

### 3.12.8 Driver Notification Enhancement

This enables:
- FCM notifications with shared ride flags
- Driver app UI customization for shared rides
- Different acceptance/rejection logic for shared rides
- Shared ride specific analytics and metrics

---

## 3.13 DriverQuote Shared Ride Enhancement

### 3.13.1 DriverQuote Domain Type Modification

**Location**: DriverQuote domain type definition

**Add sharedEntityId field to DriverQuote**:
```haskell
data DriverQuote = DriverQuote
  { -- existing fields...
    sharedEntityId :: Maybe (Id SharedEntity.SharedEntity)  -- NEW: shared entity reference
  }
```

### 3.13.2 buildDriverQuote Function Enhancement

**Location**: `Backend/app/provider-platform/dynamic-offer-driver-app/Main/src/Domain/Action/UI/Driver.hs:1435+` - in `buildDriverQuote` function

**Add sharedEstimateId to DriverQuote record construction**:
```haskell
      pure
        DDrQuote.DriverQuote
          { id = guid,
            requestId = searchReq.id,
            searchTryId = sd.searchTryId,
            searchRequestForDriverId = Just sd.id,
            clientId = clientId,
            driverId,
            driverName = driver.firstName,
            driverRating = SP.roundToOneDecimal <$> driverStats.rating,
            status = DDrQuote.Active,
            vehicleVariant = sd.vehicleVariant,
            vehicleServiceTier = sd.vehicleServiceTier,
            distance = searchReq.estimatedDistance,
            distanceToPickup = sd.actualDistanceToPickup,
            durationToPickup = sd.durationToPickup,
            currency = sd.currency,
            distanceUnit = sd.distanceUnit,
            createdAt = now,
            updatedAt = now,
            validTill = addUTCTime driverQuoteExpirationSeconds now,
            providerId = searchReq.providerId,
            estimatedFare,
            fareParams,
            specialLocationTag = searchReq.specialLocationTag,
            goHomeRequestId = sd.goHomeRequestId,
            tripCategory = tripCategory,
            estimateId = Id estimateId,
            clientSdkVersion = mbClientVersion',
            clientBundleVersion = mbBundleVersion',
            clientConfigVersion = mbConfigVersion',
            clientDevice = getDeviceFromText mbDevice',
            backendConfigVersion = Nothing,
            backendAppVersion = Just deploymentVersion.getDeploymentVersion,
            merchantOperatingCityId = Just searchReq.merchantOperatingCityId,
            vehicleServiceTierName = sd.vehicleServiceTierName,
            coinsRewardedOnGoldTierRide = sd.coinsRewardedOnGoldTierRide,
            sharedEntityId = sd.sharedEntityId  -- NEW: pass shared entity ID from SearchRequestForDriver
          }
```

### 3.13.3 Data Source

**Extract sharedEntityId from SearchRequestForDriver**:
- The `buildDriverQuote` function receives `sd :: SearchRequestForDriver` parameter
- Use `sd.sharedEntityId` to populate the new field in DriverQuote
- This ensures the shared entity ID flows from SearchRequestForDriver → DriverQuote

### 3.13.4 Key Benefits

1. **Quote Context**: Driver quotes now carry shared ride context
2. **Booking Integration**: Shared ride quotes can be properly linked to shared entities
3. **Analytics**: Track quote success rates for shared vs regular rides
4. **Business Logic**: Enable shared ride specific quote processing
5. **Driver Selection**: Identify which quotes are for shared rides during selection

### 3.13.5 Dependencies

- `DriverQuote` domain type must include `sharedEntityId :: Maybe (Id SharedEntity)` field
- `SearchRequestForDriver.sharedEntityId` field must be available (from section 3.12)
- Storage queries must support the new field for create/update operations
- Import `qualified Domain.Types.SharedEntity as SharedEntity`

### 3.13.6 Database Schema

- `driver_quote` table needs `shared_entity_id` column with foreign key to `shared_entity` table
- Column should be nullable since regular rides won't have shared entities

### 3.13.7 Logic Flow Extension

1. **TripQuoteDetail**: Contains `sharedEntityId` from estimate
2. **SearchTry**: Contains `sharedEntityId` from TripQuoteDetail
3. **SearchRequestForDriver**: Contains `sharedEntityId` from SearchTry
4. **DriverQuote**: Contains `sharedEntityId` from SearchRequestForDriver
5. **Quote Processing**: Driver quotes carry shared ride context throughout the system

### 3.13.8 Quote Management Enhancement

This enables:
- Shared ride specific quote validation
- Proper linking between driver quotes and shared entities
- Enhanced analytics for shared ride quote performance
- Business logic differentiation between shared and regular quotes
- Complete audit trail from estimate to driver quote for shared rides

### 3.13.9 Integration Points

- **Booking Creation**: When converting quotes to bookings, shared ride context is preserved
- **Driver Selection**: System can prioritize or filter quotes based on shared ride status
- **Fare Calculation**: Shared ride specific fare logic can be applied at quote level
- **Monitoring**: Track shared ride quote acceptance rates and driver preferences

