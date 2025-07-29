# Chunk 3 Implementation Plan: Batch Creation and Driver Pooling

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

-- Check if this is a shared ride estimate using conditional assignment
(estimate, remainingEstimateBppIds) <- 
  if (estimate'.tripCategory == Just (Trip.RideShare _))
    then do
      -- Shared ride logic
      pairedCustomers <- getPooledRiders personId estimateId
      
      -- Fetch all estimates once (including current estimate)
      allSearchRequestIds <- return $ searchRequest.id : pairedCustomers
      allEstimates <- catMaybes <$> mapM QEstimate.findBySearchRequestId allSearchRequestIds
      
      -- Pre-batching validation using fetched estimates
      let validEstimates = filter (\est -> est.status /= DEstimate.Cancelled) allEstimates
      let validCount = length validEstimates
      
      -- Validate threshold requirement  
      sharedRideConfig <- QSharedRideConfig.findByMerchantOpCityIdAndType 
                            searchRequest.merchantOperatingCityId OverlappingRoute
      let minThreshold = maybe 2 (.minRidersThreshold) sharedRideConfig
      
      when (validCount < minThreshold) $ do
        -- Release locks on paired searchIds
        releasePairedCustomerLocks pairedCustomers
        throwError $ InvalidRequest "Insufficient valid paired customers for shared ride"
      
      -- Pre-batching validation passed, create shared records
      now <- getCurrentTime
      
      -- Create sharedSearchRequest record first
      sharedSearchRequestId <- Id <$> generateGUID
      let sharedSearchRequest = SharedSearchRequest
            { id = sharedSearchRequestId,
              searchRequestIds = allSearchRequestIds, -- Correct: actual search request IDs
              merchantId = searchRequest.merchantId,
              merchantOperatingCityId = searchRequest.merchantOperatingCityId,
              vehicleServiceTierType = estimate'.vehicleServiceTierType,
              status = SharedSearchRequest.ACTIVE,
              createdAt = now,
              updatedAt = now
            }
      
      QSharedSearchRequest.create sharedSearchRequest
      
      -- Update all search requests to reference the shared search request
      QSearchRequest.updateAll allSearchRequestIds (\sr -> sr { sharedSearchRequestId = Just sharedSearchRequestId })
      
      -- Create sharedEstimate record with all required fields
      sharedEstimateId <- Id <$> generateGUID  
      let sharedEstimate = SharedEstimate
            { id = sharedEstimateId,
              sharedSearchRequestId = sharedSearchRequestId,
              individualEstimateIds = map (.id) validEstimates,
              bppEstimateId = Nothing, -- Will be set when BPP responds
              providerId = estimate'.providerId,
              providerUrl = estimate'.providerUrl,
              vehicleServiceTierType = estimate'.vehicleServiceTierType,
              estimatedFare = calculateCombinedFare validEstimates,
              discount = Nothing,
              estimatedTotalFare = calculateCombinedFare validEstimates,
              totalFareRange = Nothing,
              descriptions = [],
              pickupLocations = map getPickupLocation validEstimates,
              dropoffLocations = map getDropoffLocation validEstimates,
              status = SharedEstimate.CREATED,
              validTill = estimate'.validTill, -- Use same validity as individual estimate
              createdAt = now,
              updatedAt = now
            }
      
      -- Store sharedEstimate
      QSharedEstimate.create sharedEstimate
      
      -- Update all estimates to reference the shared estimate
      let allEstimateIds = map (.id) validEstimates
      QEstimate.updateAll allEstimateIds (\est -> est { sharedEstimateId = Just sharedEstimateId })
      
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
  
  -- Create SharedSearchRequest record  
  sharedSearchRequestId <- Id <$> generateGUID
  let sharedSearchRequest = SharedSearchRequest
        { id = sharedSearchRequestId,
          fromLocationIds = [primarySearchRequest.fromLocationId], -- Will need to collect from all search requests
          toLocationIds = maybeToList primarySearchRequest.toLocationId, -- Will need to collect from all search requests
          merchantId = primarySearchRequest.merchantId,
          merchantOperatingCityId = primarySearchRequest.merchantOperatingCityId,
          transactionId = primarySearchRequest.transactionId,
          tripCategory = primarySearchRequest.tripCategory,
          vehicleCategory = primarySearchRequest.vehicleCategory,
          estimatedDistance = primarySearchRequest.estimatedDistance,
          estimatedDuration = primarySearchRequest.estimatedDuration,
          tollCharges = primarySearchRequest.tollCharges,
          tollNames = primarySearchRequest.tollNames,
          validTill = primarySearchRequest.validTill,
          status = SharedSearchRequest.NEW,
          createdAt = now,
          updatedAt = now
        }
  
  QSharedSearchRequest.create sharedSearchRequest
  
  -- Update all search requests to reference shared search request
  QSearchRequest.updateAll searchRequestIds (\sr -> sr { sharedSearchRequestId = Just sharedSearchRequestId })
  
  -- Create SharedEstimate record
  sharedEstimateId <- Id <$> generateGUID
  let sharedEstimate = SharedEstimate
        { id = sharedEstimateId,
          transactionId = primaryEstimate.requestId.getId, -- Use search request ID as transaction ID
          merchantId = primaryEstimate.merchantId,
          merchantOperatingCityId = primaryEstimate.merchantOperatingCityId,
          vehicleServiceTier = primaryEstimate.vehicleServiceTier,
          tripCategory = primaryEstimate.tripCategory,
          estimatedDistance = primaryEstimate.estimatedDistance,
          estimatedDuration = primaryEstimate.estimatedDuration,
          currency = primaryEstimate.currency,
          distanceUnit = primaryEstimate.distanceUnit,
          totalMinFare = primaryEstimate.minFare,
          totalMaxFare = primaryEstimate.maxFare,
          tollNames = primaryEstimate.tollNames,
          validTill = primaryEstimate.validTill,
          status = SharedEstimate.NEW,
          createdAt = now,
          updatedAt = now
        }
  
  QSharedEstimate.create sharedEstimate
  
  -- Update all estimates to reference shared estimate
  QEstimate.updateAll estimateIds (\est -> est { sharedEstimateId = Just sharedEstimateId })
```

**Dependencies**:
- Add `isSharedRide :: Bool` field to `DSelectReq` type
- Import domain types: `SharedSearchRequest`, `SharedEstimate`  
- Import storage queries: `QSharedSearchRequest`, `QSharedEstimate`
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
    Just searchRequest | isJust searchRequest.sharedSearchRequestId -> do
      -- This is a shared ride - handle cleanup before calling rideSearchExpired
      handleSharedRideNoDriverFound searchRequest.sharedSearchRequestId transactionId
    _ -> return () -- Regular ride, no additional cleanup needed
  
  -- Continue with existing flow
  appBackendBapInternal <- asks (.appBackendBapInternal)
  let request = CallBAPInternal.RideSearchExpiredReq {transactionId = transactionId}
  void $ CallBAPInternal.rideSearchExpired appBackendBapInternal.apiKey appBackendBapInternal.url request
  cancelSearchTry
  cancelBookingIfApplies
  return (Complete, NormalPool, Nothing)

-- New helper function to add in Handle.hs
handleSharedRideNoDriverFound :: Maybe (Id SharedSearchRequest) -> Text -> m ()
handleSharedRideNoDriverFound mbSharedSearchRequestId transactionId = do
  whenJust mbSharedSearchRequestId $ \sharedSearchRequestId -> do
    -- Get the shared search request to find all involved customers
    mbSharedSearchRequest <- QSharedSearchRequest.findById sharedSearchRequestId
    case mbSharedSearchRequest of
      Just sharedSearchRequest -> do
        -- Release locks on all customers in the batch
        releasePairedCustomerLocks sharedSearchRequest.searchRequestIds
        
        -- Find the newest customer (latest created_at) and put back in shared waiting pool
        allSearchRequests <- catMaybes <$> mapM QSearchRequest.findById sharedSearchRequest.searchRequestIds
        case sortOn (Down . (.createdAt)) allSearchRequests of
          (newestSearchRequest:_) -> do
            -- Get the TTL from sharedRideConfig table
            sharedRideConfig <- QSharedRideConfig.findByMerchantOpCityIdAndType 
                                  sharedSearchRequest.merchantOperatingCityId OverlappingRoute
            -- Only re-pool the customer if async pooling is disabled (sync mode)
            let isAsyncEnabled = maybe True (.asyncEnabled) sharedRideConfig -- default to True if not configured
            unless isAsyncEnabled $ do
              let ttlSeconds = maybe 300 (.rePoolingTtlSeconds) sharedRideConfig -- default to 5 minutes if not configured
              -- Add newest customer back to shared waiting pool using geospatial index
              addToSharedWaitingPool newestSearchRequest ttlSeconds
              logInfo $ "Added newest customer back to shared waiting pool: " <> newestSearchRequest.id.getId <> " using transactionId: " <> newestSearchRequest.transactionId <> " with TTL: " <> show ttlSeconds <> " seconds (async disabled)"
            when isAsyncEnabled $ do
              logInfo $ "Skipping re-pooling for customer: " <> newestSearchRequest.id.getId <> " (async enabled)"
          [] -> return ()
        
        logInfo $ "Cleaned up failed shared ride batch before calling rideSearchExpired: " <> sharedSearchRequestId.getId
      Nothing -> 
        logWarning $ "SharedSearchRequest not found for sharedSearchRequestId: " <> sharedSearchRequestId.getId

-- Helper functions to add:
releasePairedCustomerLocks :: [Id SearchRequest] -> m ()
releasePairedCustomerLocks searchRequestIds = do
  mapM_ releaseSingleCustomerLock searchRequestIds
  where
    releaseSingleCustomerLock searchId = do
      let lockKey = "customer_lock:" <> searchId.getId
      Redis.del lockKey
      logDebug $ "Released lock for customer: " <> searchId.getId

addToSharedWaitingPool :: SearchRequest -> Int -> m ()
addToSharedWaitingPool searchRequest ttlSeconds = do
  -- Add customer back to shared rider waiting pool using geospatial index
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
- Import `qualified Storage.Queries.SharedSearchRequest as QSharedSearchRequest`  
- Import `Storage.CachedQueries.SharedRideConfig as QSharedRideConfig`
- Import `qualified Kernel.Storage.Hedis as Redis`
- Import `Data.List (sortOn)`
- Import `Data.Ord (Down(..))`
- Import `Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)`
- Import `qualified Data.Text.Encoding as T`
- Assume `searchRequest.sharedSearchRequestId :: Maybe (Id SharedSearchRequest)` field exists
- Assume `searchRequest.transactionId :: Text` field exists (this is the bapSearchRequestId on BAP side)
- Assume `searchRequest.createdAt :: UTCTime` field exists for sorting by latest customer
- Assume `searchRequest.fromLocation :: Location` with `lat, lon` fields exists for geospatial index
- Assume `searchRequest.estimatedSeatingCapacity :: Maybe Int` field exists for numSeats
- Assume `sharedRideConfig.rePoolingTtlSeconds :: Maybe Int` field exists in config table
- Assume `sharedRideConfig.asyncEnabled :: Maybe Bool` field exists in config table

### 3.8.2 Driver Accept Case - respondQuote Function Modification

**Location**: `Backend/app/provider-platform/dynamic-offer-driver-app/Main/src/Domain/Action/UI/Driver.hs:1358+` - in respondQuote function

**Implementation**: Add shared ride detection using `estimate.sharedEstimateId` and minimal status update logic within existing flow.

**Modified Code**:
```haskell
-- Add after line 1349 in respondQuote (after searchReq is fetched)
-- Fetch estimate to check for shared ride
estimate <- QEstimate.findById (fromMaybe searchTry.estimateId sReqFD.estimateId) >>= fromMaybeM (EstimateNotFound (fromMaybe searchTry.estimateId sReqFD.estimateId).getId)

-- Continue with existing logic...
-- ... existing merchant, driver, driverInfo, transporterConfig fetching ...
-- ... existing validations ...

-- Route to appropriate accept function and add shared ride status update
driverFCMPulledList <- case DTC.tripCategoryToPricingPolicy searchTry.tripCategory of
  DTC.EstimateBased _ -> do
    result <- acceptDynamicOfferDriverRequest merchant searchTry searchReq driver sReqFD mbBundleVersion mbClientVersion mbConfigVersion mbDevice reqOfferedValue
    -- Update shared estimate status if this is a shared ride
    when (isJust estimate.sharedEstimateId) $ do
      updateSharedEstimateStatus estimate.sharedEstimateId
    return result
  DTC.QuoteBased _ -> do
    result <- acceptStaticOfferDriverRequest (Just searchTry) driver (fromMaybe searchTry.estimateId sReqFD.estimateId) reqOfferedValue merchant clientId
    -- Update shared estimate status if this is a shared ride  
    when (isJust estimate.sharedEstimateId) $ do
      updateSharedEstimateStatus estimate.sharedEstimateId
    return result

where
  updateSharedEstimateStatus mbSharedEstId = do
    whenJust mbSharedEstId $ \sharedEstId -> do
      now <- getCurrentTime
      QSharedEstimate.updateStatus SharedEstimate.ACCEPTED sharedEstId now
      logInfo $ "Shared ride accepted by driver: " <> driverId.getId <> " for sharedEstimate: " <> sharedEstId.getId
```

**Dependencies**:
- Import `qualified Storage.Queries.Estimate as QEstimate`
- Import `qualified Storage.Queries.SharedEstimate as QSharedEstimate`
- Assume `estimate.sharedEstimateId :: Maybe (Id SharedEstimate)` field exists

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
when (isJust searchRequest.sharedSearchRequestId) $ do
  -- Shared ride must have autoAssign enabled
  when (searchRequest.autoAssignEnabledV2 /= Just True) $
    throwError $ InvalidRequest "Shared ride requires autoAssign to be enabled"
  
  -- Post-acceptance cancellation check: validate shared estimate status
  validateSharedRidePostAcceptance searchRequest.sharedSearchRequestId searchRequest.merchantOperatingCityId

-- Continue with existing autoAssign logic...
if searchRequest.autoAssignEnabledV2 == Just True
```

### 3.9.2 Validation Function

**Add to OnSelect.hs**:
```haskell
-- Chunk 3 Point 5: Post-acceptance cancellation check for shared rides
validateSharedRidePostAcceptance :: 
  (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => 
  Maybe (Id SharedSearchRequest.SharedSearchRequest) -> 
  Id MerchantOperatingCity.MerchantOperatingCity -> 
  m ()
validateSharedRidePostAcceptance mbSharedSearchRequestId _ = do
  whenJust mbSharedSearchRequestId $ \sharedSearchRequestId -> do
    -- Find the shared estimate for this batch
    mbSharedEstimate <- QSharedEstimate.findBySharedSearchRequestId sharedSearchRequestId
    case mbSharedEstimate of
      Just sharedEstimate -> do
        -- Check if shared estimate is cancelled
        when (sharedEstimate.status == SharedEstimate.Cancelled) $ do
          throwError $ InvalidRequest "Shared ride batch has been cancelled"
      Nothing -> 
        throwError $ InternalError $ "SharedEstimate not found for sharedSearchRequestId: " <> sharedSearchRequestId.getId
```

### 3.9.3 Required Imports

**Add to OnSelect.hs imports**:
```haskell
import qualified Domain.Types.SharedSearchRequest as SharedSearchRequest
import qualified Domain.Types.SharedEstimate as SharedEstimate
import qualified Domain.Types.MerchantOperatingCity as MerchantOperatingCity
import qualified Storage.Queries.SharedSearchRequest as QSharedSearchRequest
import qualified Storage.Queries.SharedEstimate as QSharedEstimate
```

### 3.9.4 Key Validation Logic

1. **AutoAssign Requirement**: Shared rides must have autoAssign enabled, otherwise throw error
2. **Shared Estimate Status Check**: Validate that the shared estimate hasn't been cancelled
3. **Simplified Validation**: Only check shared estimate status, no individual customer or count validation (as requested)

### 3.9.5 Error Handling

- **AutoAssign Disabled**: Throws `InvalidRequest "Shared ride requires autoAssign to be enabled"`
- **Cancelled Shared Estimate**: Throws `InvalidRequest "Shared ride batch has been cancelled"`
- **Missing Shared Estimate**: Throws `InternalError` with sharedSearchRequestId

### 3.9.6 Dependencies

- Assume `searchRequest.sharedSearchRequestId :: Maybe (Id SharedSearchRequest)` field exists
- Assume `searchRequest.merchantOperatingCityId :: Id MerchantOperatingCity` field exists
- Assume `sharedEstimate.status :: SharedEstimate.Status` with `Cancelled` enum value
- Domain types: `SharedSearchRequest`, `SharedEstimate` 
- Storage queries: `QSharedSearchRequest.findById`, `QSharedEstimate.findBySharedSearchRequestId`

### 3.9.7 Integration Point

This validation happens at the critical OnSelect stage before any booking/init process begins, ensuring shared rides are properly validated before proceeding to the driver acceptance flow.