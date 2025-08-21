# Shared Ride Feature - API Changes Documentation

## Overview
This document outlines all API changes required for implementing the shared ride feature across the rider and provider platforms.

---

## 📱 Rider Platform Changes

### 1. Search API - Route Caching and Geospatial Indexing

**File**: `Backend/app/rider-platform/rider-app/Main/src/Domain/Action/UI/Search.hs`

```diff
@@ -354,0 +354,15 @@
+ -- Add after QSearchRequest.createDSReq searchRequest (line 354)
+ fork "cache route response" $ do
+   sharedRideConfig <- QSharedRideConfig.findByMerchantOpCityIdAndType 
+                         merchantOperatingCityId OverlappingRoute
+   case sharedRideConfig of
+     Just config | config.enableSharedRide -> do
+       let cacheKey = "route_cache:" <> searchRequest.id.getId
+       let cacheTimeout = calculateRouteCacheExpiry config
+       Redis.setExp cacheKey (encode routeResponse) (fromIntegral cacheTimeout.getSeconds)
+     _ -> return () -- Skip if shared ride not enabled
```

**Required Imports**:
```diff
+ import qualified Storage.CachedQueries.SharedRideConfig as QSharedRideConfig
+ import qualified Kernel.Storage.Hedis as Redis
+ import Data.Aeson (encode)
```

### 2. OnSearch API - Shared Ride Filtering

**File**: `Backend/app/rider-platform/rider-app/Main/src/Domain/Action/Beckn/OnSearch.hs`

```diff 
+ shouldShowSharedRides :: 
+   SearchRequestFlow m r => 
+   Int -> 
+   Id MerchantOperatingCity -> 
+   m Bool
+ shouldShowSharedRides merchantOperatingCityId = do
+   sharedRideConfig <- QSharedRideConfig.findByMerchantOpCityIdAndType 
+                         merchantOperatingCityId OverlappingRoute
+   return $ maybe False (.enableSharedRide) sharedRideConfig
+ 
+ filterSharedRideEstimates :: 
+   SearchRequestFlow m r => 
+   [EstimateInfo] -> 
+   Int -> 
+   Id MerchantOperatingCity -> 
+   m [EstimateInfo]
+ filterSharedRideEstimates estimatesInfo merchantOperatingCityId = do
+   showSharedRides <- shouldShowSharedRides merchantOperatingCityId
+   if showSharedRides
+     then return estimatesInfo  -- Keep all estimates including RideShare
+     else return $ filter (\est -> est.tripCategory /= RideShare) estimatesInfo
```

- estimates <- traverse (buildEstimate providerInfo now searchRequest deploymentVersion) (filterEstimtesByPrefference estimatesInfo blackListedVehicles)
+ filteredEstimates <- filterEstimtesByPrefference estimatesInfo blackListedVehicles
+ sharedRideFilteredEstimates <- filterSharedRideEstimates filteredEstimates searchRequest.merchantOperatingCityId
+ estimates <- traverse (buildEstimate providerInfo now searchRequest deploymentVersion) sharedRideFilteredEstimates
```

### 3. Select API - Shared Ride Batch Creation

**File**: `Backend/app/rider-platform/rider-app/Main/src/Domain/Action/UI/Select.hs`

```diff
@@ -240,0 +240,106 @@
+ -- Add after basic estimate validation (around line 244)
+ -- Store original values with primes for immutability
+ let estimate' = estimate
+     remainingEstimateBppIds' = remainingEstimateBppIds
+ 
+ -- Check if this is a shared ride estimate and config allows sync mode
+ sharedRideConfig <- QSharedRideConfig.findByMerchantOpCityIdAndType 
+                       searchRequest.merchantOperatingCityId OverlappingRoute
+ let isAsyncEnabled = maybe True (.asyncEnabled) sharedRideConfig -- default to True if not configured
+ 
+ (estimate, remainingEstimateBppIds) <- 
+   if (estimate'.tripCategory == Just (Trip.RideShare _) && not isAsyncEnabled)
+     then do
+       -- Shared ride sync mode logic
+       pairedEstimateIds <- getPooledRiders personId estimateId
+       
+       -- Fetch all estimates once (including current estimate)
+       allEstimateIds <- return $ estimateId : pairedEstimateIds
+       allEstimates <- catMaybes <$> mapM QEstimate.findById allEstimateIds
+       
+       -- Pre-batching validation using fetched estimates
+       let validEstimates = filter (\est -> est.status /= DEstimate.Cancelled) allEstimates
+       let validCount = length validEstimates
+       
+       -- Validate threshold requirement
+       let minThreshold = maybe 2 (.minRidersThreshold) sharedRideConfig
+       
+       when (validCount < minThreshold) $ do
+         -- Release locks on paired estimate IDs
+         releasePairedCustomerLocks pairedEstimateIds
+         throwError $ InvalidRequest "Insufficient valid paired customers for shared ride"
+       
+       -- Pre-batching validation passed, create shared entity
+       now <- getCurrentTime
+       
+       -- Create unified shared_entity record
+       sharedEntityId <- Id <$> generateGUID
+       let allSearchRequestIds = map (.requestId) validEstimates
+       let trackedSearchRequests = map (\srId -> TrackedEntity { entityId = srId.getId, isActive = True }) allSearchRequestIds
+       let trackedEstimates = map (\est -> TrackedEntity { entityId = est.id.getId, isActive = True }) validEstimates
+       
+       let sharedEntity = SharedEntity
+             { id = sharedEntityId,
+               status = SharedEntityActive,
+               entityType = ESTIMATE_GROUP,
+               searchRequestIds = trackedSearchRequests,
+               estimateIds = trackedEstimates,
+               bookingIds = [],
+               rideIds = [],
+               merchantId = searchRequest.merchantId,
+               merchantOperatingCityId = searchRequest.merchantOperatingCityId,
+               vehicleCategory = estimate'.vehicleCategory,
+               tripCategory = RideShare,
+               driverId = Nothing,
+               pairingTime = Just now,
+               createdAt = now,
+               updatedAt = now
+             }
+       
+       -- Store shared entity
+       QSharedEntity.create sharedEntity
+       
+       -- Update all estimates to reference the shared entity
+       let allEstimateIds = map (.id) validEstimates
+       QEstimate.updateAll allEstimateIds (\est -> est { sharedEntityId = Just sharedEntityId })
+       
+       -- Update all search requests to reference the shared entity
+       QSearchRequest.updateAll allSearchRequestIds (\sr -> sr { sharedEntityId = Just sharedEntityId })
+       
+       -- Return values for shared ride case
+       return (head validEstimates, map (.bppEstimateId) (tail validEstimates))
+       
+     else 
+       -- Non-shared ride case - use original values
+       return (estimate', remainingEstimateBppIds')
```

### 4. Beckn ACL Select Modifications

**File**: `Backend/app/rider-platform/rider-app/Main/src/Beckn/ACL/Select.hs`

```diff
@@ -194,1 +194,4 @@
- descriptorCode = Tags.OTHER_SELECT_ESTIMATES
+ -- Check if primary estimate has sharedEstimateId to determine descriptor
+ descriptorCode = case res.estimate.sharedEntityId of
+   Just _ -> Tags.SHAREDRIDE_SELECT_ESTIMATES  -- Shared ride
+   Nothing -> Tags.OTHER_SELECT_ESTIMATES      -- Regular ride
```

### 5. OnSelect API - Shared Ride Quote Creation

**File**: `Backend/app/rider-platform/rider-app/Main/src/Domain/Action/Beckn/OnSelect.hs`

```diff
@@ -0,0 +137,136 @@
+ onSelect :: OnSelectValidatedReq -> Flow ()
+ onSelect OnSelectValidatedReq {..} = do
+   now <- getCurrentTime
+   
+   -- Check if estimate has sharedEntityId
+   (quotes, personQuoteGroups) <- case estimate.sharedEntityId of
+     Nothing -> do
+       -- REGULAR SINGLE ESTIMATE FLOW
+       quotes <- traverse (buildSelectedQuote estimate providerInfo now searchRequest) quotesInfo
+       forM_ quotes $ \quote -> do
+         triggerQuoteEvent QuoteEventData {quote = quote, person = person, merchantId = searchRequest.merchantId}
+       QQuote.createMany quotes
+       void $ QEstimate.updateStatus DEstimate.GOT_DRIVER_QUOTE estimate.id
+       return (quotes, [])  -- No personQuoteGroups for single estimate
+       
+     Just sharedEntityId -> do
+       -- SHARED ESTIMATE FLOW
+       -- Validate autoAssign is enabled for shared rides
+       unless (searchRequest.autoAssignEnabledV2 == Just True) $
+         throwError $ InvalidRequest "Auto-assign must be enabled for shared rides"
+         
+       -- Get all estimates with same sharedEntityId
+       sharedEstimates <- QEstimate.findAllBySharedEntityId sharedEntityId
+       
+       -- Create quotes grouped by person, estimate, and search request
+       personQuoteGroups <- forM sharedEstimates $ \est -> do
+         -- Fetch person for this estimate's search request
+         estSearchRequest <- QSR.findById est.requestId >>= fromMaybeM (SearchRequestDoesNotExist est.requestId.getId)
+         estPerson <- Person.findById estSearchRequest.riderId >>= fromMaybeM (PersonNotFound estSearchRequest.riderId.getId)
+         estQuotes <- traverse (buildSelectedQuote est providerInfo now searchRequest) quotesInfo
+         return (estPerson, est, estSearchRequest, estQuotes)
+         
+       -- Trigger quote events with correct person for each quote
+       let allPersonQuotePairs = concatMap (\(person, _, _, quotes) -> map (person,) quotes) personQuoteGroups
+       forM_ allPersonQuotePairs $ \(quotePerson, quote) -> do
+         triggerQuoteEvent QuoteEventData {quote = quote, person = quotePerson, merchantId = searchRequest.merchantId}
+         
+       let allQuotes = map snd allPersonQuotePairs
+       QQuote.createMany allQuotes
+       
+       -- Update status for all shared estimates
+       forM_ sharedEstimates $ \est -> 
+         void $ QEstimate.updateStatus DEstimate.GOT_DRIVER_QUOTE est.id
+         
+       -- Update shared entity status to GOT_DRIVER_QUOTE
+       void $ QSharedEntity.updateStatus DSharedEntity.GOT_DRIVER_QUOTE sharedEntityId
+       
+       return (allQuotes, personQuoteGroups)
```

---

## 🚛 Provider Platform Changes

### 1. Beckn ACL Select - Shared Ride Detection

**File**: `Backend/app/provider-platform/dynamic-offer-driver-app/Main/src/Beckn/ACL/Select.hs`

```diff
@@ -68,0 +68,12 @@
+ -- Add after existing getBookAnyEstimates call
+ bookAnyEstimates <- getBookAnyEstimates item.itemTags
+ sharedRideRemEstimates <- getSharedRideEstimates item.itemTags
+ 
+ -- Mutual exclusion check - both cannot happen at the same time
+ let (finalBookAnyEstimates, finalSharedRideEstimates) = 
+       if not (null sharedRideRemEstimates)
+         then ([], sharedRideRemEstimates)  -- Shared ride case
+         else (bookAnyEstimates, [])        -- Regular ride case
+ 
+ -- Update line 86 to append both estimate lists
+ let estimateIds = map (.estimateId) estimates <> finalBookAnyEstimates <> finalSharedRideEstimates
```

```diff
@@ -0,0 +5,5 @@
+ -- Add new function for shared ride estimates
+ getSharedRideEstimates :: (MonadFlow m, Log m) => [TG.TagGroup] -> m [Text]
+ getSharedRideEstimates itemTags = do
+   let tagGroups = filter (\tag -> tag.tagGroupDescriptor.code == show Tags.SHAREDRIDE_SELECT_ESTIMATES) itemTags
+   pure $ concatMap (map (.tagValue) . (.tagGroupList)) tagGroups
```

### 2. Provider Platform Select Handler

**File**: `Backend/app/provider-platform/dynamic-offer-driver-app/Main/src/Domain/Action/Beckn/Select.hs`

```diff
@@ -0,0 +43,43 @@
+ -- Add in select handler using the isSharedRide flag
+ when req.isSharedRide $ do
+   now <- getCurrentTime
+   
+   -- Get first estimate for reference data (no need to fetch all)
+   primaryEstimate <- QEstimate.findById (head estimateIds) >>= fromMaybeM (EstimateNotFound (head estimateIds).getId)
+   primarySearchRequest <- QSearchRequest.findById primaryEstimate.requestId >>= fromMaybeM (SearchRequestNotFound primaryEstimate.requestId.getId)
+   
+   -- Get search request IDs from estimates
+   allEstimates <- catMaybes <$> mapM QEstimate.findById estimateIds
+   let searchRequestIds = map (.requestId) allEstimates
+   
+   -- Create unified shared_entity record
+   sharedEntityId <- Id <$> generateGUID
+   let trackedSearchRequests = map (\srId -> TrackedEntity { entityId = srId.getId, isActive = True }) searchRequestIds
+   let trackedEstimates = map (\estId -> TrackedEntity { entityId = estId, isActive = True }) estimateIds
+   
+   let sharedEntity = SharedEntity
+         { id = sharedEntityId,
+           status = SharedEntityActive,
+           entityType = ESTIMATE_GROUP,
+           searchRequestIds = trackedSearchRequests,
+           estimateIds = trackedEstimates,
+           bookingIds = [],
+           rideIds = [],
+           merchantId = primarySearchRequest.merchantId,
+           merchantOperatingCityId = primarySearchRequest.merchantOperatingCityId,
+           vehicleCategory = primarySearchRequest.vehicleCategory,
+           tripCategory = RideShare,
+           driverId = Nothing,
+           pairingTime = Just now,
+           createdAt = now,
+           updatedAt = now
+         }
+   
+   -- Store shared entity
+   QSharedEntity.create sharedEntity
+   
+   -- Update all estimates to reference the shared entity
+   QEstimate.updateAll estimateIds (\est -> est { sharedEntityId = Just sharedEntityId })
+   
+   -- Update all search requests to reference the shared entity
+   QSearchRequest.updateAll searchRequestIds (\sr -> sr { sharedEntityId = Just sharedEntityId })
```

### 3. TripQuoteDetail Enhancement

**File**: `Backend/app/provider-platform/dynamic-offer-driver-app/Main/src/SharedLogic/DriverPool/Types.hs`

```diff
@@ -234,0 +234,1 @@
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
+     sharedEntityId :: Maybe (Id SharedEntity.SharedEntity)  -- NEW: shared entity reference
    }
```

### 4. Init Handler - Shared Booking Creation

**File**: `Backend/app/provider-platform/dynamic-offer-driver-app/Main/src/Domain/Action/Beckn/Init.hs`

```diff
@@ -645,0 +645,40 @@
+         Just sharedEntityId -> do
+           -- NEW SHARED BOOKING LOGIC
+           -- 1. Fetch all search requests with same sharedEntityId
+           sharedSearchRequests <- QSR.findAllBySharedEntityId sharedEntityId
+           
+           -- 2. Create bookings for each search request with sharedEntityId and collect booking IDs
+           (allBookings, allBookingIds) <- foldM (\(bookings, bookingIds) searchReq -> do
+             sharedBooking <- buildBooking searchReq driverQuote driverQuote.id.getId driverQuote.tripCategory now (mbPaymentMethod <&> (.id)) paymentUrl (Just driverQuote.distanceToPickup) req.initReqDetails searchReq.configInExperimentVersions driverQuote.coinsRewardedOnGoldTierRide (Just sharedEntityId)
+             
+             -- 3. Trigger BookingCreatedEvent for each person
+             triggerBookingCreatedEvent BookingEventData {booking = sharedBooking, personId = searchReq.riderId, merchantId = transporter.id}
+             
+             -- 4. Create booking in database
+             QRB.createBooking sharedBooking
+             
+             -- 5. Append booking ID to list
+             let updatedBookingIds = bookingIds ++ [sharedBooking.id.getId]
+             return (bookings ++ [sharedBooking], updatedBookingIds)
+           ) ([], []) sharedSearchRequests
+           
+           -- 6. Update only the original customer's search try to COMPLETED
+           QST.updateStatus DST.COMPLETED (searchTry.id)
+           
+           -- 7. Update shared entity status and booking IDs in one go
+           void $ QSharedEntity.updateStatusAndBookingIds DSharedEntity.BOOKING_CREATED sharedEntityId allBookingIds
+           
+           -- 8. Return only the original customer's booking
+           let originalBooking = find (\booking -> booking.transactionId == searchRequest.transactionId) allBookings
+           case originalBooking of
+             Just origBooking -> return (origBooking, Just driverQuote.driverName, Just driverQuote.driverId.getId)
+             Nothing -> throwError $ InternalError "Original customer booking not found in shared bookings"
```

### 5. Confirm Handler - Shared Ride Initialization

**File**: `Backend/app/provider-platform/dynamic-offer-driver-app/Main/src/Domain/Action/Beckn/Confirm.hs`

```diff
@@ -820,0 +820,43 @@
+     Just sharedBookingId -> do
+       -- NEW SHARED RIDE LOGIC
+       -- 1. Fetch all bookings with same sharedBookingId
+       sharedBookings <- QRB.findAllBySharedBookingId sharedBookingId
+       
+       -- 2. Create HashMap to store transactionId mapped to bookingId
+       let transactionIdToBookingMap = HM.fromList $ map (\sharedBooking -> (sharedBooking.transactionId, sharedBooking.id.getId)) sharedBookings
+       
+       -- 3. Call initializeRide for each booking and collect ride data
+       (allRideData, allRideIds) <- foldM (\(rideDataList, rideIds) (idx, sharedBooking) -> do
+         let oneTimeChecks = idx == 0  -- True for first call, False for rest
+         
+         -- Call initializeRide with oneTimeChecks flag and shared vehicle
+         (ride, rideDetails, _) <- initializeRide merchant driver sharedBooking Nothing (Just req.enableFrequentLocationUpdates) driverQuote.clientId (Just req.enableOtpLessRide) oneTimeChecks vehicle
+         
+         -- Store ride data and append ride ID to list
+         let rideData = (ride, rideDetails, vehicle)
+         let updatedRideIds = rideIds ++ [ride.id.getId]
+         return (rideDataList ++ [rideData], updatedRideIds)
+       ) ([], []) (zip [0..] sharedBookings)
+       
+       -- 4. Deactivate quotes only once (already handled by oneTimeChecks in initializeRide)
+       void $ deactivateExistingQuotes booking.merchantOperatingCityId merchant.id driver.id driverQuote.searchTryId $ mkPrice (Just driverQuote.currency) driverQuote.estimatedFare
+       
+       -- 5. Update shared entity status and rideIds column
+       void $ QSharedEntity.updateStatusAndRideIds DSharedEntity.RIDE_INITIALIZED sharedBookingId allRideIds
+       
+       -- 6. Return original customer's ride info with transactionIdToBookingMap
+       let originalRideData = find (\(ride, _, _) -> ride.bookingId == booking.id) allRideData
+       case originalRideData of
+         Just (origRide, _, _) -> do
+           uBooking2 <- QRB.findById booking.id >>= fromMaybeM (BookingNotFound booking.id.getId)
+           mkDConfirmResp (Just $ RideInfo {ride = origRide, driver, vehicle}) uBooking2 riderDetails (Just transactionIdToBookingMap)
+         Nothing -> throwError $ InternalError "Original customer ride not found in shared rides"
```

### 6. Cancel Handler - Shared Ride Cancellation

**File**: `Backend/app/provider-platform/dynamic-offer-driver-app/Main/src/Domain/Action/Beckn/Cancel.hs`

```diff
@@ -131,0 +131,5 @@
+ -- NEW: Add shared ride cancellation check
+ when (sharedRideChecks && isJust booking.sharedEntityId) $ 
+   handleSharedRideCancel booking ride
```

```diff
@@ -0,0 +52,52 @@
+ handleSharedRideCancel :: SRB.Booking -> SRide.Ride -> Flow ()
+ handleSharedRideCancel booking ride = do
+   -- 1. Fetch shared entity data
+   sharedEntityId <- booking.sharedEntityId & fromMaybeM (InternalError "SharedEntityId not found")
+   sharedEntity <- QSharedEntity.findById sharedEntityId >>= fromMaybeM (SharedEntityNotFound sharedEntityId)
+   
+   -- 2. Get current threshold from config
+   threshold <- getSharedRideThreshold booking.merchantOperatingCityId
+   
+   -- 3. Analyze ride states
+   let rideList = sharedEntity.rideIds
+       bookingList = sharedEntity.bookingIds
+   
+   (activeRideCount, inProgressRideCount, activeRideIds) <- 
+     analyzeSharedRideStates rideList ride.id
+   
+   -- 4. Decision logic based on ride states (activeRideCount excludes current ride)
+   if inProgressRideCount >= 1 || activeRideCount >= threshold
+     then handleSingleRideCancel booking ride sharedEntityId sharedEntity
+     else handleBelowThresholdCancel booking ride sharedEntityId activeRideIds sharedEntity
+ 
+ handleSingleRideCancel :: SRB.Booking -> SRide.Ride -> Text -> DSharedEntity.SharedEntity -> Flow ()
+ handleSingleRideCancel booking ride sharedEntityId sharedEntity = do
+   -- Update booking and ride lists at app level
+   let updatedBookingList = updateTrackedEntityStatus sharedEntity.bookingIds booking.id.getId False
+       updatedRideList = updateTrackedEntityStatus sharedEntity.rideIds ride.id.getId False
+   
+   -- Single update query with new JSON strings
+   void $ QSharedEntity.updateBookingAndRideLists sharedEntityId updatedBookingList updatedRideList
+ 
+ handleBelowThresholdCancel :: SRB.Booking -> SRide.Ride -> Text -> [Text] -> DSharedEntity.SharedEntity -> Flow ()
+ handleBelowThresholdCancel booking ride sharedEntityId activeRideIds sharedEntity = do
+   -- 1. Mark ALL bookings and rides as inactive at app level
+   let updatedBookingList = map (\entity -> entity { isActive = False }) sharedEntity.bookingIds
+       updatedRideList = map (\entity -> entity { isActive = False }) sharedEntity.rideIds
+   
+   -- 2. Single update query: set all inactive + status CANCELLED
+   void $ QSharedEntity.updateStatusAndLists DSharedEntity.CANCELLED sharedEntityId updatedBookingList updatedRideList
+   
+   -- 3. Fork cancel for OTHER active rides only (current ride handled by main flow)
+   forM_ activeRideIds $ \rideId -> do
+     fork ("cancelSharedRide-" <> rideId) $ do
+       otherRide <- QRide.findById (Id rideId) >>= fromMaybeM (RideNotFound rideId)
+       otherBooking <- QRB.findById otherRide.bookingId >>= fromMaybeM (BookingNotFound otherRide.bookingId.getId)
+       -- Call cancel with sharedRideChecks=false to avoid recursion
+       void $ cancel 
+         (CancelRideReq otherRide.bookingId Nothing Nothing Nothing) 
+         merchant 
+         otherBooking 
+         Nothing 
+         False  -- sharedRideChecks = false
```

---

## 🗄️ Database Schema Changes

### 1. Person Table
```sql
ALTER TABLE atlas_app.person 
ADD COLUMN kyc_verified BOOLEAN DEFAULT FALSE;
```

### 2. Search Request Table
```sql
ALTER TABLE atlas_app.search_request 
ADD COLUMN nearby_riders_count INTEGER,
ADD COLUMN shared_entity_id TEXT;
```

### 3. Estimate Table
```sql
ALTER TABLE atlas_app.estimate 
ADD COLUMN shared_entity_id TEXT;
```

### 4. Quote Table
```sql
ALTER TABLE atlas_app.quote 
ADD COLUMN shared_entity_id TEXT;
```

### 5. Booking Table
```sql
ALTER TABLE atlas_app.booking 
ADD COLUMN shared_booking_id TEXT;
```

### 6. Ride Table
```sql
ALTER TABLE atlas_app.ride 
ADD COLUMN shared_entity_id TEXT;
```

### 7. Unified SharedEntity Table
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

---

## 🏷️ Beckn Tag Definitions

### File: `lib/beckn-spec/src/BecknV2/OnDemand/Tags.hs`

```diff
@@ -99,0 +99,1 @@
   | SAFETY_PLUS_INFO
   | INSURANCE_INFO
+  | SHARED_RIDE_INFO  -- ADD THIS LINE
   deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)
```

```diff
@@ -117,0 +117,1 @@
     DELIVERY -> (Just "Delivery Information", Nothing)
     DRIVER_REACHED_DESTINATION_INFO -> (Just "Driver Reached Destination Information", Nothing)
+    SHARED_RIDE_INFO -> (Just "Shared Ride Information", Nothing)  -- ADD THIS LINE
     _ -> (Just $ convertToSentence tagGroup, Nothing)
```

```diff
@@ -475,0 +475,2 @@
   | INSURED_AMOUNT
   | RESERVED_RIDE_TAG
   | RESERVED_PRICING_TAG
+  | SHAREDRIDE_SELECT_ESTIMATES  -- ADD THIS LINE
+  | TRANSACTION_ID_TO_BOOKING_MAP  -- ADD THIS LINE
   deriving (Show, Eq, Generic, ToJSON, FromJSON)
```

---

## 📦 Storage Query Functions

### Required New Query Functions

```haskell
-- Storage.Queries.SearchRequest
updateNearbyRidersCount :: Id SearchRequest -> Int -> SqlDB ()
findAllBySharedEntityId :: Text -> m [SearchRequest]

-- Storage.Queries.Estimate
findAllBySharedEntityId :: Text -> m [Estimate]
updateAll :: [Id Estimate] -> (Estimate -> Estimate) -> m ()

-- Storage.Queries.Quote
createMany :: [Quote] -> m ()

-- Storage.Queries.SharedEntity
create :: SharedEntity -> m ()
findById :: Text -> m (Maybe SharedEntity)
updateStatus :: EntityStatus -> Text -> m ()
updateStatusAndBookingIds :: EntityStatus -> Text -> [Text] -> m ()
updateStatusAndRideIds :: EntityStatus -> Text -> [Text] -> m ()
updateBookingAndRideLists :: Text -> [TrackedEntity] -> [TrackedEntity] -> m ()

-- Storage.Queries.Booking
findAllBySharedBookingId :: Text -> m [Booking]

-- Storage.CachedQueries.SharedRideConfig
findByMerchantOpCityIdAndType :: Id MerchantOperatingCity -> ShareRideType -> m (Maybe SharedRideConfig)
```

---

## 🔄 Redis Data Structures

### Route Cache
- **Key Pattern**: `route_cache:{searchId}`
- **Value**: JSON encoded route response
- **TTL**: Configurable (default: 30 minutes)

### Search Hotspots GSI
- **Key**: `searchHotSpots`
- **Members**: `{searchId}:{validTill}`
- **Coordinates**: Pickup location lat/lon
- **Auto-expiry**: Based on search validTill

### Pooled Riders Cache
- **Key Pattern**: `pooled_riders:{estimateId}`
- **Value**: JSON array of paired search request IDs
- **TTL**: 5 minutes (configurable)

### Customer Locks
- **Key Pattern**: `customer_lock:{searchRequestId}`
- **Value**: Timestamp of lock acquisition
- **TTL**: 2 minutes (configurable)

---

## 🎯 Key Implementation Notes

1. **Unified SharedEntity Design**: Uses single `shared_entity` table approach instead of separate tables
2. **Field Naming**: Uses `sharedEntityId` (camelCase) in domain types, `shared_entity_id` (snake_case) in database
3. **Backward Compatibility**: All changes maintain compatibility with existing single-ride flows
4. **Trip Category Based**: Shared rides identified by `tripCategory == RideShare`, not additional flags
5. **Auto-assign Requirement**: Shared rides require `autoAssignEnabledV2 == Just True`
6. **oneTimeChecks Optimization**: Prevents redundant operations in shared ride initialization
7. **TrackedEntity Structure**: Uses `{entityId: Text, isActive: Bool}` for flexible entity tracking

---

## 🧪 Testing Strategy

### API Testing Priorities:
1. **Shared Ride Search Flow**: Verify estimates appear only when hotspot threshold met
2. **Batch Creation**: Test pre-batching validation and shared entity creation
3. **Quote Generation**: Verify quotes created for all participants in shared ride
4. **Booking Creation**: Test individual bookings with shared entity references
5. **Ride Initialization**: Verify optimization with oneTimeChecks flag
6. **Cancellation Logic**: Test threshold-based cancellation decisions
7. **Tag Propagation**: Verify Beckn tags flow correctly between platforms

### Rollback Capability:
- Feature flag `enableSharedRide` for instant disable
- Database columns have default values for backward compatibility
- Redis structures are non-breaking additions 