# Chunk 4 Implementation Plan: OnSelect Shared Entity Logic

## Overview
Implementation of OnSelect.hs logic to handle shared estimates using the unified `shared_entity` table approach.

## ✅ COMPLETED: OnSelect.hs sharedEntityId Integration

### Implementation Details

**Location**: `Backend/app/rider-platform/rider-app/Main/src/Domain/Action/Beckn/OnSelect.hs`

### Key Changes Made:

## COMPLETE onSelect Handler Rewrite

```haskell
onSelect :: OnSelectValidatedReq -> Flow ()
onSelect OnSelectValidatedReq {..} = do
  now <- getCurrentTime
  
  -- Check if estimate has sharedEntityId
  (quotes, personQuoteGroups) <- case estimate.sharedEntityId of
    Nothing -> do
      -- REGULAR SINGLE ESTIMATE FLOW
      quotes <- traverse (buildSelectedQuote estimate providerInfo now searchRequest) quotesInfo
      forM_ quotes $ \quote -> do
        triggerQuoteEvent QuoteEventData {quote = quote, person = person, merchantId = searchRequest.merchantId}
      QQuote.createMany quotes
      void $ QEstimate.updateStatus DEstimate.GOT_DRIVER_QUOTE estimate.id
      return (quotes, [])  -- No personQuoteGroups for single estimate
      
    Just sharedEntityId -> do
      -- SHARED ESTIMATE FLOW
      -- Validate autoAssign is enabled for shared rides
      unless (searchRequest.autoAssignEnabledV2 == Just True) $
        throwError $ InvalidRequest "Auto-assign must be enabled for shared rides"
        
      -- Get all estimates with same sharedEntityId
      sharedEstimates <- QEstimate.findAllBySharedEntityId sharedEntityId
      
      -- Create quotes grouped by person, estimate, and search request
      personQuoteGroups <- forM sharedEstimates $ \est -> do
        -- Fetch person for this estimate's search request
        estSearchRequest <- QSR.findById est.requestId >>= fromMaybeM (SearchRequestDoesNotExist est.requestId.getId)
        estPerson <- Person.findById estSearchRequest.riderId >>= fromMaybeM (PersonNotFound estSearchRequest.riderId.getId)
        estQuotes <- traverse (buildSelectedQuote est providerInfo now searchRequest) quotesInfo
        return (estPerson, est, estSearchRequest, estQuotes)
        
      -- Trigger quote events with correct person for each quote
      let allPersonQuotePairs = concatMap (\(person, _, _, quotes) -> map (person,) quotes) personQuoteGroups
      forM_ allPersonQuotePairs $ \(quotePerson, quote) -> do
        triggerQuoteEvent QuoteEventData {quote = quote, person = quotePerson, merchantId = searchRequest.merchantId}
        
      let allQuotes = map snd allPersonQuotePairs
      QQuote.createMany allQuotes
      
      -- Update status for all shared estimates
      forM_ sharedEstimates $ \est -> 
        void $ QEstimate.updateStatus DEstimate.GOT_DRIVER_QUOTE est.id
        
      -- Update shared entity status to GOT_DRIVER_QUOTE
      void $ QSharedEntity.updateStatus DSharedEntity.GOT_DRIVER_QUOTE sharedEntityId
      
      return (allQuotes, personQuoteGroups)
  
  -- AUTO-ASSIGN LOGIC
  if searchRequest.autoAssignEnabledV2 == Just True
    then do
      -- Find exophone once for both normal and shared rides
      exophone <- findRandomExophone merchantOperatingCityId
      
      case estimate.sharedEntityId of
        Nothing -> do
          -- NORMAL RIDE AUTO-ASSIGN: Get single lowest fare quote
          let lowestFareQuote = selectLowestFareQuote quotes
          case lowestFareQuote of
            Just (autoAssignQuote, False) -> do
              isLockAcquired <- SConfirm.tryInitTriggerLock autoAssignQuote.requestId
              when isLockAcquired $ do
                let dConfirmReq = SConfirm.DConfirmReq 
                      { personId = person.id, 
                        quote = autoAssignQuote,
                        paymentMethodId = searchRequest.selectedPaymentMethodId 
                      }
                dConfirmRes <- SConfirm.confirm dConfirmReq exophone
                -- Post-confirmation logic (same as current onSelect)
                becknInitReq <- ACL.buildInitReqV2 dConfirmRes
                handle (errHandler dConfirmRes.booking) $ do
                  Metrics.startMetricsBap Metrics.INIT dConfirmRes.merchant.name searchRequest.id.getId dConfirmRes.booking.merchantOperatingCityId.getId
                  void . withShortRetry $ CallBPP.initV2 dConfirmRes.providerUrl becknInitReq searchRequest.merchantId
            _ -> do
              -- When auto-assign doesn't happen, send notifications
              bppDetails <- forM ((.providerId) <$> quotes) (\bppId -> CQBPP.findBySubscriberIdAndDomain bppId Context.MOBILITY >>= fromMaybeM (InternalError $ "BPP details not found for providerId:-" <> bppId <> "and domain:-" <> show Context.MOBILITY))
              Notify.notifyOnDriverOfferIncoming estimate.id estimate.tripCategory quotes person bppDetails
            
        Just sharedEntityId -> do
          -- SHARED RIDE AUTO-ASSIGN: Get array of quotes with person data (min for each estimate)
          let autoAssignQuotesWithPersonData = getQuotesForSharedEntityWithPersonData personQuoteGroups
          
          -- Acquire lock for original estimate first
          isLockAcquired <- SConfirm.tryInitTriggerLock estimate.requestId
          when isLockAcquired $ do
            -- Confirm all quotes and collect results with booking IDs and searchRequest-to-booking mapping
            (confirmResults, allBookingIds, searchRequestToBookingMap) <- foldM (\(results, bookingIds, searchReqMap) (quotePerson, quoteSearchRequest, autoAssignQuote) -> do
              let dConfirmReq = SConfirm.DConfirmReq 
                    { personId = quotePerson.id,  -- Use quote's corresponding person
                      quote = autoAssignQuote,
                      paymentMethodId = quoteSearchRequest.selectedPaymentMethodId  -- Use quote's search request payment method
                    }
              dConfirmRes <- SConfirm.confirm dConfirmReq exophone
              
              -- Append booking ID to list and add to HashMap mapping
              let updatedBookingIds = bookingIds ++ [dConfirmRes.booking.id.getId]
              let updatedSearchReqMap = Map.insert (quoteSearchRequest.id.getId) (dConfirmRes.booking.id.getId) searchReqMap
              return (results ++ [dConfirmRes], updatedBookingIds, updatedSearchReqMap)
            ) ([], [], Map.empty) autoAssignQuotesWithPersonData
            
            -- Update shared entity status and booking IDs
            void $ QSharedEntity.updateStatusAndBookingIds DSharedEntity.BOOKING_CONFIRMED sharedEntityId allBookingIds
            
            -- Filter to get original estimate's confirmation result and call initV2 once
            let originalConfirmResult = find (\dConfirmRes -> dConfirmRes.booking.estimateId == estimate.id) confirmResults
            case originalConfirmResult of
              Just dConfirmRes -> do
                -- Add searchRequestToBookingMap to DConfirmRes before passing to buildInitReqV2
                let dConfirmResWithMap = dConfirmRes { searchRequestToBookingMap = Just searchRequestToBookingMap }
                becknInitReq <- ACL.buildInitReqV2 dConfirmResWithMap
                handle (errHandler dConfirmRes.booking) $ do
                  Metrics.startMetricsBap Metrics.INIT dConfirmRes.merchant.name searchRequest.id.getId dConfirmRes.booking.merchantOperatingCityId.getId
                  void . withShortRetry $ CallBPP.initV2 dConfirmRes.providerUrl becknInitReq searchRequest.merchantId
              Nothing -> pure ()
    else do
      -- NON-AUTO-ASSIGN FLOW: Send notifications to customer
      bppDetails <- forM ((.providerId) <$> quotes) (\bppId -> CQBPP.findBySubscriberIdAndDomain bppId Context.MOBILITY >>= fromMaybeM (InternalError $ "BPP details not found for providerId:-" <> bppId <> "and domain:-" <> show Context.MOBILITY))
      Notify.notifyOnDriverOfferIncoming estimate.id estimate.tripCategory quotes person bppDetails
```

#### 2. Updated buildSelectedQuote Function
Added sharedEntityId propagation to Quote creation:
```haskell
let quote = DQuote.Quote {
  -- ... other fields ...
  sharedEntityId = estimate.sharedEntityId,
  ..
}
```


### Logic Flow:

1. **Single Estimate Path** (`estimate.sharedEntityId = Nothing`):
   - Creates quotes for the single estimate
   - Updates estimate status to `GOT_DRIVER_QUOTE`
   - Triggers quote events
   - Stores quotes in database

2. **Shared Estimate Path** (`estimate.sharedEntityId = Just sharedEntityId`):
   - **Validates autoAssign is enabled**: Checks `searchRequest.autoAssignEnabledV2 == Just True`, throws error if disabled
   - Fetches all estimates with the same `sharedEntityId` using `QEstimate.findAllBySharedEntityId`
   - **Fetches person for each estimate**: Gets person from estimate's search request for proper event triggering
   - **Creates quotes grouped by person and estimate**: Stores (person, estimate, quotes) tuples
   - **Triggers quote events with correct person**: Each quote event uses the quote's corresponding person
   - Updates status for all shared estimates to `GOT_DRIVER_QUOTE`
   - **Updates shared entity status to `GOT_DRIVER_QUOTE`** using `QSharedEntity.updateStatus`
   - Stores all quotes in database with `sharedEntityId` populated
   - **Enhanced auto-assign processing**: For each lowest fare quote, individually processes lock acquisition and confirmation
- **Booking ID collection**: Uses `foldM` to collect booking IDs from all confirm results and updates shared entity booking list

### Dependencies:

#### Required Domain Type Updates:
```haskell
-- In SharedLogic.Confirm
data DConfirmRes = DConfirmRes
  { providerId :: Text,
    providerUrl :: BaseUrl,
    itemId :: Text,
    fromLoc :: DL.Location,
    toLoc :: Maybe DL.Location,
    stops :: [DL.Location],
    vehicleVariant :: DV.VehicleVariant,
    bppQuoteId :: Text,
    booking :: DRB.Booking,
    riderPhone :: Maybe Text,
    riderName :: Maybe Text,
    searchRequestId :: Id DSReq.SearchRequest,
    merchant :: DM.Merchant,
    city :: Context.City,
    maxEstimatedDistance :: Maybe Distance,
    paymentMethodInfo :: Maybe DMPM.PaymentMethodInfo,
    confirmResDetails :: Maybe DConfirmResDetails,
    isAdvanceBookingEnabled :: Maybe Bool,
    isInsured :: Maybe Bool,
    insuredAmount :: Maybe Text,
    searchRequestToBookingMap :: Maybe (Map Text Text)  -- NEW FIELD: Maps search request IDs to booking IDs
  }
```

#### Required Query Functions:
```haskell
-- In Storage.Queries.Estimate
findAllBySharedEntityId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) 
                        => Text 
                        -> m [Domain.Types.Estimate.Estimate]

-- In Storage.Queries.SharedEntity
updateStatus :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r)
             => Domain.Types.SharedEntity.EntityStatus
             -> Text
             -> m ()
```

#### Required Helper Functions:
```haskell
-- In Domain.Action.Beckn.OnSelect
getQuotesForSharedEntityWithPersonData :: [(Person, Estimate, SearchRequest, [Quote])] -> [(Person, SearchRequest, Quote)]
-- Function to get lowest fare quote for each estimate with person and search request data

findRandomExophone :: Id MerchantOperatingCity -> Flow Exophone
-- Function to find random exophone (copy from SharedLogic.Confirm)
```

#### Required Function Signature Update:
```haskell
-- In SharedLogic.Confirm
SConfirm.confirm :: DConfirmReq -> Exophone -> Flow ()
-- Updated to accept exophone as separate parameter
```

#### Required Domain Type Fields:
- `Estimate.sharedEntityId :: Maybe Text` ✅ (assumed present)
- `Quote.sharedEntityId :: Maybe Text` ✅ (assumed present via schema changes)
- `SharedEntity.EntityStatus` with `GOT_DRIVER_QUOTE` enum value ✅ (assumed present)

### Benefits:

1. **Unified Quote Creation**: All estimates in a shared ride group get quotes simultaneously
2. **Consistent Status Updates**: All shared estimates move to `GOT_DRIVER_QUOTE` status together
3. **Shared Entity Tracking**: Shared entity status updated to `GOT_DRIVER_QUOTE` for centralized state management
4. **Event Synchronization**: Quote events fired for all participants
5. **Data Integrity**: `sharedEntityId` propagated from Estimate to Quote for tracking
6. **Auto-Assign Validation**: Ensures shared rides only proceed when auto-assignment is enabled
7. **Individual Quote Selection**: Each estimate gets its own lowest fare quote independently, no global minimum calculation
8. **Individual Quote Processing**: Each quote in shared rides is processed individually with its own lock acquisition and confirmation
9. **Shared Entity Booking Tracking**: Collects all booking IDs from confirm results and updates shared entity with `BOOKING_CONFIRMED` status

### Testing Strategy:

1. **Unit Tests**:
   - Test single estimate flow (sharedEntityId = Nothing)
   - Test shared estimate flow (sharedEntityId = Just "shared-id") with autoAssign enabled
   - Test shared estimate flow with autoAssign disabled (should throw error)
   - Verify quote creation count matches estimate count
   - Verify sharedEntityId propagation
   - Verify shared entity status update to `GOT_DRIVER_QUOTE`
   - **Test individual quote selection logic**: 
     - Multiple estimates each get their own lowest fare quote
     - No global minimum calculation
     - Edge cases: no quotes, empty estimates
   - **Test individual quote processing**:
     - Normal rides: single quote processing
     - Shared rides: multiple individual quote confirmations
     - Verify each quote gets its own lock acquisition and confirmation
     - Test exophone finding for each quote
   - **Test shared entity booking tracking**:
     - Verify booking IDs collection from all confirm results
     - Test shared entity status update to `BOOKING_CONFIRMED`
     - Verify booking IDs list update in shared entity

2. **Integration Tests**:
   - Test complete OnSelect flow for shared rides
   - Verify database state after shared quote creation
   - Verify shared entity status consistency across database
   - Test quote event triggering for all participants

#### 3. Key Implementation Notes

**DConfirmReq Structure (unchanged)**:
```haskell
data DConfirmReq = DConfirmReq
  { personId :: Id DPerson.Person,
    quote :: DQuote.Quote,  -- Keep original single quote field
    paymentMethodId :: Maybe (Id PaymentMethod)
  }
```

**SConfirm.confirm Function Signature Update**:
```haskell
SConfirm.confirm :: DConfirmReq -> Exophone -> Flow ()
-- Updated to accept exophone as separate parameter
```

#### Helper Functions for Quote Selection:
```haskell
-- Get lowest fare quote for each estimate with person and search request data
-- This function takes [(Person, Estimate, SearchRequest, [Quote])] and returns [(Person, SearchRequest, Quote)]
-- For each estimate, it finds the minimum fare quote and returns it with corresponding person and search request
-- Result: [(person1, searchReq1, minQuoteForEstimate1), (person2, searchReq2, minQuoteForEstimate2), ...]
getQuotesForSharedEntityWithPersonData :: [(Person, Estimate, SearchRequest, [Quote])] -> [(Person, SearchRequest, Quote)]
getQuotesForSharedEntityWithPersonData = 
  concatMap (\(person, _, searchRequest, quotes) -> 
    case selectLowestFareQuote quotes of
      Just (lowestQuote, _) -> [(person, searchRequest, lowestQuote)]  -- Min quote with person and search request data
      Nothing -> []  -- No quotes for this estimate
  )

-- Example:
-- Input: [(person1, estimate1, searchReq1, [quote1, quote2, quote3]), 
--         (person2, estimate2, searchReq2, [quote4, quote5]), 
--         (person3, estimate3, searchReq3, [quote6])]
-- Output: [(person1, searchReq1, cheapestFromEstimate1), (person2, searchReq2, cheapestFromEstimate2), (person3, searchReq3, quote6)]
```

### Next Steps:

1. Implement `QEstimate.findAllBySharedEntityId` query function
2. Implement `QSharedEntity.updateStatus` query function
3. **Copy `findRandomExophone` function from SharedLogic.Confirm to OnSelect**
4. **Implement `getQuotesForSharedEntityWithPersonData` helper function**
5. **Update `SConfirm.confirm` function signature to accept exophone as separate parameter**
6. **Update all `SConfirm.confirm` calls to pass exophone parameter**
7. Ensure Quote domain type has `sharedEntityId` field
8. Ensure SharedEntity domain type has `EntityStatus` with `GOT_DRIVER_QUOTE` enum
9. Verify database schema has `shared_entity_id` column in quote table
10. **Update auto-assign logic to find exophone once and process quotes individually**
11. Run comprehensive tests including individual quote processing scenarios
12. **Update Init.hs transformer to add shared ride tag group when searchRequestToBookingMap is present**
13. **Create makeSharedRideTagGroup function to pass searchRequestToBookingMap in stringified format**
14. Move to next chunk of shared ride implementation

### Init.hs Transformer Updates:

#### Update mkItemTags function:
```haskell
-- In Beckn/OnDemand/Transformer/Init.hs
mkItemTags :: SharedLogic.Confirm.DConfirmRes -> [Spec.TagGroup]
mkItemTags res =
  let itemTags = if maybe False Trip.isDeliveryTrip res.booking.tripCategory then mkDeliveryTagGroup res else []
      itemTags' = mkAdvancedBookingEnabledTagGroup res : itemTags
      itemTags'' = mkInsuranceTagGroup res : itemTags'
      itemTags''' = case res.searchRequestToBookingMap of
                      Just searchReqToBookingMap -> makeSharedRideTagGroup searchReqToBookingMap : itemTags''
                      Nothing -> itemTags''
   in itemTags'''
```

#### Create makeSharedRideTagGroup function:
```haskell
-- In Beckn/OnDemand/Transformer/Init.hs
makeSharedRideTagGroup :: Map Text Text -> Spec.TagGroup
makeSharedRideTagGroup searchRequestToBookingMap =
  Spec.TagGroup
    { tagGroupDisplay = Just False,
      tagGroupDescriptor =
        Just $
          Spec.Descriptor
            { descriptorCode = Just $ show Tags.SHARED_RIDE_INFO,  -- See Required Tag Definitions section
              descriptorName = Just "Shared Ride Info",
              descriptorShortDesc = Nothing
            },
      tagGroupList =
        Just
          [ Spec.Tag
              { tagDescriptor =
                  Just $
                    Spec.Descriptor
                      { descriptorCode = Just $ show Tags.SEARCH_REQUEST_TO_BOOKING_MAP,  -- See Required Tag Definitions section
                        descriptorName = Just "Search Request to Booking Mapping",
                        descriptorShortDesc = Nothing
                      },
                tagDisplay = Just False,
                tagValue = Just $ decodeUtf8 $ toStrict $ encode $ HashMap.toList searchRequestToBookingMap  -- JSON encode the HashMap as list of tuples
              }
          ]
    }
```


#### Required Tag Definitions:
```haskell
-- In lib/beckn-spec/src/BecknV2/OnDemand/Tags.hs

-- 1. Add SHARED_RIDE_INFO to BecknTagGroup enum (around line 99):
  | SAFETY_PLUS_INFO
  | INSURANCE_INFO
  | SHARED_RIDE_INFO  -- ADD THIS LINE
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

-- 2. Add SHARED_RIDE_INFO descriptor in getTagGroupDescriptor function (around line 117):
    DELIVERY -> (Just "Delivery Information", Nothing)
    DRIVER_REACHED_DESTINATION_INFO -> (Just "Driver Reached Destination Information", Nothing)
    SHARED_RIDE_INFO -> (Just "Shared Ride Information", Nothing)  -- ADD THIS LINE
    _ -> (Just $ convertToSentence tagGroup, Nothing)

-- 3. Add SEARCH_REQUEST_TO_BOOKING_MAP to BecknTag enum (around line 477):
  | INSURED_AMOUNT
  | RESERVED_RIDE_TAG
  | RESERVED_PRICING_TAG
  | SEARCH_REQUEST_TO_BOOKING_MAP  -- ADD THIS LINE
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

-- 4. Add SEARCH_REQUEST_TO_BOOKING_MAP descriptor in getTagDescriptor function (around line 530):
    IS_INSURED -> (Just "is insured", Nothing)
    INSURED_AMOUNT -> (Just "insured amount", Nothing)
    NYREGULAR_SUBSCRIPTION_CHARGE -> (Just "NYRegular subscription charge", Nothing)
    SEARCH_REQUEST_TO_BOOKING_MAP -> (Just "Search Request to Booking Mapping", Nothing)  -- ADD THIS LINE
    _ -> (Just $ convertToSentence tag, Nothing)

-- 5. Add SEARCH_REQUEST_TO_BOOKING_MAP to tag group mapping in getTagGroup function (around line 582):
    IS_INSURED -> INSURANCE_INFO
    INSURED_AMOUNT -> INSURANCE_INFO
    NYREGULAR_SUBSCRIPTION_CHARGE -> GENERAL_INFO
    SEARCH_REQUEST_TO_BOOKING_MAP -> SHARED_RIDE_INFO  -- ADD THIS LINE
    a -> error $ "getTagGroup function of CompleteTag class is not defined for " <> T.pack (show a) <> " tag"
```

## 🔴 IMPORTANT NOTES:

- **Unified SharedEntity Design**: Uses single `shared_entity` table approach
- **Field Naming**: Uses `sharedEntityId` (camelCase) in domain types
- **Database Column**: Uses `shared_entity_id` (snake_case) in database schema
- **Backward Compatibility**: Non-shared estimates continue to work unchanged
- **SConfirm.confirm Signature Change**: Function now accepts exophone as separate parameter
- **Exophone Optimization**: Single exophone lookup shared across all quotes in shared ride
- **Individual Quote Processing**: Each quote processed separately but with shared exophone

---

## 🚧 NEXT: SharedLogic.Confirm sharedEntityId Integration

### Implementation Details

**Location**: `Backend/app/rider-platform/rider-app/Main/src/SharedLogic/Confirm.hs`

### OnSelect to Confirm Flow Integration

After the OnSelect handler processes shared quotes and triggers auto-assignment, the flow proceeds to `SConfirm.confirm` function. This function needs to be updated to handle shared entity bookings.

### Required Changes in SharedLogic.Confirm

#### 1. Update Confirm Function to Handle SharedEntityId

```haskell
confirm :: DConfirmReq -> Exophone -> Flow DConfirmRes
confirm req exophone = do
  -- Existing validation logic...
  
  -- Create booking with sharedEntityId from quote
  booking <- buildBooking' searchRequest quote now exophone quote.sharedEntityId
  
  -- Rest of existing confirm logic...
  
  where
    buildBooking' :: SearchRequest -> Quote -> UTCTime -> Exophone -> Maybe Text -> Flow Booking
    buildBooking' searchReq quote' now' exophone' mbSharedEntityId = do
      -- Existing buildBooking logic with sharedEntityId parameter
      -- Set sharedBookingId = mbSharedEntityId in booking record
      ...
```

#### 2. SharedEntityId Propagation Flow

**OnSelect → Confirm Integration:**
1. **OnSelect creates quotes** with `sharedEntityId` populated from estimates
2. **Auto-assign selects quotes** with `sharedEntityId` for shared rides  
3. **Confirm receives quotes** containing `sharedEntityId`
4. **Confirm creates bookings** with `sharedBookingId = quote.sharedEntityId`
5. **Booking records linked** to shared entity via `sharedBookingId` field

#### 3. Confirm Function Signature (No Change Required)

The existing `confirm` function signature remains unchanged:
```haskell
confirm :: DConfirmReq -> Exophone -> Flow DConfirmRes

data DConfirmReq = DConfirmReq
  { personId :: Id Person,
    quote :: Quote,  -- Quote now contains sharedEntityId
    paymentMethodId :: Maybe (Id PaymentMethod)
  }
```

### Implementation Plan for SharedLogic.Confirm

#### Required Changes:

1. **Update buildBooking calls** in confirm function to pass `quote.sharedEntityId`
2. **Modify buildBooking function signature** to accept `Maybe Text` sharedEntityId parameter  
3. **Set sharedBookingId field** in booking record creation
4. **Ensure booking domain type** has `sharedBookingId :: Maybe Text` field

#### Code Changes Required:

```haskell
-- In confirm function, replace:
booking <- buildBooking searchRequest quote now exophone

-- With:
booking <- buildBooking searchRequest quote now exophone quote.sharedEntityId

-- Update buildBooking signature:
buildBooking :: SearchRequest -> Quote -> UTCTime -> Exophone -> Maybe Text -> Flow Booking
buildBooking searchReq quote' now' exophone' mbSharedEntityId = do
  -- Existing logic...
  pure Booking {
    -- ... other fields ...
    sharedBookingId = mbSharedEntityId,
    ...
  }
```

### Key Benefits:

1. **Seamless Integration**: OnSelect → Confirm flow maintains sharedEntityId throughout
2. **Individual Booking Records**: Each customer gets separate booking with shared entity reference
3. **Consistent Data Model**: Both Init.hs and Confirm.hs create bookings with sharedEntityId
4. **Backward Compatibility**: Non-shared quotes continue to work (sharedEntityId = Nothing)

### Testing Requirements:

1. **Unit Tests**:
   - Test confirm with shared quotes (sharedEntityId present)
   - Test confirm with regular quotes (sharedEntityId = Nothing)
   - Verify sharedBookingId propagation to booking records

2. **Integration Tests**:
   - Test OnSelect → AutoAssign → Confirm flow for shared rides
   - Verify booking creation with correct sharedEntityId
   - Test database state consistency

### Dependencies:

- `Quote.sharedEntityId :: Maybe Text` field ✅ (from OnSelect implementation)
- `Booking.sharedBookingId :: Maybe Text` field ✅ (schema changes assumed done)
- Updated buildBooking function signature in SharedLogic.Confirm

### Next Steps:

1. Update SharedLogic.Confirm buildBooking calls to pass sharedEntityId
2. Modify buildBooking function signature to accept sharedEntityId parameter
3. Ensure booking record sets sharedBookingId field
4. Test OnSelect → Confirm integration for shared rides
5. Verify booking creation consistency across both Init.hs and Confirm.hs paths

---

#### Required InitReq Type Changes:
```haskell
-- In app/provider-platform/dynamic-offer-driver-app/Main/src/Domain/Action/Beckn/Init.hs

-- 1. Add HashMap import (around line 34):
import qualified Data.HashMap.Strict as HashMap

-- 2. Update InitReq type to include searchRequestToBookingMap field (around line 82):
data InitReq = InitReq
  { fulfillmentId :: FulfillmentId,
    vehicleVariant :: Veh.VehicleVariant,
    bapId :: Text,
    bapUri :: BaseUrl,
    bapCity :: Context.City,
    bapCountry :: Context.Country,
    maxEstimatedDistance :: Maybe HighPrecMeters,
    paymentMethodInfo :: Maybe DMPM.PaymentMethodInfo,
    bppSubscriberId :: Maybe Text,
    riderPhoneNumber :: Text,
    mbRiderName :: Maybe Text,
    estimateId :: Text,
    initReqDetails :: Maybe InitReqDetails,
    isAdvanceBookingEnabled :: Maybe Bool,
    isInsured :: Maybe Bool,
    insuredAmount :: Maybe Text,
    searchRequestToBookingMap :: Maybe (HashMap Text Text)  -- ADD THIS LINE
  }
```

#### Required Init.hs Transformer Changes:
```haskell
-- In app/provider-platform/dynamic-offer-driver-app/Main/src/Beckn/OnDemand/Transformer/Init.hs

-- 1. Add imports (after line 23):
import qualified Data.HashMap.Strict as HashMap
import Data.Text.Encoding (decodeUtf8, encodeUtf8)  
import Data.ByteString.Lazy (toStrict)

-- 2. Update buildDInitReq function to extract searchRequestToBookingMap (around line 53, after isInsured line):
  let isAdvanceBookingEnabled = Just $ getAdvancedBookingEnabled orderItem.itemTags
  let (isInsured, insuredAmount) = getIsInsured orderItem.itemTags
  let searchRequestToBookingMap = getSearchRequestToBookingMap orderItem.itemTags  -- ADD THIS LINE
  pure $ Domain.Action.Beckn.Init.InitReq {bapCity = bapCity_, bapCountry = bapCountry_, bapId = bapId_, bapUri = bapUri_, fulfillmentId = fulfillmentId_, maxEstimatedDistance = maxEstimatedDistance_, paymentMethodInfo = paymentMethodInfo_, vehicleVariant = vehicleVariant_, bppSubscriberId = bppSubscriberId_, estimateId = estimateId, ..}

-- 3. Add helper function to extract searchRequestToBookingMap (after getIsInsured function, around line 134):
getSearchRequestToBookingMap :: Maybe [Spec.TagGroup] -> Maybe (HashMap Text Text)
getSearchRequestToBookingMap tagGroups =
  let tagValue = Utils.getTagV2 Tag.SHARED_RIDE_INFO Tag.SEARCH_REQUEST_TO_BOOKING_MAP tagGroups
   in case tagValue of
        Just jsonStr -> case A.decode (toStrict $ encodeUtf8 jsonStr) of
          Just pairs -> Just $ HashMap.fromList (pairs :: [(Text, Text)])
          Nothing -> Nothing
        Nothing -> Nothing
```

#### Required Import for JSON Decoding:
```haskell
-- Also add this import for UTF8 encoding:
import Data.Text.Encoding (encodeUtf8)
```


---
## 🚧 IN PROGRESS: Init.hs sharedEntityId Integration

### Implementation Details

**Location**: `Backend/app/provider-platform/dynamic-offer-driver-app/Main/src/Domain/Action/Beckn/Init.hs`

### Complete Init Handler Rewrite

Replace the existing handler function (lines 118-151) with the following implementation:

```haskell
handler ::
  ( CacheFlow m r,
    EsqDBFlow m r,
    EventStreamFlow m r,
    EncFlow m r
  ) =>
  Id DM.Merchant ->
  InitReq ->
  ValidatedInitReq ->
  m InitRes
handler merchantId req validatedReq = do
  transporter <- QM.findById merchantId >>= fromMaybeM (MerchantNotFound merchantId.getId)
  now <- getCurrentTime
  paymentId <- generateGUID
  let searchRequest = validatedReq.searchRequest
      riderName = req.mbRiderName
      riderPhoneNumber = req.riderPhoneNumber
  whenJust req.isAdvanceBookingEnabled $ \isAdvanceBookingEnabled' -> do
    QSR.updateIsAdvancedBookingEnabled isAdvanceBookingEnabled' searchRequest.id
  (mbPaymentMethod, paymentUrl) <- fetchPaymentMethodAndUrl searchRequest.merchantOperatingCityId
  (booking, driverName, driverId) <-
    case validatedReq.quote of
      ValidatedEstimate driverQuote searchTry -> do
        -- CHECK FOR SHARED ENTITY ID
        case driverQuote.sharedEntityId of
          Nothing -> do
            -- EXISTING SINGLE BOOKING LOGIC
            booking <- buildBooking searchRequest driverQuote driverQuote.id.getId driverQuote.tripCategory now (mbPaymentMethod <&> (.id)) paymentUrl (Just driverQuote.distanceToPickup) req.initReqDetails searchRequest.configInExperimentVersions driverQuote.coinsRewardedOnGoldTierRide driverQuote.sharedEntityId
            triggerBookingCreatedEvent BookingEventData {booking = booking, personId = driverQuote.driverId, merchantId = transporter.id}
            QRB.createBooking booking
            QST.updateStatus DST.COMPLETED (searchTry.id)
            return (booking, Just driverQuote.driverName, Just driverQuote.driverId.getId)
            
          Just sharedEntityId -> do
            -- NEW SHARED BOOKING LOGIC
            -- 1. Fetch all search requests with same sharedEntityId
            sharedSearchRequests <- QSR.findAllBySharedEntityId sharedEntityId
            
            -- 2. Create bookings for each search request with sharedEntityId and collect booking IDs
            (allBookings, allBookingIds) <- foldM (\(bookings, bookingIds) searchReq -> do
              sharedBooking <- buildBooking searchReq driverQuote driverQuote.id.getId driverQuote.tripCategory now (mbPaymentMethod <&> (.id)) paymentUrl (Just driverQuote.distanceToPickup) req.initReqDetails searchReq.configInExperimentVersions driverQuote.coinsRewardedOnGoldTierRide (Just sharedEntityId)
              
              -- 3. Trigger BookingCreatedEvent for each person
              triggerBookingCreatedEvent BookingEventData {booking = sharedBooking, personId = searchReq.riderId, merchantId = transporter.id}
              
              -- 4. Create booking in database
              QRB.createBooking sharedBooking
              
              -- 5. Append booking ID to list
              let updatedBookingIds = bookingIds ++ [sharedBooking.id.getId]
              return (bookings ++ [sharedBooking], updatedBookingIds)
            ) ([], []) sharedSearchRequests
            
            -- 6. Update only the original customer's search try to COMPLETED
            QST.updateStatus DST.COMPLETED (searchTry.id)
            
            -- 7. Update shared entity status and booking IDs in one go
            void $ QSharedEntity.updateStatusAndBookingIds DSharedEntity.BOOKING_CREATED sharedEntityId allBookingIds
            
            -- 8. Return only the original customer's booking
            let originalBooking = find (\booking -> booking.transactionId == searchRequest.transactionId) allBookings
            case originalBooking of
              Just origBooking -> return (origBooking, Just driverQuote.driverName, Just driverQuote.driverId.getId)
              Nothing -> throwError $ InternalError "Original customer booking not found in shared bookings"
              
      ValidatedQuote quote -> do
        booking <- buildBooking searchRequest quote quote.id.getId quote.tripCategory now (mbPaymentMethod <&> (.id)) paymentUrl Nothing req.initReqDetails searchRequest.configInExperimentVersions Nothing Nothing
        QRB.createBooking booking
        when booking.isScheduled $ void $ addScheduledBookingInRedis booking
        return (booking, Nothing, Nothing)
        
  fork "Updating Demand Hotspots on booking" $ do
    let lat = searchRequest.fromLocation.lat
        lon = searchRequest.fromLocation.lon
        merchantOpCityId = searchRequest.merchantOperatingCityId
    transporterConfig <- CCT.findByMerchantOpCityId merchantOpCityId Nothing >>= fromMaybeM (TransporterConfigNotFound merchantOpCityId.getId)
    DemandHotspots.updateDemandHotspotsOnBooking searchRequest.id merchantOpCityId transporterConfig (Maps.LatLong lat lon)
  let paymentMethodInfo = req.paymentMethodInfo
      bppSubscriberId = req.bppSubscriberId
      estimateId = req.estimateId
      cancellationFee = Nothing
  pure InitRes {vehicleVariant = req.vehicleVariant, ..}
```

### Logic Flow for Shared Bookings:

1. **SharedEntityId Check**: After line 130, check if `driverQuote.sharedEntityId` exists
2. **Single Booking Path** (`sharedEntityId = Nothing`): 
   - Executes existing logic (lines 131-135)
   - Creates single booking, triggers event, updates search try
3. **Shared Booking Path** (`sharedEntityId = Just sharedEntityId`):
   - **Fetch all search requests**: Use `QSR.findAllBySharedEntityId sharedEntityId`
   - **Create multiple bookings**: Use `foldM` to create bookings and collect booking IDs efficiently
   - **Trigger events for each person**: Each booking triggers `BookingCreatedEvent` with correct `riderId`
   - **Update original search try**: Update only the original customer's search try status to `COMPLETED`
   - **Update shared entity status and booking IDs**: Set shared entity status to `BOOKING_CREATED` and update booking IDs list in one operation
   - **Return original booking**: Filter and return only the booking for the original customer's transaction

### Required Dependencies:

#### New Query Functions:
```haskell
-- In Storage.Queries.SearchRequest
findAllBySharedEntityId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) 
                        => Text 
                        -> m [Domain.Types.SearchRequest.SearchRequest]

-- SearchTry query not needed since we only update original customer's searchTry

-- In Storage.Queries.SharedEntity
updateStatusAndBookingIds :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r)
                          => Domain.Types.SharedEntity.EntityStatus
                          -> Text
                          -> [Text]  -- booking IDs list
                          -> m ()
-- Note: This function will be used in both OnSelect (BOOKING_CONFIRMED) and Init (BOOKING_CREATED)
```

#### Required Domain Type Fields:
- `DriverQuote.sharedEntityId :: Maybe Text` ✅ (assumed present)
- `SearchRequest.riderId :: Id Person` ✅ (already exists)
- `Booking.sharedBookingId :: Maybe Text` ✅ (schema changes assumed done)
- `SharedEntity.EntityStatus` with `BOOKING_CREATED` and `BOOKING_CONFIRMED` enum values

#### Required Imports:
```haskell
import qualified Storage.Queries.SharedEntity as QSharedEntity
import qualified Domain.Types.SharedEntity as DSharedEntity
```

### Key Benefits:

1. **Individual Booking Creation**: Each customer gets their own booking record with sharedEntityId stored in sharedBookingId field
2. **Proper Event Triggering**: `BookingCreatedEvent` fired for each customer with correct `riderId`
3. **Original Search Try Management**: Only the original customer's search try updated to `COMPLETED`
4. **Shared Entity Tracking**: Shared entity status updated for centralized state management
5. **Original Response Contract**: Returns only the original customer's booking to maintain API compatibility
6. **Transactional Integrity**: All shared bookings created atomically

### Testing Strategy:

1. **Unit Tests**:
   - Test single booking flow (sharedEntityId = Nothing)
   - Test shared booking flow (sharedEntityId = Just "shared-id")
   - Verify booking creation count matches search request count
   - Verify each booking has correct riderId from corresponding search request
   - Verify shared entity status update to `BOOKING_CREATED`
   - Test original booking filtering logic
   - Edge cases: empty shared search requests, missing original booking

2. **Integration Tests**:
   - Test complete Init flow for shared rides
   - Verify database state after shared booking creation
   - Verify all BookingCreatedEvents triggered with correct riderIds
   - Verify shared entity status consistency
   - Test search try status update for original customer only

### Next Steps:

1. Implement `QSR.findAllBySharedEntityId` query function
2. Implement `QSharedEntity.updateStatusAndBookingIds` query function
3. Update buildBooking function signature to accept sharedEntityId parameter
4. Ensure DriverQuote domain type has `sharedEntityId` field
5. Ensure Booking domain type has `sharedBookingId` field
6. Ensure SharedEntity domain type has `EntityStatus` with `BOOKING_CREATED` enum
7. Add required imports to Init.hs
8. Run comprehensive tests for both single and shared booking scenarios
9. Move to next chunk of shared ride implementation

## 🔴 IMPORTANT NOTES:

- **Unified SharedEntity Design**: Uses single `shared_entity` table approach
- **Field Naming**: Uses `sharedEntityId` (camelCase) in domain types
- **Database Column**: Uses `shared_entity_id` (snake_case) in database schema
- **Backward Compatibility**: Non-shared driver quotes continue to work unchanged
- **Event Integrity**: Each customer's booking triggers event with their specific riderId
- **Response Contract**: Only original customer's booking returned to maintain API compatibility

---

## 🚧 NEXT: Confirm.hs Shared Ride Initialization

### Implementation Details

**Location**: `Backend/app/provider-platform/dynamic-offer-driver-app/Main/src/Domain/Action/Beckn/Confirm.hs`

### Update handleDynamicOfferFlow Function

Replace the existing handleDynamicOfferFlow implementation (lines 121-127) with:

```haskell
handleDynamicOfferFlow isNewRider driver driverQuote booking riderDetails = do
  updateBookingDetails isNewRider booking riderDetails
  uBooking <- QRB.findById booking.id >>= fromMaybeM (BookingNotFound booking.id.getId)
  
  -- FETCH VEHICLE ONCE FOR ALL BOOKINGS
  vehicle <- QVeh.findById driver.id >>= fromMaybeM (VehicleNotFound driver.id.getId) -- Handle in other places where initialize ride is being called
  
  -- CHECK IF BOOKING HAS SHARED ENTITY ID
  case uBooking.sharedBookingId of
    Nothing -> do
      -- EXISTING SINGLE RIDE LOGIC
      (ride, rideDetails, _) <- initializeRide merchant driver uBooking Nothing (Just req.enableFrequentLocationUpdates) driverQuote.clientId (Just req.enableOtpLessRide) True vehicle -- oneTimeChecks = True
      void $ deactivateExistingQuotes booking.merchantOperatingCityId merchant.id driver.id driverQuote.searchTryId $ mkPrice (Just driverQuote.currency) driverQuote.estimatedFare
      uBooking2 <- QRB.findById booking.id >>= fromMaybeM (BookingNotFound booking.id.getId)
      mkDConfirmResp (Just $ RideInfo {ride, driver, vehicle}) uBooking2 riderDetails Nothing
      
    Just sharedBookingId -> do
      -- NEW SHARED RIDE LOGIC
      -- 1. Fetch all bookings with same sharedBookingId
      sharedBookings <- QRB.findAllBySharedBookingId sharedBookingId
      
      -- 2. Create HashMap to store transactionId mapped to bookingId
      let transactionIdToBookingMap = HM.fromList $ map (\sharedBooking -> (sharedBooking.transactionId, sharedBooking.id.getId)) sharedBookings
      
      -- 3. Call initializeRide for each booking and collect ride data
      (allRideData, allRideIds) <- foldM (\(rideDataList, rideIds) (idx, sharedBooking) -> do
        let oneTimeChecks = idx == 0  -- True for first call, False for rest
        
        -- Call initializeRide with oneTimeChecks flag and shared vehicle
        (ride, rideDetails, _) <- initializeRide merchant driver sharedBooking Nothing (Just req.enableFrequentLocationUpdates) driverQuote.clientId (Just req.enableOtpLessRide) oneTimeChecks vehicle
        
        -- Store ride data and append ride ID to list
        let rideData = (ride, rideDetails, vehicle)
        let updatedRideIds = rideIds ++ [ride.id.getId]
        return (rideDataList ++ [rideData], updatedRideIds)
      ) ([], []) (zip [0..] sharedBookings)
      
      -- 4. Deactivate quotes only once (already handled by oneTimeChecks in initializeRide)
      void $ deactivateExistingQuotes booking.merchantOperatingCityId merchant.id driver.id driverQuote.searchTryId $ mkPrice (Just driverQuote.currency) driverQuote.estimatedFare -- If u end up creating driver_quote multiple times for each estimate in driver side also update this call.
      
      -- 5. Update shared entity status and rideIds column
      void $ QSharedEntity.updateStatusAndRideIds DSharedEntity.RIDE_INITIALIZED sharedBookingId allRideIds
      
      -- 6. Return original customer's ride info with transactionIdToBookingMap
      let originalRideData = find (\(ride, _, _) -> ride.bookingId == booking.id) allRideData
      case originalRideData of
        Just (origRide, _, _) -> do
          uBooking2 <- QRB.findById booking.id >>= fromMaybeM (BookingNotFound booking.id.getId)
          mkDConfirmResp (Just $ RideInfo {ride = origRide, driver, vehicle}) uBooking2 riderDetails (Just transactionIdToBookingMap)
        Nothing -> throwError $ InternalError "Original customer ride not found in shared rides"
```

### Required Type Updates:

#### Update DConfirmResp Type:
```haskell
-- In Domain.Action.Beckn.Confirm
data DConfirmResp = DConfirmResp
  { booking :: DRB.Booking,
    rideInfo :: Maybe RideInfo,
    fromLocation :: DL.Location,
    toLocation :: Maybe DL.Location,
    riderDetails :: DRD.RiderDetails,
    riderMobileCountryCode :: Text,
    riderPhoneNumber :: Text,
    riderName :: Maybe Text,
    transporter :: DM.Merchant,
    vehicleVariant :: DV.VehicleVariant,
    quoteType :: ValidatedQuote,
    cancellationFee :: Maybe PriceAPIEntity,
    paymentId :: Maybe Text,
    isAlreadyFav :: Maybe Bool,
    favCount :: Maybe Int,
    transactionIdToBookingMap :: Maybe (HM.HashMap Text Text)  -- NEW FIELD
  }
```

#### Update mkDConfirmResp Function Signature:
```haskell
-- Change function signature to accept transactionIdToBookingMap parameter
mkDConfirmResp :: Maybe RideInfo -> DRB.Booking -> DRD.RiderDetails -> Maybe (HM.HashMap Text Text) -> Flow DConfirmResp

-- Update function implementation to include transactionIdToBookingMap in response:
mkDConfirmResp mbRideInfo uBooking riderDetails mbTransactionIdToBookingMap = do
  -- ... existing logic ...
  pure $
    DConfirmResp
      { booking = uBooking,
        rideInfo = mbRideInfo,
        riderDetails,
        -- ... other existing fields ...
        transactionIdToBookingMap = mbTransactionIdToBookingMap  -- ADD THIS LINE
      }
```

#### Update All mkDConfirmResp Calls:
```haskell
-- Update all existing calls to pass Nothing for regular rides:

-- Line 127 - In handleDynamicOfferFlow single ride case:
mkDConfirmResp (Just $ RideInfo {ride, driver, vehicle}) uBooking2 riderDetails Nothing

-- Line 134 - In handleRideOtpFlow:
mkDConfirmResp Nothing uBooking riderDetails Nothing

-- Line 148 - In handleMeterRideFlow:
mkDConfirmResp (Just $ RideInfo {ride, driver, vehicle}) uBooking2 riderDetails Nothing

-- Line 187 - In handleStaticOfferFlow:
mkDConfirmResp Nothing uBooking riderDetails Nothing

-- In handleDynamicOfferFlow shared ride case (new):
mkDConfirmResp (Just $ RideInfo {ride = origRide, driver, vehicle}) uBooking2 riderDetails (Just transactionIdToBookingMap)
```

### Update initializeRide Function Signature

**Current signature:**
```haskell
initializeRide ::
  Merchant ->
  DPerson.Person ->
  DBooking.Booking ->
  Maybe Text ->      -- mbOtpCode
  Maybe Bool ->      -- enableFrequentLocationUpdates  
  Maybe Text ->      -- mbClientId
  Maybe Bool ->      -- enableOtpLessRide
  Flow (DRide.Ride, SRD.RideDetails, DVeh.Vehicle)
```

**Updated signature:**
```haskell
initializeRide ::
  Merchant ->
  DPerson.Person ->
  DBooking.Booking ->
  Maybe Text ->      -- mbOtpCode
  Maybe Bool ->      -- enableFrequentLocationUpdates  
  Maybe Text ->      -- mbClientId
  Maybe Bool ->      -- enableOtpLessRide
  Bool ->           -- oneTimeChecks flag
  DVeh.Vehicle ->   -- vehicle (fetched once in Confirm)
  Flow (DRide.Ride, SRD.RideDetails, DVeh.Vehicle)
```

### Update All Existing initializeRide Calls

**In handleDynamicOfferFlow (line 124):**
```haskell
-- Before:
(ride, _, vehicle) <- initializeRide merchant driver uBooking Nothing (Just req.enableFrequentLocationUpdates) driverQuote.clientId (Just req.enableOtpLessRide)

-- After (with vehicle fetched once):
vehicle <- QVeh.findById driver.id >>= fromMaybeM (VehicleNotFound driver.id.getId)
(ride, rideDetails, _) <- initializeRide merchant driver uBooking Nothing (Just req.enableFrequentLocationUpdates) driverQuote.clientId (Just req.enableOtpLessRide) True vehicle
```

**In handleMeterRideFlow (line 146):**
```haskell
-- Before:
(ride, _, vehicle) <- initializeRide merchant driver uBooking dynamicReferralCode (Just req.enableFrequentLocationUpdates) Nothing (Just req.enableOtpLessRide)

-- After (with vehicle fetched once):
vehicle <- QVeh.findById driver.id >>= fromMaybeM (VehicleNotFound driver.id.getId)
(ride, rideDetails, _) <- initializeRide merchant driver uBooking dynamicReferralCode (Just req.enableFrequentLocationUpdates) Nothing (Just req.enableOtpLessRide) True vehicle
```

### Required Dependencies:

#### New Query Functions:
```haskell
-- In Storage.Queries.Booking
findAllBySharedBookingId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) 
                         => Text 
                         -> m [Domain.Types.Booking.Booking]

-- In Storage.Queries.SharedEntity
updateStatusAndRideIds :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r)
                       => Domain.Types.SharedEntity.EntityStatus
                       -> Text
                       -> [Text]  -- ride IDs list
                       -> m ()
```

#### Required Domain Type Fields:
- `Booking.sharedBookingId :: Maybe Text` ✅ (schema changes assumed done)
- `SharedEntity.EntityStatus` with `RIDE_INITIALIZED` enum value
- `SharedEntity.rideIds :: [Text]` field

#### Required Imports:
```haskell
import qualified Storage.Queries.SharedEntity as QSharedEntity
import qualified Domain.Types.SharedEntity as DSharedEntity
```

### Key Implementation Points:

1. **Vehicle Optimization**: Fetch vehicle once in Confirm and pass to all initializeRide calls
2. **Check sharedBookingId**: If booking has `sharedBookingId`, fetch all related bookings
3. **oneTimeChecks flag**: Pass `True` for first call, `False` for subsequent calls to initializeRide
4. **Collect ride data**: Store all `(ride, rideDetails, vehicle)` tuples in list
5. **Update shared entity**: Update status to `RIDE_INITIALIZED` and store all ride IDs
6. **Return original ride**: Filter and return only the original customer's ride info
7. **Backward compatibility**: All existing single ride calls get `oneTimeChecks=True`

### Next Steps:

1. Update initializeRide function signature in SharedLogic/Ride.hs
2. Update all existing initializeRide calls to pass oneTimeChecks parameter
3. Implement oneTimeChecks logic inside initializeRide function
4. Implement required query functions
5. Add required imports and domain types
6. Test shared ride initialization flow

---

## 🚧 NEXT: SharedLogic.Ride oneTimeChecks Implementation

### Implementation Details

**Location**: `Backend/app/provider-platform/dynamic-offer-driver-app/Main/src/SharedLogic/Ride.hs`

### Update initializeRide Function with oneTimeChecks Logic

Replace the existing initializeRide function with the following implementation:

```haskell
initializeRide ::
  Merchant ->
  DPerson.Person ->
  DBooking.Booking ->
  Maybe Text ->
  Maybe Bool ->
  Maybe Text ->
  Maybe Bool ->
  Bool ->           -- oneTimeChecks flag
  Flow (DRide.Ride, SRD.RideDetails, DVeh.Vehicle)
initializeRide merchant driver booking mbOtpCode enableFrequentLocationUpdates mbClientId enableOtpLessRide oneTimeChecks = do
  let merchantId = merchant.id
  otpCode <-
    case mbOtpCode of
      Just otp -> pure otp
      Nothing -> do
        riderId <- booking.riderId & fromMaybeM (BookingFieldNotPresent "riderId")
        riderDetails <- QRiderD.findById riderId >>= fromMaybeM (RiderDetailsNotFound riderId.getId)
        case riderDetails.otpCode of
          Nothing -> do
            otpCode <- generateOTPCode
            QRiderD.updateOtpCode (Just otpCode) riderDetails.id
            pure otpCode
          Just otp -> pure otp
  
  -- ONETIME CHECKS FOR DRIVER GO HOME STATUS
  ghrId <- if oneTimeChecks
    then CQDGR.setDriverGoHomeIsOnRideStatus driver.id booking.merchantOperatingCityId True
    else CQDGR.getDriverGoHomeRequestInfo driver.id booking.merchantOperatingCityId True
  
  -- ONETIME CHECKS FOR PREVIOUS RIDE STATUS  
  previousRideInprogress <- bool (QDI.findByPrimaryKey driver.id) (pure Nothing) (booking.isScheduled || not oneTimeChecks)
  
  let isDriverOnRide = bool (Just False) (previousRideInprogress >>= Just . isJust <$> (.driverTripEndLocation)) (isJust previousRideInprogress)
  now <- getCurrentTime
  -- Vehicle now passed as parameter instead of fetched inside function
  ride <- buildRide driver booking ghrId otpCode enableFrequentLocationUpdates mbClientId previousRideInprogress now vehicle merchant.onlinePayment enableOtpLessRide
  rideDetails <- buildRideDetails booking ride driver vehicle

  QRB.updateStatus booking.id DBooking.TRIP_ASSIGNED
  QRide.createRide ride
  QRideD.create rideDetails
  Redis.withWaitOnLockRedisWithExpiry (isOnRideWithAdvRideConditionKey driver.id.getId) 4 4 $ do
    -- ONETIME CHECKS FOR SCHEDULED BOOKING UPDATES
    when (not booking.isScheduled && oneTimeChecks) $ do
      whenJust (booking.toLocation) $ \toLoc -> do
        -- SHARED ENTITY CHECK FOR TRIP CATEGORY UPDATE
        when (isNothing booking.sharedBookingId) $
          QDI.updateTripCategoryAndTripEndLocationByDriverId (cast driver.id) (Just ride.tripCategory) (Just (Maps.LatLong toLoc.lat toLoc.lon))
      QDI.updateOnRide True (cast driver.id)
    when oneTimeChecks $ Redis.unlockRedis (offerQuoteLockKeyWithCoolDown ride.driverId)
    
    -- SHARED RIDE ADVANCE BOOKING CHECK
    if (isDriverOnRide == Just True && isJust booking.sharedBookingId)
      then throwError AdvanceBookingInSharedRide
      else when (isDriverOnRide == Just True) $ QDI.updateHasAdvancedRide (cast ride.driverId) True
    
    -- ONETIME CHECKS FOR EDIT DESTINATION UNLOCK
    when oneTimeChecks $ Redis.unlockRedis (editDestinationLockKey ride.driverId)
    
  unless booking.isScheduled $ void $ LF.rideDetails ride.id DRide.NEW merchantId ride.driverId booking.fromLocation.lat booking.fromLocation.lon (Just ride.isAdvanceBooking) (Just (LT.Car $ LT.CarRideInfo {pickupLocation = LatLong (booking.fromLocation.lat) (booking.fromLocation.lon), minDistanceBetweenTwoPoints = Nothing, rideStops = Just $ map (\stop -> LatLong stop.lat stop.lon) booking.stops}))

  triggerRideCreatedEvent RideEventData {ride = ride, personId = cast driver.id, merchantId = merchantId}
  QBE.logDriverAssignedEvent (cast driver.id) booking.id ride.id booking.distanceUnit

  -- ONETIME CHECKS FOR DRIVER NOTIFICATIONS
  if booking.isScheduled
    then Notify.driverScheduledRideAcceptanceAlert booking.merchantOperatingCityId Notification.SCHEDULED_RIDE_NOTIFICATION notificationTitle (messageForScheduled booking) driver driver.deviceToken
    else when oneTimeChecks $ Notify.notifyDriverWithProviders booking.merchantOperatingCityId notificationType notificationTitle (message booking) driver driver.deviceToken EmptyDynamicParam

  -- SHARED ENTITY CHECK FOR DRIVER SCORE EVENT
  when (isNothing booking.sharedBookingId) $ do
    fork "DriverScoreEventHandler OnNewRideAssigned" $
      DS.driverScoreEventHandler booking.merchantOperatingCityId DST.OnNewRideAssigned {merchantId = merchantId, driverId = ride.driverId, currency = ride.currency, distanceUnit = booking.distanceUnit}

  -- SHTODO: Need to check how to make it sharedRide compatible
  when oneTimeChecks $ notifyRideRelatedNotificationOnEvent ride now DRN.RIDE_ASSIGNED
  -- SHTODO: Need to check how to make it sharedRide compatible
  when oneTimeChecks $ notifyRideRelatedNotificationOnEvent ride now DRN.PICKUP_TIME

  return (ride, rideDetails, vehicle)
  where
    -- ... existing helper functions remain unchanged ...
```

### Key Changes in initializeRide:

#### 1. Driver Go Home Status (Line ~30):
```haskell
-- Before:
ghrId <- CQDGR.setDriverGoHomeIsOnRideStatus driver.id booking.merchantOperatingCityId True

-- After:
ghrId <- if oneTimeChecks
  then CQDGR.setDriverGoHomeIsOnRideStatus driver.id booking.merchantOperatingCityId True
  else CQDGR.getDriverGoHomeRequestInfo driver.id booking.merchantOperatingCityId True
```

#### 2. Previous Ride Status Check (Line ~31):
```haskell
-- Before:
previousRideInprogress <- bool (QDI.findByPrimaryKey driver.id) (pure Nothing) (booking.isScheduled)

-- After:
previousRideInprogress <- bool (QDI.findByPrimaryKey driver.id) (pure Nothing) (booking.isScheduled || not oneTimeChecks)
```

#### 3. Scheduled Booking Updates (Line ~42):
```haskell
-- Before:
when (not booking.isScheduled) $ do

-- After:
when (not booking.isScheduled && oneTimeChecks) $ do
```

#### 4. Shared Entity Trip Category Update (Line ~44):
```haskell
-- Before:
QDI.updateTripCategoryAndTripEndLocationByDriverId (cast driver.id) (Just ride.tripCategory) (Just (Maps.LatLong toLoc.lat toLoc.lon))

-- After:
when (isNothing booking.sharedBookingId) $
  QDI.updateTripCategoryAndTripEndLocationByDriverId (cast driver.id) (Just ride.tripCategory) (Just (Maps.LatLong toLoc.lat toLoc.lon))
```

#### 5. Advance Booking in Shared Ride Check (Line ~47):
```haskell
-- Before:
when (isDriverOnRide == Just True) $ QDI.updateHasAdvancedRide (cast ride.driverId) True

-- After:
if (isDriverOnRide == Just True && isJust booking.sharedBookingId)
  then throwError AdvanceBookingInSharedRide
  else when (isDriverOnRide == Just True) $ QDI.updateHasAdvancedRide (cast ride.driverId) True
```

#### 6. Edit Destination Lock Unlock (Line ~111):
```haskell
-- Before:
Redis.unlockRedis (editDestinationLockKey ride.driverId)

-- After:
when oneTimeChecks $ Redis.unlockRedis (editDestinationLockKey ride.driverId)
```

#### 7. Driver Notification (Line ~119):
```haskell
-- Before:
else Notify.notifyDriverWithProviders booking.merchantOperatingCityId notificationType notificationTitle (message booking) driver driver.deviceToken EmptyDynamicParam

-- After:
else when oneTimeChecks $ Notify.notifyDriverWithProviders booking.merchantOperatingCityId notificationType notificationTitle (message booking) driver driver.deviceToken EmptyDynamicParam
```

#### 8. Driver Score Event (Line ~121):
```haskell
-- Before:
fork "DriverScoreEventHandler OnNewRideAssigned" $
  DS.driverScoreEventHandler booking.merchantOperatingCityId DST.OnNewRideAssigned {merchantId = merchantId, driverId = ride.driverId, currency = ride.currency, distanceUnit = booking.distanceUnit}

-- After:
when (isNothing booking.sharedBookingId) $ do
  fork "DriverScoreEventHandler OnNewRideAssigned" $
    DS.driverScoreEventHandler booking.merchantOperatingCityId DST.OnNewRideAssigned {merchantId = merchantId, driverId = ride.driverId, currency = ride.currency, distanceUnit = booking.distanceUnit}
```

### Required Dependencies:

#### New Import:
```haskell
import qualified Storage.CachedQueries.Driver.GoHomeRequest as CQDGR (getDriverGoHomeRequestInfo)
```

#### Required Domain Type Fields:
- `Booking.sharedBookingId :: Maybe Text` ✅ (schema changes assumed done)

#### Required Error Type:
```haskell
-- In Tools.Error or appropriate error module
data Error = ... | AdvanceBookingInSharedRide | ...
```

### Key Implementation Points:

1. **oneTimeChecks for Driver Go Home**: Uses `setDriverGoHomeIsOnRideStatus` for first booking, `getDriverGoHomeRequestInfo` for subsequent bookings
2. **oneTimeChecks for Previous Ride**: Checks previous ride status only when `oneTimeChecks=True` or booking is scheduled
3. **oneTimeChecks for Scheduled Updates**: Only updates trip category and sets onRide status for first booking
4. **Shared Entity Trip Category**: Only updates trip category if booking is not part of shared entity
5. **Advance Booking Validation**: Throws error if driver has advance booking and booking is part of shared entity
6. **oneTimeChecks for Lock Management**: Only unlocks edit destination lock for first booking
7. **oneTimeChecks for Driver Notifications**: Only sends driver assignment notification for first booking
8. **Shared Entity Driver Score**: Only triggers driver score event if booking is not part of shared entity

### Performance Benefits:

1. **Reduced Driver Status Queries**: Subsequent shared bookings skip expensive driver status checks
2. **Single Trip Category Update**: Only first booking updates driver's trip category and location
3. **Optimized Driver State Management**: Prevents redundant driver status updates for shared rides
4. **Advance Booking Safety**: Prevents conflicting advance bookings in shared rides
5. **Single Lock Management**: Edit destination lock only unlocked once for shared rides
6. **Single Driver Notification**: Driver only notified once per shared ride assignment
7. **Single Driver Score Event**: Driver score only triggered once per shared ride, not per booking

### Next Steps:

1. Add `getDriverGoHomeRequestInfo` import to SharedLogic.Ride
2. Add `AdvanceBookingInSharedRide` error type to error module
3. Update all initializeRide calls to pass oneTimeChecks parameter
4. Test oneTimeChecks optimization with shared ride scenarios
5. Verify advance booking validation in shared rides
6. Test performance improvements for shared ride initialization


---

## 🚧 NEXT: OnConfirm.hs Shared Ride Tag Implementation

### Implementation Details

**Location**: `Backend/app/provider-platform/dynamic-offer-driver-app/Main/src/Beckn/ACL/OnConfirm.hs`

### Add Required Imports

```haskell
-- Add these imports to OnConfirm.hs
import qualified Data.HashMap.Strict as HM
import Data.Text.Encoding (decodeUtf8)
import Data.ByteString.Lazy (toStrict)
import Data.Aeson (encode)
import qualified BecknV2.OnDemand.Tags as Tags
```

### Update tfFulfillments Function

Replace the `fulfillmentTags = Nothing` line in tfFulfillments with:

```haskell
tfFulfillments :: DConfirm.DConfirmResp -> Maybe [Spec.Fulfillment]
tfFulfillments res =
  Just
    [ Spec.Fulfillment
        { fulfillmentAgent = Nothing,
          fulfillmentCustomer = tfCustomer res,
          fulfillmentId = Just res.booking.quoteId,
          fulfillmentState = Utils.mkFulfillmentState <$> bookingStatusCode res.quoteType,
          fulfillmentStops = Utils.mkStops' res.booking.fromLocation res.booking.toLocation res.booking.stops res.booking.specialZoneOtpCode,
          fulfillmentTags = mkTags res,  -- UPDATED LINE
          fulfillmentType = Just $ UtilsV2.tripCategoryToFulfillmentType res.booking.tripCategory,
          fulfillmentVehicle = tfVehicle res
        }
    ]
```

### Implement mkTags Function

```haskell
-- Add this function to OnConfirm.hs
mkTags :: DConfirm.DConfirmResp -> Maybe [Spec.TagGroup]
mkTags res = 
  case res.transactionIdToBookingMap of
    Nothing -> Nothing  -- No shared ride tags for regular rides
    Just transactionIdToBookingMap -> Just [mkSharedRideTagGroup transactionIdToBookingMap]
```

### Implement mkSharedRideTagGroup Function

```haskell
-- Add this function to OnConfirm.hs
mkSharedRideTagGroup :: HM.HashMap Text Text -> Spec.TagGroup
mkSharedRideTagGroup transactionIdToBookingMap =
  Spec.TagGroup
    { tagGroupDisplay = Just False,
      tagGroupDescriptor =
        Just $
          Spec.Descriptor
            { descriptorCode = Just $ show Tags.SHARED_RIDE_INFO,
              descriptorName = Just "Shared Ride Information",
              descriptorShortDesc = Nothing
            },
      tagGroupList =
        Just
          [ Spec.Tag
              { tagDescriptor =
                  Just $
                    Spec.Descriptor
                      { descriptorCode = Just $ show Tags.TRANSACTION_ID_TO_BOOKING_MAP,
                        descriptorName = Just "Transaction ID to Booking Mapping",
                        descriptorShortDesc = Nothing
                      },
                tagDisplay = Just False,
                tagValue = Just $ decodeUtf8 $ toStrict $ encode $ HM.toList transactionIdToBookingMap
              }
          ]
    }
```

### Required Tag Enum Definitions in Tags.hs

#### 1. Add SHARED_RIDE_INFO to BecknTagGroup enum:

```haskell
-- In lib/beckn-spec/src/BecknV2/OnDemand/Tags.hs (around line 99)
  | SAFETY_PLUS_INFO
  | INSURANCE_INFO
  | SHARED_RIDE_INFO  -- ADD THIS LINE
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)
```

#### 2. Add SHARED_RIDE_INFO descriptor in getTagGroupDescriptor function:

```haskell
-- In lib/beckn-spec/src/BecknV2/OnDemand/Tags.hs (around line 117)
    DELIVERY -> (Just "Delivery Information", Nothing)
    DRIVER_REACHED_DESTINATION_INFO -> (Just "Driver Reached Destination Information", Nothing)
    SHARED_RIDE_INFO -> (Just "Shared Ride Information", Nothing)  -- ADD THIS LINE
    _ -> (Just $ convertToSentence tagGroup, Nothing)
```

#### 3. Add TRANSACTION_ID_TO_BOOKING_MAP to BecknTag enum:

```haskell
-- In lib/beckn-spec/src/BecknV2/OnDemand/Tags.hs (around line 475)
  | INSURED_AMOUNT
  | RESERVED_RIDE_TAG
  | RESERVED_PRICING_TAG
  | TRANSACTION_ID_TO_BOOKING_MAP  -- ADD THIS LINE
  deriving (Show, Eq, Generic, ToJSON, FromJSON)
```

#### 4. Add TRANSACTION_ID_TO_BOOKING_MAP descriptor in getTagDescriptor function:

```haskell
-- In lib/beckn-spec/src/BecknV2/OnDemand/Tags.hs (around line 527)
    IS_INSURED -> (Just "is insured", Nothing)
    INSURED_AMOUNT -> (Just "insured amount", Nothing)
    NYREGULAR_SUBSCRIPTION_CHARGE -> (Just "NYRegular subscription charge", Nothing)
    TRANSACTION_ID_TO_BOOKING_MAP -> (Just "Transaction ID to Booking Mapping", Nothing)  -- ADD THIS LINE
    _ -> (Just $ convertToSentence tag, Nothing)
```

#### 5. Add TRANSACTION_ID_TO_BOOKING_MAP to tag group mapping in getTagGroup function:

```haskell
-- In lib/beckn-spec/src/BecknV2/OnDemand/Tags.hs (around line 578)
    IS_INSURED -> INSURANCE_INFO
    INSURED_AMOUNT -> INSURANCE_INFO
    NYREGULAR_SUBSCRIPTION_CHARGE -> GENERAL_INFO
    TRANSACTION_ID_TO_BOOKING_MAP -> SHARED_RIDE_INFO  -- ADD THIS LINE
    a -> error $ "getTagGroup function of CompleteTag class is not defined for " <> T.pack (show a) <> " tag"
```

### Key Implementation Points:

1. **Conditional Tag Creation**: Only creates shared ride tags when `transactionIdToBookingMap` is present (shared rides)
2. **HashMap Serialization**: Converts HashMap to list of tuples and JSON encodes it as tag value
3. **UTF8 Encoding**: Properly encodes the JSON string for Beckn spec compatibility
4. **Tag Structure**: Follows same pattern as existing tag groups like `mkAdvancedBookingEnabledTagGroup`
5. **Null Handling**: Returns `Nothing` for regular rides, `Just [TagGroup]` for shared rides

### Testing Strategy:

1. **Unit Tests**:
   - Test mkTags with Nothing transactionIdToBookingMap (should return Nothing)
   - Test mkTags with Just HashMap (should return Just [TagGroup])
   - Test mkSharedRideTagGroup with various HashMap contents
   - Verify JSON encoding of HashMap.toList produces valid format

2. **Integration Tests**:
   - Test complete OnConfirm flow for shared rides
   - Verify fulfillmentTags contains shared ride information
   - Test Beckn spec compliance of generated tags

### Dependencies:

- `DConfirm.DConfirmResp.transactionIdToBookingMap :: Maybe (HM.HashMap Text Text)` field ✅ (from previous implementation)
- Required imports: HashMap, Text encoding, Aeson, Tags module
- Tag enum definitions in Tags.hs


## 🚧 NEXT: Rider Platform OnConfirm.hs Tag Parsing Implementation

### Implementation Details

**Location**: `Backend/app/rider-platform/rider-app/Main/src/Beckn/ACL/OnConfirm.hs`

### Add Required Imports

```haskell
-- Add these imports to rider-platform OnConfirm.hs
import qualified Data.HashMap.Strict as HM
import Data.Aeson (decode)
import Data.Text.Encoding (encodeUtf8)
import Data.ByteString.Lazy (fromStrict)
```

### Update parseData Function (Line 58)

In the `parseData` function, under the `if isDriverDetailsPresent then` block, add the transactionIdToBookingMap parsing:

```haskell
parseData :: Spec.ConfirmReqMessage -> [DCommon.DFareBreakup] -> Either Text DOnConfirm.OnConfirmReq
parseData message fareBreakups = do
  let order = message.confirmReqMessageOrder
  -- ... existing code ...
  
  if isDriverDetailsPresent
    then do
      let agentPerson = fulf >>= (.fulfillmentAgent) >>= (.agentPerson)
          tagGroups = agentPerson >>= (.personTags)
          tagGroupsFullfillment = order.orderFulfillments >>= listToMaybe >>= (.fulfillmentTags)
          driverImage = agentPerson >>= (.personImage) >>= (.imageUrl)
          driverMobileCountryCode = Just "+91" -- TODO: check how to get countrycode via ONDC
          driverRating :: Maybe Centesimal = readMaybe . T.unpack =<< getTagV2' Tag.DRIVER_DETAILS Tag.RATING tagGroups
          driverRegisteredAt :: Maybe UTCTime = readMaybe . T.unpack =<< getTagV2' Tag.DRIVER_DETAILS Tag.REGISTERED_AT tagGroups
          isDriverBirthDay = isJust $ getTagV2' Tag.DRIVER_DETAILS Tag.IS_DRIVER_BIRTHDAY tagGroups
          isFreeRide = isJust $ getTagV2' Tag.DRIVER_DETAILS Tag.IS_FREE_RIDE tagGroups
          previousRideEndPos = getLocationFromTagV2 tagGroupsFullfillment Tag.FORWARD_BATCHING_REQUEST_INFO Tag.PREVIOUS_RIDE_DROP_LOCATION_LAT Tag.PREVIOUS_RIDE_DROP_LOCATION_LON
          vehicleAge :: Maybe Months = readMaybe . T.unpack =<< getTagV2' Tag.VEHICLE_AGE_INFO Tag.VEHICLE_AGE tagGroupsFullfillment
          driverAlternatePhoneNumber :: Maybe Text = getTagV2' Tag.DRIVER_DETAILS Tag.DRIVER_ALTERNATE_NUMBER tagGroups
          isAlreadyFav = isJust $ getTagV2' Tag.DRIVER_DETAILS Tag.IS_ALREADY_FAVOURITE tagGroups
          favCount :: Maybe Int = readMaybe . T.unpack =<< getTagV2' Tag.DRIVER_DETAILS Tag.FAVOURITE_COUNT tagGroups
          driverAccountId = getTagV2' Tag.DRIVER_DETAILS Tag.DRIVER_ACCOUNT_ID tagGroups
          isSafetyPlus' = isJust $ getTagV2' Tag.DRIVER_DETAILS Tag.IS_SAFETY_PLUS tagGroups
          -- ADD THIS LINE: Parse transactionIdToBookingMap from fulfillment tags
          transactionIdToBookingMap = parseTransactionIdToBookingMap (fulf >>= (.fulfillmentTags))
      -- ... rest of existing code ...
      Right $ DOnConfirm.RideAssigned DOnConfirm.RideAssignedInfo {fareBreakups = Just fareBreakups, isSafetyPlus = isSafetyPlus', ..}
    else Right $ DOnConfirm.BookingConfirmed DOnConfirm.BookingConfirmedInfo {specialZoneOtp = mbRideOtp, ..}
```

### Add parseTransactionIdToBookingMap Helper Function

```haskell
-- Add this helper function in OnConfirm.hs
parseTransactionIdToBookingMap :: Maybe [Spec.TagGroup] -> Maybe (HM.HashMap Text Text)
parseTransactionIdToBookingMap tagGroups = do
  jsonString <- getTagV2' Tag.SHARED_RIDE_INFO Tag.TRANSACTION_ID_TO_BOOKING_MAP tagGroups
  let byteString = fromStrict $ encodeUtf8 jsonString
  tupleList <- decode byteString :: Maybe [(Text, Text)]
  return $ HM.fromList tupleList
```

### Update RideAssignedInfo Datatype

```haskell
-- In Domain/Action/Beckn/OnConfirm.hs, update RideAssignedInfo datatype:
data RideAssignedInfo = RideAssignedInfo
  { bppBookingId :: Id DRB.BPPBooking,
    bppRideId :: Id DRide.BPPRide,
    driverName :: Text,
    driverImage :: Maybe Text,
    driverMobileNumber :: Text,
    driverMobileCountryCode :: Maybe Text,
    driverRating :: Maybe Centesimal,
    driverRegisteredAt :: Maybe UTCTime,
    isDriverBirthDay :: Bool,
    vehicleAge :: Maybe Months,
    driverAlternatePhoneNumber :: Maybe Text,
    isFreeRide :: Bool,
    previousRideEndPos :: Maybe LatLong,
    rideOtp :: Text,
    vehicleNumber :: Text,
    vehicleColor :: Maybe Text,
    vehicleModel :: Text,
    fareBreakups :: Maybe [DCommon.DFareBreakup],
    isAlreadyFav :: Bool,
    favCount :: Maybe Int,
    driverAccountId :: Maybe Payment.AccountId,
    isSafetyPlus :: Bool,
    transactionIdToBookingMap :: Maybe (HM.HashMap Text Text)  -- ADD THIS LINE
  }
```

### Update ValidatedRideAssignedReq Datatype

```haskell
-- In Domain/Action/Beckn/Common.hs, update ValidatedRideAssignedReq datatype:
data ValidatedRideAssignedReq = ValidatedRideAssignedReq
  { bookingDetails :: BookingDetails,
    isDriverBirthDay :: Bool,
    isFreeRide :: Bool,
    vehicleAge :: Maybe Months,
    onlinePaymentParameters :: Maybe OnlinePaymentParameters,
    previousRideEndPos :: Maybe LatLong,
    booking :: DRB.Booking,
    fareBreakups :: Maybe [DFareBreakup],
    driverTrackingUrl :: Maybe BaseUrl,
    isAlreadyFav :: Bool,
    favCount :: Maybe Int,
    isSafetyPlus :: Bool,
    transactionIdToBookingMap :: Maybe (HM.HashMap Text Text)  -- ADD THIS LINE
  }
```

### Update validateRequest Function

```haskell
-- In Domain/Action/Beckn/OnConfirm.hs, in validateRequest function, ensure the transactionIdToBookingMap is passed through:
-- The existing validateRequest function should use .. syntax to automatically include the new field when creating ValidatedRideAssignedReq from RideAssignedInfo
```

### Key Implementation Points:

1. **Tag Parsing**: Uses existing `getTagV2'` function pattern to extract the JSON string from fulfillment tags
2. **JSON Decoding**: Converts the JSON string back to list of tuples and then to HashMap
3. **Conditional Parsing**: Only attempts to parse when the tag is present (shared rides)
4. **Error Handling**: Uses Maybe types throughout to handle missing tags gracefully
5. **Record Syntax**: Uses `..` syntax in RideAssignedInfo construction to automatically include the new field

### Testing Strategy:

1. **Unit Tests**:
   - Test parseTransactionIdToBookingMap with valid JSON string
   - Test parseTransactionIdToBookingMap with invalid JSON (should return Nothing)
   - Test parseTransactionIdToBookingMap with missing tag (should return Nothing)
   - Verify HashMap construction from parsed tuples

2. **Integration Tests**:
   - Test complete OnConfirm flow for shared rides with tags
   - Test OnConfirm flow for regular rides (should not break)
   - Verify RideAssignedInfo contains correct transactionIdToBookingMap
   - Test round-trip: provider tags → BAP parsing → HashMap reconstruction

### Dependencies:

- `Tag.SHARED_RIDE_INFO` and `Tag.TRANSACTION_ID_TO_BOOKING_MAP` enums ✅ (from Tags.hs implementation)
- Required imports: HashMap, Aeson decode, Text encoding
- Updated RideAssignedInfo datatype with new field
- Existing `getTagV2'` function infrastructure

---

## 🚧 NEXT: OnConfirm.hs Shared Ride Processing Implementation

### Implementation Details

**Location**: `Backend/app/rider-platform/rider-app/Main/src/Domain/Action/Beckn/OnConfirm.hs`

### Revised Approach: Extract Common Logic to Avoid Code Duplication

Instead of duplicating the booking processing logic, we should extract the common validation logic from `validateRequest` and reuse it for shared rides.

#### Problem Identified:
The original approach would duplicate the booking processing logic between `validateRequest` and a new `processSharedRideBooking` function, violating DRY principles.

#### Solution: Extract Common Helper Function

**1. Extract Common Booking Processing Logic:**
```haskell
-- Extract the common booking processing logic from validateRequest
processBookingForTransaction :: (CacheFlow m r, EsqDBFlow m r, EncFlow m r, EsqDBReplicaFlow m r, HasHttpClientOptions r c, HasLongDurationRetryCfg r c, HasField "minTripDistanceForReferralCfg" r (Maybe Distance)) => RideAssignedInfo -> Text -> m DCommon.ValidatedRideAssignedReq
processBookingForTransaction RideAssignedInfo{..} transactionId = do
  let bookingDetails = DCommon.BookingDetails {otp = rideOtp, isInitiatedByCronJob = False, ..}
  booking <- QRB.findByTransactionId transactionId >>= fromMaybeM (BookingDoesNotExist $ "transactionId:-" <> transactionId)
  mbMerchant <- CQM.findById booking.merchantId
  let onlinePayment = maybe False (.onlinePayment) mbMerchant
  onlinePaymentParameters <-
    if onlinePayment
      then do
        person <- runInReplica $ QPerson.findById booking.riderId >>= fromMaybeM (PersonNotFound booking.riderId.getId)
        customerPaymentId <- person.customerPaymentId & fromMaybeM (CustomerPaymentIdNotFound booking.riderId.getId)
        paymentMethodId <- booking.paymentMethodId & fromMaybeM (PaymentMethodIdNotFound booking.id.getId)
        driverAccountId_ <- driverAccountId & fromMaybeM (DriverAccountIdNotFound booking.id.getId)
        let merchantOperatingCityId = person.merchantOperatingCityId
        email <- mapM decrypt person.email
        return $ Just DCommon.OnlinePaymentParameters {driverAccountId = driverAccountId_, ..}
      else return Nothing
  return $ DCommon.ValidatedRideAssignedReq {onlinePaymentParameters, driverTrackingUrl = Nothing, ..}
```

**2. Update validateRequest to Use Helper:**
```haskell
validateRequest (RideAssigned rideAssignedInfo) transactionId = do
  validatedReq <- processBookingForTransaction rideAssignedInfo transactionId
  return $ ValidatedRideAssigned validatedReq
```

**3. Add originalRideAssignedInfo to ValidatedRideAssignedReq:**
```haskell
-- In Domain/Action/Beckn/Common.hs, update ValidatedRideAssignedReq datatype:
data ValidatedRideAssignedReq = ValidatedRideAssignedReq
  { bookingDetails :: BookingDetails,
    isDriverBirthDay :: Bool,
    isFreeRide :: Bool,
    vehicleAge :: Maybe Months,
    onlinePaymentParameters :: Maybe OnlinePaymentParameters,
    previousRideEndPos :: Maybe LatLong,
    booking :: DRB.Booking,
    fareBreakups :: Maybe [DFareBreakup],
    driverTrackingUrl :: Maybe BaseUrl,
    isAlreadyFav :: Bool,
    favCount :: Maybe Int,
    isSafetyPlus :: Bool,
    transactionIdToBookingMap :: Maybe (HM.HashMap Text Text),
    originalRideAssignedInfo :: RideAssignedInfo  -- ADD THIS FIELD
  }
```

**4. Update onConfirm for Shared Rides:**
```haskell
onConfirm (ValidatedRideAssigned req) = do
  case req.transactionIdToBookingMap of
    Nothing -> 
      -- Regular single ride flow
      DCommon.rideAssignedReqHandler req
    Just transactionIdToBookingMap -> do
      -- Shared ride flow - revalidate for each transaction
      forM_ (HM.toList transactionIdToBookingMap) $ \(transactionId, _bookingId) -> do
        validatedReq <- processBookingForTransaction req.originalRideAssignedInfo transactionId
        DCommon.rideAssignedReqHandler validatedReq
```

### Key Benefits of Revised Approach:

1. **No Code Duplication**: Reuses existing `validateRequest` logic
2. **Cleaner Architecture**: Single source of truth for booking processing
3. **Easier Maintenance**: Changes to booking validation only need to be made in one place
4. **Consistent Behavior**: Same validation logic for both single and shared rides

### Required Changes:

1. **Extract processBookingForTransaction** helper function from validateRequest logic
2. **Add originalRideAssignedInfo field** to `DCommon.ValidatedRideAssignedReq` to preserve original request data
3. **Refactor validateRequest** to use the helper function
4. **Update onConfirm** to iterate through shared bookings using the helper
5. **Update RideAssignedInfo parsing** to populate originalRideAssignedInfo field

### Testing Strategy:

1. **Unit Tests**:
   - Test processBookingForTransaction with different transaction IDs
   - Test onConfirm with shared rides (transactionIdToBookingMap present)
   - Test onConfirm with regular rides (transactionIdToBookingMap = Nothing)
   - Verify each booking gets correct payment parameters

2. **Integration Tests**:
   - Test complete OnConfirm flow for shared rides
   - Verify all bookings in shared ride get processed correctly
   - Test that each customer gets their own booking details and payment parameters

---

## 🚧 NEXT: Cancel.hs Shared Ride Cancellation Logic

### Implementation Details

**Location**: `Backend/app/provider-platform/dynamic-offer-driver-app/Main/src/Domain/Action/Beckn/Cancel.hs`

### Overview
Implement shared ride cancellation logic that handles different scenarios based on ride status and participant thresholds.

### Required Changes:

#### 1. Update cancel Function Signature
```haskell
-- Current:
cancel ::
  CancelRideReq ->
  DM.Merchant ->
  SRB.Booking ->
  Maybe ST.SearchTry ->
  Flow (Bool, Maybe PriceAPIEntity)

-- Updated:
cancel ::
  CancelRideReq ->
  DM.Merchant ->
  SRB.Booking ->
  Maybe ST.SearchTry ->
  Bool ->  -- sharedRideChecks
  Flow (Bool, Maybe PriceAPIEntity)
```

#### 2. Add Shared Ride Check (After Line 131)
```haskell
whenJust mbRide $ \ride -> do
  -- Existing ride cancellation logic...
  
  -- NEW: Add shared ride cancellation check
  when (sharedRideChecks && isJust booking.sharedEntityId) $ 
    handleSharedRideCancel booking ride
```

#### 3. Implement handleSharedRideCancel Function
```haskell
handleSharedRideCancel :: SRB.Booking -> SRide.Ride -> Flow ()
handleSharedRideCancel booking ride = do
  -- 1. Fetch shared entity data
  sharedEntityId <- booking.sharedEntityId & fromMaybeM (InternalError "SharedEntityId not found")
  sharedEntity <- QSharedEntity.findById sharedEntityId >>= fromMaybeM (SharedEntityNotFound sharedEntityId)
  
  -- 2. Get current threshold from config
  threshold <- getSharedRideThreshold booking.merchantOperatingCityId
  
  -- 3. Analyze ride states
  let rideList = sharedEntity.rideIds
      bookingList = sharedEntity.bookingIds
  
  (activeRideCount, inProgressRideCount, activeRideIds) <- 
    analyzeSharedRideStates rideList ride.id
  
  -- 4. Decision logic based on ride states (activeRideCount excludes current ride)
  if inProgressRideCount >= 1 || activeRideCount >= threshold
    then handleSingleRideCancel booking ride sharedEntityId sharedEntity
    else handleBelowThresholdCancel booking ride sharedEntityId activeRideIds sharedEntity
```

### Decision Logic Implementation:

#### Case 1: INPROGRESS Rides Exist OR activeCount >= threshold
```haskell
-- Consolidated function for both scenarios that only cancel current ride
handleSingleRideCancel :: SRB.Booking -> SRide.Ride -> Text -> DSharedEntity.SharedEntity -> Flow ()
handleSingleRideCancel booking ride sharedEntityId sharedEntity = do
  -- Update booking and ride lists at app level
  let updatedBookingList = updateTrackedEntityStatus sharedEntity.bookingIds booking.id.getId False
      updatedRideList = updateTrackedEntityStatus sharedEntity.rideIds ride.id.getId False
  
  -- Single update query with new JSON strings
  void $ QSharedEntity.updateBookingAndRideLists sharedEntityId updatedBookingList updatedRideList
```

#### Case 2: No INPROGRESS Rides && activeCount < threshold
```haskell
handleBelowThresholdCancel :: SRB.Booking -> SRide.Ride -> Text -> [Text] -> DSharedEntity.SharedEntity -> Flow ()
handleBelowThresholdCancel booking ride sharedEntityId activeRideIds sharedEntity = do
  -- 1. Mark ALL bookings and rides as inactive at app level
  let updatedBookingList = map (\entity -> entity { isActive = False }) sharedEntity.bookingIds
      updatedRideList = map (\entity -> entity { isActive = False }) sharedEntity.rideIds
  
  -- 2. Single update query: set all inactive + status CANCELLED
  void $ QSharedEntity.updateStatusAndLists DSharedEntity.CANCELLED sharedEntityId updatedBookingList updatedRideList
  
  -- 3. Fork cancel for OTHER active rides only (current ride handled by main flow)
  forM_ activeRideIds $ \rideId -> do
    fork ("cancelSharedRide-" <> rideId) $ do
      otherRide <- QRide.findById (Id rideId) >>= fromMaybeM (RideNotFound rideId)
      otherBooking <- QRB.findById otherRide.bookingId >>= fromMaybeM (BookingNotFound otherRide.bookingId.getId)
      -- Call cancel with sharedRideChecks=false to avoid recursion
      void $ cancel 
        (CancelRideReq otherRide.bookingId Nothing Nothing Nothing) 
        merchant 
        otherBooking 
        Nothing 
        False  -- sharedRideChecks = false
```

#### 4. Update notifyOnCancel Call (Line 162)
```haskell
-- Current:
Notify.notifyOnCancel booking.merchantOperatingCityId booking driver bookingCR.source

-- Updated:
Notify.notifyOnCancel booking.merchantOperatingCityId booking driver bookingCR.source booking.sharedEntityId
```

#### 5. Update API Layer (API/Beckn/Cancel.hs Line 100)
```haskell
-- Pass sharedRideChecks=true from API layer
result <- cancel req merchant booking mbActiveSearchTry True
```

### Helper Functions Required:

#### updateTrackedEntityStatus Function
```haskell
-- Helper function to update isActive status for a specific entity in the list
updateTrackedEntityStatus :: [TrackedEntity] -> Text -> Bool -> [TrackedEntity]
updateTrackedEntityStatus entities entityId newStatus =
  map (\entity -> if entity.entityId == entityId 
                  then entity { isActive = newStatus }
                  else entity) entities
```

#### analyzeSharedRideStates Function
```haskell
analyzeSharedRideStates :: [TrackedEntity] -> Id Ride -> Flow (Int, Int, [Text])
analyzeSharedRideStates rideList currentRideId = do
  -- Exclude current ride from active rides list
  let activeRideIds = map (.entityId) $ filter (\entity -> entity.isActive && entity.entityId /= currentRideId.getId) rideList
  
  rideStatuses <- forM activeRideIds $ \rideId -> do
    ride <- QRide.findById (Id rideId) >>= fromMaybeM (RideNotFound rideId)
    return (rideId, ride.status)
  
  let activeCount = length activeRideIds
      inProgressCount = length $ filter (\(_, status) -> status == SRide.INPROGRESS) rideStatuses
  
  return (activeCount, inProgressCount, activeRideIds)
```

### Required Dependencies:

#### New Query Functions:
```haskell
-- In Storage.Queries.SharedEntity
findById :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => Text -> m (Maybe SharedEntity)

-- Simple update functions that take pre-processed JSON strings
updateBookingAndRideLists :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r)
                          => Text                 -- sharedEntityId
                          -> [TrackedEntity]      -- updated booking list
                          -> [TrackedEntity]      -- updated ride list
                          -> m ()

updateStatusAndLists :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r)
                     => EntityStatus           -- new status
                     -> Text                   -- sharedEntityId
                     -> [TrackedEntity]        -- updated booking list
                     -> [TrackedEntity]        -- updated ride list
                     -> m ()

-- In Storage.Queries.SharedRideConfig  
getSharedRideThreshold :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r)
                       => Id MerchantOperatingCity
                       -> m Int
```

#### Required Imports:
```haskell
import qualified Storage.Queries.SharedEntity as QSharedEntity
import qualified Storage.Queries.SharedRideConfig as QSharedRideConfig
import qualified Domain.Types.SharedEntity as DSharedEntity
```

#### Domain Types:
```haskell
-- TrackedEntity structure in SharedEntity
data TrackedEntity = TrackedEntity
  { entityId :: Text,
    isActive :: Bool
  }

-- SharedEntity status enum
data EntityStatus = ... | CANCELLED | ...
```

### Key Benefits:

1. **Intelligent Cancellation**: Different logic based on ride progress and participant count
2. **Threshold Protection**: Prevents shared rides from falling below minimum participants
3. **Cascade Cancellation**: Automatically cancels remaining rides when threshold not met
4. **In-Progress Protection**: Doesn't disrupt ongoing rides
5. **Recursive Prevention**: Uses sharedRideChecks flag to prevent infinite recursion
6. **Efficient Updates**: Single queries for batch status updates

### Testing Strategy:

1. **Unit Tests**:
   - Test each decision branch (inprogress, threshold met, below threshold)
   - Test analyzeSharedRideStates with various ride combinations
   - Test recursive cancellation prevention
   - Verify shared entity status updates

2. **Integration Tests**:
   - Test complete shared ride cancellation flows
   - Verify notification handling for shared rides
   - Test threshold-based decision making
   - Verify database consistency after cancellations

### Next Steps:

1. Implement required query functions in SharedEntity and SharedRideConfig modules
2. Add TrackedEntity and EntityStatus to domain types
3. Update cancel function signature and all calling sites
4. Implement handleSharedRideCancel and helper functions
5. Update notifyOnCancel to handle sharedEntityId parameter
6. Test shared ride cancellation scenarios

---

## ✅ COMPLETED: Rider Platform Common.hs Shared Ride Cancellation

### Implementation Details

**Location**: `Backend/app/rider-platform/rider-app/Main/src/Domain/Action/Beckn/Common.hs`

### Staged Changes Applied:

#### 1. Added Required Imports (Lines 100-101)
```haskell
import qualified Storage.Queries.SharedEntity as QSharedEntity
import qualified Domain.Types.SharedEntity as DSharedEntity
```

#### 2. Updated cancellationTransaction Function (Lines 880-885)
```haskell
-- Handle shared ride cancellation
whenJust booking.sharedEntityId $ \sharedEntityId -> 
  whenJust mbRide $ \ride -> 
    handleSharedRideCancel sharedEntityId ride booking cancellationSource
-- notify customer
bppDetails <- CQBPP.findBySubscriberIdAndDomain booking.providerId Context.MOBILITY >>= fromMaybeM (InternalError $ "BPP details not found for providerId:-" <> booking.providerId <> "and domain:-" <> show Context.MOBILITY)
Notify.notifyOnBookingCancelled booking cancellationSource bppDetails mbRide otherParties booking.sharedEntityId
```

#### 3. Implemented handleSharedRideCancel Function (Lines 1378-1413)
```haskell
-- Handle shared ride cancellation
handleSharedRideCancel ::
  ( CacheFlow m r,
    EsqDBFlow m r,
    MonadFlow m
  ) =>
  Text ->
  DRide.Ride ->
  DRB.Booking ->
  BookingCancellationSource ->
  m ()
handleSharedRideCancel sharedEntityId ride booking source = do
  -- Fetch shared entity
  sharedEntity <- QSharedEntity.findById sharedEntityId >>= fromMaybeM (InternalError $ "SharedEntity not found for id: " <> sharedEntityId)
  
  -- Fetch threshold from shared ride config
  threshold <- getSharedRideThreshold booking.merchantOperatingCityId "BATCHING" -- hardcoded for now
  
  -- Get all active ride IDs (excluding current ride)
  let activeRideIds = map (.entityId) $ filter (\entity -> entity.isActive && entity.entityId /= ride.id.getId) sharedEntity.rideIds
  
  -- Check if any ride has INPROGRESS status
  inProgressExists <- checkIfAnyRideInProgress activeRideIds
  
  -- Update booking and ride isActive to false
  let updatedBookingList = updateTrackedEntityStatus sharedEntity.bookingIds booking.id.getId False
      updatedRideList = updateTrackedEntityStatus sharedEntity.rideIds ride.id.getId False
      remainingActiveCount = length $ filter (.isActive) updatedRideList
  
  if (inProgressExists || remainingActiveCount >= threshold) && (source /= ByDriver)
    then do
      -- Case 1: INPROGRESS rides exist OR threshold met AND not driver cancellation - just update current booking/ride
      void $ QSharedEntity.updateBookingAndRideLists sharedEntityId updatedBookingList updatedRideList
    else do
      -- Case 2: Below threshold OR driver cancellation - mark all as inactive and cancel entity
      let allInactiveBookingList = map (\entity -> entity { DSharedEntity.isActive = False }) updatedBookingList
          allInactiveRideList = map (\entity -> entity { DSharedEntity.isActive = False }) updatedRideList
      void $ QSharedEntity.updateStatusAndLists DSharedEntity.CANCELLED sharedEntityId allInactiveBookingList allInactiveRideList

-- Helper function to check if any ride has INPROGRESS status
checkIfAnyRideInProgress :: [Text] -> m Bool
checkIfAnyRideInProgress rideIds = do
  rideStatuses <- forM rideIds $ \rideId -> do
    mbRide <- QRide.findById (Id rideId)
    case mbRide of
      Just r -> return (Just r.status)
      Nothing -> return Nothing
  return $ any (== Just DRide.INPROGRESS) rideStatuses
```

#### 4. Implemented updateTrackedEntityStatus Helper (Lines 1415-1420)
```haskell
-- Helper function to update isActive status for a specific entity in the list
updateTrackedEntityStatus :: [DSharedEntity.TrackedEntity] -> Text -> Bool -> [DSharedEntity.TrackedEntity]
updateTrackedEntityStatus entities entityId newStatus =
  map (\entity -> if entity.entityId == entityId 
                  then entity { DSharedEntity.isActive = newStatus }
                  else entity) entities
```

#### 5. Updated Notification Call
```haskell
-- Updated to pass sharedEntityId parameter
Notify.notifyOnBookingCancelled booking cancellationSource bppDetails mbRide otherParties booking.sharedEntityId
```

### Key Implementation Features:

1. **Conditional Shared Ride Processing**: Only executes when `booking.sharedEntityId` exists
2. **Source-Aware Cancellation**: Different behavior based on cancellation source (driver vs other)
3. **Threshold-Based Decision Making**: Uses shared ride config threshold for intelligent cancellation decisions
4. **INPROGRESS Ride Protection**: Preserves ongoing rides by checking INPROGRESS status
5. **Driver Cancellation Override**: Driver cancellations always cancel entire shared ride regardless of threshold
6. **Individual Entity Deactivation**: Marks specific booking and ride as inactive in shared entity
7. **Smart Status Management**: Simplified two-case logic with source consideration
8. **Efficient Updates**: Reuses existing helper functions for TrackedEntity status updates
9. **Notification Integration**: Passes sharedEntityId to notification system

### Logic Flow:

1. **Check for Shared Entity**: During cancellation, check if booking has `sharedEntityId`
2. **Fetch Shared Entity**: Retrieve shared entity data from database
3. **Fetch Threshold**: Get shared ride threshold from config using merchantOperatingCityId and "BATCHING" type
4. **Check INPROGRESS Rides**: Check if any active rides (excluding current) have INPROGRESS status
5. **Update Entity Status**: Mark current booking and ride as inactive in respective lists
6. **Conditional Decision Logic**:
   - **Case 1**: If (INPROGRESS rides exist OR threshold met) AND not driver cancellation → Update lists only, keep entity active
   - **Case 2**: If below threshold OR driver cancellation → Mark all as inactive and set status to CANCELLED
7. **Enhanced Notifications**: Pass sharedEntityId to notification system

### Benefits:

1. **Granular Control**: Individual ride cancellation within shared entity
2. **Driver Authority**: Driver cancellations have override power to cancel entire shared ride
3. **Threshold Protection**: Prevents shared rides from falling below minimum viable participants (except driver cancellation)
4. **INPROGRESS Protection**: Preserves ongoing rides from being affected by other cancellations
5. **Source-Aware Logic**: Different behavior based on who initiates the cancellation
6. **Configurable Rules**: Uses merchant-specific threshold settings for flexible business rules
7. **Status Integrity**: Shared entity remains active while viable rides continue (non-driver cancellations)
8. **Complete Cancellation**: Driver cancellations or below-threshold scenarios cancel entire ride
9. **Backward Compatibility**: Regular bookings (no sharedEntityId) unchanged
10. **Minimal Changes**: Reuses existing functions and patterns for maintainability

### Dependencies Satisfied:

- ✅ `QSharedEntity.findById` function
- ✅ `QSharedEntity.updateStatusAndLists` function  
- ✅ `QSharedEntity.updateBookingAndRideLists` function
- ✅ `DSharedEntity.TrackedEntity` type with `entityId` and `isActive` fields
- ✅ `DSharedEntity.CANCELLED` status enum
- ✅ `booking.sharedEntityId :: Maybe Text` field

### New Dependencies Required:

- 🔄 `getSharedRideThreshold :: Id MerchantOperatingCity -> Text -> m Int` function
- 🔄 `QRide.findById :: Id Ride -> m (Maybe Ride)` function
- 🔄 `DRide.INPROGRESS` status enum
- 🔄 `BookingCancellationSource` type with `ByDriver` constructor
- 🔄 Additional imports: `qualified Storage.Queries.Ride as QRide`, `qualified Domain.Types.Ride as DRide`

### Testing Covered:

1. **Unit Tests**:
   - Test handleSharedRideCancel with source-aware threshold-based decision logic
   - Test checkIfAnyRideInProgress with various ride status combinations
   - Test updateTrackedEntityStatus helper function
   - **Source-Specific Tests**:
     - Verify driver cancellation (ByDriver): Should always cancel entire shared ride
     - Verify customer cancellation with INPROGRESS rides: Should only update lists
     - Verify customer cancellation with threshold met: Should only update lists
     - Verify customer cancellation below threshold: Should cancel entire shared ride
   - Test threshold fetching from shared ride config
   - Test error handling for missing shared entity or rides

2. **Integration Tests**:
   - Test complete shared ride cancellation flow with different source scenarios
   - Verify database state consistency after source-aware cancellations
   - Test notification system with sharedEntityId parameter
   - Test integration with shared ride config for threshold values
   - Verify INPROGRESS ride protection behavior (non-driver cancellations)
   - Verify driver cancellation override behavior
   - Verify backward compatibility with regular bookings

---

## 🚧 PLANNED: Provider Platform Shared Ride Cancellation Implementation

### Simplified Implementation Plan

**Location**: `Backend/app/provider-platform/dynamic-offer-driver-app/Main/src/Domain/Action/UI/Ride/CancelRide.hs`

### Required Changes:

#### Step 1: **Add SharedEntity Imports**
```haskell
-- Add these imports after line 65 in CancelRide.hs
import qualified Storage.Queries.SharedEntity as QSharedEntity
import qualified Domain.Types.SharedEntity as DSharedEntity
```

#### Step 2: **Update driverCancelRideHandler Implementation**
```haskell
-- Modify driverCancelRideHandler function (lines 139-147) - add shared entity update before calling cancelRideHandler
driverCancelRideHandler ::
  ServiceHandle Flow ->
  Id DP.Person ->
  Id DRide.Ride ->
  CancelRideReq ->
  Flow CancelRideResp
driverCancelRideHandler shandle personId rideId req = do
  -- Check if ride has sharedEntityId and update shared entity
  ride <- shandle.findRideById rideId >>= fromMaybeM (RideDoesNotExist rideId.getId)
  whenJust ride.sharedEntityId $ \sharedEntityId -> do
    sharedEntity <- QSharedEntity.findById sharedEntityId >>= fromMaybeM (InternalError $ "SharedEntity not found for id: " <> sharedEntityId)
    
    -- Update shared entity: mark all booking and ride isActive as false AND set status as CANCELLED
    let updatedBookingList = map (\entity -> entity { isActive = False }) sharedEntity.bookingIds
        updatedRideList = map (\entity -> entity { isActive = False }) sharedEntity.rideIds
    void $ QSharedEntity.updateStatusAndLists DSharedEntity.CANCELLED sharedEntityId updatedBookingList updatedRideList
  
  -- Call normal cancelRideHandler
  withLogTag ("rideId-" <> rideId.getId) $
    cancelRideHandler shandle (PersonRequestorId personId) rideId req
```

### Logic Flow:

#### **Simplified Shared Ride Cancellation Process:**

1. **Driver Initiates Cancellation**: Driver cancels a ride through `driverCancelRideHandler`

2. **SharedEntity Check**: System checks if the ride has `sharedEntityId`

3. **Shared Entity Update** (if `sharedEntityId` exists):
   - Fetches shared entity from database
   - Marks ALL booking and ride entities as `isActive = false` in shared entity
   - Updates shared entity in database

4. **Normal Cancellation**: Calls regular `cancelRideHandler` which handles all the rest

### Key Benefits:

1. **Simplified Logic**: No complex iteration or BAP control needed
2. **Leverages Existing Code**: Uses existing cancellation flow
3. **Atomic Update**: All shared entities marked inactive before cancellation
4. **Clean Separation**: Shared ride logic separated from regular cancellation

### Required Dependencies:

#### New Query Functions:
```haskell
-- In Storage.Queries.SharedEntity
findById :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => Text -> m (Maybe SharedEntity)

updateBookingAndRideLists :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r)
                          => Text                 -- sharedEntityId
                          -> [TrackedEntity]      -- updated booking list
                          -> [TrackedEntity]      -- updated ride list
                          -> m ()
```

#### Required Domain Type Fields:
- `Ride.sharedEntityId :: Maybe Text` ✅ (schema changes assumed done)
- `SharedEntity.rideIds :: [TrackedEntity]` with `TrackedEntity {entityId, isActive}` structure

#### Step 3: **Update Internal.hs cancelRideTransaction Function**
```haskell
-- In cancelRideTransaction function, BEFORE lines 171-173, ADD:
  allNoShowCharges <- case ride.sharedEntityId of
    Just sharedEntityId -> do
      sharedEntity <- QSharedEntity.findById sharedEntityId >>= fromMaybeM (InternalError $ "SharedEntity not found for id: " <> sharedEntityId)
      let otherActiveRideIds = map (.entityId) $ filter (\entity -> entity.isActive && entity.entityId /= ride.id.getId) sharedEntity.rideIds
      otherNoShowCharges <- forM otherActiveRideIds $ \rideId -> do
        otherRide <- QRide.findById (Id rideId) >>= fromMaybeM (RideNotFound rideId)
        otherBooking <- QRB.findById otherRide.bookingId >>= fromMaybeM (BookingNotFound otherRide.bookingId.getId)
        otherNoShowCharges <- if transporterConfig.canAddCancellationFee then calculateNoShowCharges otherBooking otherRide else return Nothing
        void $ QRide.updateStatusAndRideEndedBy otherRide.id DRide.CANCELLED rideEndedBy
        void $ QRB.updateStatus otherBooking.id SRB.CANCELLED
        let otherBookingCReason = bookingCReason { bookingId = otherBooking.id, rideId = Just otherRide.id }
        QBCR.upsert otherBookingCReason
        return (otherNoShowCharges, otherBooking, otherRide)
      return $ (cancellationFee, booking, ride) : otherNoShowCharges
    Nothing -> return [(cancellationFee, booking, ride)]

-- THEN continue with existing lines 171-173:
  void $ QRide.updateStatusAndRideEndedBy ride.id DRide.CANCELLED rideEndedBy
  QBCR.upsert bookingCReason  
  void $ QRB.updateStatus booking.id SRB.CANCELLED
  when (bookingCReason.source == SBCR.ByDriver) $ QDriverStats.updateIdleTime driverId
  
-- REPLACE lines 175-182 with:
  forM_ allNoShowCharges $ \(noShowFee, bkg, rd) -> do
    case (noShowFee, bkg.riderId) of
      (Just fee, Just rid) -> do
        QRide.updateCancellationFeeIfCancelledField (Just fee.amount) rd.id
        riderDetails <- QRiderDetails.findById rid >>= fromMaybeM (RiderDetailsNotFound rid.getId)
        QRiderDetails.updateCancellationDues (fee.amount + riderDetails.cancellationDues) rid
      _ -> do
        logError "cancelRideTransaction: riderId in booking or cancellationFee is not present"
```

### Implementation Status: 🚧 **PLANNED**

Simplified step-by-step implementation plan:
- 📝 Step 1: Add SharedEntity imports
- 📝 Step 2: Add shared entity update logic to driverCancelRideHandler with status CANCELLED
- 📝 Step 3: Add shared ride handling logic in Internal.hs before lines 171-173

---
