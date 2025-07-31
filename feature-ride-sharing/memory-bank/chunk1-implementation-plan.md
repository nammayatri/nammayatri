# Chunk 1 Implementation Plan: Initial Search and Pre-Pooling Checks

## Overview
This document outlines the specific implementation plan for Chunk 1 of the shared ride feature, mapping each requirement to existing APIs and code changes needed.

## 🔴 IMPORTANT NOTE: Domain Structure Design
**Do NOT add flags to identify shared ride estimates.** Instead, use the existing domain structure:

- **TripCategory**: `RideShare` will be a new trip category (alongside existing categories)
- **VehicleVariant**: For RideShare trip category, vehicle variants will be:
  - `Auto` → `Auto Share`
  - `Cab` → `Cab Share` 
  - `CabPremium` → `Cab Premium Share`
  - etc.
- **Identification**: An estimate is a shared ride estimate if its `tripCategory == RideShare`
- **No additional flags needed**: `is_shared_ride` or similar flags are NOT required in database

This approach leverages existing `fare_product_type` structure mentioned in search and maintains domain consistency.

## API Mapping and Implementation Details

### 1. Customer Search Initiation
- **Status**: ✅ Already implemented
- **API**: `/rideSearch` in `Backend/app/rider-platform/rider-app/Main/src/API/UI/Search.hs:137-147`
- **Domain Logic**: `Domain.Action.UI.Search.search` in `Backend/app/rider-platform/rider-app/Main/src/Domain/Action/UI/Search.hs:256-269`
- **Action Required**: None - existing functionality handles search initiation

### 2. Route Caching
- **Status**: 🔧 Requires Implementation
- **Location**: `Backend/app/rider-platform/rider-app/Main/src/Domain/Action/UI/Search.hs:354`
- **Implementation**:
  ```haskell
  -- Add after QSearchRequest.createDSReq searchRequest (line 354)
  fork "cache route response" $ do
    sharedRideConfig <- QSharedRideConfig.findByMerchantOpCityIdAndType 
                          merchantOperatingCityId OverlappingRoute
    case sharedRideConfig of
      Just config | config.enableSharedRide -> do
        let cacheKey = "route_cache:" <> searchRequest.id.getId
        let cacheTimeout = calculateRouteCacheExpiry config
        Redis.setExp cacheKey (encode routeResponse) (fromIntegral cacheTimeout.getSeconds)
      _ -> return () -- Skip if shared ride not enabled
  ```
- **Dependencies**:
  - Import `Storage.CachedQueries.SharedRideConfig as QSharedRideConfig`
  - Import Redis and JSON encoding modules

### 3. Geospatial Indexing for Hotspots
- **Status**: 🔧 Requires Implementation  
- **Location**: `Backend/app/rider-platform/rider-app/Main/src/Domain/Action/UI/Search.hs:355`
- **Implementation**:
  ```haskell
  -- Add after route caching logic
  fork "add to search hotspots GSI" $ do
    let gsiKey = "searchHotSpots"
    let memberKey = searchRequest.id.getId <> ":" <> show searchRequest.validTill
    let lat = searchRequest.fromLocation.lat
    let lon = searchRequest.fromLocation.lon
    Redis.geoAdd gsiKey [(lon, lat, memberKey)]
  ```
- **Dependencies**:
  - Redis geospatial commands support

### 4. Check for Nearby Riders
- **Status**: 🔧 Requires Implementation  
- **Location**: New function in `Backend/app/rider-platform/rider-app/Main/src/Domain/Action/Beckn/OnSearch.hs`
- **Implementation**:
  ```haskell
  -- Add new helper function in OnSearch.hs
  checkNearbyRiders :: 
    (MonadFlow m, CacheFlow m r) => 
    LatLong -> 
    Id MerchantOperatingCity -> 
    m Int
  checkNearbyRiders sourceLocation merchantOperatingCityId = do
    sharedRideConfig <- QSharedRideConfig.findByMerchantOpCityIdAndType 
                          merchantOperatingCityId OverlappingRoute
    case sharedRideConfig of
      Just config | config.enableSharedRide -> do
        let gsiKey = "searchHotSpots"
        nearbySearches <- Redis.geoRadius gsiKey sourceLocation.lon sourceLocation.lat 
                           (fromIntegral config.searchRadius.getMeters) "m"
        return $ length nearbySearches
      _ -> return 0 -- Shared ride not enabled
  ```
- **Dependencies**:
  - Import `Storage.CachedQueries.SharedRideConfig as QSharedRideConfig` in OnSearch.hs

### 5. Decision: Offer Shared Ride (Filter in OnSearch)
- **Status**: 🔧 Requires Implementation
- **API**: `Backend/app/rider-platform/rider-app/Main/src/Domain/Action/Beckn/OnSearch.hs` - when processing estimates from BPP
- **Implementation**:
  ```haskell
  -- 1. Add in onSearch handler - call checkNearbyRiders once and update searchRequest
  -- Add this early in onSearch handler after searchRequest is available
  nearbyRidersCount <- checkNearbyRiders searchRequest.fromLocation.gps 
                         searchRequest.merchantOperatingCityId
  QSearchRequest.updateNearbyRidersCount searchRequest.id nearbyRidersCount
  
  -- 2. Add common helper function to determine if shared rides should be shown
  shouldShowSharedRides :: 
    SearchRequestFlow m r => 
    Int -> 
    Id MerchantOperatingCity -> 
    m Bool
  shouldShowSharedRides nearbyRidersCount merchantOperatingCityId = do
    sharedRideConfig <- QSharedRideConfig.findByMerchantOpCityIdAndType 
                          merchantOperatingCityId OverlappingRoute
    let featureEnabled = maybe False (.enableSharedRide) sharedRideConfig
    let minThreshold = maybe 2 (.minRidersThreshold) sharedRideConfig
    let hotspotCheck = nearbyRidersCount >= minThreshold
    return (featureEnabled && hotspotCheck)
  
  -- 3. Add filter functions that use the common helper
  filterSharedRideEstimates :: 
    SearchRequestFlow m r => 
    [EstimateInfo] -> 
    Int -> 
    Id MerchantOperatingCity -> 
    m [EstimateInfo]
  filterSharedRideEstimates estimatesInfo nearbyRidersCount merchantOperatingCityId = do
    showSharedRides <- shouldShowSharedRides nearbyRidersCount merchantOperatingCityId
    if showSharedRides
      then return estimatesInfo  -- Keep all estimates including RideShare
      else return $ filter (\est -> est.tripCategory /= RideShare) estimatesInfo
  
  filterSharedRideQuotes :: 
    SearchRequestFlow m r => 
    [QuoteInfo] -> 
    Int -> 
    Id MerchantOperatingCity -> 
    m [QuoteInfo]
  filterSharedRideQuotes quotesInfo nearbyRidersCount merchantOperatingCityId = do
    showSharedRides <- shouldShowSharedRides nearbyRidersCount merchantOperatingCityId
    if showSharedRides
      then return quotesInfo  -- Keep all quotes including RideShare
      else return $ filter (\quote -> quote.tripCategory /= RideShare) quotesInfo
  
  -- 4. Update existing onSearch handler lines:
  -- FROM: estimates <- traverse (buildEstimate providerInfo now searchRequest deploymentVersion) (filterEstimtesByPrefference estimatesInfo blackListedVehicles)
  -- TO:
  filteredEstimates <- filterEstimtesByPrefference estimatesInfo blackListedVehicles
  sharedRideFilteredEstimates <- filterSharedRideEstimates filteredEstimates nearbyRidersCount searchRequest.merchantOperatingCityId
  estimates <- traverse (buildEstimate providerInfo now searchRequest deploymentVersion) sharedRideFilteredEstimates
  
  -- Similarly for quotes:
  -- FROM: quotes <- traverse (buildQuote requestId providerInfo now searchRequest deploymentVersion) (filterQuotesByPrefference quotesInfo blackListedVehicles)  
  -- TO:
  filteredQuotes <- filterQuotesByPrefference quotesInfo blackListedVehicles
  sharedRideFilteredQuotes <- filterSharedRideQuotes filteredQuotes nearbyRidersCount searchRequest.merchantOperatingCityId
  quotes <- traverse (buildQuote requestId providerInfo now searchRequest deploymentVersion) sharedRideFilteredQuotes
  ```

### 6. Customer Selects Shared Ride (Via Normal Estimate Selection)
- **Status**: ✅ No Additional Implementation Needed
- **Location**: Estimate selection happens via normal `/rideSearch/:searchId/results` flow
- **Implementation**:
  ```haskell
  -- No changes needed to Estimate entity structure
  -- Use existing tripCategory field:
  data Estimate = Estimate
    { -- ... existing fields
      tripCategory :: TripCategory, -- RideShare for shared rides
      vehicleVariant :: VehicleVariant -- "Auto Share", "Cab Share", etc.
    }
  
  -- Frontend gets estimates normally
  -- Shared ride estimates identified by tripCategory == RideShare
  -- No additional validation needed - if estimate exists, it passed all checks
  ```

### 7. KYC Verification (Frontend-Driven Flow)
- **Status**: 🔧 Requires Implementation
- **Frontend Flow**: 
  - Customer taps on shared ride estimate
  - Frontend checks `kycVerified` field from customer profile API
  - If KYC not done, frontend shows popup for work email entry
  - Customer enters email → external service validates → OTP sent → OTP validation
  - Once OTP validation succeeds, frontend retriggers search
  - Frontend then allows estimate selection since KYC is now complete
- **Backend Implementation**:
  ```haskell
  -- Add KYC status check in select API (when customer selects estimate)
  when isSharedRideSelected $ do
    person <- QP.findById personId
    let kycVerified = fromMaybe False person.kycVerified
    unless kycVerified $ 
      throwError $ InvalidRequest "KYC verification required for shared rides"
  ```
- **Profile API Changes**:
  - Add `kycVerified :: Bool` field to `ProfileRes` 
  - Include KYC status in customer profile response
- **Dependencies**:
  - Add `kycVerified :: Maybe Bool` to `Person` entity (database column)
  - OTP flow APIs (future implementation - separate from this chunk)

## Configuration Requirements

### SharedRideConfig (Already Implemented)
- **Status**: ✅ Table exists and cached queries available
- **Usage**: Import `Storage.CachedQueries.SharedRideConfig as QSharedRideConfig`
- **Available Methods**:
  - `findByMerchantOpCityIdAndType :: Id MerchantOperatingCity -> ShareRideType -> m (Maybe SharedRideConfig)`
  - Cache expiry calculation and other utility functions already available

## Database Schema Changes Required

### Person Table
```sql
ALTER TABLE atlas_app.person 
ADD COLUMN kyc_verified BOOLEAN DEFAULT FALSE;
```

### Search Request Table  
```sql
ALTER TABLE atlas_app.search_request 
ADD COLUMN nearby_riders_count INTEGER;
```

### Storage Query Function
Add to `Storage.Queries.SearchRequest`:
```haskell
updateNearbyRidersCount :: 
  Id SearchRequest -> 
  Int -> 
  SqlDB ()
updateNearbyRidersCount searchRequestId count = do
  now <- getCurrentTime
  update $ \tbl -> do
    set tbl [SearchRequestNearbyRidersCount =. val (Just count), 
             SearchRequestUpdatedAt =. val now]
    where_ $ tbl ^. SearchRequestId ==. val searchRequestId.getId
```

## Redis Data Structures

### Route Cache
- **Key Pattern**: `route_cache:{searchId}`
- **Value**: JSON encoded route response
- **TTL**: Configurable (default: 30 minutes)

### Search Hotspots GSI
- **Key**: `searchHotSpots`
- **Members**: `{searchId}:{validTill}`
- **Coordinates**: Pickup location lat/lon
- **Auto-expiry**: Based on search validTill

## Implementation Priority

1. **Phase 1**: Route caching and GSI indexing
2. **Phase 2**: Nearby riders check and response modification  
3. **Phase 3**: Shared ride selection and KYC verification
4. **Phase 4**: Integration testing and configuration

## Testing Strategy

1. **Unit Tests**: Each new function with mock Redis
2. **Integration Tests**: End-to-end search flow with shared ride options
3. **Load Tests**: Redis GSI performance with concurrent searches
4. **Feature Flags**: Gradual rollout with `enableSharedRide` config

## Rollback Plan

- Feature flag `enableSharedRide` to disable instantly
- Redis data structures are non-breaking additions
- Database columns have default values for backward compatibility