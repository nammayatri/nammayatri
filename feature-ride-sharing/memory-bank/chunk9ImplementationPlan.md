# Chunk 9 Implementation Plan: Asynchronous Rider Pooling (Cron Job)

## Overview
This document outlines the implementation plan for Chunk 9, which implements the **Asynchronous Rider Pooling** system via cron jobs. This handles customers who couldn't be matched immediately in the sync flow and provides continuous background matching.

## Flow Summary
Based on sharedRideFlowCore.md, Chunk 9 implements a **periodic cron job** that:

1. **Fetches Waiting Riders** - Get all customers from GSI waiting pool
2. **Initial Filtering** - Remove locked/expired searches
3. **Iterative Matching Loop** - Process each customer through pooling logic
4. **Batch Creation** - Create shared entities for successful matches
5. **Notification** - Inform customers of successful matches

## 🔴 CORE ASYNC LOGIC

### **Cron Trigger Pattern**
- **Frequency**: Configurable interval (e.g., every 30 seconds)
- **Processing**: HashMap-based efficient iteration
- **Reuse**: Leverages same pooling logic from Chunk 4

### **Key Differences from Sync Flow**
- **Batch Processing**: Handles multiple customers per cycle
- **Background Execution**: No immediate user response required
- **Notification-Based**: Uses push notifications for match alerts
- **Queue Management**: Processes async pooling queue from Chunk 2

## Implementation Details

### 1. Cron Job Scheduler Integration
- **Status**: 🔧 Requires Implementation
- **Location**: `Backend/app/rider-platform/rider-app/Main/src/Scheduler/SharedRidePooling.hs`
- **Implementation**:
  ```haskell
  module Scheduler.SharedRidePooling where

  import qualified Domain.Action.UI.SharedRide.Pooling as Pooling
  import qualified Storage.CachedQueries.SharedRideConfig as QSharedRideConfig
  import qualified Data.HashMap.Strict as HM

  -- Main cron job entry point
  runAsyncRiderPoolingJob :: 
    (MonadFlow m, CacheFlow m r, EsqDBFlow m r, EsqDBReplicaFlow m r) => 
    m ()
  runAsyncRiderPoolingJob = do
    logInfo "Starting async rider pooling cron job"
    
    -- Get all merchant operating cities with async pooling enabled
    enabledConfigs <- getAsyncPoolingConfigs
    
    -- Process each city/vehicle category combination
    mapM_ processAsyncPoolingForConfig enabledConfigs
    
    logInfo "Completed async rider pooling cron job"

  getAsyncPoolingConfigs :: Flow [SharedRideConfig]
  getAsyncPoolingConfigs = do
    -- Get all shared ride configs where enableSharedRide=true and enableSyncPooling=false
    allConfigs <- QSharedRideConfig.findAllConfigs
    return $ filter (\cfg -> cfg.enableSharedRide && not cfg.enableSyncPooling) allConfigs

  processAsyncPoolingForConfig :: SharedRideConfig -> Flow ()
  processAsyncPoolingForConfig config = do
    logInfo $ "Processing async pooling for " <> show config.merchantOperatingCityId <> " - " <> show config.vehicleCategory
    
    -- Fetch waiting riders from GSI
    waitingRiders <- fetchWaitingRiders config
    
    -- Process pooling queue entries
    queueEntries <- fetchPoolingQueue
    let relevantEntries = filter (\qe -> qe.merchantOperatingCityId == config.merchantOperatingCityId 
                                       && qe.vehicleCategory == config.vehicleCategory) queueEntries
    
    -- Combine GSI riders with queue entries
    allWaitingCustomers <- combineWaitingCustomers waitingRiders relevantEntries
    
    -- Apply initial filtering
    validCustomers <- applyInitialFiltering allWaitingCustomers
    
    -- Process iterative matching loop
    processIterativeMatching validCustomers config
  ```

### 2. Waiting Riders Fetching from GSI
- **Status**: 🔧 Requires Implementation
- **Location**: Same module as above
- **Implementation**:
  ```haskell
  -- Fetch all waiting riders from ShareRideCustomerLoc GSI
  fetchWaitingRiders :: SharedRideConfig -> Flow [AsyncWaitingCustomer]
  fetchWaitingRiders config = do
    let gsiKey = "ShareRideCustomerLoc"
    
    -- Get all members from the geospatial index
    allMembers <- Redis.getAllMembers gsiKey
    
    -- Parse and validate each member
    validCustomers <- catMaybes <$> mapM parseAsyncWaitingCustomer allMembers
    
    -- Filter by merchant operating city and vehicle category
    let relevantCustomers = filter (\wc -> wc.merchantOperatingCityId == config.merchantOperatingCityId
                                        && wc.vehicleCategory == config.vehicleCategory) validCustomers
    
    return relevantCustomers

  parseAsyncWaitingCustomer :: (Text, Double, Double) -> Flow (Maybe AsyncWaitingCustomer)
  parseAsyncWaitingCustomer (memberKey, lat, lon) = do
    -- Format: "searchId:validTill:numSeats"
    case T.splitOn ":" memberKey of
      [searchIdText, validTillText, numSeatsText] -> do
        case (readMaybe (T.unpack validTillText), readMaybe (T.unpack numSeatsText)) of
          (Just validTill, Just numSeats) -> do
            let searchRequestId = Id searchIdText
            mbSearchRequest <- runInReplica $ QSR.findById searchRequestId
            case mbSearchRequest of
              Just sr -> return $ Just $ AsyncWaitingCustomer
                { searchRequestId = searchRequestId
                , searchRequest = sr
                , validTill = validTill
                , numSeats = numSeats
                , location = LatLong lat lon
                , merchantOperatingCityId = sr.merchantOperatingCityId
                , vehicleCategory = sr.vehicleCategory
                }
              Nothing -> return Nothing
          _ -> return Nothing
      _ -> return Nothing

  data AsyncWaitingCustomer = AsyncWaitingCustomer
    { searchRequestId :: Id SSR.SearchRequest
    , searchRequest :: SSR.SearchRequest
    , validTill :: UTCTime
    , numSeats :: Int
    , location :: LatLong
    , merchantOperatingCityId :: Id MerchantOperatingCity
    , vehicleCategory :: VehicleCategory
    } deriving (Show, Eq)

  -- Fetch entries from async pooling queue (added by Chunk 2)
  fetchPoolingQueue :: Flow [PoolingQueueEntry]
  fetchPoolingQueue = do
    let queueKey = "shared_ride_pooling_queue"
    queueItems <- Redis.lrange queueKey 0 (-1) -- Get all items
    catMaybes <$> mapM decodeQueueEntry queueItems

  decodeQueueEntry :: Text -> Flow (Maybe PoolingQueueEntry)
  decodeQueueEntry itemText = return $ decode $ T.encodeUtf8 itemText

  combineWaitingCustomers :: 
    [AsyncWaitingCustomer] -> 
    [PoolingQueueEntry] -> 
    Flow [AsyncWaitingCustomer]
  combineWaitingCustomers gsiCustomers queueEntries = do
    -- Convert queue entries to AsyncWaitingCustomer format
    queueCustomers <- mapM convertQueueEntryToCustomer queueEntries
    
    -- Combine and deduplicate based on searchRequestId
    let allCustomers = gsiCustomers ++ catMaybes queueCustomers
    let deduplicatedCustomers = HM.elems $ HM.fromList $ map (\c -> (c.searchRequestId, c)) allCustomers
    
    return deduplicatedCustomers

  convertQueueEntryToCustomer :: PoolingQueueEntry -> Flow (Maybe AsyncWaitingCustomer)
  convertQueueEntryToCustomer queueEntry = do
    mbSearchRequest <- runInReplica $ QSR.findById queueEntry.searchRequestId
    case mbSearchRequest of
      Just sr -> return $ Just $ AsyncWaitingCustomer
        { searchRequestId = queueEntry.searchRequestId
        , searchRequest = sr
        , validTill = addUTCTime (5 * 60) queueEntry.queuedAt -- 5 minutes from queue time
        , numSeats = queueEntry.requestedSeats
        , location = sr.fromLocation.gps
        , merchantOperatingCityId = queueEntry.merchantOperatingCityId
        , vehicleCategory = queueEntry.vehicleCategory
        }
      Nothing -> return Nothing
  ```

### 3. Initial Filtering and Validation
- **Status**: 🔧 Requires Implementation
- **Location**: Same module as above
- **Implementation**:
  ```haskell
  -- Apply initial filtering as per sharedRideFlowCore.md
  applyInitialFiltering :: [AsyncWaitingCustomer] -> Flow [AsyncWaitingCustomer]
  applyInitialFiltering waitingCustomers = do
    now <- getCurrentTime
    
    -- Filter out locked searches
    unlockedCustomers <- filterM (not <$> isSearchLocked . (.searchRequestId)) waitingCustomers
    
    -- Filter out expired searches
    let validCustomers = filter (\wc -> wc.validTill > now) unlockedCustomers
    
    -- Filter out customers who no longer have valid search requests
    activeCustomers <- filterM hasActiveSearchRequest validCustomers
    
    return activeCustomers

  isSearchLocked :: Id SSR.SearchRequest -> Flow Bool
  isSearchLocked searchId = do
    let lockKey = "search_lock:" <> searchId.getId
    Redis.exists lockKey

  hasActiveSearchRequest :: AsyncWaitingCustomer -> Flow Bool
  hasActiveSearchRequest customer = do
    mbSearch <- runInReplica $ QSR.findById customer.searchRequestId
    case mbSearch of
      Just sr -> return $ sr.validTill > customer.validTill -- Search request still valid
      Nothing -> return False
  ```

### 4. Iterative Matching Loop
- **Status**: 🔧 Requires Implementation
- **Location**: Same module as above
- **Implementation**:
  ```haskell
  -- Main iterative matching loop (per sharedRideFlowCore.md)
  processIterativeMatching :: [AsyncWaitingCustomer] -> SharedRideConfig -> Flow ()
  processIterativeMatching validCustomers config = do
    -- Create processing HashMap to track processed customers
    let processingMap = HM.fromList $ map (\c -> (c.searchRequestId, False)) validCustomers
    
    -- Process each unprocessed customer
    _ <- foldM (processCustomerForMatching config) processingMap validCustomers
    
    return ()

  processCustomerForMatching :: 
    SharedRideConfig -> 
    HM.HashMap (Id SSR.SearchRequest) Bool -> 
    AsyncWaitingCustomer -> 
    Flow (HM.HashMap (Id SSR.SearchRequest) Bool)
  processCustomerForMatching config processingMap customer = do
    -- Check if this customer has already been processed in this cycle
    case HM.lookup customer.searchRequestId processingMap of
      Just True -> return processingMap -- Already processed
      _ -> do
        logInfo $ "Processing async pooling for customer: " <> customer.searchRequestId.getId
        
        -- Mark current customer as being processed
        let updatedMap = HM.insert customer.searchRequestId True processingMap
        
        -- Invoke the same Rider Pooling Logic from Chunk 4
        poolingResult <- Pooling.invokeRiderPoolingLogic customer.searchRequest customer.numSeats
        
        case poolingResult of
          Pooling.PoolingSuccess (sharedSearchRequestId, sharedEstimateId) -> do
            -- Successful match found
            logInfo $ "Async pooling success for: " <> customer.searchRequestId.getId
            
            -- Notify all customers in the match
            notifyCustomersOfMatch sharedSearchRequestId sharedEstimateId
            
            -- Remove from GSI and queue
            cleanupMatchedCustomer customer
            
            -- Mark all matched customers as processed
            matchedCustomers <- getMatchedCustomerIds sharedSearchRequestId
            let finalMap = foldl (\m cId -> HM.insert cId True m) updatedMap matchedCustomers
            
            return finalMap
            
          Pooling.PoolingNoMatch -> do
            -- No match found, customer remains in waiting pool
            logDebug $ "No async match found for: " <> customer.searchRequestId.getId
            return updatedMap
            
          Pooling.PoolingError errorMsg -> do
            -- Error in pooling, log and continue
            logError $ "Async pooling error for " <> customer.searchRequestId.getId <> ": " <> errorMsg
            return updatedMap
  ```

### 5. Customer Notification System
- **Status**: 🔧 Requires Implementation
- **Location**: Same module as above
- **Implementation**:
  ```haskell
  -- Notify customers when a match is found
  notifyCustomersOfMatch :: 
    Id SharedSearchRequest -> 
    Id SharedEstimate -> 
    Flow ()
  notifyCustomersOfMatch sharedSearchRequestId sharedEstimateId = do
    -- Get all search requests in the shared batch
    sharedSearchRequest <- QSharedSR.findById sharedSearchRequestId >>= 
                          fromMaybeM (SharedSearchRequestNotFound sharedSearchRequestId.getId)
    
    -- Get customer IDs for notification
    customerIds <- mapM getCustomerIdFromSearchRequest sharedSearchRequest.searchRequestIds
    
    -- Send push notifications to all matched customers
    mapM_ (sendSharedRideMatchNotification sharedSearchRequestId sharedEstimateId) (catMaybes customerIds)
    
    logInfo $ "Sent match notifications to " <> show (length customerIds) <> " customers"

  getCustomerIdFromSearchRequest :: Id SSR.SearchRequest -> Flow (Maybe (Id Person))
  getCustomerIdFromSearchRequest searchRequestId = do
    mbSearchRequest <- runInReplica $ QSR.findById searchRequestId
    return $ fmap (.riderId) mbSearchRequest

  sendSharedRideMatchNotification :: 
    Id SharedSearchRequest -> 
    Id SharedEstimate -> 
    Id Person -> 
    Flow ()
  sendSharedRideMatchNotification sharedSearchRequestId sharedEstimateId customerId = do
    -- Create notification payload
    let notificationData = SharedRideMatchNotification
          { sharedSearchRequestId = sharedSearchRequestId
          , sharedEstimateId = sharedEstimateId
          , matchedAt = getCurrentTime
          , message = "Great! We found you a shared ride match."
          }
    
    -- Send FCM notification
    sendFCMNotification customerId "SHARED_RIDE_MATCH" (encode notificationData)
    
    logInfo $ "Sent shared ride match notification to customer: " <> customerId.getId

  data SharedRideMatchNotification = SharedRideMatchNotification
    { sharedSearchRequestId :: Id SharedSearchRequest
    , sharedEstimateId :: Id SharedEstimate
    , matchedAt :: UTCTime
    , message :: Text
    } deriving (Generic, Show, ToJSON, FromJSON)

  getMatchedCustomerIds :: Id SharedSearchRequest -> Flow [Id SSR.SearchRequest]
  getMatchedCustomerIds sharedSearchRequestId = do
    sharedSearchRequest <- QSharedSR.findById sharedSearchRequestId >>= 
                          fromMaybeM (SharedSearchRequestNotFound sharedSearchRequestId.getId)
    return sharedSearchRequest.searchRequestIds
  ```

### 6. Cleanup and Queue Management
- **Status**: 🔧 Requires Implementation
- **Location**: Same module as above
- **Implementation**:
  ```haskell
  -- Clean up matched customers from GSI and queue
  cleanupMatchedCustomer :: AsyncWaitingCustomer -> Flow ()
  cleanupMatchedCustomer customer = do
    -- Remove from GSI
    removeFromGSI customer
    
    -- Remove from pooling queue
    removeFromPoolingQueue customer.searchRequestId

  removeFromGSI :: AsyncWaitingCustomer -> Flow ()
  removeFromGSI customer = do
    let gsiKey = "ShareRideCustomerLoc"
    let memberKey = customer.searchRequestId.getId <> ":" <> 
                   show customer.validTill <> ":" <> 
                   show customer.numSeats
    Redis.geoRem gsiKey [memberKey]

  removeFromPoolingQueue :: Id SSR.SearchRequest -> Flow ()
  removeFromPoolingQueue searchRequestId = do
    let queueKey = "shared_ride_pooling_queue"
    queueItems <- Redis.lrange queueKey 0 (-1)
    
    -- Filter out items for this search request
    filteredItems <- filterM (not <$> isQueueItemForSearch searchRequestId) queueItems
    
    -- Replace queue with filtered items
    Redis.del [queueKey]
    when (not $ null filteredItems) $ 
      Redis.lpush queueKey filteredItems

  isQueueItemForSearch :: Id SSR.SearchRequest -> Text -> Flow Bool
  isQueueItemForSearch searchRequestId itemText = do
    case decodeQueueEntry itemText of
      Just queueEntry -> return $ queueEntry.searchRequestId == searchRequestId
      Nothing -> return False

  -- Periodic cleanup of expired entries
  cleanupExpiredEntries :: Flow ()
  cleanupExpiredEntries = do
    now <- getCurrentTime
    
    -- Clean up expired GSI entries
    cleanupExpiredGSIEntries now
    
    -- Clean up expired queue entries
    cleanupExpiredQueueEntries now

  cleanupExpiredGSIEntries :: UTCTime -> Flow ()
  cleanupExpiredGSIEntries now = do
    let gsiKey = "ShareRideCustomerLoc"
    allMembers <- Redis.getAllMembers gsiKey
    
    expiredMembers <- filterM (isGSIMemberExpired now) (map (\(m,_,_) -> m) allMembers)
    
    unless (null expiredMembers) $ do
      Redis.geoRem gsiKey expiredMembers
      logInfo $ "Cleaned up " <> show (length expiredMembers) <> " expired GSI entries"

  isGSIMemberExpired :: UTCTime -> Text -> Flow Bool
  isGSIMemberExpired now memberKey = do
    case T.splitOn ":" memberKey of
      [_, validTillText, _] -> do
        case readMaybe (T.unpack validTillText) of
          Just validTill -> return $ now > validTill
          Nothing -> return True -- Invalid format, consider expired
      _ -> return True

  cleanupExpiredQueueEntries :: UTCTime -> Flow ()
  cleanupExpiredQueueEntries now = do
    let queueKey = "shared_ride_pooling_queue"
    queueItems <- Redis.lrange queueKey 0 (-1)
    
    validItems <- filterM (not <$> isQueueEntryExpired now) queueItems
    
    when (length validItems /= length queueItems) $ do
      Redis.del [queueKey]
      unless (null validItems) $ 
        Redis.lpush queueKey validItems
      logInfo $ "Cleaned up " <> show (length queueItems - length validItems) <> " expired queue entries"

  isQueueEntryExpired :: UTCTime -> Text -> Flow Bool
  isQueueEntryExpired now itemText = do
    case decodeQueueEntry itemText of
      Just queueEntry -> do
        let expiryTime = addUTCTime (10 * 60) queueEntry.queuedAt -- 10 minutes expiry
        return $ now > expiryTime
      Nothing -> return True -- Invalid format, consider expired
  ```

## Scheduler Configuration

### Cron Job Setup
- **Status**: 🔧 Requires Scheduler Integration
- **Location**: `Backend/app/rider-platform/rider-app/Main/src/App.hs` or scheduler configuration
- **Implementation**:
  ```haskell
  -- Add to main application scheduler
  import qualified Scheduler.SharedRidePooling as SharedRidePooling

  setupSharedRideScheduler :: Flow ()
  setupSharedRideScheduler = do
    -- Schedule async rider pooling job
    scheduleRecurringJob 
      "async-rider-pooling" 
      (30 * 1000) -- 30 seconds interval
      SharedRidePooling.runAsyncRiderPoolingJob
    
    -- Schedule cleanup job
    scheduleRecurringJob
      "shared-ride-cleanup"
      (5 * 60 * 1000) -- 5 minutes interval
      SharedRidePooling.cleanupExpiredEntries
  ```

## API Extensions for Async Status

### Customer Status Check API
- **Status**: 🔧 Requires Implementation
- **Location**: `Backend/app/rider-platform/rider-app/Main/src/API/UI/SharedRide.hs`
- **Implementation**:
  ```haskell
  -- New API endpoint for checking async pooling status
  type SharedRideAPI = 
    "shared-rides" :> "pooling-status" :> Capture "searchId" (Id SSR.SearchRequest)
    :> Get '[JSON] AsyncPoolingStatusRes

  data AsyncPoolingStatusRes = AsyncPoolingStatusRes
    { status :: AsyncPoolingStatus
    , estimatedWaitTime :: Maybe Int -- seconds
    , queuePosition :: Maybe Int
    , matchedSharedSearchRequestId :: Maybe (Id SharedSearchRequest)
    } deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

  data AsyncPoolingStatus 
    = InQueue          -- Customer is in async pooling queue
    | BeingProcessed   -- Currently being processed by cron
    | MatchFound       -- Match found, waiting for customer action
    | Expired          -- Request expired, no longer in queue
    deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

  getAsyncPoolingStatus :: 
    Id SSR.SearchRequest -> 
    (Id Person, Id Merchant) -> 
    FlowHandler AsyncPoolingStatusRes
  getAsyncPoolingStatus searchRequestId (personId, merchantId) = 
    withFlowHandlerAPI $ checkAsyncPoolingStatus searchRequestId personId
  ```

## Configuration Requirements

### New SharedRideConfig Fields
```yaml
# Async pooling specific configs
asyncPoolingIntervalSeconds: Int           # Cron job frequency (30)
asyncPoolingBatchSize: Int                 # Max customers per batch (100)
asyncPoolingTimeoutMinutes: Int            # Max time in queue (10)
asyncNotificationRetryCount: Int           # FCM retry attempts (3)
```

## Error Handling

### Async Pooling Errors
```haskell
data AsyncPoolingError 
  = CronJobFailed Text
  | GSIFetchFailed 
  | QueueProcessingFailed Text
  | NotificationFailed (Id Person) Text
  | BatchCreationFailed Text
  | CleanupFailed Text
```

## Monitoring and Metrics

### Key Metrics to Track
```haskell
-- Metrics to expose for monitoring
data AsyncPoolingMetrics = AsyncPoolingMetrics
  { totalCustomersProcessed :: Int
  , successfulMatches :: Int
  , timeoutExpiries :: Int
  , averageWaitTime :: Double
  , cronJobDuration :: Double
  , notificationSuccess :: Int
  , notificationFailures :: Int
  } deriving (Generic, Show, ToJSON)
```

## Testing Strategy

### Unit Tests
1. **GSI fetching logic** - Test parsing and filtering
2. **Queue management** - Test queue operations and cleanup
3. **Iterative matching** - Test HashMap-based processing
4. **Notification system** - Test FCM integration

### Integration Tests
1. **End-to-end async flow** - Queue to notification
2. **Cron job execution** - Scheduled job testing
3. **Cross-chunk integration** - Integration with Chunk 4 pooling
4. **Concurrent processing** - Multiple cron instances

### Load Tests
1. **Large queue processing** - Thousands of waiting customers
2. **GSI performance** - Large geospatial index queries
3. **Notification throughput** - Mass FCM notifications
4. **Memory usage** - HashMap processing efficiency

## Success Metrics

- **Async match rate**: Percentage of queued customers matched
- **Average wait time**: Time from queue to match notification
- **Cron job performance**: Processing time and throughput
- **Notification delivery**: FCM success rates
- **Queue growth rate**: Balance between additions and processing

## Implementation Priority

1. **Phase 1**: Basic cron job and GSI fetching
2. **Phase 2**: Iterative matching loop integration
3. **Phase 3**: Notification system and status APIs
4. **Phase 4**: Advanced monitoring and cleanup

## Dependencies

- **From Chunk 2**: Async queue management, pooling interfaces
- **From Chunk 4**: Complete pooling logic implementation
- **External**: FCM service, cron scheduler, monitoring system