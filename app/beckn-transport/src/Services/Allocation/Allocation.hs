module Services.Allocation.Allocation where

import Beckn.Types.Common
import Beckn.Types.Core.Taxi.Common.CancellationSource (CancellationSource (..))
import Beckn.Types.Id
import Beckn.Utils.NonEmpty
import Data.Generics.Labels ()
import qualified Data.Text as T
import EulerHS.Prelude
import qualified Types.API.RideBooking as RideBooking
import Types.App
import Types.Storage.AllocationEvent (AllocationEventType (..))
import qualified Types.Storage.CancellationReason as SCR
import Types.Storage.Organization
import qualified Types.Storage.RideBooking as SRB
import qualified Types.Storage.RideBookingCancellationReason as SBCR
import qualified Types.Storage.RideRequest as SRR
import Utils.Common

newtype OrderTime = OrderTime
  { utcTime :: UTCTime
  }
  deriving (Generic, Show)

data RequestData
  = Allocation
  | Cancellation
  | DriverResponse RideBooking.DriverResponse
  deriving (Generic, Show, FromJSON, ToJSON)

data RideRequest = RideRequest
  { requestId :: Id SRR.RideRequest,
    rideBookingId :: Id SRB.RideBooking,
    requestData :: RequestData
  }
  deriving (Generic, Show)

data NotificationStatus
  = Notified
  | Rejected
  | Ignored
  deriving (Eq, Show)

data CurrentNotification = CurrentNotification
  { driverId :: Id Driver,
    expiryTime :: UTCTime
  }
  deriving (Show)

data RideStatus
  = Confirmed
  | AwaitingReassignment
  | Assigned
  | Completed
  | Cancelled
  deriving (Show, Eq, Ord, Read, Generic, ToJSON, FromJSON)

data RideInfo = RideInfo
  { rideBookingId :: Id SRB.RideBooking,
    rideStatus :: RideStatus,
    orderTime :: OrderTime,
    reallocationsCount :: Int
  }
  deriving (Generic)

data AllocatorCancellationReason = AllocationTimeExpired | NoDriversInRange | ReallocationLimitExceed

instance ToJSON AllocatorCancellationReason where
  toJSON = \case
    AllocationTimeExpired -> "ALLOCATION_TIME_EXPIRED"
    NoDriversInRange -> "NO_DRIVERS_IN_RANGE"
    ReallocationLimitExceed -> "REALLOCATION_LIMIT_EXCEED"

type MonadHandler m =
  ( MonadCatch m,
    MonadTime m,
    MonadClock m,
    MonadGuid m,
    Log m
  )

data AllocatorMetricsHandle m = AllocatorMetricsHandle
  { incrementTaskCounter :: m (),
    incrementFailedTaskCounter :: m (),
    putTaskDuration :: Milliseconds -> m (),
    incrementErrorCounter :: SomeException -> m ()
  }

data ServiceHandle m = ServiceHandle
  { getDriverSortMode :: m SortMode,
    getConfiguredNotificationTime :: m Seconds,
    getConfiguredAllocationTime :: m Seconds,
    getConfiguredReallocationsLimit :: m Int,
    getDriverBatchSize :: m Int,
    getRequests :: ShortId Organization -> Integer -> m [RideRequest],
    getDriverPool :: Id SRB.RideBooking -> m [Id Driver],
    getCurrentNotifications :: Id SRB.RideBooking -> m [CurrentNotification],
    cleanupOldNotifications :: m Int,
    sendNewRideNotifications :: Id SRB.RideBooking -> NonEmpty (Id Driver) -> m (),
    sendRideNotAssignedNotification :: Id SRB.RideBooking -> Id Driver -> m (),
    addNotificationStatuses :: Id SRB.RideBooking -> NonEmpty (Id Driver) -> UTCTime -> m (),
    updateNotificationStatuses :: Id SRB.RideBooking -> NotificationStatus -> NonEmpty (Id Driver) -> m (),
    resetLastRejectionTimes :: NonEmpty (Id Driver) -> m (),
    getAttemptedDrivers :: Id SRB.RideBooking -> m [Id Driver],
    getDriversWithNotification :: m [Id Driver],
    getTopDriversByIdleTime :: Int -> [Id Driver] -> m [Id Driver],
    checkAvailability :: NonEmpty (Id Driver) -> m [Id Driver],
    assignDriver :: Id SRB.RideBooking -> Id Driver -> m (),
    cancelRideBooking :: Id SRB.RideBooking -> SBCR.RideBookingCancellationReason -> m (),
    cleanupNotifications :: Id SRB.RideBooking -> m (),
    addAllocationRequest :: ShortId Organization -> Id SRB.RideBooking -> m (),
    getRideInfo :: Id SRB.RideBooking -> m RideInfo,
    removeRequest :: Id SRR.RideRequest -> m (),
    logEvent :: AllocationEventType -> Id SRB.RideBooking -> m (),
    logDriverEvents :: AllocationEventType -> Id SRB.RideBooking -> NonEmpty (Id Driver) -> m (),
    metrics :: AllocatorMetricsHandle m
  }

process :: MonadHandler m => ServiceHandle m -> ShortId Organization -> Integer -> m Int
process handle@ServiceHandle {..} shortOrgId requestsNum = do
  cleanedNotificationsCount <- cleanupOldNotifications
  when (cleanedNotificationsCount > 0) $ logInfo $ "Cleaned notifications count: " <> show cleanedNotificationsCount
  rideRequests <- getRequests shortOrgId requestsNum
  let rideRequestsNum = length rideRequests
  unless (rideRequestsNum == 0)
    . measuringDurationToLog INFO ("processing " <> show rideRequestsNum <> " ride requests")
    $ traverse_ (processRequest handle shortOrgId) rideRequests
  pure rideRequestsNum

processRequest :: MonadHandler m => ServiceHandle m -> ShortId Organization -> RideRequest -> m ()
processRequest handle@ServiceHandle {..} shortOrgId rideRequest = do
  metrics.incrementTaskCounter
  measuringDuration (\dur _ -> metrics.putTaskDuration dur) $ do
    let requestId = rideRequest.requestId
    let rideBookingId = rideRequest.rideBookingId
    rideInfo <- getRideInfo rideBookingId
    let rideStatus = rideInfo.rideStatus
    eres <- try $
      withLogTag ("RideRequest_" <> rideBookingId.getId) $ do
        logInfo "Start processing request"
        case rideRequest.requestData of
          Allocation ->
            case rideStatus of
              Confirmed -> processAllocation handle shortOrgId rideInfo
              AwaitingReassignment -> processAllocation handle shortOrgId rideInfo
              Cancelled -> logInfo "Ride is cancelled, allocation request skipped"
              _ ->
                logWarning $ "Ride status is " <> show rideStatus <> ", allocation request skipped"
          DriverResponse response ->
            case rideStatus of
              Confirmed -> processDriverResponse handle response rideBookingId
              AwaitingReassignment -> processDriverResponse handle response rideBookingId
              Assigned -> do
                logInfo "Ride is assigned, response request skipped"
                sendRideNotAssignedNotification rideBookingId response.driverId
              Cancelled -> do
                logInfo "Ride is cancelled, response request skipped"
                sendRideNotAssignedNotification rideBookingId response.driverId
              _ -> logWarning $ "Ride status is " <> show rideStatus <> ", response request skipped"
          Cancellation ->
            case rideStatus of
              status | status == Confirmed || status == Assigned -> do
                cancel handle rideBookingId ByUser Nothing
                logEvent ConsumerCancelled rideBookingId
              _ ->
                logWarning $ "Ride status is " <> show rideStatus <> ", cancellation request skipped"
        logInfo "End processing request"
    whenLeft eres $
      \(exc :: SomeException) -> do
        let message = "Error processing request " <> show requestId <> ": " <> makeLogSomeException exc
        logError message
        metrics.incrementErrorCounter exc
        metrics.incrementFailedTaskCounter
    removeRequest requestId

processDriverResponse :: MonadHandler m => ServiceHandle m -> RideBooking.DriverResponse -> Id SRB.RideBooking -> m ()
processDriverResponse handle@ServiceHandle {..} response rideBookingId = do
  currentNotifications <- getCurrentNotifications rideBookingId
  logInfo $ "getCurrentNotifications" <> show currentNotifications
  if response.driverId `elem` map (.driverId) currentNotifications
    then case response.status of
      RideBooking.ACCEPT -> do
        logInfo $ "Assigning driver" <> show response.driverId
        assignDriver rideBookingId response.driverId
        cleanupNotifications rideBookingId
        logDriverEvents MarkedAsAccepted rideBookingId $ singleton response.driverId
      RideBooking.REJECT ->
        processRejection handle rideBookingId response.driverId
    else logDriverNoLongerNotified rideBookingId response.driverId

processAllocation ::
  MonadHandler m =>
  ServiceHandle m ->
  ShortId Organization ->
  RideInfo ->
  m ()
processAllocation handle@ServiceHandle {..} shortOrgId rideInfo = do
  let rideBookingId = rideInfo.rideBookingId
  let orderTime = rideInfo.orderTime
  currentTime <- getCurrentTime
  allocationTimeFinished <- isAllocationTimeFinished handle currentTime orderTime
  allocationLimitExceed <- isReallocationLimitExceed handle rideInfo.reallocationsCount
  logInfo $ "isAllocationTimeFinished " <> show allocationTimeFinished
  if allocationTimeFinished || allocationLimitExceed
    then do
      let cancellationReason = if allocationTimeFinished then AllocationTimeExpired else ReallocationLimitExceed
      cancel handle rideBookingId ByAllocator $ Just cancellationReason
      logEvent AllocationTimeFinished rideBookingId
    else do
      currentNotifications <- getCurrentNotifications rideBookingId
      logInfo $ "getCurrentNotification " <> show currentNotifications
      case currentNotifications of
        notification : notifications -> do
          let notificationTimeFinished = currentTime > notification.expiryTime
          if notificationTimeFinished
            then do
              processExpiration handle rideBookingId $ fmap (.driverId) $ notification :| notifications
              proceedToNewDrivers handle rideBookingId shortOrgId
            else checkRideLater handle shortOrgId rideBookingId
        [] ->
          proceedToNewDrivers handle rideBookingId shortOrgId

processRejection ::
  MonadHandler m =>
  ServiceHandle m ->
  Id SRB.RideBooking ->
  Id Driver ->
  m ()
processRejection ServiceHandle {..} rideBookingId driverId = do
  logInfo "Processing rejection"
  resetLastRejectionTimes $ singleton driverId
  updateNotificationStatuses rideBookingId Rejected $ singleton driverId
  logDriverEvents MarkedAsRejected rideBookingId $ singleton driverId

processExpiration :: MonadHandler m => ServiceHandle m -> Id SRB.RideBooking -> NonEmpty (Id Driver) -> m ()
processExpiration ServiceHandle {..} rideBookingId driverIds = do
  logInfo "Processing expiration"
  resetLastRejectionTimes driverIds
  updateNotificationStatuses rideBookingId Ignored driverIds
  logDriverEvents MarkedAsIgnored rideBookingId driverIds

proceedToNewDrivers ::
  MonadHandler m =>
  ServiceHandle m ->
  Id SRB.RideBooking ->
  ShortId Organization ->
  m ()
proceedToNewDrivers handle@ServiceHandle {..} rideBookingId shortOrgId = do
  logInfo "Proceed to new drivers"
  driverPool <- getDriverPool rideBookingId
  logInfo $ "DriverPool " <> T.intercalate ", " (getId <$> driverPool)
  attemptedDrivers <- getAttemptedDrivers rideBookingId
  let newDrivers = filter (`notElem` attemptedDrivers) driverPool
  case newDrivers of
    driverId : driverIds -> do
      availableDrivers <- checkAvailability $ driverId :| driverIds
      driversWithNotification <- getDriversWithNotification
      let filteredPool = filter (`notElem` driversWithNotification) availableDrivers
      logInfo $ "Filtered_DriverPool " <> T.intercalate ", " (getId <$> filteredPool)
      processFilteredPool handle rideBookingId filteredPool shortOrgId
    [] -> do
      cancel handle rideBookingId ByAllocator $ Just NoDriversInRange
      logEvent EmptyDriverPool rideBookingId

processFilteredPool ::
  MonadHandler m =>
  ServiceHandle m ->
  Id SRB.RideBooking ->
  [Id Driver] ->
  ShortId Organization ->
  m ()
processFilteredPool handle@ServiceHandle {..} rideBookingId filteredPool shortOrgId = do
  sortMode <- getDriverSortMode
  batchSize <- getDriverBatchSize
  topDrivers <-
    case sortMode of
      ETA -> pure $ take batchSize filteredPool
      IdleTime -> getTopDriversByIdleTime batchSize filteredPool
  case topDrivers of
    driverId : driverIds -> do
      let drivers = driverId :| driverIds
      currentTime <- getCurrentTime
      notificationTime <- fromIntegral <$> getConfiguredNotificationTime
      let expiryTime = addUTCTime notificationTime currentTime
      sendNewRideNotifications rideBookingId drivers
      addNotificationStatuses rideBookingId drivers expiryTime
      logInfo $ "Notified drivers " <> show (map getId topDrivers)
      logDriverEvents NotificationSent rideBookingId drivers
    [] -> do
      logInfo "All new drivers are unavailable or already have notifications. Waiting."
  checkRideLater handle shortOrgId rideBookingId

checkRideLater :: MonadHandler m => ServiceHandle m -> ShortId Organization -> Id SRB.RideBooking -> m ()
checkRideLater ServiceHandle {..} shortOrgId rideBookingId = do
  addAllocationRequest shortOrgId rideBookingId
  logInfo "Check ride later"

cancel :: MonadHandler m => ServiceHandle m -> Id SRB.RideBooking -> CancellationSource -> Maybe AllocatorCancellationReason -> m ()
cancel ServiceHandle {..} rideBookingId cancellationSource mbReasonCode = do
  logInfo "Cancelling ride"
  rideBookingCancellationReason <- buildRideBookingCancellationReason
  cancelRideBooking rideBookingId rideBookingCancellationReason
  cleanupNotifications rideBookingId
  where
    buildRideBookingCancellationReason = do
      guid <- generateGUID
      return
        SBCR.RideBookingCancellationReason
          { id = guid,
            driverId = Nothing,
            rideBookingId = rideBookingId,
            rideId = Nothing,
            source = cancellationSource,
            reasonCode = SCR.CancellationReasonCode . encodeToText <$> mbReasonCode,
            additionalInfo = Nothing
          }

isAllocationTimeFinished :: MonadHandler m => ServiceHandle m -> UTCTime -> OrderTime -> m Bool
isAllocationTimeFinished ServiceHandle {..} currentTime orderTime = do
  configuredAllocationTime <- fromIntegral <$> getConfiguredAllocationTime
  let elapsedSearchTime = diffUTCTime currentTime (orderTime.utcTime)
  pure $ elapsedSearchTime > configuredAllocationTime

isReallocationLimitExceed :: MonadHandler m => ServiceHandle m -> Int -> m Bool
isReallocationLimitExceed ServiceHandle {..} reallocationsCount = do
  reallocationsLimit <- getConfiguredReallocationsLimit
  pure $ reallocationsCount >= reallocationsLimit

logDriverNoLongerNotified :: Log m => Id SRB.RideBooking -> Id Driver -> m ()
logDriverNoLongerNotified rideBookingId driverId =
  logInfo $
    "Driver "
      <> show driverId
      <> " is no longer notified about ride "
      <> show rideBookingId
      <> ", response request skipped"
