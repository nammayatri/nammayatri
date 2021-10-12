module Services.Allocation.Allocation where

import Beckn.Types.Common
import Beckn.Types.Id
import Beckn.Types.Mobility.Order (CancellationSource (..))
import Beckn.Utils.NonEmpty
import Data.Generics.Labels ()
import qualified Data.Text as T
import Data.Time.Clock (UTCTime, addUTCTime, diffUTCTime)
import EulerHS.Prelude
import qualified Types.API.Ride as Ride (DriverResponse (..), NotificationStatus (..))
import Types.App
import Types.Storage.AllocationEvent (AllocationEventType (..))
import qualified Types.Storage.CancellationReason as SCR
import Types.Storage.Organization
import qualified Types.Storage.RideCancellationReason as SRCR
import qualified Types.Storage.RideRequest as SRR
import Utils.Common

newtype OrderTime = OrderTime
  { utcTime :: UTCTime
  }
  deriving (Generic, Show)

data RequestData
  = Allocation
  | Cancellation
  | DriverResponse Ride.DriverResponse
  deriving (Generic, Show, FromJSON, ToJSON)

data RideRequest = RideRequest
  { requestId :: Id SRR.RideRequest,
    rideId :: Id Ride,
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
  | Assigned
  | InProgress
  | Completed
  | Cancelled
  deriving (Show, Eq, Ord, Read, Generic, ToJSON, FromJSON)

data RideInfo = RideInfo
  { rideId :: Id Ride,
    rideStatus :: RideStatus,
    orderTime :: OrderTime
  }
  deriving (Generic)

data AllocatorCancellationReason = AllocationTimeExpired | NoDriversInRange

instance ToJSON AllocatorCancellationReason where
  toJSON = \case
    AllocationTimeExpired -> "ALLOCATION_TIME_EXPIRED"
    NoDriversInRange -> "NO_DRIVERS_IN_RANGE"

type MonadHandler m =
  ( MonadCatch m,
    MonadTime m,
    MonadClock m,
    Log m
  )

data BTMMetricsHandle m = BTMMetricsHandle
  { incrementTaskCounter :: m (),
    incrementFailedTaskCounter :: m (),
    putTaskDuration :: Milliseconds -> m ()
  }

data ServiceHandle m = ServiceHandle
  { getDriverSortMode :: m SortMode,
    getConfiguredNotificationTime :: m Seconds,
    getConfiguredAllocationTime :: m Seconds,
    getDriverBatchSize :: m Int,
    getRequests :: ShortId Organization -> Integer -> m [RideRequest],
    getDriverPool :: Id Ride -> m [Id Driver],
    getCurrentNotifications :: Id Ride -> m [CurrentNotification],
    cleanupOldNotifications :: m Int,
    sendNewRideNotifications :: Id Ride -> NonEmpty (Id Driver) -> m (),
    sendRideNotAssignedNotification :: Id Ride -> Id Driver -> m (),
    addNotificationStatuses :: Id Ride -> NonEmpty (Id Driver) -> UTCTime -> m (),
    updateNotificationStatuses :: Id Ride -> NotificationStatus -> NonEmpty (Id Driver) -> m (),
    resetLastRejectionTimes :: NonEmpty (Id Driver) -> m (),
    getAttemptedDrivers :: Id Ride -> m [Id Driver],
    getDriversWithNotification :: m [Id Driver],
    getTopDriversByIdleTime :: Int -> [Id Driver] -> m [Id Driver],
    checkAvailability :: NonEmpty (Id Driver) -> m [Id Driver],
    assignDriver :: Id Ride -> Id Driver -> m (),
    cancelRide :: Id Ride -> SRCR.RideCancellationReason -> m (),
    cleanupNotifications :: Id Ride -> m (),
    addAllocationRequest :: ShortId Organization -> Id Ride -> m (),
    getRideInfo :: Id Ride -> m RideInfo,
    removeRequest :: Id SRR.RideRequest -> m (),
    logEvent :: AllocationEventType -> Id Ride -> m (),
    logDriverEvents :: AllocationEventType -> Id Ride -> NonEmpty (Id Driver) -> m (),
    metrics :: BTMMetricsHandle m
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
    let rideId = rideRequest.rideId
    rideInfo <- getRideInfo rideId
    let rideStatus = rideInfo.rideStatus
    eres <- try $
      withLogTag ("RideRequest_" <> rideId.getId) $ do
        logInfo "Start processing request"
        case rideRequest.requestData of
          Allocation ->
            case rideStatus of
              Confirmed -> processAllocation handle shortOrgId rideInfo
              Cancelled -> logInfo "Ride is cancelled, allocation request skipped"
              _ ->
                logWarning $ "Ride status is " <> show rideStatus <> ", allocation request skipped"
          DriverResponse response ->
            case rideStatus of
              Confirmed -> do
                currentNotifications <- getCurrentNotifications rideId
                logInfo $ "getCurrentNotifications" <> show currentNotifications
                if response.driverId `elem` map (.driverId) currentNotifications
                  then case response.status of
                    Ride.ACCEPT -> do
                      logInfo $ "Assigning driver" <> show response.driverId
                      assignDriver rideId response.driverId
                      cleanupNotifications rideId
                      logDriverEvents MarkedAsAccepted rideId $ singleton response.driverId
                    Ride.REJECT ->
                      processRejection handle rideId response.driverId
                  else logDriverNoLongerNotified rideId response.driverId
              Assigned -> do
                logInfo "Ride is assigned, response request skipped"
                sendRideNotAssignedNotification rideId response.driverId
              Cancelled -> do
                logInfo "Ride is cancelled, response request skipped"
                sendRideNotAssignedNotification rideId response.driverId
              _ -> logWarning $ "Ride status is " <> show rideStatus <> ", response request skipped"
          Cancellation ->
            case rideStatus of
              status | status == Confirmed || status == Assigned -> do
                cancel handle rideId ByUser Nothing
                logEvent ConsumerCancelled rideId
              _ ->
                logWarning $ "Ride status is " <> show rideStatus <> ", cancellation request skipped"
        logInfo "End processing request"
    whenLeft eres $
      \(err :: SomeException) -> do
        let message = "Error processing request " <> show requestId <> ": " <> show err
        logError message
        metrics.incrementFailedTaskCounter
    removeRequest requestId

processAllocation ::
  MonadHandler m =>
  ServiceHandle m ->
  ShortId Organization ->
  RideInfo ->
  m ()
processAllocation handle@ServiceHandle {..} shortOrgId rideInfo = do
  let rideId = rideInfo.rideId
  let orderTime = rideInfo.orderTime
  currentTime <- getCurrentTime
  allocationTimeFinished <- isAllocationTimeFinished handle currentTime orderTime
  logInfo $ "isAllocationTimeFinished " <> show allocationTimeFinished
  if allocationTimeFinished
    then do
      cancel handle rideId ByAllocator $ Just AllocationTimeExpired
      logEvent AllocationTimeFinished rideId
    else do
      currentNotifications <- getCurrentNotifications rideId
      logInfo $ "getCurrentNotification " <> show currentNotifications
      case currentNotifications of
        notification : notifications -> do
          let notificationTimeFinished = currentTime > notification.expiryTime
          if notificationTimeFinished
            then do
              processExpiration handle rideId $ fmap (.driverId) $ notification :| notifications
              proceedToNewDrivers handle rideId shortOrgId
            else checkRideLater handle shortOrgId rideId
        [] ->
          proceedToNewDrivers handle rideId shortOrgId

processRejection ::
  MonadHandler m =>
  ServiceHandle m ->
  Id Ride ->
  Id Driver ->
  m ()
processRejection ServiceHandle {..} rideId driverId = do
  logInfo "Processing rejection"
  resetLastRejectionTimes $ singleton driverId
  updateNotificationStatuses rideId Rejected $ singleton driverId
  logDriverEvents MarkedAsRejected rideId $ singleton driverId

processExpiration :: MonadHandler m => ServiceHandle m -> Id Ride -> NonEmpty (Id Driver) -> m ()
processExpiration ServiceHandle {..} rideId driverIds = do
  logInfo "Processing expiration"
  resetLastRejectionTimes driverIds
  updateNotificationStatuses rideId Ignored driverIds
  logDriverEvents MarkedAsIgnored rideId driverIds

proceedToNewDrivers ::
  MonadHandler m =>
  ServiceHandle m ->
  Id Ride ->
  ShortId Organization ->
  m ()
proceedToNewDrivers handle@ServiceHandle {..} rideId shortOrgId = do
  logInfo "Proceed to new drivers"
  driverPool <- getDriverPool rideId
  logInfo $ "DriverPool " <> T.intercalate ", " (getId <$> driverPool)
  attemptedDrivers <- getAttemptedDrivers rideId
  let newDrivers = filter (`notElem` attemptedDrivers) driverPool
  case newDrivers of
    driverId : driverIds -> do
      availableDrivers <- checkAvailability $ driverId :| driverIds
      driversWithNotification <- getDriversWithNotification
      let filteredPool = filter (`notElem` driversWithNotification) availableDrivers
      logInfo $ "Filtered_DriverPool " <> T.intercalate ", " (getId <$> filteredPool)
      processFilteredPool handle rideId filteredPool shortOrgId
    [] -> do
      cancel handle rideId ByAllocator $ Just NoDriversInRange
      logEvent EmptyDriverPool rideId

processFilteredPool ::
  MonadHandler m =>
  ServiceHandle m ->
  Id Ride ->
  [Id Driver] ->
  ShortId Organization ->
  m ()
processFilteredPool handle@ServiceHandle {..} rideId filteredPool shortOrgId = do
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
      sendNewRideNotifications rideId drivers
      addNotificationStatuses rideId drivers expiryTime
      logInfo $ "Notified drivers " <> show (map getId topDrivers)
      logDriverEvents NotificationSent rideId drivers
    [] -> do
      logInfo "All new drivers are unavailable or already have notifications. Waiting."
  checkRideLater handle shortOrgId rideId

checkRideLater :: MonadHandler m => ServiceHandle m -> ShortId Organization -> Id Ride -> m ()
checkRideLater ServiceHandle {..} shortOrgId rideId = do
  addAllocationRequest shortOrgId rideId
  logInfo "Check ride later"

cancel :: MonadHandler m => ServiceHandle m -> Id Ride -> CancellationSource -> Maybe AllocatorCancellationReason -> m ()
cancel ServiceHandle {..} rideId cancellationSource mbReasonCode = do
  logInfo "Cancelling ride"
  cancelRide rideId rideCancellationReason
  cleanupNotifications rideId
  where
    rideCancellationReason =
      SRCR.RideCancellationReason
        { rideId = cast rideId,
          source = cancellationSource,
          reasonCode = SCR.CancellationReasonCode . encodeToText <$> mbReasonCode,
          additionalInfo = Nothing
        }

isAllocationTimeFinished :: MonadHandler m => ServiceHandle m -> UTCTime -> OrderTime -> m Bool
isAllocationTimeFinished ServiceHandle {..} currentTime orderTime = do
  configuredAllocationTime <- fromIntegral <$> getConfiguredAllocationTime
  let elapsedSearchTime = diffUTCTime currentTime (orderTime.utcTime)
  pure $ elapsedSearchTime > configuredAllocationTime

logDriverNoLongerNotified :: Log m => Id Ride -> Id Driver -> m ()
logDriverNoLongerNotified rideId driverId =
  logInfo $
    "Driver "
      <> show driverId
      <> " is no longer notified about ride "
      <> show rideId
      <> ", response request skipped"
