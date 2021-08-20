module Services.Allocation.Allocation where

import Beckn.Types.Common
import Beckn.Types.Id
import Beckn.Types.Mobility.Order (CancellationSource (..))
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

data CurrentNotification
  = CurrentNotification (Id Driver) UTCTime
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
    getRequests :: ShortId Organization -> Integer -> m [RideRequest],
    getDriverPool :: Id Ride -> m [Id Driver],
    getCurrentNotification :: Id Ride -> m (Maybe CurrentNotification),
    cleanupOldNotifications :: m Int,
    sendNewRideNotification :: Id Ride -> Id Driver -> m (),
    sendRideNotAssignedNotification :: Id Ride -> Id Driver -> m (),
    addNotificationStatus :: Id Ride -> Id Driver -> UTCTime -> m (),
    updateNotificationStatus :: Id Ride -> Id Driver -> NotificationStatus -> m (),
    resetLastRejectionTime :: Id Driver -> m (),
    getAttemptedDrivers :: Id Ride -> m [Id Driver],
    getDriversWithNotification :: m [Id Driver],
    getFirstDriverInTheQueue :: NonEmpty (Id Driver) -> m (Id Driver),
    checkAvailability :: NonEmpty (Id Driver) -> m [Id Driver],
    assignDriver :: Id Ride -> Id Driver -> m (),
    cancelRide :: Id Ride -> SRCR.RideCancellationReason -> m (),
    cleanupNotifications :: Id Ride -> m (),
    addAllocationRequest :: ShortId Organization -> Id Ride -> m (),
    getRideInfo :: Id Ride -> m RideInfo,
    removeRequest :: Id SRR.RideRequest -> m (),
    logEvent :: AllocationEventType -> Id Ride -> Maybe (Id Driver) -> m (),
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
                mCurrentNotification <- getCurrentNotification rideId
                logInfo $ "getCurrentNotification " <> show mCurrentNotification
                case mCurrentNotification of
                  Just (CurrentNotification driverId _) -> do
                    if driverId == response.driverId
                      then case response.status of
                        Ride.ACCEPT -> do
                          logInfo $ "Assigning driver" <> show response.driverId
                          assignDriver rideId response.driverId
                          cleanupNotifications rideId
                          logEvent MarkedAsAccepted rideId $ Just response.driverId
                        Ride.REJECT ->
                          processRejection handle False rideId response.driverId shortOrgId
                      else logDriverNoLongerNotified rideId response.driverId
                  Nothing ->
                    logDriverNoLongerNotified rideId response.driverId
              Assigned ->
                logInfo "Ride is assigned, response request skipped"
              Cancelled ->
                logInfo "Ride is cancelled, response request skipped"
              _ -> logWarning $ "Ride status is " <> show rideStatus <> ", response request skipped"
          Cancellation ->
            case rideStatus of
              status | status == Confirmed || status == Assigned -> do
                cancel handle rideId ByUser Nothing
                logEvent ConsumerCancelled rideId Nothing
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
      logEvent AllocationTimeFinished rideId Nothing
    else do
      mCurrentNotification <- getCurrentNotification rideId
      logInfo $ "getCurrentNotification " <> show mCurrentNotification
      case mCurrentNotification of
        Just (CurrentNotification driverId expiryTime) -> do
          let notificationTimeFinished = currentTime > expiryTime
          if notificationTimeFinished
            then processExpiredNotification handle shortOrgId rideId driverId
            else checkRideLater handle shortOrgId rideId
        Nothing ->
          proceedToNextDriver handle rideId shortOrgId

processExpiredNotification ::
  MonadHandler m =>
  ServiceHandle m ->
  ShortId Organization ->
  Id Ride ->
  Id Driver ->
  m ()
processExpiredNotification handle@ServiceHandle {..} shortOrgId rideId driverId = do
  logInfo $ "Notified ride not assigned to driver " <> getId driverId
  sendRideNotAssignedNotification rideId driverId
  processRejection handle True rideId driverId shortOrgId

processRejection ::
  MonadHandler m =>
  ServiceHandle m ->
  Bool ->
  Id Ride ->
  Id Driver ->
  ShortId Organization ->
  m ()
processRejection handle@ServiceHandle {..} ignored rideId driverId shortOrgId = do
  logInfo "Processing rejection"
  let status = if ignored then Ignored else Rejected
  let event = if ignored then MarkedAsIgnored else MarkedAsRejected
  resetLastRejectionTime driverId
  updateNotificationStatus rideId driverId status
  logEvent event rideId $ Just driverId
  proceedToNextDriver handle rideId shortOrgId

proceedToNextDriver ::
  MonadHandler m =>
  ServiceHandle m ->
  Id Ride ->
  ShortId Organization ->
  m ()
proceedToNextDriver handle@ServiceHandle {..} rideId shortOrgId = do
  logInfo "Proceed to next driver"
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
      logEvent EmptyDriverPool rideId Nothing

processFilteredPool ::
  MonadHandler m =>
  ServiceHandle m ->
  Id Ride ->
  [Id Driver] ->
  ShortId Organization ->
  m ()
processFilteredPool handle@ServiceHandle {..} rideId filteredPool shortOrgId = do
  case filteredPool of
    driverId : driverIds -> do
      sortMode <- getDriverSortMode
      firstDriver <-
        case sortMode of
          ETA -> pure driverId
          IdleTime -> getFirstDriverInTheQueue $ driverId :| driverIds
      currentTime <- getCurrentTime
      notificationTime <- fromIntegral <$> getConfiguredNotificationTime
      let expiryTime = addUTCTime notificationTime currentTime
      sendNewRideNotification rideId firstDriver
      addNotificationStatus rideId firstDriver expiryTime
      logInfo $ "Notified driver " <> getId firstDriver
      logEvent NotificationSent rideId $ Just firstDriver
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
