module Services.Allocation.Allocation where

import Beckn.Types.Common
import Beckn.Types.Id
import Beckn.Types.Storage.Organization
import Data.Generics.Labels ()
import qualified Data.Text as T
import Data.Time.Clock (NominalDiffTime, UTCTime, addUTCTime, diffUTCTime)
import EulerHS.Prelude
import qualified Types.API.Ride as DriverResponse (DriverResponse (..), NotificationStatus (..))
import Types.App
import Types.Storage.AllocationEvent (AllocationEventType (..))
import qualified Types.Storage.RideRequest as SRR
import Utils.Common

newtype OrderTime = OrderTime
  { utcTime :: UTCTime
  }
  deriving (Generic, Show)

data RequestData
  = Allocation
  | Cancellation
  deriving (Show)

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

data MetricsHandle m = MetricsHandle
  { incrementTaskCounter :: m (),
    incrementFailedTaskCounter :: m (),
    putTaskDuration :: Double -> m ()
  }

type MonadHandler m = (MonadCatch m, MonadTime m, Log m)

data ServiceHandle m = ServiceHandle
  { getDriverSortMode :: m SortMode,
    getConfiguredNotificationTime :: m NominalDiffTime,
    getConfiguredAllocationTime :: m NominalDiffTime,
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
    getDriverResponse :: Id Ride -> Id Driver -> m (Maybe DriverResponse.DriverResponse),
    assignDriver :: Id Ride -> Id Driver -> m (),
    cancelRide :: Id Ride -> m (),
    cleanupNotifications :: Id Ride -> m (),
    addAllocationRequest :: ShortId Organization -> Id Ride -> m (),
    getRideInfo :: Id Ride -> m RideInfo,
    removeRequest :: Id SRR.RideRequest -> m (),
    logEvent :: AllocationEventType -> Id Ride -> Maybe (Id Driver) -> m (),
    metricsHandle :: MetricsHandle m
  }

process :: MonadHandler m => ServiceHandle m -> ShortId Organization -> Integer -> m Int
process handle@ServiceHandle {..} shortOrgId requestsNum = do
  getRequestsStartTime <- getCurrentTime
  cleanedNotificationsCount <- cleanupOldNotifications
  when (cleanedNotificationsCount > 0) $ logInfo $ "Cleaned notifications count: " <> show cleanedNotificationsCount
  rides <- getRequests shortOrgId requestsNum
  let ridesNum = length rides
  unless (ridesNum == 0) $
    withLogTag "Allocation service" $ do
      getRequestsEndTime <- getCurrentTime
      let getRequestsTime = diffUTCTime getRequestsEndTime getRequestsStartTime
      logInfo $ show getRequestsTime <> " time spent for getRequests"

      reqsHandlingStartTime <- getCurrentTime
      traverse_ (processRequest handle shortOrgId) rides
      reqsHandlingEndTime <- getCurrentTime
      let reqsHandlingTime = diffUTCTime reqsHandlingEndTime reqsHandlingStartTime
      logInfo $
        "Handled " <> show ridesNum <> " ride requests for " <> show reqsHandlingTime
  pure ridesNum

processRequest :: MonadHandler m => ServiceHandle m -> ShortId Organization -> RideRequest -> m ()
processRequest handle@ServiceHandle {..} shortOrgId rideRequest = do
  incrementTaskCounter metricsHandle
  processStartTime <- getCurrentTime
  let requestId = rideRequest.requestId
  let rideId = rideRequest.rideId
  rideInfo <- getRideInfo rideId
  let rideStatus = rideInfo.rideStatus
  eres <- try $ do
    withLogTag ("RideRequest_" <> rideId.getId) $ do
      logInfo "Start processing request"
      case rideRequest.requestData of
        Allocation ->
          case rideStatus of
            Confirmed -> processAllocation handle shortOrgId rideInfo
            Cancelled -> logInfo "Ride is cancelled, allocation request skipped"
            _ ->
              logWarning $ "Ride status is " <> show rideStatus <> ", allocation request skipped"
        Cancellation ->
          case rideStatus of
            status | status == Confirmed || status == Assigned -> do
              cancel handle rideId
              logEvent ConsumerCancelled rideId Nothing
            _ ->
              logWarning $ "Ride status is " <> show rideStatus <> ", cancellation request skipped"

      logInfo "End processing request"
  whenLeft eres $
    \(err :: SomeException) -> do
      let message = "Error processing request " <> show requestId <> ": " <> show err
      logError message
      incrementFailedTaskCounter metricsHandle

  removeRequest requestId
  processEndTime <- getCurrentTime
  putTaskDuration metricsHandle . realToFrac $ diffUTCTime processEndTime processStartTime

processAllocation ::
  MonadHandler m =>
  ServiceHandle m ->
  ShortId Organization ->
  RideInfo ->
  m ()
processAllocation handle@ServiceHandle {..} shortOrgId rideInfo = do
  let rideId = rideInfo.rideId
  let orderTime = rideInfo.orderTime
  allocationTimeFinished <- isAllocationTimeFinished handle orderTime
  logInfo $ "isAllocationTimeFinished " <> show allocationTimeFinished
  if allocationTimeFinished
    then do
      cancel handle rideId
      logEvent AllocationTimeFinished rideId Nothing
    else do
      mCurrentNotification <- getCurrentNotification rideId
      logInfo $ "getCurrentNotification " <> show mCurrentNotification
      case mCurrentNotification of
        Just currentNotification ->
          processCurrentNotification handle shortOrgId rideId currentNotification
        Nothing ->
          proceedToNextDriver handle rideId shortOrgId

processCurrentNotification ::
  MonadHandler m =>
  ServiceHandle m ->
  ShortId Organization ->
  Id Ride ->
  CurrentNotification ->
  m ()
processCurrentNotification
  handle@ServiceHandle {..}
  shortOrgId
  rideId
  (CurrentNotification driverId expiryTime) = do
    mResponse <- getDriverResponse rideId driverId
    case mResponse of
      Just driverResponse ->
        if driverResponse.respondedAt <= expiryTime
          then case driverResponse.status of
            DriverResponse.ACCEPT -> do
              logInfo $ "assigning driver" <> show driverId
              assignDriver rideId driverId
              cleanupNotifications rideId
              logEvent AcceptedByDriver rideId $ Just driverId
            DriverResponse.REJECT ->
              processRejection handle False rideId driverId shortOrgId
          else processExpiredNotification handle shortOrgId rideId driverId
      Nothing -> do
        now <- getCurrentTime
        let notificationTimeFinished = now > expiryTime
        if notificationTimeFinished
          then processExpiredNotification handle shortOrgId rideId driverId
          else checkRideLater handle shortOrgId rideId

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
  logInfo "processing rejection"
  let status = if ignored then Ignored else Rejected
  let event = if ignored then IgnoredByDriver else RejectedByDriver
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
  logInfo "proceed to next driver"
  driverPool <- getDriverPool rideId
  logInfo $ "DriverPool " <> T.intercalate ", " (getId <$> driverPool)

  case driverPool of
    driverId : driverIds -> do
      availableDrivers <- checkAvailability $ driverId :| driverIds
      attemptedDrivers <- getAttemptedDrivers rideId
      driversWithNotification <- getDriversWithNotification

      let canNotify driver =
            driver `elem` availableDrivers
              && driver `notElem` attemptedDrivers
              && driver `notElem` driversWithNotification

      let filteredPool = filter canNotify driverPool
      processFilteredPool handle rideId filteredPool shortOrgId
    [] -> cancel handle rideId

processFilteredPool ::
  MonadHandler m =>
  ServiceHandle m ->
  Id Ride ->
  [Id Driver] ->
  ShortId Organization ->
  m ()
processFilteredPool handle@ServiceHandle {..} rideId driverPool shortOrgId = do
  case driverPool of
    driverId : driverIds -> do
      sortMode <- getDriverSortMode
      firstDriver <-
        case sortMode of
          ETA -> pure driverId
          IdleTime -> getFirstDriverInTheQueue $ driverId :| driverIds
      currentTime <- getCurrentTime
      notificationTime <- getConfiguredNotificationTime
      let expiryTime = addUTCTime notificationTime currentTime
      sendNewRideNotification rideId firstDriver
      addNotificationStatus rideId firstDriver expiryTime
      logInfo $ "Notified driver " <> getId firstDriver
      logEvent NotificationSent rideId $ Just firstDriver
      checkRideLater handle shortOrgId rideId
    [] -> do
      cancel handle rideId
      logEvent EmptyDriverPool rideId Nothing

checkRideLater :: MonadHandler m => ServiceHandle m -> ShortId Organization -> Id Ride -> m ()
checkRideLater ServiceHandle {..} shortOrgId rideId = do
  addAllocationRequest shortOrgId rideId
  logInfo "Check ride later"

cancel :: MonadHandler m => ServiceHandle m -> Id Ride -> m ()
cancel ServiceHandle {..} rideId = do
  logInfo "Cancelling ride"
  cancelRide rideId
  cleanupNotifications rideId

isAllocationTimeFinished :: MonadHandler m => ServiceHandle m -> OrderTime -> m Bool
isAllocationTimeFinished ServiceHandle {..} orderTime = do
  currentTime <- getCurrentTime
  configuredAllocationTime <- getConfiguredAllocationTime
  let elapsedSearchTime = diffUTCTime currentTime (orderTime.utcTime)
  pure $ elapsedSearchTime > configuredAllocationTime
