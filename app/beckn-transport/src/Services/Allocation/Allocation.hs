{-# LANGUAGE OverloadedLabels #-}

module Services.Allocation.Allocation where

import Beckn.Types.Common
import Beckn.Types.Id
import Data.Generics.Labels ()
import qualified Data.Text as T
import Data.Time.Clock (NominalDiffTime, UTCTime, addUTCTime, diffUTCTime)
import EulerHS.Prelude
import qualified Types.API.Ride as DriverResponse (DriverResponse (..), NotificationStatus (..))
import Types.App
import Types.Storage.AllocationEvent (AllocationEventType (..))
import qualified Types.Storage.RideRequest as SRR

newtype OrderTime = OrderTime
  { utcTime :: UTCTime
  }
  deriving (Generic, Show)

data RequestData
  = Allocation
  | Cancellation
  deriving (Show)

data RequestHeader = RequestHeader
  { requestId :: Id SRR.RideRequest,
    rideId :: Id Ride,
    requestTime :: UTCTime
  }
  deriving (Generic, Show)

data RideRequest = RideRequest
  { requestHeader :: RequestHeader,
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

type MonadHandler m = (MonadCatch m, MonadTime m, Log m)

data ServiceHandle m = ServiceHandle
  { getDriverSortMode :: m SortMode,
    getConfiguredNotificationTime :: m NominalDiffTime,
    getConfiguredAllocationTime :: m NominalDiffTime,
    getRequests :: Integer -> m [RideRequest],
    getDriverPool :: Id Ride -> m [Id Driver],
    getCurrentNotification :: Id Ride -> m (Maybe CurrentNotification),
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
    addAllocationRequest :: Id Ride -> m (),
    getRideInfo :: Id Ride -> m RideInfo,
    removeRequest :: Id SRR.RideRequest -> m (),
    logEvent :: AllocationEventType -> Id Ride -> m ()
  }

process :: MonadHandler m => ServiceHandle m -> Integer -> m Int
process handle@ServiceHandle {..} requestsNum = do
  getRequestsStartTime <- getCurrentTime
  rides <- getRequests requestsNum
  let ridesNum = length rides
  unless (ridesNum == 0) $
    withLogContext "Allocation service" $ do
      getRequestsEndTime <- getCurrentTime
      let getRequestsTime = diffUTCTime getRequestsEndTime getRequestsStartTime
      logInfo $ show getRequestsTime <> " time spent for getRequests"

      reqsHandlingStartTime <- getCurrentTime
      traverse_ (processRequest handle) rides
      reqsHandlingEndTime <- getCurrentTime
      let reqsHandlingTime = diffUTCTime reqsHandlingEndTime reqsHandlingStartTime
      logInfo $
        "Handled " <> show ridesNum <> " ride requests for " <> show reqsHandlingTime
  pure ridesNum

processRequest :: MonadHandler m => ServiceHandle m -> RideRequest -> m ()
processRequest handle@ServiceHandle {..} rideRequest = do
  let requestHeader = rideRequest ^. #requestHeader
  let requestId = requestHeader ^. #requestId
  let rideId = requestHeader ^. #rideId
  rideInfo <- getRideInfo rideId
  let rideStatus = rideInfo ^. #rideStatus
  let orderTime = rideInfo ^. #orderTime
  eres <- try $ do
    withLogContext ("RideRequest_" <> rideId ^. #getId) $ do
      logInfo "Start processing request"
      case rideRequest ^. #requestData of
        Allocation ->
          case rideStatus of
            Confirmed -> processAllocation handle requestHeader orderTime
            Cancelled -> logInfo "Ride is cancelled, allocation request skipped"
            _ ->
              logWarning $ "Ride status is " <> show rideStatus <> ", allocation request skipped"
        Cancellation ->
          case rideStatus of
            status | status == Confirmed || status == Assigned -> do
              cancel handle requestHeader
              logEvent ConsumerCancelled (requestHeader ^. #rideId)
            _ ->
              logWarning $ "Ride status is " <> show rideStatus <> ", cancellation request skipped"

      logInfo "End processing request"
  whenLeft eres $
    \(err :: SomeException) -> do
      let message = "Error processing request " <> show requestId <> ": " <> show err
      logError message

  removeRequest $ requestHeader ^. #requestId

processAllocation :: MonadHandler m => ServiceHandle m -> RequestHeader -> OrderTime -> m ()
processAllocation handle@ServiceHandle {..} requestHeader orderTime = do
  allocationTimeFinished <- isAllocationTimeFinished handle orderTime
  logInfo $ "isAllocationTimeFinished " <> show allocationTimeFinished
  if allocationTimeFinished
    then do
      cancel handle requestHeader
      logEvent AllocationTimeFinished (requestHeader ^. #rideId)
    else do
      mCurrentNotification <- getCurrentNotification (requestHeader ^. #rideId)
      logInfo $ "getCurrentNotification " <> show mCurrentNotification
      case mCurrentNotification of
        Just currentNotification ->
          processCurrentNotification handle requestHeader currentNotification
        Nothing ->
          proceedToNextDriver handle requestHeader

processCurrentNotification ::
  MonadHandler m =>
  ServiceHandle m ->
  RequestHeader ->
  CurrentNotification ->
  m ()
processCurrentNotification
  handle@ServiceHandle {..}
  requestHeader
  (CurrentNotification driverId expiryTime) = do
    let rideId = requestHeader ^. #rideId
    notificationTimeFinished <- isNotificationTimeFinished handle expiryTime
    logInfo $ "isNotificationTimeFinished " <> show notificationTimeFinished
    if notificationTimeFinished
      then do
        logInfo $ "Notified ride not assigned to driver " <> getId driverId
        sendRideNotAssignedNotification rideId driverId
        processRejection handle True requestHeader driverId
      else do
        mResponse <- getDriverResponse rideId driverId
        logInfo $ "getDriverResponse " <> show mResponse
        case mResponse of
          Just driverResponse ->
            case driverResponse ^. #_status of
              DriverResponse.ACCEPT -> do
                logInfo $ "assigning driver" <> show driverId
                assignDriver rideId driverId
                cleanupNotifications $ requestHeader ^. #rideId
                logEvent AcceptedByDriver (requestHeader ^. #rideId)
              DriverResponse.REJECT ->
                processRejection handle False requestHeader driverId
          Nothing ->
            checkRideLater handle requestHeader

processRejection :: MonadHandler m => ServiceHandle m -> Bool -> RequestHeader -> Id Driver -> m ()
processRejection handle@ServiceHandle {..} ignored requestHeader driverId = do
  logInfo "processing rejection"
  let rideId = requestHeader ^. #rideId
  let status = if ignored then Ignored else Rejected
  let event = if ignored then IgnoredByDriver else RejectedByDriver
  resetLastRejectionTime driverId
  updateNotificationStatus rideId driverId status
  logEvent event (requestHeader ^. #rideId)
  proceedToNextDriver handle requestHeader

proceedToNextDriver :: MonadHandler m => ServiceHandle m -> RequestHeader -> m ()
proceedToNextDriver handle@ServiceHandle {..} requestHeader = do
  logInfo "proceed to next driver"
  let rideId = requestHeader ^. #rideId
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
      processFilteredPool handle requestHeader filteredPool
    [] -> cancel handle requestHeader

processFilteredPool :: MonadHandler m => ServiceHandle m -> RequestHeader -> [Id Driver] -> m ()
processFilteredPool handle@ServiceHandle {..} requestHeader driverPool = do
  let rideId = requestHeader ^. #rideId
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
      logEvent NotificationSent (requestHeader ^. #rideId)
      checkRideLater handle requestHeader
    [] -> do
      cancel handle requestHeader
      logEvent EmptyDriverPool (requestHeader ^. #rideId)

checkRideLater :: MonadHandler m => ServiceHandle m -> RequestHeader -> m ()
checkRideLater ServiceHandle {..} requestHeader = do
  addAllocationRequest (requestHeader ^. #rideId)
  logInfo "Check ride later"

cancel :: MonadHandler m => ServiceHandle m -> RequestHeader -> m ()
cancel ServiceHandle {..} requestHeader = do
  logInfo "Cancelling ride"
  cancelRide $ requestHeader ^. #rideId
  cleanupNotifications $ requestHeader ^. #rideId

isAllocationTimeFinished :: MonadHandler m => ServiceHandle m -> OrderTime -> m Bool
isAllocationTimeFinished ServiceHandle {..} orderTime = do
  currentTime <- getCurrentTime
  configuredAllocationTime <- getConfiguredAllocationTime
  let elapsedSearchTime = diffUTCTime currentTime (orderTime ^. #utcTime)
  pure $ elapsedSearchTime > configuredAllocationTime

isNotificationTimeFinished :: MonadHandler m => ServiceHandle m -> UTCTime -> m Bool
isNotificationTimeFinished ServiceHandle {..} expiryTime = do
  currentTime <- getCurrentTime
  pure $ currentTime > expiryTime

logInfo :: MonadHandler m => Text -> m ()
logInfo = logOutput INFO

logWarning :: MonadHandler m => Text -> m ()
logWarning = logOutput WARNING

logError :: MonadHandler m => Text -> m ()
logError = logOutput ERROR
