{-# LANGUAGE OverloadedLabels #-}

module Services.Allocation.Allocation where

import Data.Generics.Labels ()
import qualified Data.Text as T
import Data.Time.Clock (NominalDiffTime, UTCTime, addUTCTime, diffUTCTime)
import EulerHS.Prelude
import qualified Types.API.Ride as DriverResponse (DriverResponse (..), NotificationStatus (..))
import Types.App
import Types.Storage.AllocationEvent (AllocationEventType (..))

newtype OrderTime = OrderTime
  { utcTime :: UTCTime
  }
  deriving (Generic, Show)

data RequestData
  = Allocation
  | Cancellation
  deriving (Show)

data RequestHeader = RequestHeader
  { requestId :: RideRequestId,
    rideId :: RideId,
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
  = CurrentNotification DriverId UTCTime
  deriving (Show)

data RideStatus
  = Confirmed
  | Assigned
  | InProgress
  | Completed
  | Cancelled
  deriving (Show, Eq, Ord, Read, Generic, ToJSON, FromJSON)

data RideInfo = RideInfo
  { rideId :: RideId,
    rideStatus :: RideStatus,
    orderTime :: OrderTime
  }
  deriving (Generic)

data ServiceHandle m = ServiceHandle
  { getCurrentTime :: m UTCTime,
    getDriverSortMode :: m SortMode,
    getConfiguredNotificationTime :: m NominalDiffTime,
    getConfiguredAllocationTime :: m NominalDiffTime,
    getRequests :: Integer -> m [RideRequest],
    getDriverPool :: RideId -> m [DriverId],
    getCurrentNotification :: RideId -> m (Maybe CurrentNotification),
    sendNewRideNotification :: RideId -> DriverId -> m (),
    sendRideNotAssignedNotification :: RideId -> DriverId -> m (),
    addNotificationStatus :: RideId -> DriverId -> UTCTime -> m (),
    updateNotificationStatus :: RideId -> DriverId -> NotificationStatus -> m (),
    resetLastRejectionTime :: DriverId -> m (),
    getAttemptedDrivers :: RideId -> m [DriverId],
    getDriversWithNotification :: m [DriverId],
    getFirstDriverInTheQueue :: NonEmpty DriverId -> m DriverId,
    checkAvailability :: NonEmpty DriverId -> m [DriverId],
    getDriverResponse :: RideId -> DriverId -> m (Maybe DriverResponse.DriverResponse),
    assignDriver :: RideId -> DriverId -> m (),
    cancelRide :: RideId -> m (),
    cleanupNotifications :: RideId -> m (),
    addAllocationRequest :: RideId -> m (),
    getRideInfo :: RideId -> m RideInfo,
    removeRequest :: RideRequestId -> m (),
    runSafely :: forall a. (FromJSON a, ToJSON a) => m a -> m (Either Text a),
    logInfo :: Text -> Text -> m (),
    logWarning :: Text -> Text -> m (),
    logError :: Text -> Text -> m (),
    logEvent :: AllocationEventType -> RideId -> m ()
  }

process :: Monad m => ServiceHandle m -> Integer -> m Int
process handle@ServiceHandle {..} requestsNum = do
  getRequestsStartTime <- getCurrentTime
  rides <- getRequests requestsNum
  getRequestsEndTime <- getCurrentTime
  let getRequestsTime = diffUTCTime getRequestsEndTime getRequestsStartTime
  logInfo "AllocationService" $ show getRequestsTime <> " time spent for getRequests"

  reqsHandlingStartTime <- getCurrentTime
  let ridesNum = length rides
  traverse_ (processRequest handle) rides
  reqsHandlingEndTime <- getCurrentTime
  let reqsHandlingTime = diffUTCTime reqsHandlingEndTime reqsHandlingStartTime
  logInfo "AllocationService" $
    "Handled " <> show ridesNum <> " ride requests for " <> show reqsHandlingTime
  pure ridesNum

processRequest :: Monad m => ServiceHandle m -> RideRequest -> m ()
processRequest handle@ServiceHandle {..} rideRequest = do
  let requestHeader = rideRequest ^. #requestHeader
  let requestId = requestHeader ^. #requestId
  rideInfo <- getRideInfo $ requestHeader ^. #rideId
  let rideStatus = rideInfo ^. #rideStatus
  let orderTime = rideInfo ^. #orderTime
  eres <- runSafely $ do
    logRequestInfo handle requestHeader "Start processing request"
    case rideRequest ^. #requestData of
      Allocation ->
        case rideStatus of
          Confirmed -> processAllocation handle requestHeader orderTime
          Cancelled -> logRequestInfo handle requestHeader "Ride is cancelled, allocation request skipped"
          _ ->
            logRequestWarning handle requestHeader $
              "Ride status is " <> show rideStatus <> ", allocation request skipped"
      Cancellation ->
        case rideStatus of
          Confirmed -> do
            cancel handle requestHeader
            logEvent ConsumerCancelled (requestHeader ^. #rideId)
          _ ->
            logRequestWarning handle requestHeader $
              "Ride status is " <> show rideStatus <> ", cancellation request skipped"

    logRequestInfo handle requestHeader "End processing request"
  whenLeft eres $
    \err -> do
      let message = "Error processing request " <> show requestId <> ": " <> err
      logError "AllocationService" message

  removeRequest $ requestHeader ^. #requestId

processAllocation :: Monad m => ServiceHandle m -> RequestHeader -> OrderTime -> m ()
processAllocation handle@ServiceHandle {..} requestHeader orderTime = do
  allocationTimeFinished <- isAllocationTimeFinished handle orderTime
  logRequestInfo handle requestHeader $ "isAllocationTimeFinished " <> show allocationTimeFinished
  if allocationTimeFinished
    then do
      cancel handle requestHeader
      logEvent AllocationTimeFinished (requestHeader ^. #rideId)
    else do
      mCurrentNotification <- getCurrentNotification (requestHeader ^. #rideId)
      logRequestInfo handle requestHeader ("getCurrentNotification " <> show mCurrentNotification)
      case mCurrentNotification of
        Just currentNotification ->
          processCurrentNotification handle requestHeader currentNotification
        Nothing ->
          proceedToNextDriver handle requestHeader

processCurrentNotification ::
  Monad m =>
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
    logRequestInfo handle requestHeader $ "isNotificationTimeFinished " <> show notificationTimeFinished
    if notificationTimeFinished
      then do
        logRequestInfo handle requestHeader $ "Notified ride not assigned to driver " <> _getDriverId driverId
        sendRideNotAssignedNotification rideId driverId
        processRejection handle True requestHeader driverId
      else do
        mResponse <- getDriverResponse rideId driverId
        logRequestInfo handle requestHeader ("getDriverResponse " <> show mResponse)
        case mResponse of
          Just driverResponse ->
            case driverResponse ^. #_status of
              DriverResponse.ACCEPT -> do
                logRequestInfo handle requestHeader ("assigning driver" <> show driverId)
                assignDriver rideId driverId
                cleanupNotifications $ requestHeader ^. #rideId
                logEvent AcceptedByDriver (requestHeader ^. #rideId)
              DriverResponse.REJECT ->
                processRejection handle False requestHeader driverId
          Nothing ->
            checkRideLater handle requestHeader

processRejection :: Monad m => ServiceHandle m -> Bool -> RequestHeader -> DriverId -> m ()
processRejection handle@ServiceHandle {..} ignored requestHeader driverId = do
  logRequestInfo handle requestHeader "processing rejection"
  let rideId = requestHeader ^. #rideId
  let status = if ignored then Ignored else Rejected
  let event = if ignored then IgnoredByDriver else RejectedByDriver
  resetLastRejectionTime driverId
  updateNotificationStatus rideId driverId status
  logEvent event (requestHeader ^. #rideId)
  proceedToNextDriver handle requestHeader

proceedToNextDriver :: Monad m => ServiceHandle m -> RequestHeader -> m ()
proceedToNextDriver handle@ServiceHandle {..} requestHeader = do
  logRequestInfo handle requestHeader "proceed to next driver"
  let rideId = requestHeader ^. #rideId
  driverPool <- getDriverPool rideId
  logRequestInfo handle requestHeader ("DriverPool " <> T.intercalate ", " (_getDriverId <$> driverPool))

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

processFilteredPool :: Monad m => ServiceHandle m -> RequestHeader -> [DriverId] -> m ()
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
      logRequestInfo handle requestHeader $ "Notified driver " <> _getDriverId firstDriver
      logEvent NotificationSent (requestHeader ^. #rideId)
      checkRideLater handle requestHeader
    [] -> do
      cancel handle requestHeader
      logEvent EmptyDriverPool (requestHeader ^. #rideId)

checkRideLater :: Monad m => ServiceHandle m -> RequestHeader -> m ()
checkRideLater handle@ServiceHandle {..} requestHeader = do
  addAllocationRequest (requestHeader ^. #rideId)
  logRequestInfo handle requestHeader "Check ride later"

cancel :: Monad m => ServiceHandle m -> RequestHeader -> m ()
cancel handle@ServiceHandle {..} requestHeader = do
  logRequestInfo handle requestHeader "Cancelling ride"
  cancelRide $ requestHeader ^. #rideId
  cleanupNotifications $ requestHeader ^. #rideId

isAllocationTimeFinished :: Monad m => ServiceHandle m -> OrderTime -> m Bool
isAllocationTimeFinished ServiceHandle {..} orderTime = do
  currentTime <- getCurrentTime
  configuredAllocationTime <- getConfiguredAllocationTime
  let elapsedSearchTime = diffUTCTime currentTime (orderTime ^. #utcTime)
  pure $ elapsedSearchTime > configuredAllocationTime

isNotificationTimeFinished :: Monad m => ServiceHandle m -> UTCTime -> m Bool
isNotificationTimeFinished ServiceHandle {..} expiryTime = do
  currentTime <- getCurrentTime
  pure $ currentTime > expiryTime

logRequestInfo :: Monad m => ServiceHandle m -> RequestHeader -> Text -> m ()
logRequestInfo ServiceHandle {..} header = logInfo $ "RideRequest_" <> _getRideId (header ^. #rideId)

logRequestWarning :: Monad m => ServiceHandle m -> RequestHeader -> Text -> m ()
logRequestWarning ServiceHandle {..} header = logWarning $ "RideRequest_" <> _getRideId (header ^. #rideId)

logRequestError :: Monad m => ServiceHandle m -> RequestHeader -> Text -> m ()
logRequestError ServiceHandle {..} header = logError $ "RideRequest_" <> _getRideId (header ^. #rideId)
