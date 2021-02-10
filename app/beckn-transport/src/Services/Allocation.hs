{-# LANGUAGE OverloadedLabels #-}

module Services.Allocation where

import Data.Generics.Labels ()
import Data.Time.Clock (NominalDiffTime, UTCTime, diffUTCTime)
import EulerHS.Prelude
import Types.App

newtype RequestId = RequestId
  { getRequestId :: Text
  }
  deriving (Show)

newtype OrderTime = OrderTime
  { utcTime :: UTCTime
  }
  deriving (Generic, Show)

data RequestData
  = Allocation OrderTime
  | Cancellation
  deriving (Show)

data RequestHeader = RequestHeader
  { requestId :: RequestId,
    rideId :: RideId,
    requestTime :: UTCTime
  }
  deriving (Generic, Show)

data RideRequest = RideRequest
  { requestHeader :: RequestHeader,
    requestData :: RequestData
  }
  deriving (Generic, Show)

data DriverResponse
  = Accept
  | Reject
  deriving (Show)

data NotificationStatus
  = Notified UTCTime
  | Rejected
  | Ignored
  | Accepted
  deriving (Eq, Show)

data CurrentNotification
  = CurrentNotification DriverId UTCTime

requestsPerIteration :: Integer
requestsPerIteration = 50

data ServiceHandle m = ServiceHandle
  { -- Get current time
    getCurrentTime :: m UTCTime,
    -- Get driver pool sort mode
    -- Based on the config setting
    getDriverSortMode :: m SortMode,
    -- Get the time the driver has to accept a ride
    -- Based on the config setting
    getConfiguredNotificationTime :: m NominalDiffTime,
    -- Get the time allowed for allocation of the driver
    -- Based on the config setting
    getConfiguredAllocationTime :: m NominalDiffTime,
    -- Get N oldest requests
    -- Can be done as a select query from the RideRequest table.
    getRequests :: Integer -> m [RideRequest],
    -- Get driver pool for this ride from Redis
    getDriverPool :: RideId -> m [DriverId],
    -- Get the driver that is currently being notified about this ride,
    -- and the time when the notification was sent.
    -- Can be done as a select from the NotificationStatus table.
    getCurrentNotification :: RideId -> m (Maybe CurrentNotification),
    -- Send notification to a driver about a ride
    sendNotification :: RideId -> DriverId -> m (),
    -- Add notification status
    -- Can be done as an insert to the NotificationStatus table.
    addNotificationStatus :: RideId -> DriverId -> NotificationStatus -> m (),
    -- Update notification status
    -- Can be done as an update of the NotificationStatus table.
    updateNotificationStatus :: RideId -> DriverId -> NotificationStatus -> m (),
    -- Reset last rejection time
    -- Can be done as update of the DriverStats table
    resetLastRejectionTime :: DriverId -> m (),
    -- Get drivers that were notified or attempted to be notified about the ride.
    -- This includes drivers that rejected or ignored the notification.
    -- Can be done as a select from the NotificationStatus table.
    getAttemptedDrivers :: RideId -> m [DriverId],
    -- Get drivers that are currently being notified for any ride.
    getDriversWithNotification :: m [DriverId],
    -- Get the driver that has been idle the most.
    -- Can be done as a select from the DriverStats table
    getFirstDriverInTheQueue :: NonEmpty DriverId -> m DriverId,
    -- Check which drivers of the ones provided are available (online and don't have a ride assigned)
    -- Can be done as a select query from the Driver table
    checkAvailability :: NonEmpty DriverId -> m [DriverId],
    -- Get the response if it was registered for this ride by this driver
    -- Can be done as a Redis lookup
    getDriverResponse :: RideId -> DriverId -> m (Maybe DriverResponse),
    -- Assign a ride to a driver
    assignDriver :: RideId -> DriverId -> m (),
    -- Cancel the ride
    cancelRide :: RideId -> m (),
    -- Reset request time to match the current time
    -- Can be done as update of the RideRequest table
    resetRequestTime :: RequestId -> m (),
    -- Set the status of the request in the RideRequest table to completed.
    completeRequest :: RequestId -> m ()
  }

process :: Monad m => ServiceHandle m -> m Int
process handle@ServiceHandle {..} = do
  rides <- getRequests requestsPerIteration
  traverse_ (processRequest handle) rides
  pure $ length rides

processRequest :: Monad m => ServiceHandle m -> RideRequest -> m ()
processRequest handle@ServiceHandle {..} rideRequest = do
  case rideRequest ^. #requestData of
    Allocation orderTime ->
      processAllocation handle (rideRequest ^. #requestHeader) orderTime
    Cancellation ->
      cancel handle $ rideRequest ^. #requestHeader

processAllocation :: Monad m => ServiceHandle m -> RequestHeader -> OrderTime -> m ()
processAllocation handle@ServiceHandle {..} requestHeader orderTime = do
  allocationTimeFinished <- isAllocationTimeFinished handle orderTime
  if allocationTimeFinished
    then cancel handle requestHeader
    else do
      mCurrentNotification <- getCurrentNotification (requestHeader ^. #rideId)
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
  (CurrentNotification driverId notificationTime) = do
    let rideId = requestHeader ^. #rideId
    notificationTimeFinished <- isNotificationTimeFinished handle notificationTime
    if notificationTimeFinished
      then processRejection handle True requestHeader driverId
      else do
        mResponse <- getDriverResponse rideId driverId
        case mResponse of
          Just Accept -> do
            assignDriver rideId driverId
            updateNotificationStatus rideId driverId Accepted
            completeRequest $ requestHeader ^. #requestId
          Just Reject ->
            processRejection handle False requestHeader driverId
          Nothing ->
            processRideLater handle requestHeader

processRejection :: Monad m => ServiceHandle m -> Bool -> RequestHeader -> DriverId -> m ()
processRejection handle@ServiceHandle {..} ignored requestHeader driverId = do
  let rideId = requestHeader ^. #rideId
  let status = if ignored then Ignored else Rejected
  resetLastRejectionTime driverId
  updateNotificationStatus rideId driverId status
  proceedToNextDriver handle requestHeader

proceedToNextDriver :: Monad m => ServiceHandle m -> RequestHeader -> m ()
proceedToNextDriver handle@ServiceHandle {..} requestHeader = do
  let rideId = requestHeader ^. #rideId
  driverPool <- getDriverPool rideId

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
      time <- getCurrentTime
      sendNotification rideId firstDriver
      addNotificationStatus rideId firstDriver $ Notified time
      processRideLater handle requestHeader
    [] -> cancel handle requestHeader

cancel :: Monad m => ServiceHandle m -> RequestHeader -> m ()
cancel ServiceHandle {..} requestHeader = do
  cancelRide $ requestHeader ^. #rideId
  completeRequest $ requestHeader ^. #requestId

processRideLater :: Monad m => ServiceHandle m -> RequestHeader -> m ()
processRideLater ServiceHandle {..} requestHeader =
  resetRequestTime $ requestHeader ^. #requestId

isAllocationTimeFinished :: Monad m => ServiceHandle m -> OrderTime -> m Bool
isAllocationTimeFinished ServiceHandle {..} orderTime = do
  currentTime <- getCurrentTime
  configuredAllocationTime <- getConfiguredAllocationTime
  let elapsedSearchTime = diffUTCTime currentTime (orderTime ^. #utcTime)
  pure $ elapsedSearchTime > configuredAllocationTime

isNotificationTimeFinished :: Monad m => ServiceHandle m -> UTCTime -> m Bool
isNotificationTimeFinished ServiceHandle {..} notificationTime = do
  currentTime <- getCurrentTime
  configuredNotificationTime <- getConfiguredNotificationTime
  let elapsedNotificationTime = diffUTCTime currentTime notificationTime
  pure $ elapsedNotificationTime > configuredNotificationTime
