{-# LANGUAGE OverloadedLabels #-}

module Services.Allocation where

import Data.Generics.Labels ()
import Data.Time.Clock (NominalDiffTime, UTCTime, diffUTCTime)
import EulerHS.Prelude
import Types.App

-- Current implementation assumes for simplicity that Redis does not go down.
-- This can be easily extended later by recreating the data.

data Ride = Ride
  { id :: RideId,
    orderedAt :: UTCTime
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
  | NotAvailable
  | Accepted
  deriving (Eq, Show)

data CurrentNotification
  = CurrentNotification DriverId UTCTime

ridesPerIteration :: Integer
ridesPerIteration = 50

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
    -- Get top N rides that need allocation, sorted by order confirmation time
    -- Can be done as a select query from the AllocationRequest table.
    getTopRidesToAllocate :: Integer -> m [Ride],
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
    -- Set the status of the ride in the AllocationRequest table to completed.
    cleanupRide :: RideId -> m ()
  }

process :: Monad m => ServiceHandle m -> m Int
process handle@ServiceHandle {..} = do
  rides <- getTopRidesToAllocate ridesPerIteration
  traverse_ (processRide handle) rides
  pure $ length rides

processRide :: Monad m => ServiceHandle m -> Ride -> m ()
processRide handle@ServiceHandle {..} ride = do
  let rideId = ride ^. #id
  allocationTimeFinished <- isAllocationTimeFinished handle ride
  if allocationTimeFinished
    then cancel handle rideId
    else do
      mCurrentNotification <- getCurrentNotification rideId
      case mCurrentNotification of
        Just currentNotification ->
          processCurrentNotification handle rideId currentNotification
        Nothing ->
          proceedToNextDriver handle rideId

processCurrentNotification ::
  Monad m =>
  ServiceHandle m ->
  RideId ->
  CurrentNotification ->
  m ()
processCurrentNotification
  handle@ServiceHandle {..}
  rideId
  (CurrentNotification driverId notificationTime) = do
    notificationTimeFinished <- isNotificationTimeFinished handle notificationTime
    if notificationTimeFinished
      then processRejection handle True rideId driverId
      else do
        mResponse <- getDriverResponse rideId driverId
        case mResponse of
          Just Accept -> do
            assignDriver rideId driverId
            updateNotificationStatus rideId driverId Accepted
            cleanupRide rideId
          Just Reject -> processRejection handle False rideId driverId
          Nothing -> pure ()

processRejection :: Monad m => ServiceHandle m -> Bool -> RideId -> DriverId -> m ()
processRejection handle@ServiceHandle {..} ignored rideId driverId = do
  let status = if ignored then Ignored else Rejected
  resetLastRejectionTime driverId
  updateNotificationStatus rideId driverId status
  proceedToNextDriver handle rideId

proceedToNextDriver :: Monad m => ServiceHandle m -> RideId -> m ()
proceedToNextDriver handle@ServiceHandle {..} rideId = do
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
      processFilteredPool handle rideId filteredPool
    [] -> cancel handle rideId

processFilteredPool :: Monad m => ServiceHandle m -> RideId -> [DriverId] -> m ()
processFilteredPool handle@ServiceHandle {..} rideId = \case
  driverId : driverIds -> do
    sortMode <- getDriverSortMode
    firstDriver <-
      case sortMode of
        ETA -> pure driverId
        IdleTime -> getFirstDriverInTheQueue $ driverId :| driverIds
    time <- getCurrentTime
    sendNotification rideId firstDriver
    addNotificationStatus rideId firstDriver $ Notified time
  [] -> cancel handle rideId

cancel :: Monad m => ServiceHandle m -> RideId -> m ()
cancel ServiceHandle {..} rideId = do
  cancelRide rideId
  cleanupRide rideId

isAllocationTimeFinished :: Monad m => ServiceHandle m -> Ride -> m Bool
isAllocationTimeFinished ServiceHandle {..} ride = do
  currentTime <- getCurrentTime
  configuredAllocationTime <- getConfiguredAllocationTime
  let elapsedSearchTime = diffUTCTime currentTime (ride ^. #orderedAt)
  pure $ elapsedSearchTime > configuredAllocationTime

isNotificationTimeFinished :: Monad m => ServiceHandle m -> UTCTime -> m Bool
isNotificationTimeFinished ServiceHandle {..} notificationTime = do
  currentTime <- getCurrentTime
  configuredNotificationTime <- getConfiguredNotificationTime
  let elapsedNotificationTime = diffUTCTime currentTime notificationTime
  pure $ elapsedNotificationTime > configuredNotificationTime
