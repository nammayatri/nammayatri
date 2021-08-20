module Services.Allocation.Allocation where

import Beckn.Types.Common
import Beckn.Types.Id
import Beckn.Types.Mobility.Order (CancellationSource (..))
import Data.Generics.Labels ()
import qualified Data.Text as T
import Data.Time.Clock (UTCTime, addUTCTime, diffUTCTime)
import EulerHS.Prelude
import qualified Types.API.RideBooking as RideBooking
import Types.App
import Types.Storage.AllocationEvent (AllocationEventType (..))
import qualified Types.Storage.CancellationReason as SCR
import Types.Storage.Organization
import qualified Types.Storage.RideCancellationReason as SRCR
import qualified Types.Storage.RideBooking as SRB
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

data CurrentNotification
  = CurrentNotification (Id Driver) UTCTime
  deriving (Show)

data RideStatus
  = Confirmed
  | Assigned
  | Completed
  | Cancelled
  deriving (Show, Eq, Ord, Read, Generic, ToJSON, FromJSON)

data RideInfo = RideInfo
  { rideBookingId :: Id SRB.RideBooking,
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
    getDriverPool :: Id SRB.RideBooking -> m [Id Driver],
    getCurrentNotification :: Id SRB.RideBooking -> m (Maybe CurrentNotification),
    cleanupOldNotifications :: m Int,
    sendNewRideNotification :: Id SRB.RideBooking -> Id Driver -> m (),
    sendRideNotAssignedNotification :: Id SRB.RideBooking -> Id Driver -> m (),
    addNotificationStatus :: Id SRB.RideBooking -> Id Driver -> UTCTime -> m (),
    updateNotificationStatus :: Id SRB.RideBooking -> Id Driver -> NotificationStatus -> m (),
    resetLastRejectionTime :: Id Driver -> m (),
    getAttemptedDrivers :: Id SRB.RideBooking -> m [Id Driver],
    getDriversWithNotification :: m [Id Driver],
    getFirstDriverInTheQueue :: NonEmpty (Id Driver) -> m (Id Driver),
    checkAvailability :: NonEmpty (Id Driver) -> m [Id Driver],
    assignDriver :: Id SRB.RideBooking -> Id Driver -> m (),
    cancelRide :: Id SRB.RideBooking -> SRCR.RideCancellationReason -> m (),
    cleanupNotifications :: Id SRB.RideBooking -> m (),
    addAllocationRequest :: ShortId Organization -> Id SRB.RideBooking -> m (),
    getRideInfo :: Id SRB.RideBooking -> m RideInfo,
    removeRequest :: Id SRR.RideRequest -> m (),
    logEvent :: AllocationEventType -> Id SRB.RideBooking -> Maybe (Id Driver) -> m (),
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
              Cancelled -> logInfo "Ride is cancelled, allocation request skipped"
              _ ->
                logWarning $ "Ride status is " <> show rideStatus <> ", allocation request skipped"
          DriverResponse response ->
            case rideStatus of
              Confirmed -> do
                mCurrentNotification <- getCurrentNotification rideBookingId
                logInfo $ "getCurrentNotification " <> show mCurrentNotification
                case mCurrentNotification of
                  Just (CurrentNotification driverId _) -> do
                    if driverId == response.driverId
                      then case response.status of
                        RideBooking.ACCEPT -> do
                          logInfo $ "Assigning driver" <> show response.driverId
                          assignDriver rideBookingId response.driverId
                          cleanupNotifications rideBookingId
                          logEvent MarkedAsAccepted rideBookingId $ Just response.driverId
                        RideBooking.REJECT ->
                          processRejection handle False rideBookingId response.driverId shortOrgId
                      else logDriverNoLongerNotified rideBookingId response.driverId
                  Nothing ->
                    logDriverNoLongerNotified rideBookingId response.driverId
              Assigned ->
                logInfo "Ride is assigned, response request skipped"
              Cancelled ->
                logInfo "Ride is cancelled, response request skipped"
              _ -> logWarning $ "Ride status is " <> show rideStatus <> ", response request skipped"
          Cancellation ->
            case rideStatus of
              status | status == Confirmed || status == Assigned -> do
                cancel handle rideBookingId ByUser Nothing
                logEvent ConsumerCancelled rideBookingId Nothing
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
  let rideBookingId = rideInfo.rideBookingId
  let orderTime = rideInfo.orderTime
  currentTime <- getCurrentTime
  allocationTimeFinished <- isAllocationTimeFinished handle currentTime orderTime
  logInfo $ "isAllocationTimeFinished " <> show allocationTimeFinished
  if allocationTimeFinished
    then do
      cancel handle rideBookingId ByAllocator $ Just AllocationTimeExpired
      logEvent AllocationTimeFinished rideBookingId Nothing
    else do
      mCurrentNotification <- getCurrentNotification rideBookingId
      logInfo $ "getCurrentNotification " <> show mCurrentNotification
      case mCurrentNotification of
        Just (CurrentNotification driverId expiryTime) -> do
          let notificationTimeFinished = currentTime > expiryTime
          if notificationTimeFinished
            then processExpiredNotification handle shortOrgId rideBookingId driverId
            else checkRideLater handle shortOrgId rideBookingId
        Nothing ->
          proceedToNextDriver handle rideBookingId shortOrgId

processExpiredNotification ::
  MonadHandler m =>
  ServiceHandle m ->
  ShortId Organization ->
  Id SRB.RideBooking ->
  Id Driver ->
  m ()
processExpiredNotification handle@ServiceHandle {..} shortOrgId rideBookingId driverId = do
  logInfo $ "Notified ride not assigned to driver " <> getId driverId
  sendRideNotAssignedNotification rideBookingId driverId
  processRejection handle True rideBookingId driverId shortOrgId

processRejection ::
  MonadHandler m =>
  ServiceHandle m ->
  Bool ->
  Id SRB.RideBooking ->
  Id Driver ->
  ShortId Organization ->
  m ()
processRejection handle@ServiceHandle {..} ignored rideBookingId driverId shortOrgId = do
  logInfo "Processing rejection"
  let status = if ignored then Ignored else Rejected
  let event = if ignored then MarkedAsIgnored else MarkedAsRejected
  resetLastRejectionTime driverId
  updateNotificationStatus rideBookingId driverId status
  logEvent event rideBookingId $ Just driverId
  proceedToNextDriver handle rideBookingId shortOrgId

proceedToNextDriver ::
  MonadHandler m =>
  ServiceHandle m ->
  Id SRB.RideBooking ->
  ShortId Organization ->
  m ()
proceedToNextDriver handle@ServiceHandle {..} rideBookingId shortOrgId = do
  logInfo "Proceed to next driver"
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
      logEvent EmptyDriverPool rideBookingId Nothing

processFilteredPool ::
  MonadHandler m =>
  ServiceHandle m ->
  Id SRB.RideBooking ->
  [Id Driver] ->
  ShortId Organization ->
  m ()
processFilteredPool handle@ServiceHandle {..} rideBookingId filteredPool shortOrgId = do
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
      sendNewRideNotification rideBookingId firstDriver
      addNotificationStatus rideBookingId firstDriver expiryTime
      logInfo $ "Notified driver " <> getId firstDriver
      logEvent NotificationSent rideBookingId $ Just firstDriver
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
  cancelRide rideBookingId rideCancellationReason
  cleanupNotifications rideBookingId
  where
    rideCancellationReason =
      SRCR.RideCancellationReason
        { rideBookingId = rideBookingId,
          source = cancellationSource,
          reasonCode = SCR.CancellationReasonCode . encodeToText <$> mbReasonCode,
          additionalInfo = Nothing
        }

isAllocationTimeFinished :: MonadHandler m => ServiceHandle m -> UTCTime -> OrderTime -> m Bool
isAllocationTimeFinished ServiceHandle {..} currentTime orderTime = do
  configuredAllocationTime <- fromIntegral <$> getConfiguredAllocationTime
  let elapsedSearchTime = diffUTCTime currentTime (orderTime.utcTime)
  pure $ elapsedSearchTime > configuredAllocationTime

logDriverNoLongerNotified :: Log m => Id SRB.RideBooking -> Id Driver -> m ()
logDriverNoLongerNotified rideBookingId driverId =
  logInfo $
    "Driver "
      <> show driverId
      <> " is no longer notified about ride "
      <> show rideBookingId
      <> ", response request skipped"
