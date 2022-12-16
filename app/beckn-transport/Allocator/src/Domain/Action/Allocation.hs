module Domain.Action.Allocation where

import Beckn.Types.Common
import Beckn.Types.Id
import Beckn.Utils.Common
import Beckn.Utils.NonEmpty
import Data.Generics.Labels ()
import qualified Data.Text as T
import qualified Domain.Types.AllocationEvent as DAllocEvent (AllocationEventType (..))
import qualified Domain.Types.Booking as SRB
import qualified Domain.Types.BookingCancellationReason as SBCR
import qualified Domain.Types.CancellationReason as SCR
import Domain.Types.Merchant
import Domain.Types.Person (Driver)
import qualified Domain.Types.RideRequest as SRR
import EulerHS.Prelude
import SharedLogic.DriverPool.Types

data RequestData
  = Allocation
  | Cancellation
  | DriverResponse DriverResponseType
  deriving (Generic, Show, FromJSON, ToJSON)

data DriverResponseType = DriverResponseType
  { driverId :: Id Driver,
    response :: Response
  }
  deriving (Show, Generic, FromJSON, ToJSON)

data Response
  = Accept
  | Reject
  deriving (Show, Generic, ToJSON, FromJSON)

data RideRequest = RideRequest
  { requestId :: Id SRR.RideRequest,
    bookingId :: Id SRB.Booking,
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
  { incrementTaskCounter :: ShortId Subscriber -> m (),
    incrementFailedTaskCounter :: ShortId Subscriber -> m (),
    putTaskDuration :: ShortId Subscriber -> Milliseconds -> m (),
    incrementErrorCounter :: SomeException -> m ()
  }

data ServiceHandle m = ServiceHandle
  { getConfiguredNotificationTime :: m Seconds,
    getConfiguredAllocationTime :: m Seconds,
    getConfiguredReallocationsLimit :: m Int,
    getRequests :: ShortId Subscriber -> Integer -> m [RideRequest],
    prepareDriverPoolBatches :: Id SRB.Booking -> m (),
    getNextDriverPoolBatch :: Id SRB.Booking -> m [DriverPoolResult],
    cleanupDriverPoolBatches :: Id SRB.Booking -> m (),
    getCurrentNotifications :: Id SRB.Booking -> m [CurrentNotification],
    incrementPoolRadiusStep :: Id SRB.Booking -> m (),
    cleanupOldNotifications :: m Int,
    sendNewRideNotifications :: Id SRB.Booking -> NonEmpty (Id Driver) -> m (),
    sendRideNotAssignedNotification :: Id SRB.Booking -> Id Driver -> m (),
    addNotificationStatuses :: Id SRB.Booking -> NonEmpty (Id Driver) -> UTCTime -> m (),
    updateNotificationStatuses :: Id SRB.Booking -> NotificationStatus -> NonEmpty (Id Driver) -> m (),
    resetLastRejectionTimes :: NonEmpty (Id Driver) -> m (),
    getDriversWithNotification :: m [Id Driver],
    getTopDriversByIdleTime :: Int -> [Id Driver] -> m [Id Driver],
    checkAvailability :: [DriverPoolResult] -> m [DriverPoolResult],
    assignDriver :: Id SRB.Booking -> Id Driver -> m (),
    cancelBooking :: Id SRB.Booking -> SBCR.BookingCancellationReason -> m (),
    cleanupNotifications :: Id SRB.Booking -> m (),
    addAllocationRequest :: ShortId Subscriber -> Id SRB.Booking -> m (),
    getBooking :: Id SRB.Booking -> m SRB.Booking,
    removeRequest :: Id SRR.RideRequest -> m (),
    logEvent :: DAllocEvent.AllocationEventType -> Id SRB.Booking -> m (),
    logDriverEvents :: DAllocEvent.AllocationEventType -> Id SRB.Booking -> NonEmpty (Id Driver) -> m (),
    metrics :: AllocatorMetricsHandle m
  }

process :: MonadHandler m => ServiceHandle m -> ShortId Subscriber -> Integer -> m Int
process handle@ServiceHandle {..} subscriberId requestsNum = do
  cleanedNotificationsCount <- cleanupOldNotifications
  when (cleanedNotificationsCount > 0) $ logInfo $ "Cleaned notifications count: " <> show cleanedNotificationsCount
  rideRequests <- getRequests subscriberId requestsNum
  let rideRequestsNum = length rideRequests
  unless (rideRequestsNum == 0)
    . measuringDurationToLog INFO ("processing " <> show rideRequestsNum <> " ride requests")
    $ traverse_ (processRequest handle subscriberId) rideRequests
  pure rideRequestsNum

processRequest :: MonadHandler m => ServiceHandle m -> ShortId Subscriber -> RideRequest -> m ()
processRequest handle@ServiceHandle {..} subscriberId rideRequest = do
  metrics.incrementTaskCounter subscriberId
  measuringDuration (\dur _ -> metrics.putTaskDuration subscriberId dur) $ do
    let requestId = rideRequest.requestId
    let bookingId = rideRequest.bookingId
    booking <- getBooking bookingId
    eres <- try $
      withLogTag ("RideRequest_" <> bookingId.getId) $ do
        logInfo "Start processing request"
        case rideRequest.requestData of
          Allocation ->
            case booking.status of
              SRB.CONFIRMED -> processAllocation handle subscriberId booking
              SRB.AWAITING_REASSIGNMENT -> processAllocation handle subscriberId booking
              SRB.CANCELLED -> logInfo "Ride is cancelled, allocation request skipped"
              _ ->
                logWarning $ "Ride status is " <> show booking.status <> ", allocation request skipped"
          DriverResponse response ->
            case booking.status of
              SRB.CONFIRMED -> processDriverResponse handle response bookingId
              SRB.AWAITING_REASSIGNMENT -> processDriverResponse handle response bookingId
              SRB.TRIP_ASSIGNED -> do
                logInfo "Ride is assigned, response request skipped"
                sendRideNotAssignedNotification bookingId response.driverId
              SRB.CANCELLED -> do
                logInfo "Ride is cancelled, response request skipped"
                sendRideNotAssignedNotification bookingId response.driverId
              _ -> logWarning $ "Ride status is " <> show booking.status <> ", response request skipped"
          Cancellation ->
            case booking.status of
              status | status == SRB.CONFIRMED || status == SRB.TRIP_ASSIGNED -> do
                cancel handle bookingId SBCR.ByUser Nothing
                logEvent DAllocEvent.ConsumerCancelled bookingId
              _ ->
                logWarning $ "Ride status is " <> show booking.status <> ", cancellation request skipped"
        logInfo "End processing request"
    whenLeft eres $
      \(exc :: SomeException) -> do
        let message = "Error processing request " <> show requestId <> ": " <> makeLogSomeException exc
        logError message
        metrics.incrementErrorCounter exc
        metrics.incrementFailedTaskCounter subscriberId
    removeRequest requestId

processDriverResponse :: MonadHandler m => ServiceHandle m -> DriverResponseType -> Id SRB.Booking -> m ()
processDriverResponse handle@ServiceHandle {..} response bookingId = do
  currentNotifications <- getCurrentNotifications bookingId
  logInfo $ "getCurrentNotifications" <> show currentNotifications
  if response.driverId `elem` map (.driverId) currentNotifications
    then case response.response of
      Accept -> do
        logInfo $ "Assigning driver" <> show response.driverId
        assignDriver bookingId response.driverId
        cleanupDriverPoolBatches bookingId
        cleanupNotifications bookingId
        logDriverEvents DAllocEvent.MarkedAsAccepted bookingId $ singleton response.driverId
      Reject ->
        processRejection handle bookingId response.driverId
    else logDriverNoLongerNotified bookingId response.driverId

processAllocation ::
  MonadHandler m =>
  ServiceHandle m ->
  ShortId Subscriber ->
  SRB.Booking ->
  m ()
processAllocation handle@ServiceHandle {..} subscriberId booking = do
  let bookingId = booking.id
  let orderTime = booking.createdAt
  currentTime <- getCurrentTime
  allocationTimeFinished <- isAllocationTimeFinished handle currentTime orderTime
  allocationLimitExceed <- isReallocationLimitExceed handle booking.reallocationsCount
  logInfo $ "isAllocationTimeFinished " <> show allocationTimeFinished
  case (allocationTimeFinished, allocationLimitExceed) of
    (True, _) -> do
      cancel handle bookingId SBCR.ByAllocator $ Just AllocationTimeExpired
      logEvent DAllocEvent.AllocationTimeFinished bookingId
    (_, True) -> do
      cancel handle bookingId SBCR.ByAllocator $ Just ReallocationLimitExceed
      logEvent DAllocEvent.ReallocationLimitExceed bookingId
    _ -> do
      currentNotifications <- getCurrentNotifications bookingId
      logInfo $ "getCurrentNotification " <> show currentNotifications
      case currentNotifications of
        notification : notifications -> do
          let notificationTimeFinished = currentTime > notification.expiryTime
          if notificationTimeFinished
            then do
              processExpiration handle bookingId $ fmap (.driverId) $ notification :| notifications
              proceedToNewDrivers handle bookingId subscriberId
            else checkRideLater handle subscriberId bookingId
        [] ->
          proceedToNewDrivers handle bookingId subscriberId

processRejection ::
  MonadHandler m =>
  ServiceHandle m ->
  Id SRB.Booking ->
  Id Driver ->
  m ()
processRejection ServiceHandle {..} bookingId driverId = do
  logInfo "Processing rejection"
  resetLastRejectionTimes $ singleton driverId
  updateNotificationStatuses bookingId Rejected $ singleton driverId
  logDriverEvents DAllocEvent.MarkedAsRejected bookingId $ singleton driverId

processExpiration :: MonadHandler m => ServiceHandle m -> Id SRB.Booking -> NonEmpty (Id Driver) -> m ()
processExpiration ServiceHandle {..} bookingId driverIds = do
  logInfo "Processing expiration"
  resetLastRejectionTimes driverIds
  updateNotificationStatuses bookingId Ignored driverIds
  logDriverEvents DAllocEvent.MarkedAsIgnored bookingId driverIds

proceedToNewDrivers ::
  MonadHandler m =>
  ServiceHandle m ->
  Id SRB.Booking ->
  ShortId Subscriber ->
  m ()
proceedToNewDrivers handle@ServiceHandle {..} bookingId subscriberId = do
  logInfo "Proceed to new drivers"
  driverPool <- getDriverPool handle bookingId
  availableDriversPool <- checkAvailability driverPool
  driversWithNotification <- getDriversWithNotification
  let filteredPool = filter (\dpRes -> dpRes.driverId `notElem` driversWithNotification) availableDriversPool
  logInfo $ "Filtered_DriverPool " <> T.intercalate ", " (getDriverPoolDescs filteredPool)
  processFilteredPool handle bookingId filteredPool subscriberId

getDriverPool :: MonadHandler m => ServiceHandle m -> Id SRB.Booking -> m [DriverPoolResult]
getDriverPool ServiceHandle {..} bookingId = do
  driverPool <- getNextDriverPoolBatch bookingId
  logInfo $ "DriverPool " <> T.intercalate ", " (getDriverPoolDescs driverPool)
  if not $ null driverPool
    then return driverPool
    else getNextRadiusBatch
  where
    getNextRadiusBatch = do
      incrementPoolRadiusStep bookingId
      prepareDriverPoolBatches bookingId
      getNextDriverPoolBatch bookingId

getDriverPoolDescs :: [DriverPoolResult] -> [Text]
getDriverPoolDescs pool =
  (\driverPoolResult -> driverPoolResult.driverId.getId <> "(" <> show driverPoolResult.distanceToPickup <> "m)")
    <$> pool

processFilteredPool ::
  MonadHandler m =>
  ServiceHandle m ->
  Id SRB.Booking ->
  [DriverPoolResult] ->
  ShortId Subscriber ->
  m ()
processFilteredPool handle@ServiceHandle {..} bookingId filteredPool subscriberId = do
  case (.driverId) <$> filteredPool of
    driverId : driverIds -> do
      let drivers = driverId :| driverIds
      currentTime <- getCurrentTime
      notificationTime <- fromIntegral <$> getConfiguredNotificationTime
      let expiryTime = addUTCTime notificationTime currentTime
      sendNewRideNotifications bookingId drivers
      addNotificationStatuses bookingId drivers expiryTime
      logInfo $ "Notified drivers " <> show (map (.driverId.getId) filteredPool)
      logDriverEvents DAllocEvent.NotificationSent bookingId drivers
    [] -> do
      logInfo "All new drivers are unavailable or already have notifications. Waiting."
  checkRideLater handle subscriberId bookingId

checkRideLater :: MonadHandler m => ServiceHandle m -> ShortId Subscriber -> Id SRB.Booking -> m ()
checkRideLater ServiceHandle {..} subscriberId bookingId = do
  addAllocationRequest subscriberId bookingId
  logInfo "Check ride later"

cancel :: MonadHandler m => ServiceHandle m -> Id SRB.Booking -> SBCR.CancellationSource -> Maybe AllocatorCancellationReason -> m ()
cancel ServiceHandle {..} bookingId cancellationSource mbReasonCode = do
  logInfo "Cancelling ride"
  bookingCancellationReason <- buildBookingCancellationReason
  cancelBooking bookingId bookingCancellationReason
  cleanupDriverPoolBatches bookingId
  cleanupNotifications bookingId
  where
    buildBookingCancellationReason = do
      guid <- generateGUID
      return
        SBCR.BookingCancellationReason
          { id = guid,
            driverId = Nothing,
            bookingId = bookingId,
            rideId = Nothing,
            source = cancellationSource,
            reasonCode = SCR.CancellationReasonCode . encodeToText <$> mbReasonCode,
            additionalInfo = Nothing
          }

isAllocationTimeFinished :: MonadHandler m => ServiceHandle m -> UTCTime -> UTCTime -> m Bool
isAllocationTimeFinished ServiceHandle {..} currentTime orderTime = do
  configuredAllocationTime <- fromIntegral <$> getConfiguredAllocationTime
  let elapsedSearchTime = diffUTCTime currentTime orderTime
  pure $ elapsedSearchTime > configuredAllocationTime

isReallocationLimitExceed :: MonadHandler m => ServiceHandle m -> Int -> m Bool
isReallocationLimitExceed ServiceHandle {..} reallocationsCount = do
  reallocationsLimit <- getConfiguredReallocationsLimit
  pure $ reallocationsCount >= reallocationsLimit

logDriverNoLongerNotified :: Log m => Id SRB.Booking -> Id Driver -> m ()
logDriverNoLongerNotified bookingId driverId =
  logInfo $
    "Driver "
      <> show driverId
      <> " is no longer notified about ride "
      <> show bookingId
      <> ", response request skipped"
