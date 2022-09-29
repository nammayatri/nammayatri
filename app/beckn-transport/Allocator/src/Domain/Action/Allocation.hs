module Domain.Action.Allocation where

import Beckn.Types.Common
import Beckn.Types.Id
import Beckn.Utils.Common
import Beckn.Utils.Dhall (FromDhall)
import Beckn.Utils.NonEmpty
import Data.Generics.Labels ()
import qualified Data.Text as T
import Domain.Types.AllocationEvent (AllocationEventType (..))
import qualified Domain.Types.Booking as SRB
import qualified Domain.Types.BookingCancellationReason as SBCR
import qualified Domain.Types.CancellationReason as SCR
import Domain.Types.DriverPool
import Domain.Types.Organization
import Domain.Types.Person (Driver)
import qualified Domain.Types.RideRequest as SRR
import EulerHS.Prelude

newtype OrderTime = OrderTime
  { utcTime :: UTCTime
  }
  deriving (Generic, Show)

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

data RideStatus
  = New
  | Confirmed
  | AwaitingReassignment
  | Assigned
  | Completed
  | Cancelled
  deriving (Show, Eq, Ord, Read, Generic, ToJSON, FromJSON)

data RideInfo = RideInfo
  { bookingId :: Id SRB.Booking,
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

data SortMode
  = ETA
  | IdleTime
  deriving (Eq, Generic, FromDhall)

data ServiceHandle m = ServiceHandle
  { getDriverSortMode :: m SortMode,
    getConfiguredNotificationTime :: m Seconds,
    getConfiguredAllocationTime :: m Seconds,
    getConfiguredReallocationsLimit :: m Int,
    getDriverBatchSize :: m Int,
    getRequests :: ShortId Organization -> Integer -> m [RideRequest],
    getDriverPool :: Id SRB.Booking -> m SortedDriverPool,
    getCurrentNotifications :: Id SRB.Booking -> m [CurrentNotification],
    cleanupOldNotifications :: m Int,
    sendNewRideNotifications :: Id SRB.Booking -> NonEmpty (Id Driver) -> m (),
    sendRideNotAssignedNotification :: Id SRB.Booking -> Id Driver -> m (),
    addNotificationStatuses :: Id SRB.Booking -> NonEmpty (Id Driver) -> UTCTime -> m (),
    updateNotificationStatuses :: Id SRB.Booking -> NotificationStatus -> NonEmpty (Id Driver) -> m (),
    resetLastRejectionTimes :: NonEmpty (Id Driver) -> m (),
    getAttemptedDrivers :: Id SRB.Booking -> m [Id Driver],
    getDriversWithNotification :: m [Id Driver],
    getTopDriversByIdleTime :: Int -> [Id Driver] -> m [Id Driver],
    checkAvailability :: SortedDriverPool -> m SortedDriverPool,
    assignDriver :: Id SRB.Booking -> Id Driver -> m (),
    cancelBooking :: Id SRB.Booking -> SBCR.BookingCancellationReason -> m (),
    cleanupNotifications :: Id SRB.Booking -> m (),
    addAllocationRequest :: ShortId Organization -> Id SRB.Booking -> m (),
    getRideInfo :: Id SRB.Booking -> m RideInfo,
    removeRequest :: Id SRR.RideRequest -> m (),
    logEvent :: AllocationEventType -> Id SRB.Booking -> m (),
    logDriverEvents :: AllocationEventType -> Id SRB.Booking -> NonEmpty (Id Driver) -> m (),
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
    let bookingId = rideRequest.bookingId
    rideInfo <- getRideInfo bookingId
    let rideStatus = rideInfo.rideStatus
    eres <- try $
      withLogTag ("RideRequest_" <> bookingId.getId) $ do
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
              Confirmed -> processDriverResponse handle response bookingId
              AwaitingReassignment -> processDriverResponse handle response bookingId
              Assigned -> do
                logInfo "Ride is assigned, response request skipped"
                sendRideNotAssignedNotification bookingId response.driverId
              Cancelled -> do
                logInfo "Ride is cancelled, response request skipped"
                sendRideNotAssignedNotification bookingId response.driverId
              _ -> logWarning $ "Ride status is " <> show rideStatus <> ", response request skipped"
          Cancellation ->
            case rideStatus of
              status | status == Confirmed || status == Assigned -> do
                cancel handle bookingId SBCR.ByUser Nothing
                logEvent ConsumerCancelled bookingId
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

processDriverResponse :: MonadHandler m => ServiceHandle m -> DriverResponseType -> Id SRB.Booking -> m ()
processDriverResponse handle@ServiceHandle {..} response bookingId = do
  currentNotifications <- getCurrentNotifications bookingId
  logInfo $ "getCurrentNotifications" <> show currentNotifications
  if response.driverId `elem` map (.driverId) currentNotifications
    then case response.response of
      Accept -> do
        logInfo $ "Assigning driver" <> show response.driverId
        assignDriver bookingId response.driverId
        cleanupNotifications bookingId
        logDriverEvents MarkedAsAccepted bookingId $ singleton response.driverId
      Reject ->
        processRejection handle bookingId response.driverId
    else logDriverNoLongerNotified bookingId response.driverId

processAllocation ::
  MonadHandler m =>
  ServiceHandle m ->
  ShortId Organization ->
  RideInfo ->
  m ()
processAllocation handle@ServiceHandle {..} shortOrgId rideInfo = do
  let bookingId = rideInfo.bookingId
  let orderTime = rideInfo.orderTime
  currentTime <- getCurrentTime
  allocationTimeFinished <- isAllocationTimeFinished handle currentTime orderTime
  allocationLimitExceed <- isReallocationLimitExceed handle rideInfo.reallocationsCount
  logInfo $ "isAllocationTimeFinished " <> show allocationTimeFinished
  if allocationTimeFinished || allocationLimitExceed
    then do
      let cancellationReason = if allocationTimeFinished then AllocationTimeExpired else ReallocationLimitExceed
      cancel handle bookingId SBCR.ByAllocator $ Just cancellationReason
      logEvent AllocationTimeFinished bookingId
    else do
      currentNotifications <- getCurrentNotifications bookingId
      logInfo $ "getCurrentNotification " <> show currentNotifications
      case currentNotifications of
        notification : notifications -> do
          let notificationTimeFinished = currentTime > notification.expiryTime
          if notificationTimeFinished
            then do
              processExpiration handle bookingId $ fmap (.driverId) $ notification :| notifications
              proceedToNewDrivers handle bookingId shortOrgId
            else checkRideLater handle shortOrgId bookingId
        [] ->
          proceedToNewDrivers handle bookingId shortOrgId

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
  logDriverEvents MarkedAsRejected bookingId $ singleton driverId

processExpiration :: MonadHandler m => ServiceHandle m -> Id SRB.Booking -> NonEmpty (Id Driver) -> m ()
processExpiration ServiceHandle {..} bookingId driverIds = do
  logInfo "Processing expiration"
  resetLastRejectionTimes driverIds
  updateNotificationStatuses bookingId Ignored driverIds
  logDriverEvents MarkedAsIgnored bookingId driverIds

proceedToNewDrivers ::
  MonadHandler m =>
  ServiceHandle m ->
  Id SRB.Booking ->
  ShortId Organization ->
  m ()
proceedToNewDrivers handle@ServiceHandle {..} bookingId shortOrgId = do
  logInfo "Proceed to new drivers"
  driverPool <- getDriverPool bookingId
  logInfo $ "DriverPool " <> T.intercalate ", " (getDriverPoolDescs driverPool)
  attemptedDrivers <- getAttemptedDrivers bookingId
  let newDriversPool = filterDriverPool (`notElem` attemptedDrivers) driverPool
  if not $ null (getDriverIds newDriversPool)
    then do
      availableDriversPool <- checkAvailability newDriversPool
      driversWithNotification <- getDriversWithNotification
      let filteredPool = filterDriverPool (`notElem` driversWithNotification) availableDriversPool
      logInfo $ "Filtered_DriverPool " <> T.intercalate ", " (getDriverPoolDescs filteredPool)
      processFilteredPool handle bookingId filteredPool shortOrgId
    else do
      cancel handle bookingId SBCR.ByAllocator $ Just NoDriversInRange
      logEvent EmptyDriverPool bookingId
  where
    getDriverPoolDescs :: SortedDriverPool -> [Text]
    getDriverPoolDescs pool =
      (\driverPoolResult -> driverPoolResult.driverId.getId <> "(" <> show driverPoolResult.distanceToPickup <> "m)")
        <$> getSortedDriverPool pool

processFilteredPool ::
  MonadHandler m =>
  ServiceHandle m ->
  Id SRB.Booking ->
  SortedDriverPool ->
  ShortId Organization ->
  m ()
processFilteredPool handle@ServiceHandle {..} bookingId filteredPool shortOrgId = do
  sortMode <- getDriverSortMode
  batchSize <- getDriverBatchSize
  topDrivers <-
    case sortMode of
      ETA -> pure $ take batchSize (getDriverIds filteredPool)
      IdleTime -> getTopDriversByIdleTime batchSize (getDriverIds filteredPool)
  case topDrivers of
    driverId : driverIds -> do
      let drivers = driverId :| driverIds
      currentTime <- getCurrentTime
      notificationTime <- fromIntegral <$> getConfiguredNotificationTime
      let expiryTime = addUTCTime notificationTime currentTime
      sendNewRideNotifications bookingId drivers
      addNotificationStatuses bookingId drivers expiryTime
      logInfo $ "Notified drivers " <> show (map getId topDrivers)
      logDriverEvents NotificationSent bookingId drivers
    [] -> do
      logInfo "All new drivers are unavailable or already have notifications. Waiting."
  checkRideLater handle shortOrgId bookingId

checkRideLater :: MonadHandler m => ServiceHandle m -> ShortId Organization -> Id SRB.Booking -> m ()
checkRideLater ServiceHandle {..} shortOrgId bookingId = do
  addAllocationRequest shortOrgId bookingId
  logInfo "Check ride later"

cancel :: MonadHandler m => ServiceHandle m -> Id SRB.Booking -> SBCR.CancellationSource -> Maybe AllocatorCancellationReason -> m ()
cancel ServiceHandle {..} bookingId cancellationSource mbReasonCode = do
  logInfo "Cancelling ride"
  bookingCancellationReason <- buildBookingCancellationReason
  cancelBooking bookingId bookingCancellationReason
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

isAllocationTimeFinished :: MonadHandler m => ServiceHandle m -> UTCTime -> OrderTime -> m Bool
isAllocationTimeFinished ServiceHandle {..} currentTime orderTime = do
  configuredAllocationTime <- fromIntegral <$> getConfiguredAllocationTime
  let elapsedSearchTime = diffUTCTime currentTime (orderTime.utcTime)
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
