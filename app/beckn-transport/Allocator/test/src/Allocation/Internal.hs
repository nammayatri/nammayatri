{-# LANGUAGE OverloadedLabels #-}

module Allocation.Internal where

import Beckn.Types.Id
import Beckn.Utils.Common
import qualified Data.Map as Map
import qualified Data.Time as Time
import Domain.Action.Allocation as Alloc
import qualified Domain.Types.Booking as SRB
import Domain.Types.DriverPool
import Domain.Types.Merchant
import Domain.Types.Person (Driver)
import qualified Domain.Types.RideRequest as SRR
import EulerHS.Prelude hiding (id)
import Test.Tasty.HUnit
import Utils.GuidGenerator ()
import Utils.SilentLogger ()

type NotificationStatusMap = (Map (Id SRB.Booking, Id Driver) (NotificationStatus, UTCTime))

org1 :: ShortId Subscriber
org1 = ShortId "Org1"

numRequestsToProcess :: Integer
numRequestsToProcess = 10

allocationTime :: Seconds
allocationTime = 3

notificationTime :: Seconds
notificationTime = 1

reallocationsLimit :: Int
reallocationsLimit = 3

isNotified :: UTCTime -> (NotificationStatus, UTCTime) -> Bool
isNotified _ (Notified, _) = True
isNotified _ _ = False

attemptedNotification ::
  Id SRB.Booking ->
  (Id SRB.Booking, Id Driver) ->
  (NotificationStatus, UTCTime) ->
  Bool
attemptedNotification bookingId (id, _) (status, _) =
  id == bookingId && status `elem` [Rejected, Ignored]

addBooking :: Repository -> Id SRB.Booking -> Int -> IO ()
addBooking Repository {..} bookingId reallocationsCount = do
  currTime <- Time.getCurrentTime
  let rideInfo =
        RideInfo
          { bookingId = bookingId,
            rideStatus = Confirmed,
            orderTime = OrderTime currTime,
            reallocationsCount = reallocationsCount
          }
  modifyIORef bookingsVar $ Map.insert bookingId rideInfo

updateBooking :: Repository -> Id SRB.Booking -> RideStatus -> IO ()
updateBooking Repository {..} bookingId status = do
  modifyIORef bookingsVar $ Map.adjust (\rideInfo -> rideInfo {rideStatus = status}) bookingId

addRequest :: RequestData -> Repository -> Id SRB.Booking -> IO ()
addRequest requestData Repository {..} bookingId = do
  currentId <- readIORef currentIdVar
  let requestId = Id $ show currentId
  let request =
        RideRequest
          { requestId = requestId,
            bookingId = bookingId,
            requestData = requestData
          }
  modifyIORef currentIdVar (+ 1)
  modifyIORef rideRequestsVar $ Map.insert requestId request

addResponse :: Repository -> Id SRB.Booking -> Id Driver -> Alloc.Response -> IO ()
addResponse repository@Repository {..} bookingId driverId status = do
  let driverResponse = DriverResponseType driverId status
  addRequest (DriverResponse driverResponse) repository bookingId

addDriverPool :: Repository -> Map (Id SRB.Booking) [Id Driver] -> IO ()
addDriverPool Repository {..} driversMap = do
  let driverPool = mkDefaultDriverPool <$> driversMap
  writeIORef driverPoolVar driverPool

addSortedDriverPool :: Repository -> Map (Id SRB.Booking) SortedDriverPool -> IO ()
addSortedDriverPool Repository {..} driverPool = do
  writeIORef driverPoolVar driverPool

mkDefaultDriverPoolItem :: Id Driver -> DriverPoolItem
mkDefaultDriverPoolItem driverId =
  DriverPoolItem
    { driverId,
      distanceToPickup = 100
    }

mkDefaultDriverPool :: [Id Driver] -> SortedDriverPool
mkDefaultDriverPool driverIds = mkSortedDriverPool $ mkDefaultDriverPoolItem <$> driverIds

addDriverInPool :: Repository -> Id SRB.Booking -> Id Driver -> IO ()
addDriverInPool Repository {..} bookingId driverId = do
  driversPool <- readIORef driverPoolVar
  let newDriversPool = Map.adjust (addItemToPool $ mkDefaultDriverPoolItem driverId) bookingId driversPool
  writeIORef driverPoolVar newDriversPool

checkRideStatus :: Repository -> Id SRB.Booking -> RideStatus -> IO ()
checkRideStatus Repository {..} bookingId expectedStatus = do
  bookings <- readIORef bookingsVar
  case Map.lookup bookingId bookings of
    Nothing -> assertFailure $ "Booking " <> show (bookingId.getId) <> " not found"
    Just rideInfo -> rideInfo.rideStatus @?= expectedStatus

checkNotificationStatus :: Repository -> Id SRB.Booking -> Id Driver -> NotificationStatus -> IO ()
checkNotificationStatus Repository {..} bookingId driverId expectedStatus = do
  notifications <- readIORef notificationStatusVar
  case Map.lookup (bookingId, driverId) notifications of
    Nothing ->
      assertFailure $
        "Notification for bookingId=" <> show (bookingId.getId) <> " and driverId=" <> show (driverId.getId) <> " is not found"
    Just (status, _) -> status @?= expectedStatus

checkFreeNotificationStatus :: Repository -> Id SRB.Booking -> Id Driver -> IO ()
checkFreeNotificationStatus Repository {..} bookingId driverId = do
  notifications <- readIORef notificationStatusVar
  Map.lookup (bookingId, driverId) notifications @?= Nothing

addNotification ::
  Id SRB.Booking ->
  UTCTime ->
  NotificationStatusMap ->
  Id Driver ->
  NotificationStatusMap
addNotification bookingId expiryTime notificationStatuses driverId =
  Map.insert
    (bookingId, driverId)
    (Notified, expiryTime)
    notificationStatuses

updateNotification ::
  Id SRB.Booking ->
  NotificationStatus ->
  NotificationStatusMap ->
  Id Driver ->
  NotificationStatusMap
updateNotification bookingId nStatus notificationStatuses driverId =
  Map.adjust
    (\(_, expiryTime) -> (nStatus, expiryTime))
    (bookingId, driverId)
    notificationStatuses

toCurrentNotification :: ((Id SRB.Booking, Id Driver), (NotificationStatus, UTCTime)) -> CurrentNotification
toCurrentNotification ((_, driverId), (_, expiryTime)) =
  CurrentNotification driverId expiryTime

handle :: Repository -> ServiceHandle IO
handle repository@Repository {..} =
  ServiceHandle
    { getDriverSortMode = pure ETA,
      getConfiguredAllocationTime = pure allocationTime,
      getConfiguredNotificationTime = pure notificationTime,
      getConfiguredReallocationsLimit = pure reallocationsLimit,
      getDriverBatchSize = pure 5,
      getRequests = \_ numRides -> do
        rideRequests <- readIORef rideRequestsVar
        let requests = Map.elems rideRequests
        pure $ take (fromIntegral numRides) requests,
      getDriverPool = \bookingId -> do
        poolMap <- readIORef driverPoolVar
        let pool = fromMaybe (mkSortedDriverPool []) $ Map.lookup bookingId poolMap
        pure pool,
      sendNewRideNotifications = \_ _ -> pure (),
      getCurrentNotifications = \rideId -> do
        notificationStatus <- readIORef notificationStatusVar
        let filtered =
              Map.toList $
                Map.filterWithKey
                  (\(id, _) (notification, _) -> id == rideId && notification == Notified)
                  notificationStatus
        pure $ map toCurrentNotification filtered,
      cleanupOldNotifications = do
        compareTime <- Time.getCurrentTime <&> Time.addUTCTime (-300)
        modifyIORef notificationStatusVar $
          Map.filter (\(_, expiryTime) -> compareTime < expiryTime)
        return 0,
      addNotificationStatuses = \rideId driverIds expiryTime ->
        modifyIORef notificationStatusVar $
          \notificationStatuses ->
            foldl' (addNotification rideId expiryTime) notificationStatuses driverIds,
      updateNotificationStatuses = \rideId nStatus driverIds ->
        modifyIORef notificationStatusVar $
          \notificationStatuses ->
            foldl' (updateNotification rideId nStatus) notificationStatuses driverIds,
      resetLastRejectionTimes = \_ -> pure (),
      getAttemptedDrivers = \rideId -> do
        notificationStatus <- readIORef notificationStatusVar
        let filtered =
              fmap snd $
                Map.keys $
                  Map.filterWithKey (attemptedNotification rideId) notificationStatus
        pure filtered,
      getDriversWithNotification = do
        notificationStatus <- readIORef notificationStatusVar
        let filtered = fmap snd $ Map.keys $ Map.filter (\(status, _) -> status == Notified) notificationStatus
        pure filtered,
      assignDriver = \bookingId driverId -> do
        modifyIORef assignmentsVar $ (:) (bookingId, driverId)
        modifyIORef bookingsVar $ Map.adjust (#rideStatus .~ Assigned) bookingId
        modifyIORef onRideVar $ (:) driverId,
      cancelBooking = \bookingId _ -> do
        modifyIORef bookingsVar $ Map.adjust (#rideStatus .~ Cancelled) bookingId
        assignments <- readIORef assignmentsVar
        let driversForBookingId = map snd $ filter (\(rbId, _) -> bookingId == rbId) assignments
        modifyIORef onRideVar $ filter (`notElem` driversForBookingId),
      cleanupNotifications = \rideId ->
        modifyIORef notificationStatusVar $ Map.filterWithKey (\(r, _) _ -> r /= rideId),
      getTopDriversByIdleTime = \count driverIds -> pure $ take count driverIds,
      checkAvailability = \driversPool -> do
        onRide <- readIORef onRideVar
        -- trying to break sorting elements by distance
        pure (mkSortedDriverPool . sortBy (compare `on` (.driverId)) . filter (\item -> item.driverId `notElem` onRide) . getSortedDriverPool $ driversPool),
      sendRideNotAssignedNotification = \_ _ -> pure (),
      removeRequest = modifyIORef rideRequestsVar . Map.delete,
      addAllocationRequest = \_ -> addRequest Allocation repository,
      getRideInfo = \bookingId -> do
        bookings <- readIORef bookingsVar
        case Map.lookup bookingId bookings of
          Just rideInfo -> pure rideInfo
          Nothing -> assertFailure $ "Booking " <> show bookingId <> " not found in the map.",
      logEvent = \_ _ -> pure (),
      logDriverEvents = \_ _ _ -> pure (),
      metrics =
        AllocatorMetricsHandle
          { incrementTaskCounter = \_ -> return (),
            incrementFailedTaskCounter = \_ -> return (),
            putTaskDuration = \_ _ -> return (),
            incrementErrorCounter = \_ -> return ()
          }
    }

data Repository = Repository
  { currentIdVar :: IORef Int,
    driverPoolVar :: IORef (Map (Id SRB.Booking) SortedDriverPool),
    bookingsVar :: IORef (Map (Id SRB.Booking) RideInfo),
    rideRequestsVar :: IORef (Map (Id SRR.RideRequest) RideRequest),
    notificationStatusVar :: IORef NotificationStatusMap,
    assignmentsVar :: IORef [(Id SRB.Booking, Id Driver)],
    onRideVar :: IORef [Id Driver]
  }

initRepository :: IO Repository
initRepository = do
  initCurrentId <- newIORef 1
  initDriverPool <- newIORef Map.empty
  initRides <- newIORef Map.empty
  initRideRequest <- newIORef Map.empty
  initNotificationStatus <- newIORef Map.empty
  initAssignments <- newIORef []
  initOnRide <- newIORef []
  let repository =
        Repository
          initCurrentId
          initDriverPool
          initRides
          initRideRequest
          initNotificationStatus
          initAssignments
          initOnRide
  pure repository
