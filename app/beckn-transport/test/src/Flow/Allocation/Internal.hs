{-# LANGUAGE OverloadedLabels #-}

module Flow.Allocation.Internal where

import Beckn.Types.Id
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map as Map
import qualified Data.Time as Time
import EulerHS.Prelude hiding (id)
import Services.Allocation.Allocation
import Test.Tasty.HUnit
import qualified Types.API.RideBooking as RideBooking
import Types.App
import Types.Storage.Organization
import qualified Types.Storage.RideBooking as SRB
import qualified Types.Storage.RideRequest as SRR
import Utils.Common
import Utils.GuidGenerator ()
import Utils.SilentLogger ()

type NotificationStatusMap = (Map (Id SRB.RideBooking, Id Driver) (NotificationStatus, UTCTime))

org1 :: ShortId Organization
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
  Id SRB.RideBooking ->
  (Id SRB.RideBooking, Id Driver) ->
  (NotificationStatus, UTCTime) ->
  Bool
attemptedNotification rideBookingId (id, _) (status, _) =
  id == rideBookingId && status `elem` [Rejected, Ignored]

addRideBooking :: Repository -> Id SRB.RideBooking -> Int -> IO ()
addRideBooking Repository {..} rideBookingId reallocationsCount = do
  currTime <- Time.getCurrentTime
  let rideInfo =
        RideInfo
          { rideBookingId = rideBookingId,
            rideStatus = Confirmed,
            orderTime = OrderTime currTime,
            reallocationsCount = reallocationsCount
          }
  modifyIORef rideBookingsVar $ Map.insert rideBookingId rideInfo

updateRideBooking :: Repository -> Id SRB.RideBooking -> RideStatus -> IO ()
updateRideBooking Repository {..} rideBookingId status = do
  modifyIORef rideBookingsVar $ Map.adjust (\rideInfo -> rideInfo {rideStatus = status}) rideBookingId

addRequest :: RequestData -> Repository -> Id SRB.RideBooking -> IO ()
addRequest requestData Repository {..} rideBookingId = do
  currentId <- readIORef currentIdVar
  let requestId = Id $ show currentId
  let request =
        RideRequest
          { requestId = requestId,
            rideBookingId = rideBookingId,
            requestData = requestData
          }
  modifyIORef currentIdVar (+ 1)
  modifyIORef rideRequestsVar $ Map.insert requestId request

addResponse :: Repository -> Id SRB.RideBooking -> Id Driver -> RideBooking.NotificationStatus -> IO ()
addResponse repository@Repository {..} rideBookingId driverId status = do
  let driverResponse = RideBooking.DriverResponse driverId status
  addRequest (DriverResponse driverResponse) repository rideBookingId

addDriverPool :: Repository -> Map (Id SRB.RideBooking) [Id Driver] -> IO ()
addDriverPool Repository {..} dP = do
  writeIORef driverPoolVar dP

addDriverInPool :: Repository -> Id SRB.RideBooking -> Id Driver -> IO ()
addDriverInPool Repository {..} rideId driverId = do
  driversPool <- readIORef driverPoolVar
  let newDriversPool = Map.adjust ((:) driverId) rideId driversPool
  writeIORef driverPoolVar newDriversPool

checkRideStatus :: Repository -> Id SRB.RideBooking -> RideStatus -> IO ()
checkRideStatus Repository {..} rideBookingId expectedStatus = do
  rideBookings <- readIORef rideBookingsVar
  case Map.lookup rideBookingId rideBookings of
    Nothing -> assertFailure $ "RideBooking " <> show (rideBookingId.getId) <> " not found"
    Just rideInfo -> rideInfo.rideStatus @?= expectedStatus

checkNotificationStatus :: Repository -> Id SRB.RideBooking -> Id Driver -> NotificationStatus -> IO ()
checkNotificationStatus Repository {..} rideBookingId driverId expectedStatus = do
  notifications <- readIORef notificationStatusVar
  case Map.lookup (rideBookingId, driverId) notifications of
    Nothing -> assertFailure " not found"
    Just (status, _) -> status @?= expectedStatus

checkFreeNotificationStatus :: Repository -> Id SRB.RideBooking -> Id Driver -> IO ()
checkFreeNotificationStatus Repository {..} rideBookingId driverId = do
  notifications <- readIORef notificationStatusVar
  Map.lookup (rideBookingId, driverId) notifications @?= Nothing

addNotification ::
  Id SRB.RideBooking ->
  UTCTime ->
  NotificationStatusMap ->
  Id Driver ->
  NotificationStatusMap
addNotification rideBookingId expiryTime notificationStatuses driverId =
  Map.insert
    (rideBookingId, driverId)
    (Notified, expiryTime)
    notificationStatuses

updateNotification ::
  Id SRB.RideBooking ->
  NotificationStatus ->
  NotificationStatusMap ->
  Id Driver ->
  NotificationStatusMap
updateNotification rideBookingId nStatus notificationStatuses driverId =
  Map.adjust
    (\(_, expiryTime) -> (nStatus, expiryTime))
    (rideBookingId, driverId)
    notificationStatuses

toCurrentNotification :: ((Id SRB.RideBooking, Id Driver), (NotificationStatus, UTCTime)) -> CurrentNotification
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
      getDriverPool = \rideId -> do
        poolMap <- readIORef driverPoolVar
        let pool = fromMaybe [] $ Map.lookup rideId poolMap
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
        currentTime <- Time.getCurrentTime
        let filtered = fmap snd $ Map.keys $ Map.filter (isNotified currentTime) notificationStatus
        pure filtered,
      assignDriver = \rideBookingId driverId -> do
        modifyIORef assignmentsVar $ (:) (rideBookingId, driverId)
        modifyIORef rideBookingsVar $ Map.adjust (#rideStatus .~ Assigned) rideBookingId
        modifyIORef onRideVar $ (:) driverId,
      cancelRideBooking = \rideBookingId _ -> do
        modifyIORef rideBookingsVar $ Map.adjust (#rideStatus .~ Cancelled) rideBookingId
        assignments <- readIORef assignmentsVar
        let driversForRideBookingId = foldl (\acc x -> if fst x == rideBookingId then (snd x) : acc else acc) [] assignments
        modifyIORef onRideVar $ filter (`notElem` driversForRideBookingId),
      cleanupNotifications = \rideId ->
        modifyIORef notificationStatusVar $ Map.filterWithKey (\(r, _) _ -> r /= rideId),
      getTopDriversByIdleTime = \count driverIds -> pure $ take count driverIds,
      checkAvailability = \draversIDs -> do
        onRide <- readIORef onRideVar
        pure (filter (`notElem` onRide) $ NonEmpty.toList draversIDs),
      sendRideNotAssignedNotification = \_ _ -> pure (),
      removeRequest = modifyIORef rideRequestsVar . Map.delete,
      addAllocationRequest = \_ -> addRequest Allocation repository,
      getRideInfo = \rideBookingId -> do
        rideBookings <- readIORef rideBookingsVar
        case Map.lookup rideBookingId rideBookings of
          Just rideInfo -> pure rideInfo
          Nothing -> assertFailure $ "RideBooking " <> show rideBookingId <> " not found in the map.",
      logEvent = \_ _ -> pure (),
      logDriverEvents = \_ _ _ -> pure (),
      metrics =
        AllocatorMetricsHandle
          { incrementTaskCounter = return (),
            incrementFailedTaskCounter = return (),
            putTaskDuration = \_ -> return (),
            incrementErrorCounter = \_ -> return ()
          }
    }

data Repository = Repository
  { currentIdVar :: IORef Int,
    driverPoolVar :: IORef (Map (Id SRB.RideBooking) [Id Driver]),
    rideBookingsVar :: IORef (Map (Id SRB.RideBooking) RideInfo),
    rideRequestsVar :: IORef (Map (Id SRR.RideRequest) RideRequest),
    notificationStatusVar :: IORef NotificationStatusMap,
    assignmentsVar :: IORef [(Id SRB.RideBooking, Id Driver)],
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
