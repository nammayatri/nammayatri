{-# LANGUAGE OverloadedLabels #-}

module Flow.Allocation where

import Beckn.Types.Id
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map as Map
import qualified Data.Time as Time
import EulerHS.Prelude hiding (id)
import Services.Allocation.Allocation
import Test.Tasty
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

rideBooking01Id :: Id SRB.RideBooking
rideBooking01Id = Id "rideBooking01"

rideBooking02Id :: Id SRB.RideBooking
rideBooking02Id = Id "rideBooking02"

allocationTime :: Seconds
allocationTime = 120

notificationTime :: Seconds
notificationTime = 15

isNotified :: UTCTime -> (NotificationStatus, UTCTime) -> Bool
isNotified currentTime (Notified, expiryTime) = expiryTime > currentTime
isNotified _ _ = False

attemptedNotification ::
  Id SRB.RideBooking ->
  (Id SRB.RideBooking, Id Driver) ->
  (NotificationStatus, UTCTime) ->
  Bool
attemptedNotification rideBookingId (id, _) (status, _) =
  id == rideBookingId && (status == Rejected || status == Ignored)

addRideBooking :: Repository -> Id SRB.RideBooking -> IO ()
addRideBooking Repository {..} rideBookingId = do
  currTime <- Time.getCurrentTime
  let rideInfo =
        RideInfo
          { rideBookingId = rideBookingId,
            rideStatus = Confirmed,
            orderTime = OrderTime currTime
          }
  modifyIORef rideBookingsVar $ Map.insert rideBookingId rideInfo

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

checkRideStatus :: Repository -> Id SRB.RideBooking -> RideStatus -> IO ()
checkRideStatus Repository {..} rideBookingId expectedStatus = do
  rideBookings <- readIORef rideBookingsVar
  case Map.lookup rideBookingId rideBookings of
    Nothing -> assertFailure $ "RideBooking " <> show (rideBookingId.getId) <> " not found"
    Just rideInfo -> rideInfo.rideStatus @?= expectedStatus

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
        currentTime <- Time.getCurrentTime
        notificationStatus <- readIORef notificationStatusVar
        let filtered =
              Map.toList $
                Map.filterWithKey
                  (\(id, _) notification -> id == rideId && isNotified currentTime notification)
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
                Map.keys $ Map.filterWithKey (attemptedNotification rideId) notificationStatus
        pure filtered,
      getDriversWithNotification = do
        currentTime <- Time.getCurrentTime
        notificationStatus <- readIORef notificationStatusVar
        let filtered = fmap snd $ Map.keys $ Map.filter (isNotified currentTime) notificationStatus
        pure filtered,
      assignDriver = \rideBookingId driverId -> do
        modifyIORef assignmentsVar $ (:) (rideBookingId, driverId)
        modifyIORef rideBookingsVar $ Map.adjust (#rideStatus .~ Assigned) rideBookingId,
      cancelRideBooking = \rideBookingId _ -> modifyIORef rideBookingsVar $ Map.adjust (#rideStatus .~ Cancelled) rideBookingId,
      cleanupNotifications = \rideId ->
        modifyIORef notificationStatusVar $ Map.filterWithKey (\(r, _) _ -> r /= rideId),
      getTopDriversByIdleTime = \count driverIds -> pure $ take count driverIds,
      checkAvailability = pure . NonEmpty.toList,
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
        BTMMetricsHandle
          { incrementTaskCounter = return (),
            incrementFailedTaskCounter = return (),
            putTaskDuration = \_ -> return (),
            incrementErrorCounter = \_ -> return ()
          }
    }

driverPool1 :: [Id Driver]
driverPool1 = [Id "driver01", Id "driver02", Id "driver03"]

driverPool2 :: [Id Driver]
driverPool2 = [Id "driver05", Id "driver07", Id "driver08"]

driverPoolPerRide :: Map (Id SRB.RideBooking) [Id Driver]
driverPoolPerRide = Map.fromList [(rideBooking01Id, driverPool1), (rideBooking02Id, driverPool2)]

data Repository = Repository
  { currentIdVar :: IORef Int,
    driverPoolVar :: IORef (Map (Id SRB.RideBooking) [Id Driver]),
    rideBookingsVar :: IORef (Map (Id SRB.RideBooking) RideInfo),
    rideRequestsVar :: IORef (Map (Id SRR.RideRequest) RideRequest),
    notificationStatusVar :: IORef NotificationStatusMap,
    assignmentsVar :: IORef [(Id SRB.RideBooking, Id Driver)]
  }

initRepository :: IO Repository
initRepository = do
  initCurrentId <- newIORef 1
  initDriverPool <- newIORef driverPoolPerRide
  initRides <- newIORef Map.empty
  initRideRequest <- newIORef Map.empty
  initNotificationStatus <- newIORef Map.empty
  initAssignments <- newIORef []

  let repository =
        Repository
          initCurrentId
          initDriverPool
          initRides
          initRideRequest
          initNotificationStatus
          initAssignments

  pure repository

twoAllocations :: TestTree
twoAllocations = testCase "Two allocations" $ do
  r@Repository {..} <- initRepository

  addRideBooking r rideBooking01Id
  addRideBooking r rideBooking02Id
  addRequest Allocation r rideBooking01Id
  addRequest Allocation r rideBooking02Id

  void $ process (handle r) org1 numRequestsToProcess
  addResponse r rideBooking01Id (Id "driver01") RideBooking.REJECT
  addResponse r rideBooking02Id (Id "driver05") RideBooking.REJECT
  void $ process (handle r) org1 numRequestsToProcess
  addResponse r rideBooking01Id (Id "driver02") RideBooking.REJECT
  addResponse r rideBooking02Id (Id "driver07") RideBooking.REJECT
  void $ process (handle r) org1 numRequestsToProcess
  addResponse r rideBooking01Id (Id "driver03") RideBooking.ACCEPT
  addResponse r rideBooking02Id (Id "driver08") RideBooking.ACCEPT
  void $ process (handle r) org1 numRequestsToProcess

  assignments <- readIORef assignmentsVar
  assignments @?= [(rideBooking02Id, Id "driver08"), (rideBooking01Id, Id "driver03")]

  checkRideStatus r rideBooking01Id Assigned
  checkRideStatus r rideBooking02Id Assigned

cancellationAfterAssignment :: TestTree
cancellationAfterAssignment = testCase "Cancellation after assignment" $ do
  r@Repository {..} <- initRepository

  addRideBooking r rideBooking01Id
  addRequest Allocation r rideBooking01Id

  void $ process (handle r) org1 numRequestsToProcess
  addResponse r rideBooking01Id (Id "driver01") RideBooking.ACCEPT

  void $ process (handle r) org1 numRequestsToProcess
  checkRideStatus r rideBooking01Id Assigned

  addRequest Cancellation r rideBooking01Id

  void $ process (handle r) org1 numRequestsToProcess
  checkRideStatus r rideBooking01Id Cancelled

allocation :: TestTree
allocation =
  testGroup
    "Allocation"
    [ twoAllocations,
      cancellationAfterAssignment
    ]
