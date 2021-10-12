{-# LANGUAGE OverloadedLabels #-}

module Flow.Allocation where

import Beckn.Types.Id
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map as Map
import Data.Time (NominalDiffTime, UTCTime, addUTCTime)
import qualified Data.Time as Time
import qualified EulerHS.Language as L
import EulerHS.Prelude hiding (id)
import Services.Allocation.Allocation
import Test.Tasty
import Test.Tasty.HUnit
import qualified Types.API.Ride as Ride
import Types.App
import Types.Metrics
import Types.Storage.Organization
import Types.Storage.Person
import qualified Types.Storage.RideRequest as SRR
import Utils.Common
import Utils.SilentLogger ()

type NotificationStatusMap = (Map (Id Ride, Id Driver) (NotificationStatus, UTCTime))

org1 :: ShortId Organization
org1 = ShortId "Org1"

numRequestsToProcess :: Integer
numRequestsToProcess = 10

ride01Id :: Id Ride
ride01Id = Id "ride01"

ride02Id :: Id Ride
ride02Id = Id "ride02"

allocationTime :: Seconds
allocationTime = 120

notificationTime :: Seconds
notificationTime = 15

isNotified :: UTCTime -> (NotificationStatus, UTCTime) -> Bool
isNotified currentTime (Notified, expiryTime) = expiryTime > currentTime
isNotified _ _ = False

attemptedNotification ::
  Id Ride ->
  (Id Ride, Id Driver) ->
  (NotificationStatus, UTCTime) ->
  Bool
attemptedNotification rideId (id, _) (status, _) =
  id == rideId && (status == Rejected || status == Ignored)

addRide :: Repository -> Id Ride -> IO ()
addRide Repository {..} rideId = do
  currTime <- Time.getCurrentTime
  let rideInfo =
        RideInfo
          { rideId = rideId,
            rideStatus = Confirmed,
            orderTime = OrderTime currTime
          }
  modifyIORef ridesVar $ Map.insert rideId rideInfo

addRequest :: RequestData -> Repository -> Id Ride -> IO ()
addRequest requestData Repository {..} rideId = do
  currentId <- readIORef currentIdVar
  let requestId = Id $ show currentId
  currTime <- Time.getCurrentTime
  let request =
        RideRequest
          { requestId = requestId,
            rideId = rideId,
            requestData = requestData
          }
  modifyIORef currentIdVar (+ 1)
  modifyIORef rideRequestsVar $ Map.insert requestId request

addResponse :: Repository -> Id Ride -> Id Driver -> Ride.NotificationStatus -> IO ()
addResponse repository@Repository {..} rideId driverId status = do
  currentTime <- Time.getCurrentTime
  let driverResponse = Ride.DriverResponse driverId status
  addRequest (DriverResponse driverResponse) repository rideId

checkRideStatus :: Repository -> Id Ride -> RideStatus -> IO ()
checkRideStatus Repository {..} rideId expectedStatus = do
  rides <- readIORef ridesVar
  case Map.lookup ride01Id rides of
    Nothing -> assertFailure $ "Ride " <> show (rideId.getId) <> " not found"
    Just rideInfo -> rideInfo.rideStatus @?= expectedStatus

addNotification ::
  Id Ride ->
  UTCTime ->
  NotificationStatusMap ->
  Id Driver ->
  NotificationStatusMap
addNotification rideId expiryTime notificationStatuses driverId =
  Map.insert
    (rideId, driverId)
    (Notified, expiryTime)
    notificationStatuses

updateNotification ::
  Id Ride ->
  NotificationStatus ->
  NotificationStatusMap ->
  Id Driver ->
  NotificationStatusMap
updateNotification rideId nStatus notificationStatuses driverId =
  Map.adjust
    (\(_, expiryTime) -> (nStatus, expiryTime))
    (rideId, driverId)
    notificationStatuses

toCurrentNotification :: ((Id Ride, Id Driver), (NotificationStatus, UTCTime)) -> CurrentNotification
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
      assignDriver = \rideId driverId -> do
        modifyIORef assignmentsVar $ (:) (rideId, driverId)
        modifyIORef ridesVar $ Map.adjust (#rideStatus .~ Assigned) rideId,
      cancelRide = \rideId _ -> modifyIORef ridesVar $ Map.adjust (#rideStatus .~ Cancelled) rideId,
      cleanupNotifications = \rideId ->
        modifyIORef notificationStatusVar $ Map.filterWithKey (\(r, _) _ -> r /= rideId),
      getTopDriversByIdleTime = \count driverIds -> pure $ take count driverIds,
      checkAvailability = pure . NonEmpty.toList,
      sendRideNotAssignedNotification = \_ _ -> pure (),
      removeRequest = modifyIORef rideRequestsVar . Map.delete,
      addAllocationRequest = \_ -> addRequest Allocation repository,
      getRideInfo = \rideId -> do
        rides <- readIORef ridesVar
        case Map.lookup rideId rides of
          Just rideInfo -> pure rideInfo
          Nothing -> assertFailure $ "Ride " <> show rideId <> " not found in the map.",
      logEvent = \_ _ -> pure (),
      logDriverEvents = \_ _ _ -> pure (),
      metrics =
        BTMMetricsHandle
          { incrementTaskCounter = return (),
            incrementFailedTaskCounter = return (),
            putTaskDuration = \_ -> return ()
          }
    }

driverPool1 :: [Id Driver]
driverPool1 = [Id "driver01", Id "driver02", Id "driver03"]

driverPool2 :: [Id Driver]
driverPool2 = [Id "driver05", Id "driver07", Id "driver08"]

driverPoolPerRide :: Map (Id Ride) [Id Driver]
driverPoolPerRide = Map.fromList [(ride01Id, driverPool1), (ride02Id, driverPool2)]

data Repository = Repository
  { currentIdVar :: IORef Int,
    driverPoolVar :: IORef (Map (Id Ride) [Id Driver]),
    ridesVar :: IORef (Map (Id Ride) RideInfo),
    rideRequestsVar :: IORef (Map (Id SRR.RideRequest) RideRequest),
    notificationStatusVar :: IORef NotificationStatusMap,
    assignmentsVar :: IORef [(Id Ride, Id Driver)]
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

  currTime <- Time.getCurrentTime
  addRide r ride01Id
  addRide r ride02Id
  addRequest Allocation r ride01Id
  addRequest Allocation r ride02Id

  process (handle r) org1 numRequestsToProcess
  addResponse r ride01Id (Id "driver01") Ride.REJECT
  addResponse r ride02Id (Id "driver05") Ride.REJECT
  process (handle r) org1 numRequestsToProcess
  addResponse r ride01Id (Id "driver02") Ride.REJECT
  addResponse r ride02Id (Id "driver07") Ride.REJECT
  process (handle r) org1 numRequestsToProcess
  addResponse r ride01Id (Id "driver03") Ride.ACCEPT
  addResponse r ride02Id (Id "driver08") Ride.ACCEPT
  process (handle r) org1 numRequestsToProcess

  assignments <- readIORef assignmentsVar
  assignments @?= [(ride02Id, Id "driver08"), (ride01Id, Id "driver03")]

  checkRideStatus r ride01Id Assigned
  checkRideStatus r ride02Id Assigned

cancellationAfterAssignment :: TestTree
cancellationAfterAssignment = testCase "Cancellation after assignment" $ do
  r@Repository {..} <- initRepository

  currTime <- Time.getCurrentTime
  addRide r ride01Id
  addRequest Allocation r ride01Id

  process (handle r) org1 numRequestsToProcess
  addResponse r ride01Id (Id "driver01") Ride.ACCEPT

  process (handle r) org1 numRequestsToProcess
  checkRideStatus r ride01Id Assigned

  addRequest Cancellation r ride01Id

  process (handle r) org1 numRequestsToProcess
  checkRideStatus r ride01Id Cancelled

allocation :: TestTree
allocation =
  testGroup
    "Allocation"
    [ twoAllocations,
      cancellationAfterAssignment
    ]
