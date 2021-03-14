module Flow.Allocation where

import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map as Map
import Data.Time (NominalDiffTime, UTCTime, addUTCTime)
import qualified Data.Time as Time
import EulerHS.Prelude hiding (id)
import Services.Allocation.Allocation
import Test.Tasty
import Test.Tasty.HUnit
import qualified Types.API.Ride as Ride
import Types.App

numRequestsToProcess :: Integer
numRequestsToProcess = 10

ride01Id :: RideId
ride01Id = RideId "ride01"

ride02Id :: RideId
ride02Id = RideId "ride02"

allocationTime :: NominalDiffTime
allocationTime = 120

notificationTime :: NominalDiffTime
notificationTime = 15

isNotified :: UTCTime -> (NotificationStatus, UTCTime) -> Bool
isNotified currentTime (Notified, expiryTime) = expiryTime > currentTime
isNotified _ _ = False

attemptedNotification ::
  RideId ->
  (RideId, DriverId) ->
  (NotificationStatus, UTCTime) ->
  Bool
attemptedNotification rideId (id, _) (status, _) =
  id == rideId && (status == Rejected || status == Ignored)

addRide :: Repository -> RideId -> IO ()
addRide Repository {..} rideId = do
  currTime <- Time.getCurrentTime
  let rideInfo =
        RideInfo
          { rideId = rideId,
            rideStatus = Confirmed,
            orderTime = OrderTime currTime
          }
  modifyIORef ridesVar $ Map.insert rideId rideInfo

addRequest :: RequestData -> Repository -> RideId -> IO ()
addRequest requestData Repository {..} rideId = do
  currentId <- readIORef currentIdVar
  let requestId = RideRequestId $ show currentId
  currTime <- Time.getCurrentTime
  let request =
        RideRequest
          { requestHeader =
              RequestHeader
                { requestId = requestId,
                  rideId = rideId,
                  requestTime = currTime
                },
            requestData = requestData
          }
  modifyIORef currentIdVar (+ 1)
  modifyIORef rideRequestsVar $ Map.insert requestId request

addResponse :: Repository -> RideId -> DriverId -> Ride.NotificationStatus -> IO ()
addResponse Repository {..} rideId driverId status = do
  currentTime <- Time.getCurrentTime
  let response = Ride.DriverResponse status currentTime
  modifyIORef responsesVar $ Map.insert (rideId, driverId) response

handle :: Repository -> ServiceHandle IO
handle repository@Repository {..} =
  ServiceHandle
    { getCurrentTime = Time.getCurrentTime,
      getDriverSortMode = pure ETA,
      getConfiguredAllocationTime = pure allocationTime,
      getConfiguredNotificationTime = pure notificationTime,
      getRequests = \numRides -> do
        rideRequests <- readIORef rideRequestsVar
        let requests = Map.elems rideRequests
        pure $ take (fromIntegral numRides) requests,
      getDriverPool = \rideId -> do
        poolMap <- readIORef driverPoolVar
        let pool = fromMaybe [] $ Map.lookup rideId poolMap
        pure pool,
      sendNewRideNotification = \_ _ -> pure (),
      getCurrentNotification = \rideId -> do
        currentTime <- Time.getCurrentTime
        notificationStatus <- readIORef notificationStatusVar
        let filtered =
              Map.toList $
                Map.filterWithKey
                  (\(id, _) notification -> id == rideId && isNotified currentTime notification)
                  notificationStatus
        case filtered of
          [((_, driverId), (_, expiryTime))] ->
            pure $ Just $ CurrentNotification driverId expiryTime
          _ -> pure Nothing,
      addNotificationStatus = \rideId driverId expiryTime -> do
        modifyIORef notificationStatusVar $
          Map.insert (rideId, driverId) (Notified, expiryTime),
      updateNotificationStatus = \rideId driverId nStatus -> do
        modifyIORef notificationStatusVar $
          Map.adjust
            (\(_, expiryTime) -> (nStatus, expiryTime))
            (rideId, driverId),
      resetLastRejectionTime = \_ -> pure (),
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
      getDriverResponse = \rideId driverId -> do
        responses <- readIORef responsesVar
        pure $ Map.lookup (rideId, driverId) responses,
      assignDriver = \rideId driverId -> do
        modifyIORef responsesVar $ Map.delete (rideId, driverId)
        modifyIORef assignmentsVar $ (:) (rideId, driverId),
      cancelRide = \_ -> pure (),
      cleanupNotifications = \rideId ->
        modifyIORef notificationStatusVar $ Map.filterWithKey (\(r, _) _ -> r /= rideId),
      getFirstDriverInTheQueue = pure . NonEmpty.head,
      checkAvailability = pure . NonEmpty.toList,
      sendRideNotAssignedNotification = \_ _ -> pure (),
      removeRequest = modifyIORef rideRequestsVar . Map.delete,
      addAllocationRequest = addRequest Allocation repository,
      getRideInfo = \rideId -> do
        rides <- readIORef ridesVar
        case Map.lookup rideId rides of
          Just rideInfo -> pure rideInfo
          Nothing -> assertFailure $ "Ride " <> show rideId <> " not found in the map.",
      runSafely = (Right <$>),
      addLogTag = \_ action -> action,
      logEvent = \_ _ -> pure (),
      logOutput = \_ _ _ -> pure ()
    }

driverPool1 :: [DriverId]
driverPool1 = [DriverId "driver01", DriverId "driver02", DriverId "driver03"]

driverPool2 :: [DriverId]
driverPool2 = [DriverId "driver05", DriverId "driver07", DriverId "driver08"]

driverPoolPerRide :: Map RideId [DriverId]
driverPoolPerRide = Map.fromList [(ride01Id, driverPool1), (ride02Id, driverPool2)]

data Repository = Repository
  { currentIdVar :: IORef Int,
    driverPoolVar :: IORef (Map RideId [DriverId]),
    ridesVar :: IORef (Map RideId RideInfo),
    rideRequestsVar :: IORef (Map RideRequestId RideRequest),
    notificationStatusVar :: IORef (Map (RideId, DriverId) (NotificationStatus, UTCTime)),
    assignmentsVar :: IORef [(RideId, DriverId)],
    responsesVar :: IORef (Map (RideId, DriverId) Ride.DriverResponse)
  }

initRepository :: IO Repository
initRepository = do
  initCurrentId <- newIORef 1
  initDriverPool <- newIORef driverPoolPerRide
  initRides <- newIORef Map.empty
  initRideRequest <- newIORef Map.empty
  initNotificationStatus <- newIORef Map.empty
  initAssignments <- newIORef []
  initResponses <- newIORef Map.empty

  let repository =
        Repository
          initCurrentId
          initDriverPool
          initRides
          initRideRequest
          initNotificationStatus
          initAssignments
          initResponses

  currTime <- Time.getCurrentTime
  addRide repository ride01Id
  addRide repository ride02Id
  addRequest Allocation repository ride01Id
  addRequest Allocation repository ride02Id
  pure repository

twoAllocations :: TestTree
twoAllocations = testCase "Two allocations" $ do
  r@Repository {..} <- initRepository

  process (handle r) numRequestsToProcess
  addResponse r ride01Id (DriverId "driver01") Ride.REJECT
  addResponse r ride02Id (DriverId "driver05") Ride.REJECT
  process (handle r) numRequestsToProcess
  addResponse r ride01Id (DriverId "driver02") Ride.REJECT
  addResponse r ride02Id (DriverId "driver07") Ride.REJECT
  process (handle r) numRequestsToProcess
  addResponse r ride01Id (DriverId "driver03") Ride.ACCEPT
  addResponse r ride02Id (DriverId "driver08") Ride.ACCEPT
  process (handle r) numRequestsToProcess

  assignments <- readIORef assignmentsVar
  assignments @?= [(ride02Id, DriverId "driver08"), (ride01Id, DriverId "driver03")]

allocation :: TestTree
allocation =
  testGroup
    "Allocation"
    [ twoAllocations
    ]
