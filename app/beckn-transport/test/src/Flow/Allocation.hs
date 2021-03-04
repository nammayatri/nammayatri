{-# LANGUAGE OverloadedLabels #-}

{-# HLINT ignore "Reduce duplication" #-}

module Flow.Allocation where

import Beckn.Types.ID
import Beckn.Types.Storage.Person
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map as Map
import Data.Time (NominalDiffTime, UTCTime, addUTCTime)
import qualified Data.Time as Time
import EulerHS.Prelude hiding (id)
import Services.Allocation.Allocation
import Test.Tasty
import Test.Tasty.HUnit
import qualified Types.API.Ride as Ride
import qualified Types.Storage.RideRequest as SRR
import Types.App

numRequestsToProcess :: Integer
numRequestsToProcess = 10

ride01Id :: ID Ride
ride01Id = ID "ride01"

ride02Id :: ID Ride
ride02Id = ID "ride02"

allocationTime :: NominalDiffTime
allocationTime = 120

notificationTime :: NominalDiffTime
notificationTime = 15

isNotified :: UTCTime -> (NotificationStatus, UTCTime) -> Bool
isNotified currentTime (Notified, expiryTime) = expiryTime > currentTime
isNotified _ _ = False

attemptedNotification ::
  ID Ride ->
  (ID Ride, ID Driver) ->
  (NotificationStatus, UTCTime) ->
  Bool
attemptedNotification rideId (id, _) (status, _) =
  id == rideId && (status == Rejected || status == Ignored)

addRide :: Repository -> ID Ride -> IO ()
addRide Repository {..} rideId = do
  currTime <- Time.getCurrentTime
  let rideInfo =
        RideInfo
          { rideId = rideId,
            rideStatus = Confirmed,
            orderTime = OrderTime currTime
          }
  modifyIORef ridesVar $ Map.insert rideId rideInfo

addRequest :: RequestData -> Repository -> ID Ride -> IO ()
addRequest requestData Repository {..} rideId = do
  currentId <- readIORef currentIdVar
  let requestId = ID $ show currentId
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

addResponse :: Repository -> ID Ride -> ID Driver -> Ride.NotificationStatus -> IO ()
addResponse Repository {..} rideId driverId status = do
  currentTime <- Time.getCurrentTime
  let response = Ride.DriverResponse status currentTime
  modifyIORef responsesVar $ Map.insert (rideId, driverId) response

checkRideStatus :: Repository -> ID Ride -> RideStatus -> IO ()
checkRideStatus Repository {..} rideId expectedStatus = do
  rides <- readIORef ridesVar
  case Map.lookup ride01Id rides of
    Nothing -> assertFailure $ "Ride " <> show (rideId ^. #getId) <> " not found"
    Just rideInfo -> rideInfo ^. #rideStatus @?= expectedStatus

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
        modifyIORef assignmentsVar $ (:) (rideId, driverId)
        modifyIORef ridesVar $ Map.adjust (#rideStatus .~ Assigned) rideId,
      cancelRide = modifyIORef ridesVar . Map.adjust (#rideStatus .~ Cancelled),
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

driverPool1 :: [ID Driver]
driverPool1 = [ID "driver01", ID "driver02", ID "driver03"]

driverPool2 :: [ID Driver]
driverPool2 = [ID "driver05", ID "driver07", ID "driver08"]

driverPoolPerRide :: Map (ID Ride) [ID Driver]
driverPoolPerRide = Map.fromList [(ride01Id, driverPool1), (ride02Id, driverPool2)]

data Repository = Repository
  { currentIdVar :: IORef Int,
    driverPoolVar :: IORef (Map (ID Ride) [ID Driver]),
    ridesVar :: IORef (Map (ID Ride) RideInfo),
    rideRequestsVar :: IORef (Map (ID SRR.RideRequest) RideRequest),
    notificationStatusVar :: IORef (Map (ID Ride, ID Driver) (NotificationStatus, UTCTime)),
    assignmentsVar :: IORef [(ID Ride, ID Driver)],
    responsesVar :: IORef (Map (ID Ride, ID Driver) Ride.DriverResponse)
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

  pure repository

twoAllocations :: TestTree
twoAllocations = testCase "Two allocations" $ do
  r@Repository {..} <- initRepository

  currTime <- Time.getCurrentTime
  addRide r ride01Id
  addRide r ride02Id
  addRequest Allocation r ride01Id
  addRequest Allocation r ride02Id

  process (handle r) numRequestsToProcess
  addResponse r ride01Id (ID "driver01") Ride.REJECT
  addResponse r ride02Id (ID "driver05") Ride.REJECT
  process (handle r) numRequestsToProcess
  addResponse r ride01Id (ID "driver02") Ride.REJECT
  addResponse r ride02Id (ID "driver07") Ride.REJECT
  process (handle r) numRequestsToProcess
  addResponse r ride01Id (ID "driver03") Ride.ACCEPT
  addResponse r ride02Id (ID "driver08") Ride.ACCEPT
  process (handle r) numRequestsToProcess

  assignments <- readIORef assignmentsVar
  assignments @?= [(ride02Id, ID "driver08"), (ride01Id, ID "driver03")]

  checkRideStatus r ride01Id Assigned
  checkRideStatus r ride02Id Assigned

cancellationAfterAssignment :: TestTree
cancellationAfterAssignment = testCase "Cancellation after assignment" $ do
  r@Repository {..} <- initRepository

  currTime <- Time.getCurrentTime
  addRide r ride01Id
  addRequest Allocation r ride01Id

  process (handle r) numRequestsToProcess
  addResponse r ride01Id (ID "driver01") Ride.ACCEPT

  process (handle r) numRequestsToProcess
  checkRideStatus r ride01Id Assigned

  addRequest Cancellation r ride01Id

  process (handle r) numRequestsToProcess
  checkRideStatus r ride01Id Cancelled

allocation :: TestTree
allocation =
  testGroup
    "Allocation"
    [ twoAllocations,
      cancellationAfterAssignment
    ]
