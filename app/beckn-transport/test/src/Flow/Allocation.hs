{-# LANGUAGE OverloadedLabels #-}

module Flow.Allocation where

import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map as Map
import Data.Time (NominalDiffTime, UTCTime, addUTCTime, getCurrentTime)
import qualified Data.Time as Time
import EulerHS.Prelude hiding (id)
import qualified Services.Allocation.Allocation as Allocation
import Test.Tasty
import Test.Tasty.HUnit
import Types.API.Ride
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

isNotified :: UTCTime -> (Allocation.NotificationStatus, UTCTime) -> Bool
isNotified currentTime (Allocation.Notified, expiryTime) = expiryTime > currentTime
isNotified _ _ = False

attemptedNotification ::
  RideId ->
  (RideId, DriverId) ->
  (Allocation.NotificationStatus, UTCTime) ->
  Bool
attemptedNotification rideId (id, _) (status, _) =
  id == rideId && (status == Allocation.Rejected || status == Allocation.Ignored)

addRide :: Repository -> RideId -> IO ()
addRide Repository {..} rideId = do
  currTime <- getCurrentTime
  let rideInfo =
        Allocation.RideInfo
          { rideId = rideId,
            rideStatus = Allocation.Confirmed,
            orderTime = Allocation.OrderTime currTime
          }
  modifyTVarIO ridesVar $ Map.insert rideId rideInfo

addAllocationRequest :: Repository -> RideId -> IO ()
addAllocationRequest Repository {..} rideId = do
  currentId <- readTVarIO currentIdVar
  let requestId = RideRequestId $ show currentId
  currTime <- getCurrentTime
  let request =
        Allocation.RideRequest
          { requestHeader =
              Allocation.RequestHeader
                { requestId = requestId,
                  rideId = rideId,
                  requestTime = currTime
                },
            requestData =
              Allocation.Allocation
          }
  modifyTVarIO currentIdVar (+ 1)
  modifyTVarIO rideRequestsVar $ Map.insert requestId request

addResponse :: Repository -> RideId -> DriverId -> NotificationStatus -> IO ()
addResponse Repository {..} rideId driverId status = do
  currentTime <- getCurrentTime
  let response = DriverResponse status currentTime
  modifyTVarIO responsesVar $ Map.insert (rideId, driverId) response

handle :: Repository -> Allocation.ServiceHandle IO
handle repository@Repository {..} =
  Allocation.ServiceHandle
    { getCurrentTime = getCurrentTime,
      getDriverSortMode = pure ETA,
      getConfiguredAllocationTime = pure allocationTime,
      getConfiguredNotificationTime = pure notificationTime,
      getRequests = \numRides -> do
        rideRequests <- readTVarIO rideRequestsVar
        let requests = Map.elems rideRequests
        pure $ take (fromIntegral numRides) requests,
      getDriverPool = \rideId -> do
        poolMap <- readTVarIO driverPoolVar
        let pool = fromMaybe [] $ Map.lookup rideId poolMap
        pure pool,
      sendNewRideNotification = \_ _ -> do
        pure (),
      getCurrentNotification = \rideId -> do
        currentTime <- getCurrentTime
        notificationStatus <- readTVarIO notificationStatusVar
        let filtered =
              Map.toList $
                Map.filterWithKey
                  (\(id, _) notification -> id == rideId && isNotified currentTime notification)
                  notificationStatus
        case filtered of
          [((_, driverId), (_, expiryTime))] ->
            pure $ Just $ Allocation.CurrentNotification driverId expiryTime
          _ -> pure Nothing,
      addNotificationStatus = \rideId driverId expiryTime -> do
        modifyTVarIO notificationStatusVar $ Map.insert (rideId, driverId) (Allocation.Notified, expiryTime),
      updateNotificationStatus = \rideId driverId nStatus -> do
        modifyTVarIO notificationStatusVar $
          Map.adjust
            (\(_, expiryTime) -> (nStatus, expiryTime))
            (rideId, driverId),
      resetLastRejectionTime = \_ -> pure (),
      getAttemptedDrivers = \rideId -> do
        notificationStatus <- readTVarIO notificationStatusVar
        let filtered =
              fmap snd $
                Map.keys $ Map.filterWithKey (attemptedNotification rideId) notificationStatus
        pure filtered,
      getDriversWithNotification = do
        currentTime <- getCurrentTime
        notificationStatus <- readTVarIO notificationStatusVar
        let filtered = fmap snd $ Map.keys $ Map.filter (isNotified currentTime) notificationStatus
        pure filtered,
      getDriverResponse = \rideId driverId -> do
        responses <- readTVarIO responsesVar
        pure $ Map.lookup (rideId, driverId) responses,
      assignDriver = \rideId driverId -> do
        modifyTVarIO responsesVar $ Map.delete (rideId, driverId)
        modifyTVarIO assignmentsVar $ (:) (rideId, driverId),
      cancelRide = \_ -> pure (),
      cleanupNotifications = \rideId -> do
        modifyTVarIO notificationStatusVar $ Map.filterWithKey (\(r, _) _ -> r /= rideId)
        pure (),
      getFirstDriverInTheQueue = pure . NonEmpty.head,
      checkAvailability = pure . NonEmpty.toList,
      sendRideNotAssignedNotification = \_ _ -> pure (),
      removeRequest = \requestId -> do
        modifyTVarIO rideRequestsVar $ Map.filterWithKey (\id _ -> id /= requestId)
        pure (),
      addAllocationRequest = \rideId ->
        addAllocationRequest repository rideId,
      getRideInfo = \rideId -> do
        rides <- readTVarIO ridesVar
        case Map.lookup rideId rides of
          Just rideInfo -> pure rideInfo
          Nothing -> assertFailure $ "Ride " <> show rideId <> " not found in the map.",
      runSafely = (Right <$>),
      logInfo = \_ _ -> pure (),
      logWarning = \_ _ -> pure (),
      logError = \_ _ -> pure (),
      logEvent = \_ _ -> pure ()
    }

driverPool1 :: [DriverId]
driverPool1 = [DriverId "driver01", DriverId "driver02", DriverId "driver03"]

driverPool2 :: [DriverId]
driverPool2 = [DriverId "driver05", DriverId "driver07", DriverId "driver08"]

driverPoolPerRide :: Map RideId [DriverId]
driverPoolPerRide = Map.fromList [(ride01Id, driverPool1), (ride02Id, driverPool2)]

data Repository = Repository
  { currentIdVar :: TVar Int,
    driverPoolVar :: TVar (Map RideId [DriverId]),
    ridesVar :: TVar (Map RideId Allocation.RideInfo),
    rideRequestsVar :: TVar (Map RideRequestId Allocation.RideRequest),
    notificationStatusVar :: TVar (Map (RideId, DriverId) (Allocation.NotificationStatus, UTCTime)),
    assignmentsVar :: TVar [(RideId, DriverId)],
    responsesVar :: TVar (Map (RideId, DriverId) DriverResponse)
  }

initRepository :: IO Repository
initRepository = do
  initCurrentId <- newTVarIO 1
  initDriverPool <- newTVarIO driverPoolPerRide
  initRides <- newTVarIO Map.empty
  initRideRequest <- newTVarIO Map.empty
  initNotificationStatus <- newTVarIO Map.empty
  initHasFound <- newTVarIO []
  initAcceptReject <- newTVarIO Map.empty

  let repository =
        Repository
          initCurrentId
          initDriverPool
          initRides
          initRideRequest
          initNotificationStatus
          initHasFound
          initAcceptReject

  currTime <- getCurrentTime
  addRide repository ride01Id
  addRide repository ride02Id
  addAllocationRequest repository ride01Id
  addAllocationRequest repository ride02Id
  pure repository

allocateDriverForRide :: TestTree
allocateDriverForRide = testCase "Find a driver for ride" $ do
  r@Repository {..} <- initRepository

  Allocation.process (handle r) numRequestsToProcess >>= \numProcessed -> numProcessed @?= 2
  addResponse r ride01Id (DriverId "driver01") REJECT
  addResponse r ride02Id (DriverId "driver05") REJECT
  Allocation.process (handle r) numRequestsToProcess >>= \numProcessed -> numProcessed @?= 2
  addResponse r ride01Id (DriverId "driver02") REJECT
  addResponse r ride02Id (DriverId "driver07") REJECT
  addResponse r ride01Id (DriverId "driver03") ACCEPT
  Allocation.process (handle r) numRequestsToProcess >>= \numProcessed -> numProcessed @?= 2
  addResponse r ride02Id (DriverId "driver08") ACCEPT
  Allocation.process (handle r) numRequestsToProcess >>= \numProcessed -> numProcessed @?= 2

  assignments <- readTVarIO assignmentsVar
  assignments @?= [(ride02Id, DriverId "driver08"), (ride01Id, DriverId "driver03")]

allocation :: TestTree
allocation =
  testGroup
    "Allocation"
    [ allocateDriverForRide
    ]

modifyTVarIO :: TVar a -> (a -> a) -> IO ()
modifyTVarIO tvar f = atomically $ modifyTVar tvar f
