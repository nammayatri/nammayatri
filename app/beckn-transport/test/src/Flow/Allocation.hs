{-# LANGUAGE OverloadedLabels #-}

module Flow.Allocation where

import qualified Beckn.Types.Storage.ProductInstance as PI
import qualified Control.Concurrent.MVar as MVar
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map as Map
import qualified Data.Time as Time
import EulerHS.Prelude
import qualified Services.Allocation.Allocation as Allocation
import Test.Tasty
import Test.Tasty.HUnit
import Types.API.Ride
import Types.App
import Utils.Time

numRequestsToProcess :: Integer
numRequestsToProcess = 10

request01Id :: RideRequestId
request01Id = RideRequestId "request01"

request02Id :: RideRequestId
request02Id = RideRequestId "request02"

ride01Id :: RideId
ride01Id = RideId "ride01"

ride02Id :: RideId
ride02Id = RideId "ride02"

rideRequestMap :: Map RideRequestId Allocation.RideRequest
rideRequestMap =
  Map.fromList
    [ ( request01Id,
        Allocation.RideRequest
          { requestHeader =
              Allocation.RequestHeader
                { requestId = request01Id,
                  rideId = ride01Id,
                  requestTime = parseTime "2018-12-06T11:39:57.153Z"
                },
            requestData =
              Allocation.Allocation $ Allocation.OrderTime $ parseTime "2018-12-06T11:39:57.153Z"
          }
      ),
      ( request02Id,
        Allocation.RideRequest
          { requestHeader =
              Allocation.RequestHeader
                { requestId = request02Id,
                  rideId = ride02Id,
                  requestTime = parseTime "2018-12-06T11:39:57.153Z"
                },
            requestData =
              Allocation.Allocation $ Allocation.OrderTime $ parseTime "2018-12-06T11:39:57.153Z"
          }
      )
    ]

modifyTVarIO :: TVar a -> (a -> a) -> IO ()
modifyTVarIO tvar f = atomically $ modifyTVar tvar f

handle :: Repository -> Allocation.ServiceHandle IO
handle Repository {..} =
  Allocation.ServiceHandle
    { getCurrentTime = pure $ parseTime "2018-12-06T11:39:57.153Z",
      getDriverSortMode = pure ETA,
      getConfiguredNotificationTime = pure 120,
      getConfiguredAllocationTime = pure 15,
      getRequests = \numRides -> do
        rideMap <- readTVarIO rideRequests
        let rides = Map.elems rideMap
        pure $ take (fromIntegral numRides) rides,
      getDriverPool = \rideId -> do
        poolMap <- readTVarIO driverPool
        let pool = fromMaybe [] $ Map.lookup rideId poolMap
        pure pool,
      sendNewRideNotification = \rideId driverId -> do
        pure (),
      getCurrentNotification = \rideId -> do
        ns <- readTVarIO notificationStatus
        let filtered = fmap snd $ Map.keys $ Map.filterWithKey (\(r, _) s -> isNotified s && r == rideId) ns
        case filtered of
          [a] -> pure $ Just $ Allocation.CurrentNotification a $ parseTime "2018-12-06T11:39:57.153Z"
          _ -> pure Nothing,
      addNotificationStatus = \rideId driverId nStatus -> do
        now <- Time.getCurrentTime
        modifyTVarIO notificationStatus $ Map.insert (rideId, driverId) (Allocation.Notified now),
      updateNotificationStatus = \rideId driverId nStatus -> do
        now <- Time.getCurrentTime
        modifyTVarIO notificationStatus $ Map.insert (rideId, driverId) nStatus,
      resetLastRejectionTime = \rideId -> pure (),
      getAttemptedDrivers = \rideId -> do
        ns <- readTVarIO notificationStatus
        let filtered = fmap snd $ Map.keys $ Map.filterWithKey (\(r, _) _ -> r == rideId) ns
        pure filtered,
      getDriversWithNotification = do
        ns <- readTVarIO notificationStatus
        let filtered = fmap snd $ Map.keys $ Map.filter isNotified ns
        pure filtered,
      getDriverResponse = \rideId driverId -> do
        ar <- readTVarIO acceptReject
        pure $ Map.lookup (rideId, driverId) ar,
      assignDriver = \rideId driverId -> do
        _ <- modifyTVarIO acceptReject $ Map.delete (rideId, driverId)
        modifyTVarIO hasFound $ (:) (rideId, driverId),
      cancelRide = \_ -> pure (),
      cleanupRide = \rideId -> do
        _ <- modifyTVarIO rideRequests $ Map.filter (\request -> request ^. #requestHeader . #rideId == rideId)
        pure (),
      getFirstDriverInTheQueue = pure . NonEmpty.head,
      checkAvailability = pure . NonEmpty.toList,
      sendRideNotAssignedNotification = \rideId driverId -> pure (),
      resetRequestTime = \_ -> pure (),
      runSafely = (Right <$>),
      logInfo = \_ _ -> pure (),
      logError = \_ _ -> pure (),
      logEvent = \_ _ -> pure (),
      getRideStatus = \rideId -> return Allocation.CONFIRMED
    }

driverPool1 :: [DriverId]
driverPool1 = [DriverId "driver01", DriverId "driver02", DriverId "driver03"]

driverPool2 :: [DriverId]
driverPool2 = [DriverId "driver05", DriverId "driver07", DriverId "driver08"]

driverPoolPerRide :: Map RideId [DriverId]
driverPoolPerRide = Map.fromList [(ride01Id, driverPool1), (ride02Id, driverPool2)]

isNotified :: Allocation.NotificationStatus -> Bool
isNotified (Allocation.Notified _) = True
isNotified _ = False

data Repository = Repository
  { driverPool :: TVar (Map RideId [DriverId]),
    rideRequests :: TVar (Map RideRequestId Allocation.RideRequest),
    notificationStatus :: TVar (Map (RideId, DriverId) Allocation.NotificationStatus),
    hasFound :: TVar [(RideId, DriverId)],
    acceptReject :: TVar (Map (RideId, DriverId) DriverResponse)
  }

initRepository :: Map RideRequestId Allocation.RideRequest -> IO Repository
initRepository rideRequests = do
  initDriverPool <- newTVarIO driverPoolPerRide
  initRideRequest <- newTVarIO rideRequests
  initNotificationStatus <- newTVarIO Map.empty
  initHasFound <- newTVarIO []
  initAcceptReject <- newTVarIO Map.empty
  pure $ Repository initDriverPool initRideRequest initNotificationStatus initHasFound initAcceptReject

allocateDriverForRide :: TestTree
allocateDriverForRide = testCase "Find a driver for ride" $ do
  r@Repository {..} <- initRepository rideRequestMap

  Allocation.process (handle r) numRequestsToProcess >>= \numProcessed -> numProcessed @?= 2
  modifyTVarIO acceptReject $ Map.insert (ride01Id, DriverId "driver01") rejectResponse
  modifyTVarIO acceptReject $ Map.insert (ride02Id, DriverId "driver05") rejectResponse
  Allocation.process (handle r) numRequestsToProcess >>= \numProcessed -> numProcessed @?= 2
  modifyTVarIO acceptReject $ Map.insert (ride01Id, DriverId "driver02") rejectResponse
  modifyTVarIO acceptReject $ Map.insert (ride02Id, DriverId "driver07") rejectResponse
  modifyTVarIO acceptReject $ Map.insert (ride01Id, DriverId "driver03") acceptResponse
  Allocation.process (handle r) numRequestsToProcess >>= \numProcessed -> numProcessed @?= 2
  modifyTVarIO acceptReject $ Map.insert (ride02Id, DriverId "driver08") acceptResponse
  Allocation.process (handle r) numRequestsToProcess >>= \numProcessed -> numProcessed @?= 2

  found <- readTVarIO hasFound
  found @?= [(ride02Id, DriverId "driver08"), (ride01Id, DriverId "driver03")]
  where
    rejectResponse = DriverResponse REJECT $ parseTime "2021-26-02T12:40:00.000Z"
    acceptResponse = DriverResponse ACCEPT $ parseTime "2021-26-02T12:40:00.000Z"

allocation :: TestTree
allocation =
  testGroup
    "Allocation"
    [ allocateDriverForRide
    ]
