{-# LANGUAGE OverloadedLabels #-}

module Flow.Allocation where

import qualified Control.Concurrent.MVar as MVar
import qualified Data.Map as Map
import qualified Data.Time as Time
import EulerHS.Prelude
import qualified Services.Allocation as Allocation
import Test.Tasty
import Test.Tasty.HUnit
import Types.App
import Utils.Time

ride01Id :: Allocation.RideId
ride01Id = Allocation.RideId "ride01"

ride02Id :: Allocation.RideId
ride02Id = Allocation.RideId "ride02"

availableRides :: [Allocation.Ride]
availableRides =
  [ Allocation.Ride
      { id = ride01Id,
        orderedAt = parseTime "2018-12-06T11:39:57.153Z"
      },
    Allocation.Ride
      { id = ride02Id,
        orderedAt = parseTime "2018-12-06T11:39:57.153Z"
      }
  ]

driverPool1 :: [DriverId]
driverPool1 = [DriverId "driver01", DriverId "driver02", DriverId "driver03"]

driverPool2 :: [DriverId]
driverPool2 = [DriverId "driver05", DriverId "driver07", DriverId "driver08"]

driverPoolPerRide :: Map Allocation.RideId [DriverId]
driverPoolPerRide = Map.fromList [(ride01Id, driverPool1), (ride02Id, driverPool2)]

data Repository = Repository
  { driverPool :: TVar (Map Allocation.RideId [DriverId]),
    rideRequest :: TVar (Map Allocation.RideId Allocation.Ride),
    notificationStatus :: TVar (Map (Allocation.RideId, DriverId) Allocation.NotificationStatus),
    hasFound :: TVar [(Allocation.RideId, DriverId)],
    acceptReject :: TVar (Map (Allocation.RideId, DriverId) Allocation.DriverResponse)
  }

initRepository :: Map Allocation.RideId Allocation.Ride -> IO Repository
initRepository rides = do
  initDriverPool <- newTVarIO driverPoolPerRide
  initRideRequest <- newTVarIO rides
  initNotificationStatus <- newTVarIO Map.empty
  initHasFound <- newTVarIO []
  initAcceptReject <- newTVarIO Map.empty
  pure $ Repository initDriverPool initRideRequest initNotificationStatus initHasFound initAcceptReject

allocateDriverForRide :: TestTree
allocateDriverForRide = testCase "Find a driver for ride" $ do
  r@Repository {..} <-
    initRepository $
      Map.fromList
        [ ( ride01Id,
            Allocation.Ride
              { id = ride01Id,
                orderedAt = parseTime "2018-12-06T11:39:57.153Z"
              }
          ),
          ( ride02Id,
            Allocation.Ride
              { id = ride02Id,
                orderedAt = parseTime "2018-12-06T11:39:57.153Z"
              }
          )
        ]

  _ <- Allocation.process $ handle r
  _ <- atomically $ modifyTVar acceptReject $ Map.insert (ride01Id, DriverId "driver01") Allocation.Accept
  _ <- Allocation.process $ handle r
  _ <- atomically $ modifyTVar acceptReject $ Map.insert (ride02Id, DriverId "driver05") Allocation.Accept
  _ <- Allocation.process $ handle r
  found <- readTVarIO hasFound
  ns <- readTVarIO notificationStatus
  rides <- readTVarIO rideRequest
  found @?= [(ride02Id, DriverId "driver05"), (ride01Id, DriverId "driver01")]
  where
    handle :: Repository -> Allocation.ServiceHandle IO
    handle Repository {..} =
      Allocation.ServiceHandle
        { getCurrentTime = pure $ parseTime "2018-12-06T11:39:57.153Z",
          getDriverSortMode = pure Allocation.ETA,
          getConfiguredNotificationTime = pure 120,
          getConfiguredAllocationTime = pure 15,
          getTopRidesToAllocate = \nmbOfRides -> do
            rideMap <- readTVarIO rideRequest
            let rides = Map.elems rideMap
            pure $ take nmbOfRides rides,
          getDriverPool = \rideId -> do
            poolMap <- readTVarIO driverPool
            pure $ Map.lookup rideId poolMap,
          sendNotification = \rideId driverId -> do
            pure (),
          getCurrentNotification = \rideId -> do
            ns <- readTVarIO notificationStatus
            let filtered = fmap snd $ Map.keys $ Map.filterWithKey (\(r, _) s -> s == Allocation.Notified && r == rideId) ns
            case filtered of
              [a] -> pure $ Just $ Allocation.CurrentNotification a $ parseTime "2018-12-06T11:39:57.153Z"
              _ -> pure Nothing,
          addNotificationStatus = \rideId driverId nStatus _ -> do
            now <- Time.getCurrentTime
            atomically $ modifyTVar notificationStatus $ Map.insert (rideId, driverId) Allocation.Notified,
          updateNotificationStatus = \rideId driverId nStatus -> do
            now <- Time.getCurrentTime
            atomically $ modifyTVar notificationStatus $ Map.update (const $ Just nStatus) (rideId, driverId),
          resetLastRejectionTime = \rideId -> pure (),
          getAttemptedDrivers = \rideId -> do
            ns <- readTVarIO notificationStatus
            let filtered = fmap snd $ Map.keys $ Map.filterWithKey (\(r, _) s -> r == rideId && s == Allocation.Notified) ns
            pure filtered,
          getDriversWithNotification = do
            ns <- readTVarIO notificationStatus
            let filtered = fmap snd $ Map.keys $ Map.filter (Allocation.Notified ==) ns
            pure filtered,
          getDriverResponse = \rideId driverId -> do
            ar <- readTVarIO acceptReject
            pure $ Map.lookup (rideId, driverId) ar,
          assignDriver = \rideId driverId -> do
            _ <- atomically $ modifyTVar acceptReject $ Map.delete (rideId, driverId)
            atomically $ modifyTVar hasFound $ (:) (rideId, driverId),
          cancelRide = atomically . modifyTVar rideRequest . Map.delete,
          cleanupRide = \rideId -> do
            _ <- atomically $ modifyTVar rideRequest $ Map.delete rideId
            pure (),
          getFirstDriverInTheQueue = \driverPool -> pure $ head driverPool,
          checkAvailability = \driverPool -> pure driverPool
        }

allocation :: TestTree
allocation =
  testGroup
    "Allocation"
    [ allocateDriverForRide
    ]
