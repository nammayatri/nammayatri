module Flow.Allocation.NearestDrivers where

import Beckn.Types.Id
import qualified Data.Map as Map
import Domain.Types.DriverPool
import qualified Domain.Types.RideBooking as SRB
import EulerHS.Prelude hiding (id)
import Flow.Allocation.Internal
import Services.Allocation.Allocation
import Test.Tasty
import Test.Tasty.HUnit

-- Filtering driver pool in allocation business logic can affect sorting.
-- This tests should check, that notifications are sent to first drivers in pool, that are nearest

rideBooking01Id :: Id SRB.RideBooking
rideBooking01Id = Id "rideBooking01"

driverPool1 :: [DriverPoolItem]
driverPool1 =
  [ DriverPoolItem (Id "driver01_100m") 100,
    DriverPoolItem (Id "driver06_200m") 200,
    DriverPoolItem (Id "driver05_300m") 300,
    DriverPoolItem (Id "driver07_400m") 400,
    DriverPoolItem (Id "driver03_500m") 500,
    DriverPoolItem (Id "driver08_600m") 600,
    DriverPoolItem (Id "driver02_700m") 700,
    DriverPoolItem (Id "driver10_800m") 800,
    DriverPoolItem (Id "driver09_900m") 900,
    DriverPoolItem (Id "driver04_1000m") 1000
  ]

driverPoolPerRide :: Map (Id SRB.RideBooking) SortedDriverPool
driverPoolPerRide = Map.fromList [(rideBooking01Id, mkSortedDriverPool driverPool1)]

nearestDrivers :: TestTree
nearestDrivers = testCase "Notifications should be sent to nearest drivers" $ do
  r@Repository {..} <- initRepository
  addRideBooking r rideBooking01Id 0
  addSortedDriverPool r driverPoolPerRide
  addRequest Allocation r rideBooking01Id
  void $ process (handle r) org1 numRequestsToProcess
  checkNotificationStatus r rideBooking01Id (Id "driver01_100m") Notified
  checkNotificationStatus r rideBooking01Id (Id "driver06_200m") Notified
  checkNotificationStatus r rideBooking01Id (Id "driver05_300m") Notified
  checkNotificationStatus r rideBooking01Id (Id "driver07_400m") Notified
  checkNotificationStatus r rideBooking01Id (Id "driver03_500m") Notified
  checkFreeNotificationStatus r rideBooking01Id (Id "driver08_600m")
  checkFreeNotificationStatus r rideBooking01Id (Id "driver02_700m")
  checkFreeNotificationStatus r rideBooking01Id (Id "driver10_800m")
  checkFreeNotificationStatus r rideBooking01Id (Id "driver09_900m")
  checkFreeNotificationStatus r rideBooking01Id (Id "driver04_1000m")
