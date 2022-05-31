module Main where

import DistanceCalculation
import EulerHS.Prelude
import FareCalculator
import Flow.Allocation.AllocationTimeFinished
import Flow.Allocation.Cancellation
import Flow.Allocation.NotificationStatus
import Flow.Allocation.OnePoolTwoRide
import Flow.Allocation.Reassignment
import Flow.Allocation.TwoAllocations
import Flow.RideAPI.CancelRide (cancelRide)
import Flow.RideAPI.EndRide (endRideTests)
import Flow.RideAPI.StartRide (startRide)
import qualified LocationUpdates as LocUpd
import RentalFareCalculator
import Test.Tasty

main :: IO ()
main = do
  LocUpd.wrapTests (specs >=> defaultMain)

--  wrapTests $ \appEnv -> defaultMain $ locationUpdatesTree appEnv

specs :: LocUpd.AppEnv -> IO TestTree
specs appEnv = do
  let rideAPI = testGroup "Ride API" [startRide, endRideTests, cancelRide]
  let allocations =
        testGroup
          "Allocations"
          [ allocationTimeFinished,
            checkNotificationStatuses,
            onePoolTwoRide,
            twoAllocations,
            cancellation,
            reassignment
          ]
  return $
    testGroup
      "Unit tests"
      [ fareCalculator,
        rentalFareCalculator,
        allocations,
        rideAPI,
        distanceCalculation,
        LocUpd.locationUpdatesTree appEnv
      ]
