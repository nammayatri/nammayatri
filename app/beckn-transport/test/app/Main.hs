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
import Test.Tasty

main :: IO ()
main = defaultMain =<< specs

specs :: IO TestTree
specs = do
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
        allocations,
        rideAPI,
        distanceCalculation
      ]
