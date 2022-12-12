module Main where

import Allocation.AllocationTimeFinished
import Allocation.Cancellation
import Allocation.NotificationStatus
import Allocation.OnePoolTwoRide
import Allocation.RadiusStep
import Allocation.Reassignment
import Allocation.TwoAllocations
import EulerHS.Prelude
import Test.Tasty

main :: IO ()
main = defaultMain =<< specs

--  wrapTests $ \appEnv -> defaultMain $ locationUpdatesTree appEnv

specs :: IO TestTree
specs = do
  return $
    testGroup
      "Allocations"
      [ allocationTimeFinished,
        checkNotificationStatuses,
        onePoolTwoRide,
        twoAllocations,
        cancellation,
        reassignment,
        radiusStep
      ]
