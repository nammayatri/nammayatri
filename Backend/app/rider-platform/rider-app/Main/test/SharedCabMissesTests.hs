{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}

-- | R18: the "who gets charged" decision (SharedLogic.SharedCab.Misses) is the only branch in the counter path --
-- everything else is plumbing (atomic increments). Falsified by swapping the BlameRider / BlameDriver arms and
-- watching the wrong counter light up.
module SharedCabMissesTests (tests) where

import qualified "rider-app" Domain.Types.Person as DP
import qualified Domain.Types.VehicleTrip as DVT
import Kernel.Prelude
import Kernel.Types.Id
import SharedLogic.SharedCab.Allocation.Types (Blame (..))
import SharedLogic.SharedCab.Misses (Charge (..), chargeFor, noShowsAfter)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Prelude

rider :: Id DP.Person
rider = Id "rider-1"

trip :: Id DVT.VehicleTrip
trip = Id "trip-1"

tests :: TestTree
tests =
  testGroup
    "SharedCab miss counters (R18 05 §8.4)"
    [ testCase "BlameRider charges the rider's no-show counter, not the trip's" $
        chargeFor BlameRider (Just rider) (Just trip) @?= Just (ChargeRider rider),
      testCase "BlameDriver charges the trip the allocation was made to, not the rider" $
        chargeFor BlameDriver (Just rider) (Just trip) @?= Just (ChargeTrip trip),
      testCase "BlameNone charges nothing, even with a rider and a trip in hand" $
        chargeFor BlameNone (Just rider) (Just trip) @?= Nothing,
      testCase "BlameRider with no booking charges nobody (never silently the driver instead)" $
        chargeFor BlameRider Nothing (Just trip) @?= Nothing,
      testCase "BlameDriver with no trip captured at claim charges nobody (never silently the rider instead)" $
        chargeFor BlameDriver (Just rider) Nothing @?= Nothing,
      testCase "only a rider no-show moves the booking's counter" $
        map (\b -> noShowsAfter b 2) [BlameRider, BlameDriver, BlameNone] @?= [3, 2, 2]
    ]
