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
import SharedLogic.SharedCab.Config (SharedCabTunables (..), tunablesFrom)
import SharedLogic.SharedCab.Misses (Charge (..), NoShowAction (..), actionAfterClose, afterNoShow, chargeFor, noShowsAfter)
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
        map (\b -> noShowsAfter b 2) [BlameRider, BlameDriver, BlameNone] @?= [3, 2, 2],
      testGroup
        "R54: reallocate or auto-cancel at the no-show cap"
        [ testCase "below the cap: reallocate" $ afterNoShow 2 1 @?= Reallocate,
          testCase "at the cap: cancel" $ afterNoShow 2 2 @?= AutoCancel,
          testCase "above the cap: cancel" $ afterNoShow 2 3 @?= AutoCancel,
          testCase "cap 1: the first no-show cancels" $ afterNoShow 1 1 @?= AutoCancel,
          testCase "a missing rider_config value takes the default cap (2)" $ afterNoShow (maxNoShows (tunablesFrom Nothing)) 1 @?= Reallocate,
          testCase "the default cap cancels at 2" $ afterNoShow (maxNoShows (tunablesFrom Nothing)) 2 @?= AutoCancel
        ],
      testGroup
        "R54: what a close does, by blame (count before the close, cap 2)"
        [ testCase "rider no-show taking the count from 0 to 1: reallocate" $ actionAfterClose BlameRider 2 0 @?= Reallocate,
          testCase "rider no-show taking the count from 1 to 2: cancel" $ actionAfterClose BlameRider 2 1 @?= AutoCancel,
          testCase "rider no-show past the cap: cancel" $ actionAfterClose BlameRider 2 5 @?= AutoCancel,
          testCase "driver blame never cancels, even at the cap" $ actionAfterClose BlameDriver 2 2 @?= Reallocate,
          testCase "no blame never cancels, even past the cap" $ actionAfterClose BlameNone 2 9 @?= Reallocate
        ]
    ]
