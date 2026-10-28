{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}

module SharedCabNoShowTests (tests) where

import SharedLogic.SharedCab.Allocation.Types (Blame (..))
import SharedLogic.SharedCab.Config (SharedCabTunables (..), tunablesFrom)
import SharedLogic.SharedCab.NoShow (NoShowAction (..), actionAfterClose, afterNoShow, noShowsAfter)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Prelude

tests :: TestTree
tests =
  testGroup
    "SharedCab no-show cap"
    [ testCase "only a rider no-show moves the booking's counter" $
        map (`noShowsAfter` 2) [BlameRider, BlameDriver, BlameNone] @?= [3, 2, 2],
      testGroup
        "reallocate or auto-cancel at the no-show cap"
        [ testCase "below the cap: reallocate" $ afterNoShow 2 1 @?= Reallocate,
          testCase "at the cap: cancel" $ afterNoShow 2 2 @?= AutoCancel,
          testCase "above the cap: cancel" $ afterNoShow 2 3 @?= AutoCancel,
          testCase "cap 1: the first no-show cancels" $ afterNoShow 1 1 @?= AutoCancel,
          testCase "a missing rider_config value takes the default cap (2)" $ afterNoShow (maxNoShows (tunablesFrom Nothing)) 1 @?= Reallocate,
          testCase "the default cap cancels at 2" $ afterNoShow (maxNoShows (tunablesFrom Nothing)) 2 @?= AutoCancel
        ],
      testGroup
        "what a close does, by blame (count before the close, cap 2)"
        [ testCase "rider no-show taking the count from 0 to 1: reallocate" $ actionAfterClose BlameRider 2 0 @?= Reallocate,
          testCase "rider no-show taking the count from 1 to 2: cancel" $ actionAfterClose BlameRider 2 1 @?= AutoCancel,
          testCase "rider no-show past the cap: cancel" $ actionAfterClose BlameRider 2 5 @?= AutoCancel,
          testCase "driver blame never cancels, even at the cap" $ actionAfterClose BlameDriver 2 2 @?= Reallocate,
          testCase "no blame never cancels, even past the cap" $ actionAfterClose BlameNone 2 9 @?= Reallocate
        ]
    ]
