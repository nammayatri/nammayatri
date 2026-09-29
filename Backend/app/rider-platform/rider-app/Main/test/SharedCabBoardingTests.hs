{-# LANGUAGE PackageImports #-}

module SharedCabBoardingTests (tests) where

import "rider-app" SharedLogic.SharedCab.Boarding (UnknownCodeStep (..), canBoard, forceHonoured, seatCheck, unknownCodeStep)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Prelude

tests :: TestTree
tests =
  testGroup
    "SharedCab boarding seat guard"
    [ testCase "allocated here: our held seat is credited back" $
        seatCheck 4 3 0 1 @?= True,
      testCase "FINDING into a full cab: refused" $
        seatCheck 4 3 1 1 @?= False,
      testCase "boarding the cab we are allocated to, when it is exactly full with us" $
        canBoard 4 3 1 True 1 @?= True,
      testCase "the same count for a booking not on this cab: full" $
        canBoard 4 3 1 False 1 @?= False,
      testGroup
        "unknown code"
        [ testCase "no confirm: asks, never boards" $
            unknownCodeStep Nothing @?= AskToConfirmUnlisted,
          testCase "forceCheckIn False: still asks" $
            unknownCodeStep (Just False) @?= AskToConfirmUnlisted,
          testCase "explicit confirm: boards unlisted" $
            unknownCodeStep (Just True) @?= BoardUnlisted
        ],
      testGroup
        "forced check-in"
        [ testCase "the allocated cab" $
            forceHonoured (Just True) (Just "ML05A1234") "ML05A1234" @?= True,
          testCase "FINDING/FALLBACK booking, no cab yet" $
            forceHonoured (Just True) Nothing "ML05A1234" @?= True,
          testCase "re-bind to another cab: never forced from home" $
            forceHonoured (Just True) (Just "ML05A9999") "ML05A1234" @?= False,
          testCase "no force asked" $
            forceHonoured Nothing Nothing "ML05A1234" @?= False
        ]
    ]
