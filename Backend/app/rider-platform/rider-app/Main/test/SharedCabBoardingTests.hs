{-# LANGUAGE PackageImports #-}

module SharedCabBoardingTests (tests) where

import "rider-app" SharedLogic.SharedCab.Boarding (canBoard, seatCheck)
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
        canBoard 4 3 1 False 1 @?= False
    ]
