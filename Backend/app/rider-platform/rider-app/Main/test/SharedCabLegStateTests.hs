{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}

module SharedCabLegStateTests (tests) where

import "rider-app" SharedLogic.SharedCab.LegState
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Prelude

tests :: TestTree
tests =
  testGroup
    "shared-cab leg state"
    [ testGroup
        "search gate: shared-cab leg detection"
        [ testCase "SHARED_CAB agency" $ isSharedCabAgency "shillong_shared_cab:SHARED_CAB" @?= True,
          testCase "bus agency" $ isSharedCabAgency "chennai_bus:MTC" @?= False,
          testCase "bare SHARED_CAB id" $ isSharedCabAgency "SHARED_CAB" @?= True
        ]
    ]
