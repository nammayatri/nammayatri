{-# LANGUAGE PackageImports #-}

module SharedCabConfigTests (tests) where

import "rider-app" SharedLogic.SharedCab.Config
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Prelude

tests :: TestTree
tests =
  testGroup
    "SharedCab tunables"
    [ testCase "no rider_config row takes the defaults" $
        tunablesFrom Nothing @?= defaultTunables,
      testCase "defaults are the 05 §10 / 04 §3 values the engine was built on" $
        defaultTunables
          @?= SharedCabTunables
            { allocationWindowSec = 480,
              atStopRadiusM = 100,
              walkBufferSec = 60,
              standTimerSec = 180,
              movingTimerSec = 90,
              maxAttempts = 2,
              fallbackAfterSec = 600,
              noCabGraceSec = 120,
              findingTimeoutSec = 1200,
              tickSec = 3,
              ltsMaxAgeSec = 60,
              autoEndAfterDropSec = 600,
              degradedTimeoutSec = 3600,
              offRouteMeters = 300,
              offRouteSec = 120
            }
    ]
