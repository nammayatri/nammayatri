{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}

module SharedCabConfigTests (tests) where

import Data.Text (Text)
import "rider-app" Domain.Types.IntegratedBPPConfig (PlatformType (..))
import "rider-app" SharedLogic.SharedCab.Config
import "rider-app" Storage.Queries.IntegratedBPPConfigExtra (pickAgencyRow)
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
              maxNoShows = 2,
              fallbackAfterSec = 600,
              noCabGraceSec = 120,
              findingTimeoutSec = 1200,
              tickSec = 3,
              ltsMaxAgeSec = 60,
              autoEndAfterDropSec = 600,
              degradedTimeoutSec = 3600,
              offRouteMeters = 300,
              offRouteSec = 120,
              boardProximityM = 150,
              boardAttemptsPer10Min = 5,
              noLocationSpotBookingsPerVehiclePerDay = 5
            },
      testGroup
        "agency key shared by several integrated configs"
        [ testCase "the APPLICATION row wins, whatever order the DB lists them in" $
            map (fmap snd . pickAgencyRow fst) [[((MULTIMODAL, "a9"), "multi" :: String), ((APPLICATION, "52"), "app")], [((APPLICATION, "52"), "app"), ((MULTIMODAL, "a9"), "multi")]] @?= [Just "app", Just "app"],
          testCase "without an APPLICATION row the lowest id is picked, deterministically" $
            fmap snd (pickAgencyRow fst [((MULTIMODAL, "b"), "second" :: String), ((PARTNERORG, "a"), "first")]) @?= Just "first",
          testCase "no rows is Nothing" $
            fmap snd (pickAgencyRow fst ([] :: [((PlatformType, Text), String)])) @?= Nothing
        ]
    ]
