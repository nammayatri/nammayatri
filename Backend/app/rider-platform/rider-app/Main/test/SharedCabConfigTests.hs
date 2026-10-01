{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}

module SharedCabConfigTests (tests) where

import Control.Applicative ((<|>))
import Control.Monad (mfilter)
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
        "agency key shared by several integrated configs: each caller gets the row of its own platform"
        [ testCase "a journey caller (MULTIMODAL) gets the MULTIMODAL row, whatever order the DB lists them in" $
            map (fmap snd . pickAgencyRow MULTIMODAL fst) rows @?= [Just "multi", Just "multi"],
          testCase "the driver proxy (APPLICATION) gets the APPLICATION row, whatever order" $
            map (fmap snd . pickAgencyRow APPLICATION fst) rows @?= [Just "app", Just "app"],
          testCase "a key shared by several rows of the caller's platform (one per city) names no row: the caller falls back to its city lookup" $
            fmap snd (pickAgencyRow MULTIMODAL fst ([((MULTIMODAL, "a"), "chennai"), ((MULTIMODAL, "b"), "delhi"), ((APPLICATION, "c"), "app")] :: [((PlatformType, Text), String)])) @?= Nothing,
          testCase "a caller whose platform has no row gets the key's only row, and Nothing when several rows are left to choose from" $
            [fmap snd (pickAgencyRow PARTNERORG fst [((APPLICATION, "52"), "app")]), fmap snd (pickAgencyRow PARTNERORG fst rowsOfTwo)] @?= [Just "app", Nothing],
          testCase "an agency with only an APPLICATION row, asked by a journey caller: the pick still names it, ConfigPilot's platform filter drops it, and the caller falls back to the city lookup" $
            let onlyApp = [((APPLICATION, "52"), "app")] :: [((PlatformType, Text), String)]
                picked = pickAgencyRow MULTIMODAL fst onlyApp
                afterConfigPilotFilter = mfilter ((== MULTIMODAL) . fst . fst) picked
                cityFallback = Just (((MULTIMODAL, "b7"), "city lookup") :: ((PlatformType, Text), String))
             in (fmap snd picked, fmap snd (afterConfigPilotFilter <|> cityFallback)) @?= (Just "app", Just "city lookup"),
          testCase "no rows is Nothing" $
            fmap snd (pickAgencyRow APPLICATION fst ([] :: [((PlatformType, Text), String)])) @?= Nothing
        ]
    ]
  where
    rows :: [[((PlatformType, Text), String)]]
    rows = [[((MULTIMODAL, "a9"), "multi"), ((APPLICATION, "52"), "app")], [((APPLICATION, "52"), "app"), ((MULTIMODAL, "a9"), "multi")]]
    rowsOfTwo :: [((PlatformType, Text), String)]
    rowsOfTwo = head rows
