{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}

module SharedCabConfigTests (tests) where

import Control.Applicative ((<|>))
import Control.Monad (mfilter)
import Data.Text (Text)
import "rider-app" Domain.Types.IntegratedBPPConfig (PlatformType (..))
import "rider-app" SharedLogic.SharedCab.Config
import "rider-app" SharedLogic.SharedCab.Enabled (cabItineraryShown, sharedCabEnabledDecision, tickChainContinues)
import "rider-app" SharedLogic.SharedCab.Schedule (ChainName (..), ChainVerdict (..), NextStep (..), SeedSource (..), WatchPlan (..), allocationSeedWanted, chainVerdict, chainsSeededBy, nextStepFor, watchPlan)
import "rider-app" Storage.Queries.IntegratedBPPConfigExtra (onlyOfPlatform, pickAgencyRow)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Prelude

tests :: TestTree
tests =
  testGroup
    "SharedCab tunables"
    [ testCase "no rider_config row takes the defaults" $
        tunablesFrom Nothing @?= defaultTunables,
      testCase "per-city switch: SHARED_CAB config and not Just False" $
        [sharedCabEnabledDecision c s | c <- [True, False], s <- [Nothing, Just True, Just False]]
          @?= [True, True, False, False, False, False],
      testCase "a tick goes on only for the chain the guard names; a lapsed guard is taken back; another chain's id ends the tick" $
        [chainVerdict g m | g <- [Nothing, Just "a", Just "b"], m <- [Nothing, Just "a"]]
          @?= [Adopt, Adopt, Foreign, Mine, Foreign, Foreign],
      testCase "the expiry tick re-seeds a lost allocation chain while a booking is live, and a lost sweep chain while cabs are on shift" $
        [watchPlan True b t r | b <- [0, 2], t <- [0, 3], r <- [0, 1]]
          @?= [ WatchPlan False False,
                WatchPlan False True,
                WatchPlan False True,
                WatchPlan False True,
                WatchPlan True True,
                WatchPlan True True,
                WatchPlan True True,
                WatchPlan True True
              ],
      testCase "every chain is watched by another, and the frequent allocation tick watches the expiry chain" $
        map chainsSeededBy [AllocationChain, ExpiryChain, SweepChain] @?= [[ExpiryChain], [AllocationChain, SweepChain], [ExpiryChain]],
      testCase "a tick that could not read the guard enqueues its next tick without writing the guard" $
        map nextStepFor [True, False] @?= [RefreshGuard, EnqueueOnly],
      testCase "a suspended city with live bookings still gets its allocation chain re-seeded; new demand needs the city on" $
        ( [allocationSeedWanted Watchdog e n | e <- [True, False], n <- [0, 3]],
          [allocationSeedWanted OnDemand e n | e <- [True, False], n <- [0, 3]],
          (watchPlan False 3 0 0).seedAllocation
        )
          @?= ([False, True, False, True], [True, True, False, False], True),
      testCase "a suspended city keeps its allocation chain only while live bookings need draining" $
        [tickChainContinues e n | e <- [True, False], n <- [0, 3]] @?= [True, True, False, True],
      testCase "a suspended city offers no cab itinerary; an enabled one needs a cab on every cab leg; no cab leg is always fine" $
        [cabItineraryShown e legs | e <- [True, False], legs <- [[], [True], [True, False]]]
          @?= [True, True, False, True, False, False],
      testCase "defaults are the values the engine was built on" $
        defaultTunables
          @?= SharedCabTunables
            { allocationWindowSec = 480,
              atStopRadiusM = 100,
              standTimerSec = 180,
              movingTimerSec = 90,
              maxAttempts = 2,
              maxNoShows = 2,
              fallbackAfterSec = 600,
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
          testCase "the driver proxy takes only an APPLICATION row: a key with only a MULTIMODAL row names none" $
            [onlyOfPlatform APPLICATION (fst . fst) (pickAgencyRow APPLICATION fst [((MULTIMODAL, "a9" :: Text), "multi" :: String)]), onlyOfPlatform APPLICATION (fst . fst) (pickAgencyRow APPLICATION fst [((APPLICATION, "52"), "app"), ((MULTIMODAL, "a9"), "multi")])] @?= [Nothing, Just ((APPLICATION, "52"), "app")],
          testCase "no rows is Nothing" $
            fmap snd (pickAgencyRow APPLICATION fst ([] :: [((PlatformType, Text), String)])) @?= Nothing
        ]
    ]
  where
    rows :: [[((PlatformType, Text), String)]]
    rows = [[((MULTIMODAL, "a9"), "multi"), ((APPLICATION, "52"), "app")], [((APPLICATION, "52"), "app"), ((MULTIMODAL, "a9"), "multi")]]
    rowsOfTwo :: [((PlatformType, Text), String)]
    rowsOfTwo = head rows
