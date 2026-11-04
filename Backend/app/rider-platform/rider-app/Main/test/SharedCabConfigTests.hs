{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}

module SharedCabConfigTests (tests) where

import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import "rider-app" Domain.Types.IntegratedBPPConfig (PlatformType (..))
import "rider-app" Domain.Types.RiderConfig (SharedCabCallMode (..))
import "rider-app" SharedLogic.IntegratedBPPConfig (newestNonCab, onlyRowOf)
import "rider-app" SharedLogic.SharedCab.Config
import "rider-app" SharedLogic.SharedCab.Enabled (cabItineraryShown, sharedCabEnabledDecision, tickChainContinues)
import "rider-app" SharedLogic.SharedCab.LegState (SharedCabState (..), legNeedsChains)
import "rider-app" SharedLogic.SharedCab.Schedule (ChainVerdict (..), SeedSource (..), WatchPlan (..), allocationSeedWanted, chainVerdict, watchPlan)
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
      testCase "the bus fleet's single config is the newest row that is not the shared-cab feed, and a city without a cab row is unchanged" $
        ( newestNonCab fst snd [("chennai_a", t 1), ("chennai_b", t 3), ("chennai_c", t 2)],
          newestNonCab fst snd [("chennai_a", t 1), ("shillong_shared_cab:SHARED_CAB", t 9), ("chennai_c", t 2)],
          newestNonCab fst snd [("shillong_shared_cab:SHARED_CAB", t 9)],
          newestNonCab fst snd ([] :: [(Text, UTCTime)])
        )
          @?= (Just ("chennai_b", t 3), Just ("chennai_c", t 2), Nothing, Nothing),
      testCase "a city with only the shared-cab row has no bus config (the nearby-bus lookup answers empty, not an error)" $
        ( newestNonCab fst snd [("shillong_shared_cab:SHARED_CAB", t 9)],
          newestNonCab fst snd [("shillong_shared_cab:SHARED_CAB", t 9), ("shillong_bus", t 1)]
        )
          @?= (Nothing, Just ("shillong_bus", t 1)),
      testCase "a rider polling a leg that is not over keeps the city's chains alive; a finished or cancelled leg does not" $
        map legNeedsChains [FINDING, ALLOCATED, FALLBACK, BOARDED, DEGRADED, DROPPED, CANCELLED]
          @?= [True, True, True, True, True, False, False],
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
              boardPingMaxAgeSec = 10,
              boardAttemptsPer10Min = 5,
              noLocationSpotBookingsPerVehiclePerDay = 5,
              maxCabSpeedMps = 14,
              callEnabled = False,
              callMode = MASKED,
              chatEnabled = False,
              pickEnabled = False,
              interestWindowSec = 600,
              demandStopsPerRoute = 5,
              maxDemandRoutes = 50,
              driverAcceptEnabled = False,
              offerTimeoutSec = 30,
              offerRetryTimeoutSec = 15
            },
      testGroup
        "agency key shared by several integrated configs: the shared-cab lookup takes exactly one row of its platform"
        [ testCase "a journey (MULTIMODAL) and the driver proxy (APPLICATION) each get their own row, whatever order the DB lists them in" $
            [map (fmap snd . onlyRowOf MULTIMODAL (fst . fst)) rows, map (fmap snd . onlyRowOf APPLICATION (fst . fst)) rows] @?= [[Right "multi", Right "multi"], [Right "app", Right "app"]],
          testCase "two rows of the platform is an error, not a guess" $
            fmap snd (onlyRowOf MULTIMODAL (fst . fst) ([((MULTIMODAL, "a"), "chennai"), ((MULTIMODAL, "b"), "delhi"), ((APPLICATION, "c"), "app")] :: [((PlatformType, Text), String)])) @?= Left 2,
          testCase "no row of the platform is not found, even when the key has another platform's row" $
            [fmap snd (onlyRowOf APPLICATION (fst . fst) [((MULTIMODAL, "a9" :: Text), "multi" :: String)]), fmap snd (onlyRowOf APPLICATION (fst . fst) ([] :: [((PlatformType, Text), String)]))] @?= [Left 0, Left 0]
        ]
    ]
  where
    rows :: [[((PlatformType, Text), String)]]
    rows = [[((MULTIMODAL, "a9"), "multi"), ((APPLICATION, "52"), "app")], [((APPLICATION, "52"), "app"), ((MULTIMODAL, "a9"), "multi")]]

t :: Int -> UTCTime
t = posixSecondsToUTCTime . fromIntegral
