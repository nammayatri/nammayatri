{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}

-- | Which tickets are a shared-route run's passengers: the run's feed, its GTFS trip, and that day's start times.
module SharedRouteManifestTests (tests) where

import Data.Time (UTCTime (..), addUTCTime, fromGregorian)
import qualified "rider-app" Domain.Types.IntegratedBPPConfig as DIBC
import "beckn-spec" Domain.Types.SharedRoute (RunRef (..), runTicketWindow)
import "mobility-core" Kernel.Types.Id (Id (..))
import "rider-app" SharedLogic.SharedRoute.Manifest (isOnRun)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Prelude

feed, otherFeed :: Id DIBC.IntegratedBPPConfig
feed = Id "feed-shuttle"
otherFeed = Id "feed-bus"

-- | 07:30 UTC on 2026-11-25
firstDeparture :: UTCTime
firstDeparture = UTCTime (fromGregorian 2026 11 25) (7 * 3600 + 30 * 60)

run :: RunRef
run = RunRef {tripId = "T-0730", departure = firstDeparture}

hours :: Double -> UTCTime
hours h = addUTCTime (realToFrac (h * 3600)) firstDeparture

tests :: TestTree
tests =
  testGroup
    "shared-route run membership"
    [ testCase "the ticket window is 12 hours either side of the first departure" $
        runTicketWindow run @?= (hours (-12), hours 12),
      testCase "same feed and trip, that day's ticket: a passenger" $
        isOnRun feed run feed (Just "T-0730") (Just (hours (-5))) @?= True,
      testCase "same feed and trip, a rider boarding mid-run: a passenger" $
        isOnRun feed run feed (Just "T-0730") (Just (hours 2)) @?= True,
      testCase "same feed and trip, the next day's ticket: not a passenger" $
        isOnRun feed run feed (Just "T-0730") (Just (hours 24)) @?= False,
      testCase "same feed and trip, the previous day's ticket: not a passenger" $
        isOnRun feed run feed (Just "T-0730") (Just (hours (-24))) @?= False,
      testCase "another feed with the same trip id: not a passenger" $
        isOnRun feed run otherFeed (Just "T-0730") (Just firstDeparture) @?= False,
      testCase "another trip on the same feed: not a passenger" $
        isOnRun feed run feed (Just "T-0800") (Just firstDeparture) @?= False,
      testCase "a ticket with no trip or no start time: not a passenger" $ do
        isOnRun feed run feed Nothing (Just firstDeparture) @?= False
        isOnRun feed run feed (Just "T-0730") Nothing @?= False
    ]
