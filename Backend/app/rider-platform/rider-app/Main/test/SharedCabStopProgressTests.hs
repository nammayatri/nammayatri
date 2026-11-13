{-# LANGUAGE OverloadedStrings #-}

module SharedCabStopProgressTests (tests) where

import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Data.Time (UTCTime (..), addUTCTime, fromGregorian)
import Kernel.External.Maps.Types (LatLong (..))
import SharedLogic.SharedCab.Allocation.Select (groupByKey)
import SharedLogic.SharedCab.Allocation.Types (AllocationState (..), ApproachPush (..), ApproachStage (..), Timer (..), blockingTimerDeadline)
import SharedLogic.SharedCab.Config (SharedCabTunables (..), defaultTunables)
import SharedLogic.SharedCab.StopProgress.Rules
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Prelude

cfg :: SharedCabTunables
cfg = defaultTunables

noCheckIn :: SharedCabTunables
noCheckIn = cfg {checkInLeadSec = 0}

t0 :: UTCTime
t0 = UTCTime (fromGregorian 2026 9 25) 36000

after :: Int -> UTCTime
after sec = addUTCTime (fromIntegral sec) t0

-- | Stops about 1.1 km apart due north; a latitude degree is ~111 km.
stopAt :: Int -> Bool -> StopMark
stopAt idx reached = StopMark {stopCode = T.pack ("S" <> show idx), stopIdx = idx, reached, coordinate = LatLong (25.57 + 0.01 * fromIntegral idx) 91.88}

fix :: Double -> [StopMark] -> CabFix
fix = fixOn Nothing

-- as fix, with gps-processor's is_on_stop
fixOn :: Maybe Bool -> Double -> [StopMark] -> CabFix
fixOn onStop lat stops = CabFix {position = LatLong lat 91.88, stops, onStop}

tests :: TestTree
tests =
  testGroup
    "SharedCab stop progress"
    [ testCase "the tick groups its live bookings by cab once, each cab's in order, the cab-less left out" $
        groupByKey snd [(1 :: Int, Just "A"), (2, Nothing), (3, Just "B"), (4, Just "A" :: Maybe String)]
          @?= Map.fromList [("A", [(1, Just "A"), (4, Just "A")]), ("B", [(3, Just "B")])],
      testCase "the driver route list splits one city-wide FINDING read by route; a route nobody waits on gets none" $
        let byRoute = groupByKey fst [(Just "R1", "b1"), (Nothing, "b2"), (Just "R2", "b3"), (Just "R1", "b4" :: String)]
         in (map snd (Map.findWithDefault [] "R1" byRoute), Map.findWithDefault [] "R9" byRoute)
              @?= (["b1", "b4"], []),
      testCase "board stop passed only once LTS marks it Reached" $
        ( boardStopPassed "S1" (fix 25.59 [stopAt 1 True, stopAt 2 False]),
          boardStopPassed "S1" (fix 25.58 [stopAt 1 False, stopAt 2 False]),
          boardStopPassed "S3" (fix 25.59 [stopAt 1 True, stopAt 2 False])
        )
          @?= (True, False, False),
      testCase "drop: clock starts on arrival, ride ends autoEndAfterDropSec later" $
        ( dropAction cfg t0 Nothing (Just (fix 25.595 [stopAt 2 True, stopAt 3 False])) "S2",
          dropAction cfg t0 Nothing (Just (fix 25.6295 [stopAt 5 False, stopAt 6 False])) "S6",
          dropAction cfg t0 Nothing (Just (fix 25.595 [stopAt 2 True, stopAt 3 False])) "S3",
          dropAction cfg t0 Nothing Nothing "S2",
          dropAction cfg (after 599) (Just t0) Nothing "S2",
          dropAction cfg (after 600) (Just t0) Nothing "S2"
        )
          @?= (StartDropClock, StartDropClock, KeepWaiting, KeepWaiting, KeepWaiting, AutoEnd),
      testCase "off route: pause only after gps-processor flags the cab for offRouteSec straight" $
        ( offRouteAction noCheckIn t0 Nothing (Just False),
          offRouteAction noCheckIn t0 Nothing (Just True),
          offRouteAction noCheckIn (after 119) (Just t0) (Just True),
          offRouteAction noCheckIn (after 120) (Just t0) (Just True),
          offRouteAction noCheckIn (after 120) (Just t0) (Just False),
          offRouteAction noCheckIn (after 120) (Just t0) Nothing
        )
          @?= (OffRouteNoChange, StartOffRouteClock, OffRouteNoChange, PauseOffRoute, ClearOffRouteClock, OffRouteNoChange),
      testCase "off route: the driver is asked checkInLeadSec before the pause, until it; never with no lead; the pause stays at offRouteSec" $
        ( offRouteAction cfg (after 59) (Just t0) (Just True),
          offRouteAction cfg (after 60) (Just t0) (Just True),
          offRouteAction cfg (after 119) (Just t0) (Just True),
          offRouteAction cfg (after 120) (Just t0) (Just True),
          offRouteAction noCheckIn (after 60) (Just t0) (Just True),
          offRouteAction cfg (after 60) (Just t0) Nothing
        )
          @?= (OffRouteNoChange, AskCheckIn (after 120), AskCheckIn (after 120), PauseOffRoute, OffRouteNoChange, OffRouteNoChange),
      testCase "moving timer: armed once, on arrival at the Upcoming board stop" $
        ( armMovingTimer cfg t0 Nothing (Just (fix 25.6195 [stopAt 5 False, stopAt 6 False])) "S5",
          armMovingTimer cfg t0 (Just (after 30)) (Just (fix 25.6195 [stopAt 5 False, stopAt 6 False])) "S5",
          armMovingTimer cfg t0 Nothing (Just (fix 25.6195 [stopAt 5 True, stopAt 6 False])) "S5",
          armMovingTimer cfg t0 Nothing (Just (fix 25.615 [stopAt 5 False, stopAt 6 False])) "S5",
          armMovingTimer cfg t0 Nothing Nothing "S5"
        )
          @?= (Just (after 90), Nothing, Nothing, Nothing, Nothing),
      testCase "moving timer with gps-processor's is_on_stop: armed when it holds the cab at the board stop, wherever the fix is; never on distance alone" $
        ( armMovingTimer cfg t0 Nothing (Just (fixOn (Just True) 25.0 [stopAt 5 False, stopAt 6 False])) "S5",
          armMovingTimer cfg t0 Nothing (Just (fixOn (Just False) 25.6195 [stopAt 5 False, stopAt 6 False])) "S5",
          armMovingTimer cfg t0 Nothing (Just (fixOn (Just True) 25.6195 [stopAt 5 True, stopAt 6 False])) "S5",
          armMovingTimer cfg t0 Nothing (Just (fixOn (Just True) 25.6195 [stopAt 4 False, stopAt 5 False, stopAt 6 False])) "S5"
        )
          @?= (Just (after 90), Nothing, Nothing, Nothing),
      testCase "moving timer: a cab accepted away from the stop starts the countdown on reaching it; a stand or moving timer is never replaced" $
        let st timer = AllocationState {vehicleNumber = "P1", timer, approachStage = Nothing, approachFrom = Nothing}
            armedWith timer = armMovingTimer cfg t0 (blockingTimerDeadline (st timer)) (Just (fix 25.6195 [stopAt 5 False, stopAt 6 False])) "S5"
         in map armedWith [NoTimer, StandTimer (after 30), MovingTimer (after 30), AwayTimer (after 30)]
              @?= [Just (after 90), Nothing, Nothing, Just (after 90)],
      testCase "approach: the first fresh fix only records where the cab was; a cab already near the stop pushes arriving soon at once" $
        ( approachUpdate cfg (fix 25.60 [stopAt 5 False]) "S5" (assigned Nothing),
          snd <$> approachUpdate cfg (fix 25.6195 [stopAt 5 False]) "S5" (assigned Nothing)
        )
          @?= (Just (assigned (Just (LatLong 25.60 91.88)), Nothing), Just (Just PushArrivingSoon)),
      testCase "approach: on the way once the cab is onTheWayM from where it was first seen, and only once" $
        -- 0.005 deg of latitude is ~555 m, 0.004 deg ~444 m
        ( push (fix 25.605 [stopAt 5 False]) (assigned (Just (LatLong 25.60 91.88))),
          push (fix 25.604 [stopAt 5 False]) (assigned (Just (LatLong 25.60 91.88))),
          approachUpdate cfg (fix 25.605 [stopAt 5 False]) "S5" (stage ApproachOnTheWay (Just (LatLong 25.60 91.88)))
        )
          @?= (Just (Just PushOnTheWay), Nothing, Nothing),
      testCase "approach: within arrivingSoonRadiusM of the board stop, skipping on the way when both are reached" $
        ( push (fix 25.6195 [stopAt 5 False]) (assigned (Just (LatLong 25.60 91.88))),
          push (fix 25.6195 [stopAt 5 False]) (stage ApproachOnTheWay (Just (LatLong 25.60 91.88)))
        )
          @?= (Just (Just PushArrivingSoon), Just (Just PushArrivingSoon)),
      testCase "approach: nothing after arriving soon or arrived, on an offer, or for a stop the cab has passed" $
        ( approachUpdate cfg (fix 25.6195 [stopAt 5 False]) "S5" (stage ApproachArrivingSoon (Just (LatLong 25.60 91.88))),
          approachUpdate cfg (fix 25.6195 [stopAt 5 False]) "S5" ((assigned (Just (LatLong 25.60 91.88))) {timer = MovingTimer (after 30), approachStage = Nothing}),
          approachUpdate cfg (fix 25.6195 [stopAt 5 False]) "S5" ((assigned Nothing) {timer = OfferTimer (after 30), approachStage = Nothing}),
          push (fix 25.6205 [stopAt 5 True, stopAt 6 False]) (stage ApproachOnTheWay (Just (LatLong 25.61 91.88)))
        )
          @?= (Nothing, Nothing, Nothing, Nothing)
    ]
  where
    stage s from = AllocationState {vehicleNumber = "P1", timer = NoTimer, approachStage = Just s, approachFrom = from}
    assigned = stage ApproachAssigned
    push cab st = snd <$> approachUpdate cfg cab "S5" st
