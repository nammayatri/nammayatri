{-# LANGUAGE OverloadedStrings #-}

module SharedCabStopProgressTests (tests) where

import qualified Data.Text as T
import Data.Time (UTCTime (..), addUTCTime, fromGregorian)
import Kernel.External.Maps.Types (LatLong (..))
import SharedLogic.SharedCab.StopProgress.Rules
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Prelude

cfg :: StopProgressConfig
cfg = StopProgressConfig {atStopRadiusM = 100, autoEndAfterDropSec = 600, offRouteMeters = 300, offRouteSec = 120, movingTimerSec = 90}

t0 :: UTCTime
t0 = UTCTime (fromGregorian 2026 9 25) 36000

after :: Int -> UTCTime
after sec = addUTCTime (fromIntegral sec) t0

-- | Stops about 1.1 km apart due north; a latitude degree is ~111 km.
stopAt :: Int -> Bool -> StopMark
stopAt idx reached = StopMark {stopCode = T.pack ("S" <> show idx), stopIdx = idx, reached, coordinate = LatLong (25.57 + 0.01 * fromIntegral idx) 91.88}

fix :: Double -> [StopMark] -> CabFix
fix lat stops = CabFix {position = LatLong lat 91.88, stops}

-- | A straight road along the stops' meridian.
road :: [LatLong]
road = [LatLong 25.57 91.88, LatLong 25.62 91.88]

tests :: TestTree
tests =
  testGroup
    "SharedCab stop progress (05 §6)"
    [ testCase "board stop passed only once LTS marks it Reached" $
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
      testCase "off route: pause only after offRouteSec continuously off the polyline" $
        ( offRouteAction cfg t0 Nothing road (Just (LatLong 25.60 91.8801)),
          offRouteAction cfg t0 Nothing road (Just (LatLong 25.60 91.89)),
          offRouteAction cfg (after 119) (Just t0) road (Just (LatLong 25.60 91.89)),
          offRouteAction cfg (after 120) (Just t0) road (Just (LatLong 25.60 91.89)),
          offRouteAction cfg (after 120) (Just t0) road (Just (LatLong 25.60 91.8801)),
          offRouteAction cfg (after 120) (Just t0) road Nothing,
          offRouteAction cfg (after 120) (Just t0) [] (Just (LatLong 25.60 91.89))
        )
          @?= (OffRouteNoChange, StartOffRouteClock, OffRouteNoChange, PauseOffRoute, ClearOffRouteClock, OffRouteNoChange, OffRouteNoChange),
      testCase "moving timer: armed once, on arrival at the Upcoming board stop" $
        ( armMovingTimer cfg t0 Nothing (Just (fix 25.6195 [stopAt 5 False, stopAt 6 False])) "S5",
          armMovingTimer cfg t0 (Just (after 30)) (Just (fix 25.6195 [stopAt 5 False, stopAt 6 False])) "S5",
          armMovingTimer cfg t0 Nothing (Just (fix 25.6195 [stopAt 5 True, stopAt 6 False])) "S5",
          armMovingTimer cfg t0 Nothing (Just (fix 25.615 [stopAt 5 False, stopAt 6 False])) "S5",
          armMovingTimer cfg t0 Nothing Nothing "S5"
        )
          @?= (Just (after 90), Nothing, Nothing, Nothing, Nothing),
      testCase "reached end: the last stop Reached, or the cab at it" $
        ( reachedRouteEnd 100 (fix 25.60 [stopAt 5 False, stopAt 6 False]),
          reachedRouteEnd 100 (fix 25.625 [stopAt 5 True, stopAt 6 False]),
          reachedRouteEnd 100 (fix 25.6295 [stopAt 5 True, stopAt 6 False]),
          reachedRouteEnd 100 (fix 25.64 [stopAt 5 True, stopAt 6 True]),
          reachedRouteEnd 100 (fix 25.6295 [])
        )
          @?= (False, False, True, True, False)
    ]
