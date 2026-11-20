{-# LANGUAGE PackageImports #-}

-- | When a campaign's TimeBound preference locks are re-applied: the next window start (first milestone timeBounds,
-- city local time) inside the campaign, in UTC, plus the margin ('nextWindowRunAt').
module PreferencePolicyWindowTests (tests) where

import Data.Time (Day, NominalDiffTime, TimeOfDay (..), UTCTime (..), addUTCTime, fromGregorian, timeOfDayToTime)
import "mobility-core" Kernel.Types.Common (Seconds (..))
import qualified "mobility-core" Kernel.Types.TimeBound as TB
import "dynamic-offer-driver-app" SharedLogic.DriverPreferencePolicy.Campaign (nextWindowRunAt, windowStartDelay)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Prelude

ist :: Seconds
ist = Seconds 19800

-- 2026-11-16 is a Monday.
mon, tue, wed, fri :: Day
mon = fromGregorian 2026 11 16
tue = fromGregorian 2026 11 17
wed = fromGregorian 2026 11 18
fri = fromGregorian 2026 11 20

at :: Day -> Int -> Int -> UTCTime
at day h m = UTCTime day (timeOfDayToTime (TimeOfDay h m 0))

-- | A local wall-clock time as UTC, for a city at @offset@ seconds from UTC.
utcOf :: Seconds -> UTCTime -> UTCTime
utcOf (Seconds offset) = addUTCTime (negate (fromIntegral offset :: NominalDiffTime))

-- | The expected run: the local window start, in UTC, plus the margin.
runAt :: Seconds -> UTCTime -> Maybe UTCTime
runAt offset localStart = Just (addUTCTime windowStartDelay (utcOf offset localStart))

campaignStart, campaignEnd :: UTCTime
campaignStart = at (fromGregorian 2026 11 1) 0 0
campaignEnd = at (fromGregorian 2026 12 31) 0 0

everyDay :: [(TimeOfDay, TimeOfDay)] -> TB.TimeBound
everyDay peaks = TB.BoundedByWeekday (TB.BoundedPeaks peaks peaks peaks peaks peaks peaks peaks)

noPeaks :: TB.BoundedPeaks
noPeaks = TB.BoundedPeaks [] [] [] [] [] [] []

hm :: Int -> Int -> TimeOfDay
hm h m = TimeOfDay h m 0

twoRanges :: TB.TimeBound
twoRanges = everyDay [(hm 8 0, hm 10 0), (hm 17 0, hm 20 0)]

next :: Seconds -> TB.TimeBound -> UTCTime -> Maybe UTCTime
next offset tb localNow = nextWindowRunAt offset campaignStart campaignEnd tb (utcOf offset localNow)

tests :: TestTree
tests =
  testGroup
    "preference policy: next campaign window run"
    [ testCase "before the first range of the day: that range" $
        next ist twoRanges (at mon 7 0) @?= runAt ist (at mon 8 0),
      testCase "between two ranges: the later range the same day" $
        next ist twoRanges (at mon 11 0) @?= runAt ist (at mon 17 0),
      testCase "inside a range: the next range, not the open one" $
        next ist twoRanges (at mon 9 0) @?= runAt ist (at mon 17 0),
      testCase "after the last range: the first range the next day" $
        next ist twoRanges (at mon 21 0) @?= runAt ist (at tue 8 0),
      testCase "exactly at a range start: the next one (strictly after now)" $
        next ist twoRanges (at mon 8 0) @?= runAt ist (at mon 17 0),
      testCase "weekly per-day ranges: a later weekday" $
        next ist (TB.BoundedByWeekday noPeaks {TB.wednesday = [(hm 18 0, hm 22 0)]}) (at mon 9 0) @?= runAt ist (at wed 18 0),
      testCase "weekly per-day ranges: the same weekday next week" $
        next ist (TB.BoundedByWeekday noPeaks {TB.monday = [(hm 6 0, hm 7 0)]}) (at mon 9 0) @?= runAt ist (at (fromGregorian 2026 11 23) 6 0),
      testCase "a range crossing midnight opens at its start, not at 00:00" $
        next ist (everyDay [(hm 22 0, hm 2 0)]) (at mon 3 0) @?= runAt ist (at mon 22 0),
      testCase "a range crossing midnight: 00:00 the next day only continues it" $
        next ist (everyDay [(hm 22 0, hm 2 0)]) (at mon 23 0) @?= runAt ist (at tue 22 0),
      testCase "overlapping ranges: a start inside another range is not an opening" $
        next ist (everyDay [(hm 8 0, hm 10 0), (hm 9 0, hm 11 0)]) (at mon 8 30) @?= runAt ist (at tue 8 0),
      testCase "day-bound ranges: the listed day, past days ignored" $
        next ist (TB.BoundedByDay [(fromGregorian 2026 11 10, [(hm 10 0, hm 11 0)]), (fri, [(hm 10 0, hm 11 0)])]) (at mon 12 0) @?= runAt ist (at fri 10 0),
      testCase "city timezone: the same local window is a different UTC instant" $ do
        next (Seconds 3600) twoRanges (at mon 11 0) @?= runAt (Seconds 3600) (at mon 17 0)
        runAt (Seconds 3600) (at mon 17 0) @?= Just (addUTCTime windowStartDelay (at mon 16 0))
        runAt ist (at mon 17 0) @?= Just (addUTCTime windowStartDelay (at mon 11 30)),
      testCase "a campaign that has not started: its first window on or after the start" $
        nextWindowRunAt ist (at wed 0 0) campaignEnd twoRanges (utcOf ist (at mon 9 0)) @?= runAt ist (at wed 8 0),
      testCase "no window before the campaign ends: nothing" $
        nextWindowRunAt ist campaignStart (at mon 12 0) twoRanges (utcOf ist (at mon 11 0)) @?= Nothing,
      testCase "unbounded: nothing" $
        next ist TB.Unbounded (at mon 9 0) @?= Nothing
    ]
