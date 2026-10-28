{-# LANGUAGE OverloadedStrings #-}

-- | Golden tests for the pure rules of Minimum Guarantee (MG) campaigns
-- ("SharedLogic.MinimumGuarantee.Rules"): slab lookup, the lowest-percent rule, the top-up
-- formula, window arithmetic, break clipping, ride counting and the settlement due day. The numbers
-- are the worked examples of the design: D1 earns 1800 of 3000 with one cancellation and is paid 840.
module MinimumGuaranteeRulesTests (tests) where

import Data.Time (Day, TimeOfDay (..), UTCTime (..), fromGregorian, secondsToDiffTime)
import qualified Domain.Types.Extra.MgEnrollment as DEME
import qualified Domain.Types.MgCampaignRule as DMR
import qualified Domain.Types.MgCampaignWindow as DMW
import Kernel.Prelude
import Kernel.Types.Id
import SharedLogic.MinimumGuarantee.Rules
import Test.Tasty
import Test.Tasty.HUnit

--------------------------------------------------------------------------------
-- fixtures
--------------------------------------------------------------------------------

day :: Day
day = fromGregorian 2026 10 28

-- | Seconds after midnight UTC of `day`.
atTime :: Integer -> UTCTime
atTime s = UTCTime day (secondsToDiffTime s)

hours :: Integer -> Integer -> Integer
hours h m = h * 3600 + m * 60

-- | India Standard Time, as a city offset.
ist :: NominalDiffTime
ist = 19800

window :: TimeOfDay -> TimeOfDay -> DMW.MgCampaignWindow
window start end =
  DMW.MgCampaignWindow
    { id = Id "w",
      campaignId = Id "c",
      startTime = start,
      endTime = end,
      createdAt = atTime 0,
      updatedAt = atTime 0
    }

rule :: DMR.MgMetric -> Maybe DMR.MgRuleParams -> [(Int, Int)] -> DMR.MgCampaignRule
rule metric params slabs =
  DMR.MgCampaignRule
    { id = Id "r",
      campaignId = Id "c",
      metric,
      params,
      slabs = [DMR.MgSlab {fromValue = from, percent} | (from, percent) <- slabs],
      createdAt = atTime 0,
      updatedAt = atTime 0
    }

cancellationRule, breakRule, ridesRule, distanceRule :: DMR.MgCampaignRule
cancellationRule = rule DMR.CANCELLATIONS Nothing [(0, 100), (1, 70), (2, 40), (3, 0)]
breakRule = rule DMR.BREAK_MINUTES Nothing [(0, 100), (121, 60), (151, 0)]
ridesRule = rule DMR.RIDES_COMPLETED Nothing [(0, 0), (3, 60), (6, 100)]
distanceRule = rule DMR.DISTANCE_KM Nothing [(0, 0), (20, 50), (40, 100)]

standardRules :: [DMR.MgCampaignRule]
standardRules = [cancellationRule, breakRule, ridesRule, distanceRule]

metrics :: Int -> Int -> Int -> Int -> DEME.MgMetrics
metrics cancellations breakMinutes rides distanceKm =
  DEME.emptyMgMetrics
    { DEME.cancellations = cancellations,
      DEME.breakSeconds = breakMinutes * 60,
      DEME.rides = rides,
      DEME.distanceMeters = distanceKm * 1000
    }

-- | The morning window 08:00-10:00 and the evening window 17:00-19:00 on `day`, in IST.
spans :: [MgWindowSpan]
spans = windowSpansOnDay ist day [window (TimeOfDay 17 0 0) (TimeOfDay 19 0 0), window (TimeOfDay 8 0 0) (TimeOfDay 10 0 0)]

-- | 08:00 IST is 02:30 UTC, 10:00 IST is 04:30 UTC, 17:00 IST is 11:30 UTC, 19:00 IST is 13:30 UTC.
morning, evening :: MgWindowSpan
morning = MgWindowSpan {spanStart = atTime (hours 2 30), spanEnd = atTime (hours 4 30)}
evening = MgWindowSpan {spanStart = atTime (hours 11 30), spanEnd = atTime (hours 13 30)}

--------------------------------------------------------------------------------
-- tests
--------------------------------------------------------------------------------

tests :: TestTree
tests =
  testGroup
    "Minimum guarantee rules"
    [ slabTests,
      payoutTests,
      topUpTests,
      windowTests,
      breakTests,
      rideTests,
      settlementDayTests,
      paginationTests
    ]

slabTests :: TestTree
slabTests =
  testGroup
    "slabs run until the next slab starts"
    [ testCase "cancellations: each count lands in its own slab" $
        map (slabPercent (DMR.slabs cancellationRule)) [0, 1, 2, 3, 10] @?= [100, 70, 40, 0, 0],
      testCase "break minutes: the bound itself belongs to the slab it starts" $
        map (slabPercent (DMR.slabs breakRule)) [0, 120, 121, 150, 151] @?= [100, 100, 60, 60, 0],
      testCase "a value below every slab pays nothing" $
        slabPercent (DMR.slabs cancellationRule) (-1) @?= 0,
      testCase "slab order in the list does not matter" $
        slabPercent (reverse (DMR.slabs cancellationRule)) 2 @?= 40,
      testCase "break minutes count whole minutes only" $
        metricValue DMR.BREAK_MINUTES (metrics 0 0 0 0) {DEME.breakSeconds = 119} @?= 1,
      testCase "distance counts whole kilometres only" $
        metricValue DMR.DISTANCE_KM (metrics 0 0 0 0) {DEME.distanceMeters = 45999} @?= 45
    ]

payoutTests :: TestTree
payoutTests =
  testGroup
    "payout percent is the lowest across the rules"
    [ testCase "example 3 of the design: 70, 60, 100 and 50 give 50" $
        payoutPercentFor standardRules (metrics 1 140 7 38) @?= 50,
      testCase "a clean day with enough rides and distance pays in full" $
        payoutPercentFor standardRules (metrics 0 0 8 52) @?= 100,
      testCase "no rules means no penalty" $
        payoutPercentFor [] (metrics 5 500 0 0) @?= 100,
      testCase "an idle driver is caught by the target rules" $
        payoutPercentFor standardRules (metrics 0 0 0 0) @?= 0,
      testCase "a limit rule at 0% can never recover" $
        limitRuleExhausted standardRules (metrics 3 0 0 0) @?= True,
      testCase "a target rule at 0% can still grow, so it is not exhausted" $
        limitRuleExhausted standardRules (metrics 0 0 0 0) @?= False,
      testCase "break time over 150 minutes exhausts the break rule" $
        limitRuleExhausted [breakRule] (metrics 0 151 0 0) @?= True
    ]

topUpTests :: TestTree
topUpTests =
  testGroup
    "top-up = shortfall x percent, rounded to a whole unit"
    [ testCase "D1: earned 1800 of 3000 at 70 percent pays 840" $
        topUpAmount 3000 1800 70 @?= 840,
      testCase "earned 2200 of 3000 at 50 percent pays 400" $
        topUpAmount 3000 2200 50 @?= 400,
      testCase "earning more than the guarantee pays nothing" $
        topUpAmount 3000 3200 100 @?= 0,
      testCase "earning exactly the guarantee pays nothing" $
        topUpAmount 3000 3000 100 @?= 0,
      testCase "0 percent pays nothing" $
        topUpAmount 3000 0 0 @?= 0,
      testCase "rounds to the nearest whole unit" $
        map (\shortfall -> topUpAmount 3000 (3000 - shortfall) 33) [100, 101, 105] @?= [33, 33, 35]
    ]

windowTests :: TestTree
windowTests =
  testGroup
    "windows"
    [ testCase "time-of-day windows become absolute spans in the city's offset, earliest first" $
        spans @?= [morning, evening],
      testCase "a window start is inside, its end is outside" $
        map (`insideAnySpan` [morning]) [spanStart morning, atTime (hours 3 0), spanEnd morning] @?= [True, True, False],
      testCase "between the windows is outside" $
        insideAnySpan (atTime (hours 8 0)) spans @?= False,
      testCase "overlapping span sets are detected" $
        spansOverlap [morning] [MgWindowSpan {spanStart = atTime (hours 4 0), spanEnd = atTime (hours 5 0)}] @?= True,
      testCase "back-to-back spans do not overlap" $
        spansOverlap [morning] [MgWindowSpan {spanStart = spanEnd morning, spanEnd = atTime (hours 6 0)}] @?= False,
      testCase "the local day of an instant follows the city's offset" $
        localDayOf ist (atTime (hours 19 0)) @?= fromGregorian 2026 10 29
    ]

breakTests :: TestTree
breakTests =
  testGroup
    "breaks are clipped to the windows"
    [ testCase "a break inside a window counts in full" $
        overlapSeconds (atTime (hours 3 0)) (atTime (hours 3 20)) spans @?= 1200,
      testCase "09:50-10:20 IST counts only the 10 minutes inside the morning window" $
        overlapSeconds (atTime (hours 4 20)) (atTime (hours 4 50)) spans @?= 600,
      testCase "a break between the windows counts nothing" $
        overlapSeconds (atTime (hours 5 30)) (atTime (hours 10 30)) spans @?= 0,
      testCase "a break spanning both windows counts the part inside each" $
        -- 09:30 IST to 17:20 IST: 30 minutes of the morning, 20 minutes of the evening
        overlapSeconds (atTime (hours 4 0)) (atTime (hours 11 50)) spans @?= 1800 + 1200,
      testCase "closing an open break adds its clipped time and clears it" $
        let open = (metrics 0 0 0 0) {DEME.breakStartedAt = Just (atTime (hours 4 0))}
            closed = closeOpenBreak (atTime (hours 6 0)) spans open
         in (DEME.breakSeconds closed, DEME.breakStartedAt closed) @?= (1800, Nothing),
      testCase "closing when no break is open changes nothing" $
        closeOpenBreak (atTime (hours 6 0)) spans (metrics 1 2 3 4) @?= metrics 1 2 3 4,
      testCase "two breaks add up" $
        let first = closeOpenBreak (atTime (hours 3 20)) spans ((metrics 0 0 0 0) {DEME.breakStartedAt = Just (atTime (hours 3 0))})
            second = closeOpenBreak (atTime (hours 4 20)) spans (first {DEME.breakStartedAt = Just (atTime (hours 4 10))})
         in DEME.breakSeconds second @?= 1200 + 600
    ]

rideTests :: TestTree
rideTests =
  testGroup
    "ride counting"
    [ testCase "a ride adds earnings, one ride and its distance" $
        let m = addCountedRide standardRules "r1" 210 6000 DEME.emptyMgMetrics
         in (DEME.earned m, DEME.rides m, DEME.distanceMeters m, DEME.countedRideIds m) @?= (210, 1, 6000, ["r1"]),
      testCase "the same ride id is counted once even if the event is delivered twice" $
        let once = addCountedRide standardRules "r1" 210 6000 DEME.emptyMgMetrics
         in addCountedRide standardRules "r1" 210 6000 once @?= once,
      testCase "a ride below the rule's minimum distance earns but is not counted as a ride" $
        let short = rule DMR.RIDES_COMPLETED (Just DMR.MgRuleParams {minRideDistanceMeters = Just 1000, minRideFare = Nothing}) [(0, 0), (3, 100)]
            m = addCountedRide [short] "r1" 80 500 DEME.emptyMgMetrics
         in (DEME.earned m, DEME.rides m) @?= (80, 0),
      testCase "a ride below the minimum fare is not counted as a ride" $
        let cheap = rule DMR.RIDES_COMPLETED (Just DMR.MgRuleParams {minRideDistanceMeters = Nothing, minRideFare = Just 50}) [(0, 0), (3, 100)]
            m = addCountedRide [cheap] "r1" 40 5000 DEME.emptyMgMetrics
         in (DEME.earned m, DEME.rides m) @?= (40, 0),
      testCase "distance is added only when it passes the distance rule's minimum" $
        let far = rule DMR.DISTANCE_KM (Just DMR.MgRuleParams {minRideDistanceMeters = Just 2000, minRideFare = Nothing}) [(0, 0), (20, 100)]
            counted = addCountedRide [far] "r1" 100 3000 DEME.emptyMgMetrics
            skipped = addCountedRide [far] "r2" 100 1500 DEME.emptyMgMetrics
         in (DEME.distanceMeters counted, DEME.distanceMeters skipped) @?= (3000, 0),
      testCase "the design's D1 day adds up to 1800, 6 rides, 45 km" $
        let rides = [("a", 210, 6000), ("b", 180, 5000), ("c", 340, 9000), ("d", 270, 6000), ("e", 420, 10000), ("f", 380, 9000)]
            m = foldl' (\acc (rideId, fare, meters) -> addCountedRide standardRules rideId fare meters acc) DEME.emptyMgMetrics rides
         in (DEME.earned m, DEME.rides m, DEME.distanceMeters m) @?= (1800, 6, 45000)
    ]

settlementDayTests :: TestTree
settlementDayTests =
  testGroup
    "which day is due for settlement"
    [ testCase "the day is due once the last window end plus grace has passed (19:00 + 120 min = 21:00 IST)" $
        lastDueDay ist 120 (TimeOfDay 19 0 0) (atTime (hours 15 30)) @?= day,
      testCase "one minute before that, the previous day is the latest due one" $
        lastDueDay ist 120 (TimeOfDay 19 0 0) (atTime (hours 15 29)) @?= fromGregorian 2026 10 27,
      testCase "with no grace the day is due at the last window end" $
        lastDueDay ist 0 (TimeOfDay 19 0 0) (atTime (hours 13 30)) @?= day,
      testCase "the last element of a list, in one pass" $
        (lastMaybe [1, 2, 3 :: Int], lastMaybe ([] :: [Int])) @?= (Just 3, Nothing)
    ]

paginationTests :: TestTree
paginationTests =
  testGroup
    "page bounds"
    [ testCase "defaults to 20 from offset 0" $
        pageBounds Nothing Nothing @?= (20, 0),
      testCase "the limit is capped at 50 and at least 1" $
        (pageBounds (Just 500) Nothing, pageBounds (Just 0) Nothing, pageBounds (Just (-3)) Nothing) @?= ((50, 0), (1, 0), (1, 0)),
      testCase "a negative offset becomes 0" $
        pageBounds (Just 10) (Just (-5)) @?= (10, 0),
      testCase "a given limit and offset are kept" $
        pageBounds (Just 10) (Just 30) @?= (10, 30)
    ]
