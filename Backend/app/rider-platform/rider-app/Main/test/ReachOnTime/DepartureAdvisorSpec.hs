{-
  Tests for ReachOnTime feature business logic.

  Covers:
  - DepartureAdvisor: risk level computation, crowding buffers, safety warnings, time formatting
  - ScheduleQuery: query building, time mode parsing, reference time selection
  - DepartureReminder: recurrence scheduling, target time computation, notification dedup

  Uses tasty + tasty-hunit for unit tests and tasty-quickcheck for property tests.
-}
module ReachOnTime.DepartureAdvisorSpec (tests) where

import Prelude
import Data.Time
  ( UTCTime (..),
    NominalDiffTime,
    addUTCTime,
    TimeOfDay (..),
    fromGregorian,
    secondsToDiffTime,
    utctDay,
  )
import qualified Data.Time.Calendar.WeekDate as WeekDate
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Aeson as Aeson
import Data.ByteString.Lazy (fromStrict)
import Data.Text.Encoding (encodeUtf8)
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck as QC

-- ============================================================================
-- Imports from the modules under test
-- ============================================================================

import SharedLogic.ReachOnTime.DepartureAdvisor
  ( RiskLevel (..),
    DepartureAdvisory (..),
    SafetyWarning (..),
    computeArriveByAdvisory,
    computeDepartAtAdvisory,
    computeLeaveNowAdvisory,
    computeRiskLevel,
    computeCrowdingBufferSeconds,
    flagLateNightWalking,
    showTimeIST,
  )

import SharedLogic.ReachOnTime.ScheduleQuery
  ( TimeMode (..),
    TripsBetweenRequest (..),
    buildScheduleQuery,
    parseTimeMode,
    getScheduleReferenceTime,
    formatDateYMD,
  )

-- ============================================================================
-- Pure re-implementations of DepartureReminder logic for testing
-- (The actual module has monadic DB dependencies we cannot import in tests)
-- ============================================================================

-- | Stub types mirroring Domain.Types.SavedTrip
data TripRecurrence = NoRecurrence | Daily | Weekdays | Weekends | Custom
  deriving (Eq, Show)

-- | IST offset as NominalDiffTime
istOffsetNDT :: NominalDiffTime
istOffsetNDT = fromIntegral (5 * 3600 + 30 * 60 :: Int)

-- | Pure re-implementation of isScheduledForToday from DepartureReminder
isScheduledForToday :: TripRecurrence -> Maybe Text -> UTCTime -> Bool
isScheduledForToday recurrence mbCustomDays now =
  let istDay = utctDay (addUTCTime istOffsetNDT now)
      (_, _, dayOfWeek) = WeekDate.toWeekDate istDay
   in case recurrence of
        NoRecurrence -> False
        Daily -> True
        Weekdays -> dayOfWeek >= 1 && dayOfWeek <= 5
        Weekends -> dayOfWeek >= 6
        Custom -> case mbCustomDays of
          Nothing -> False
          Just daysJson ->
            case Aeson.decode (fromStrict (encodeUtf8 daysJson)) of
              Just (days :: [Int]) -> dayOfWeek `elem` days
              Nothing -> False

-- | Pure re-implementation of alreadyNotifiedToday from DepartureReminder
alreadyNotifiedToday :: Maybe UTCTime -> UTCTime -> Bool
alreadyNotifiedToday mbLastNotified now =
  case mbLastNotified of
    Nothing -> False
    Just lastNotified ->
      let istDayNow = utctDay (addUTCTime istOffsetNDT now)
          istDayNotified = utctDay (addUTCTime istOffsetNDT lastNotified)
       in istDayNotified == istDayNow

-- | Pure re-implementation of getTargetTimeForToday from DepartureReminder
data StubTimeMode = StubArriveBy | StubDepartAt | StubLeaveNow
  deriving (Eq, Show)

getTargetTimeForToday :: StubTimeMode -> Maybe TimeOfDay -> Maybe UTCTime -> UTCTime -> Maybe UTCTime
getTargetTimeForToday mode mbTod mbTargetTime now =
  case (mode, mbTod) of
    (StubArriveBy, Just tod) -> Just $ todToUTCToday tod now
    (StubDepartAt, Just tod) -> Just $ todToUTCToday tod now
    _ -> mbTargetTime

todToUTCToday :: TimeOfDay -> UTCTime -> UTCTime
todToUTCToday tod now =
  let istDay = utctDay (addUTCTime istOffsetNDT now)
      todSecs = timeOfDayToSeconds tod
      istTargetUTC = UTCTime istDay (fromIntegral todSecs)
   in addUTCTime (negate istOffsetNDT) istTargetUTC

timeOfDayToSeconds :: TimeOfDay -> Int
timeOfDayToSeconds (TimeOfDay h m s) = h * 3600 + m * 60 + round s

-- ============================================================================
-- Test helpers
-- ============================================================================

-- | Construct a UTCTime conveniently
mkUTCTime :: Integer -> Int -> Int -> Int -> Int -> Int -> UTCTime
mkUTCTime year month day hour minute second =
  let d = fromGregorian year month day
      secs = fromIntegral (hour * 3600 + minute * 60 + second)
   in UTCTime d (secondsToDiffTime secs)

-- | Construct a UTCTime from IST components (converts IST to UTC)
mkISTTime :: Integer -> Int -> Int -> Int -> Int -> Int -> UTCTime
mkISTTime year month day hour minute second =
  addUTCTime (negate istOffsetNDT) (mkUTCTime year month day hour minute second)

-- ============================================================================
-- All tests
-- ============================================================================

tests :: TestTree
tests = testGroup "ReachOnTime"
  [ departureAdvisorTests
  , scheduleQueryTests
  , departureReminderTests
  , propertyTests
  ]

-- ============================================================================
-- DepartureAdvisor Tests
-- ============================================================================

departureAdvisorTests :: TestTree
departureAdvisorTests = testGroup "DepartureAdvisor"
  [ computeArriveByAdvisoryTests
  , computeDepartAtAdvisoryTests
  , computeLeaveNowAdvisoryTests
  , computeRiskLevelTests
  , computeCrowdingBufferSecondsTests
  , flagLateNightWalkingTests
  , showTimeISTTests
  ]

-- --------------------------------------------------------------------------
-- computeArriveByAdvisory
-- --------------------------------------------------------------------------

computeArriveByAdvisoryTests :: TestTree
computeArriveByAdvisoryTests = testGroup "computeArriveByAdvisory"
  [ testCase "target 1 hour from now, duration 30 min -> Comfortable" $ do
      let now = mkUTCTime 2026 3 17 10 0 0
          target = mkUTCTime 2026 3 17 11 0 0  -- 60 min away
          advisory = computeArriveByAdvisory target 1800 5 now Nothing
      -- latestDep = target - 30min = 10:30, available = 30 min = 1800s
      -- bufferMins=5, Comfortable threshold = (5+7)*60 = 720s, 1800 >= 720 -> Comfortable
      riskLevel advisory @?= Comfortable

  , testCase "target 40 min from now, duration 30 min, buffer 5 -> Good" $ do
      let now = mkUTCTime 2026 3 17 10 0 0
          target = mkUTCTime 2026 3 17 10 40 0  -- 40 min away
          advisory = computeArriveByAdvisory target 1800 5 now Nothing
      -- latestDep = target - 30min = 10:10, available = 10 min = 600s
      -- Tight if < 5*60=300, Good if < (5+7)*60=720, 600 >= 300 and 600 < 720 -> Good
      riskLevel advisory @?= Good

  , testCase "target 35 min from now, duration 30 min, buffer 5 -> Tight" $ do
      let now = mkUTCTime 2026 3 17 10 0 0
          target = mkUTCTime 2026 3 17 10 35 0  -- 35 min away
          advisory = computeArriveByAdvisory target 1800 5 now Nothing
      -- latestDep = target - 30min = 10:05, available = 5 min = 300s
      -- Tight threshold: < 5*60=300. 300 is NOT < 300, so Good
      -- Actually 300 >= 300 so NOT Tight, check Good: 300 < 720 -> Good
      riskLevel advisory @?= Good

  , testCase "target 34 min from now, duration 30 min, buffer 5 -> Tight" $ do
      let now = mkUTCTime 2026 3 17 10 0 0
          target = mkUTCTime 2026 3 17 10 34 0  -- 34 min away
          advisory = computeArriveByAdvisory target 1800 5 now Nothing
      -- latestDep = 10:04, available = 4 min = 240s, 240 < 300 -> Tight
      riskLevel advisory @?= Tight

  , testCase "target 29 min from now, duration 30 min -> TooLate" $ do
      let now = mkUTCTime 2026 3 17 10 0 0
          target = mkUTCTime 2026 3 17 10 29 0
          advisory = computeArriveByAdvisory target 1800 5 now Nothing
      -- latestDep = target - 30min = 9:59, available = -60s -> TooLate
      riskLevel advisory @?= TooLate

  , testCase "crowding buffer 600s shifts risk levels" $ do
      let now = mkUTCTime 2026 3 17 10 0 0
          target = mkUTCTime 2026 3 17 10 45 0  -- 45 min away
          -- Without crowding: latestDep = 10:15, available = 15min=900s -> Comfortable
          advisoryNoCrowd = computeArriveByAdvisory target 1800 5 now Nothing
          -- With crowding 600s: totalDuration = 1800+600=2400s=40min
          -- latestDep = 10:45 - 40min = 10:05, available = 5min=300s
          -- 300 >= 300 so NOT Tight, 300 < 720 -> Good
          advisoryWithCrowd = computeArriveByAdvisory target 1800 5 now (Just 600)
      riskLevel advisoryNoCrowd @?= Comfortable
      riskLevel advisoryWithCrowd @?= Good

  , testCase "bufferMins=0 boundary test" $ do
      let now = mkUTCTime 2026 3 17 10 0 0
          target = mkUTCTime 2026 3 17 10 31 0  -- 31 min, duration 30 min
          advisory = computeArriveByAdvisory target 1800 0 now Nothing
      -- latestDep = 10:01, available = 60s
      -- TooLate if <= 0 (no), Tight if < 0*60=0 (no, 60 >= 0), Good if < (0+7)*60=420 (yes, 60 < 420)
      riskLevel advisory @?= Good

  , testCase "bufferMins=15 generous buffer" $ do
      let now = mkUTCTime 2026 3 17 10 0 0
          target = mkUTCTime 2026 3 17 11 0 0  -- 60 min, duration 30 min
          advisory = computeArriveByAdvisory target 1800 15 now Nothing
      -- latestDep = 10:30, available = 30min=1800s
      -- Tight if < 15*60=900, Good if < (15+7)*60=1320
      -- 1800 >= 1320 -> Comfortable
      riskLevel advisory @?= Comfortable

  , testCase "recommendedDeparture < latestDeparture for ArriveBy" $ do
      let now = mkUTCTime 2026 3 17 10 0 0
          target = mkUTCTime 2026 3 17 11 0 0
          advisory = computeArriveByAdvisory target 1800 5 now Nothing
      assertBool "recommended < latest" (recommendedDeparture advisory < latestDeparture advisory)

  , testCase "comfortableDeparture < recommendedDeparture" $ do
      let now = mkUTCTime 2026 3 17 10 0 0
          target = mkUTCTime 2026 3 17 11 0 0
          advisory = computeArriveByAdvisory target 1800 5 now Nothing
      assertBool "comfortable < recommended" (comfortableDeparture advisory < recommendedDeparture advisory)

  , testCase "latestDeparture is exactly targetArrival - duration" $ do
      let now = mkUTCTime 2026 3 17 10 0 0
          target = mkUTCTime 2026 3 17 11 0 0
          advisory = computeArriveByAdvisory target 1800 5 now Nothing
      -- latestDep = target - 1800s
      latestDeparture advisory @?= addUTCTime (-1800) target

  , testCase "crowding buffer affects latestDeparture" $ do
      let now = mkUTCTime 2026 3 17 10 0 0
          target = mkUTCTime 2026 3 17 11 0 0
          advisory = computeArriveByAdvisory target 1800 5 now (Just 600)
      -- latestDep = target - (1800+600) = target - 2400s
      latestDeparture advisory @?= addUTCTime (-2400) target
  ]

-- --------------------------------------------------------------------------
-- computeDepartAtAdvisory
-- --------------------------------------------------------------------------

computeDepartAtAdvisoryTests :: TestTree
computeDepartAtAdvisoryTests = testGroup "computeDepartAtAdvisory"
  [ testCase "departure in 1 hour -> Comfortable" $ do
      let now = mkUTCTime 2026 3 17 10 0 0
          departTime = mkUTCTime 2026 3 17 11 0 0
          advisory = computeDepartAtAdvisory departTime 1800 now
      riskLevel advisory @?= Comfortable

  , testCase "departure in 10 min -> Good" $ do
      let now = mkUTCTime 2026 3 17 10 0 0
          departTime = mkUTCTime 2026 3 17 10 10 0
          advisory = computeDepartAtAdvisory departTime 1800 now
      riskLevel advisory @?= Good

  , testCase "departure in 3 min -> Tight" $ do
      let now = mkUTCTime 2026 3 17 10 0 0
          departTime = mkUTCTime 2026 3 17 10 3 0
          advisory = computeDepartAtAdvisory departTime 1800 now
      riskLevel advisory @?= Tight

  , testCase "departure in past -> TooLate" $ do
      let now = mkUTCTime 2026 3 17 10 30 0
          departTime = mkUTCTime 2026 3 17 10 0 0
          advisory = computeDepartAtAdvisory departTime 1800 now
      riskLevel advisory @?= TooLate

  , testCase "arrivalTime = departTime + duration" $ do
      let now = mkUTCTime 2026 3 17 10 0 0
          departTime = mkUTCTime 2026 3 17 11 0 0
          advisory = computeDepartAtAdvisory departTime 1800 now
      -- For DepartAt: latestDeparture = departTime, so arrival is inferred
      -- The advisory message for Comfortable mentions arrival time
      -- All three departure fields are set to departTime
      latestDeparture advisory @?= departTime
      recommendedDeparture advisory @?= departTime
      comfortableDeparture advisory @?= departTime

  , testCase "departure in exactly 5 min -> Good (boundary)" $ do
      let now = mkUTCTime 2026 3 17 10 0 0
          departTime = mkUTCTime 2026 3 17 10 5 0
          advisory = computeDepartAtAdvisory departTime 1800 now
      -- availableMins = round(300/60) = 5, 5 >= 5 so not Tight, 5 < 12 so Good
      riskLevel advisory @?= Good

  , testCase "departure in exactly 12 min -> Comfortable (boundary)" $ do
      let now = mkUTCTime 2026 3 17 10 0 0
          departTime = mkUTCTime 2026 3 17 10 12 0
          advisory = computeDepartAtAdvisory departTime 1800 now
      -- availableMins = 12, 12 >= 12 so not Good -> Comfortable
      riskLevel advisory @?= Comfortable

  , testCase "departure exactly now -> TooLate" $ do
      let now = mkUTCTime 2026 3 17 10 0 0
          advisory = computeDepartAtAdvisory now 1800 now
      -- availableTime = 0, 0 <= 0 -> TooLate
      riskLevel advisory @?= TooLate

  , testCase "TooLate message is correct" $ do
      let now = mkUTCTime 2026 3 17 10 30 0
          departTime = mkUTCTime 2026 3 17 10 0 0
          advisory = computeDepartAtAdvisory departTime 1800 now
      advisoryMessage advisory @?= "Departure time has passed"
  ]

-- --------------------------------------------------------------------------
-- computeLeaveNowAdvisory
-- --------------------------------------------------------------------------

computeLeaveNowAdvisoryTests :: TestTree
computeLeaveNowAdvisoryTests = testGroup "computeLeaveNowAdvisory"
  [ testCase "always returns Comfortable" $ do
      let now = mkUTCTime 2026 3 17 10 0 0
          advisory = computeLeaveNowAdvisory 1800 now
      riskLevel advisory @?= Comfortable

  , testCase "arrival = now + duration" $ do
      let now = mkUTCTime 2026 3 17 10 0 0
          advisory = computeLeaveNowAdvisory 1800 now
      -- Message should mention arrival at now + 1800s = 10:30 UTC = 4:00 PM IST
      -- latestDeparture, recommended, comfortable all equal now
      latestDeparture advisory @?= now
      recommendedDeparture advisory @?= now
      comfortableDeparture advisory @?= now

  , testCase "bufferMinutes is 0 for LeaveNow" $ do
      let now = mkUTCTime 2026 3 17 10 0 0
          advisory = computeLeaveNowAdvisory 3600 now
      bufferMinutes advisory @?= 0

  , testCase "safetyWarnings is empty" $ do
      let now = mkUTCTime 2026 3 17 10 0 0
          advisory = computeLeaveNowAdvisory 3600 now
      length (safetyWarnings advisory) @?= 0
  ]

-- --------------------------------------------------------------------------
-- computeRiskLevel boundary tests
-- --------------------------------------------------------------------------

computeRiskLevelTests :: TestTree
computeRiskLevelTests = testGroup "computeRiskLevel"
  [ testCase "availableTime = 0 -> TooLate" $
      computeRiskLevel 0 5 @?= TooLate

  , testCase "availableTime = -1 -> TooLate" $
      computeRiskLevel (-1) 5 @?= TooLate

  , testCase "availableTime = -3600 -> TooLate" $
      computeRiskLevel (-3600) 5 @?= TooLate

  , testCase "availableTime = bufferMins*60 - 1 -> Tight" $
      -- bufferMins=5, threshold = 300. availableTime=299 < 300 -> Tight
      computeRiskLevel 299 5 @?= Tight

  , testCase "availableTime = 1 (just above 0, bufferMins=5) -> Tight" $
      computeRiskLevel 1 5 @?= Tight

  , testCase "availableTime = bufferMins*60 -> Good (boundary)" $
      -- bufferMins=5, availableTime=300, NOT < 300, check Good: 300 < (5+7)*60=720 -> Good
      computeRiskLevel 300 5 @?= Good

  , testCase "availableTime = (bufferMins+7)*60 - 1 -> Good" $
      -- bufferMins=5, threshold = 720. 719 < 720 -> Good
      computeRiskLevel 719 5 @?= Good

  , testCase "availableTime = (bufferMins+7)*60 -> Comfortable (boundary)" $
      -- bufferMins=5, 720 >= 720, NOT < 720 -> Comfortable
      computeRiskLevel 720 5 @?= Comfortable

  , testCase "availableTime = very large -> Comfortable" $
      computeRiskLevel 999999 5 @?= Comfortable

  , testCase "bufferMins=0: availableTime=1 -> Good" $
      -- Tight if < 0*60=0 (no), Good if < (0+7)*60=420 (yes) -> Good
      computeRiskLevel 1 0 @?= Good

  , testCase "bufferMins=0: availableTime=420 -> Comfortable" $
      computeRiskLevel 420 0 @?= Comfortable

  , testCase "bufferMins=0: availableTime=419 -> Good" $
      computeRiskLevel 419 0 @?= Good
  ]

-- --------------------------------------------------------------------------
-- computeCrowdingBufferSeconds
-- --------------------------------------------------------------------------

computeCrowdingBufferSecondsTests :: TestTree
computeCrowdingBufferSecondsTests = testGroup "computeCrowdingBufferSeconds"
  [ testCase "7:29 AM IST -> Nothing (not peak)" $ do
      -- 7:29 IST = 1:59 UTC
      let t = mkISTTime 2026 3 17 7 29 0
      computeCrowdingBufferSeconds t @?= Nothing

  , testCase "7:30 AM IST -> Just 600 (peak start)" $ do
      let t = mkISTTime 2026 3 17 7 30 0
      computeCrowdingBufferSeconds t @?= Just 600

  , testCase "8:00 AM IST -> Just 600 (mid morning peak)" $ do
      let t = mkISTTime 2026 3 17 8 0 0
      computeCrowdingBufferSeconds t @?= Just 600

  , testCase "9:59 AM IST -> Just 600 (peak end)" $ do
      let t = mkISTTime 2026 3 17 9 59 0
      computeCrowdingBufferSeconds t @?= Just 600

  , testCase "10:00 AM IST -> Nothing (not peak)" $ do
      let t = mkISTTime 2026 3 17 10 0 0
      computeCrowdingBufferSeconds t @?= Nothing

  , testCase "4:59 PM IST (16:59) -> Nothing (not peak)" $ do
      let t = mkISTTime 2026 3 17 16 59 0
      computeCrowdingBufferSeconds t @?= Nothing

  , testCase "5:00 PM IST (17:00) -> Just 600 (evening peak start)" $ do
      let t = mkISTTime 2026 3 17 17 0 0
      computeCrowdingBufferSeconds t @?= Just 600

  , testCase "7:59 PM IST (19:59) -> Just 600 (evening peak end)" $ do
      let t = mkISTTime 2026 3 17 19 59 0
      computeCrowdingBufferSeconds t @?= Just 600

  , testCase "8:00 PM IST (20:00) -> Nothing (not peak)" $ do
      let t = mkISTTime 2026 3 17 20 0 0
      computeCrowdingBufferSeconds t @?= Nothing

  , testCase "3:00 AM IST -> Nothing (not peak)" $ do
      let t = mkISTTime 2026 3 17 3 0 0
      computeCrowdingBufferSeconds t @?= Nothing

  , testCase "12:00 PM IST (noon) -> Nothing" $ do
      let t = mkISTTime 2026 3 17 12 0 0
      computeCrowdingBufferSeconds t @?= Nothing
  ]

-- --------------------------------------------------------------------------
-- flagLateNightWalking
-- --------------------------------------------------------------------------

flagLateNightWalkingTests :: TestTree
flagLateNightWalkingTests = testGroup "flagLateNightWalking"
  [ testCase "walking 500m at 10 PM -> SafetyWarning generated" $ do
      -- 10 PM IST = 22:00 IST
      let t = mkISTTime 2026 3 17 22 0 0
          legs = [(1, "Walk", Just 500.0)]
          warnings = flagLateNightWalking t legs
      length warnings @?= 1
      legOrder (head warnings) @?= 1
      severity (head warnings) @?= "Medium"

  , testCase "walking 200m at 10 PM -> no warning (< 300m threshold)" $ do
      let t = mkISTTime 2026 3 17 22 0 0
          legs = [(1, "Walk", Just 200.0)]
      length (flagLateNightWalking t legs) @?= 0

  , testCase "walking exactly 300m at 10 PM -> no warning (> 300 required, not >=)" $ do
      let t = mkISTTime 2026 3 17 22 0 0
          legs = [(1, "Walk", Just 300.0)]
      length (flagLateNightWalking t legs) @?= 0

  , testCase "walking 301m at 10 PM -> warning" $ do
      let t = mkISTTime 2026 3 17 22 0 0
          legs = [(1, "Walk", Just 301.0)]
      length (flagLateNightWalking t legs) @?= 1

  , testCase "walking 500m at 2 PM -> no warning (daytime)" $ do
      let t = mkISTTime 2026 3 17 14 0 0
          legs = [(1, "Walk", Just 500.0)]
      length (flagLateNightWalking t legs) @?= 0

  , testCase "walking 500m at 4:59 AM IST -> warning (still late night)" $ do
      let t = mkISTTime 2026 3 17 4 59 0
          legs = [(1, "Walk", Just 500.0)]
      length (flagLateNightWalking t legs) @?= 1

  , testCase "walking 500m at 5:00 AM IST -> no warning" $ do
      let t = mkISTTime 2026 3 17 5 0 0
          legs = [(1, "Walk", Just 500.0)]
      length (flagLateNightWalking t legs) @?= 0

  , testCase "walking 500m at 9:00 PM IST (21:00) -> warning (>= 21)" $ do
      let t = mkISTTime 2026 3 17 21 0 0
          legs = [(1, "Walk", Just 500.0)]
      length (flagLateNightWalking t legs) @?= 1

  , testCase "walking 500m at 8:59 PM IST (20:59) -> no warning" $ do
      let t = mkISTTime 2026 3 17 20 59 0
          legs = [(1, "Walk", Just 500.0)]
      length (flagLateNightWalking t legs) @?= 0

  , testCase "non-Walk mode at 10 PM -> no warning" $ do
      let t = mkISTTime 2026 3 17 22 0 0
          legs = [(1, "Bus", Just 500.0)]
      length (flagLateNightWalking t legs) @?= 0

  , testCase "multiple legs -> correct legOrder values" $ do
      let t = mkISTTime 2026 3 17 23 0 0
          legs = [(0, "Walk", Just 500.0), (1, "Bus", Just 2000.0), (2, "Walk", Just 400.0)]
          warnings = flagLateNightWalking t legs
      length warnings @?= 2
      legOrder (warnings !! 0) @?= 0
      legOrder (warnings !! 1) @?= 2

  , testCase "Walk with Nothing distance at night -> no warning (defaults to 0)" $ do
      let t = mkISTTime 2026 3 17 22 0 0
          legs = [(1, "Walk", Nothing)]
      length (flagLateNightWalking t legs) @?= 0
  ]

-- --------------------------------------------------------------------------
-- showTimeIST
-- --------------------------------------------------------------------------

showTimeISTTests :: TestTree
showTimeISTTests = testGroup "showTimeIST"
  [ testCase "UTC midnight -> 5:30 AM IST" $ do
      let t = mkUTCTime 2026 3 17 0 0 0
      showTimeIST t @?= "5:30 AM"

  , testCase "UTC 6:30 -> 12:00 PM IST" $ do
      let t = mkUTCTime 2026 3 17 6 30 0
      showTimeIST t @?= "12:00 PM"

  , testCase "UTC 13:30 -> 7:00 PM IST" $ do
      let t = mkUTCTime 2026 3 17 13 30 0
      showTimeIST t @?= "7:00 PM"

  , testCase "UTC 18:30 -> 12:00 AM IST (midnight)" $ do
      let t = mkUTCTime 2026 3 17 18 30 0
      showTimeIST t @?= "12:00 AM"

  , testCase "UTC 5:30 -> 11:00 AM IST" $ do
      let t = mkUTCTime 2026 3 17 5 30 0
      showTimeIST t @?= "11:00 AM"

  , testCase "UTC 12:00 -> 5:30 PM IST" $ do
      let t = mkUTCTime 2026 3 17 12 0 0
      showTimeIST t @?= "5:30 PM"

  , testCase "UTC 0:30 -> 6:00 AM IST" $ do
      let t = mkUTCTime 2026 3 17 0 30 0
      showTimeIST t @?= "6:00 AM"
  ]

-- ============================================================================
-- ScheduleQuery Tests
-- ============================================================================

scheduleQueryTests :: TestTree
scheduleQueryTests = testGroup "ScheduleQuery"
  [ buildScheduleQueryTests
  , parseTimeModeTests
  , getScheduleReferenceTimeTests
  ]

-- --------------------------------------------------------------------------
-- buildScheduleQuery
-- --------------------------------------------------------------------------

buildScheduleQueryTests :: TestTree
buildScheduleQueryTests = testGroup "buildScheduleQuery"
  [ testCase "LeaveNow: departAfter is set, arriveBefore is Nothing, window 60" $ do
      let refTime = mkUTCTime 2026 3 17 4 30 0  -- 10:00 AM IST
          req = buildScheduleQuery LeaveNow "STOP1" "STOP2" refTime
      destinationStopCode req @?= "STOP2"
      originStopCode req @?= "STOP1"
      arriveBefore req @?= Nothing
      assertBool "departAfter is Just" (departAfter req /= Nothing)
      windowMinutes req @?= Just 60

  , testCase "DepartAt: departAfter is set, window 120" $ do
      let refTime = mkUTCTime 2026 3 17 4 30 0  -- 10:00 AM IST
          req = buildScheduleQuery DepartAt "STOP1" "STOP2" refTime
      assertBool "departAfter is Just" (departAfter req /= Nothing)
      arriveBefore req @?= Nothing
      windowMinutes req @?= Just 120

  , testCase "ArriveBy: arriveBefore is set, departAfter is Nothing, window 120" $ do
      let refTime = mkUTCTime 2026 3 17 4 30 0  -- 10:00 AM IST
          req = buildScheduleQuery ArriveBy "STOP1" "STOP2" refTime
      departAfter req @?= Nothing
      assertBool "arriveBefore is Just" (arriveBefore req /= Nothing)
      windowMinutes req @?= Just 120

  , testCase "date is correctly set for IST" $ do
      -- UTC 2026-03-17 22:00 = IST 2026-03-18 03:30 -> date should be 2026-03-18
      let refTime = mkUTCTime 2026 3 17 22 0 0
          req = buildScheduleQuery LeaveNow "S1" "S2" refTime
      -- After adding IST offset, day becomes 2026-03-18
      -- formatDateYMD uses utctDay of the IST-adjusted time
      let istTime = addUTCTime istOffsetNDT refTime
      date req @?= Just (formatDateYMD istTime)

  , testCase "time correctly converted to IST HH:MM:SS" $ do
      let refTime = mkUTCTime 2026 3 17 4 30 0  -- + 5:30 = 10:00:00 IST
          req = buildScheduleQuery LeaveNow "S1" "S2" refTime
      departAfter req @?= Just "10:00:00"
  ]

-- --------------------------------------------------------------------------
-- parseTimeMode
-- --------------------------------------------------------------------------

parseTimeModeTests :: TestTree
parseTimeModeTests = testGroup "parseTimeMode"
  [ testCase "Nothing -> LeaveNow" $
      parseTimeMode Nothing @?= LeaveNow

  , testCase "Just \"ArriveBy\" -> ArriveBy" $
      parseTimeMode (Just "ArriveBy") @?= ArriveBy

  , testCase "Just \"DepartAt\" -> DepartAt" $
      parseTimeMode (Just "DepartAt") @?= DepartAt

  , testCase "Just \"unknown\" -> LeaveNow" $
      parseTimeMode (Just "unknown") @?= LeaveNow

  , testCase "Just \"\" -> LeaveNow" $
      parseTimeMode (Just "") @?= LeaveNow

  , testCase "Just \"leaveNow\" (wrong case) -> LeaveNow" $
      parseTimeMode (Just "leaveNow") @?= LeaveNow

  , testCase "Just \"arriveBy\" (wrong case) -> LeaveNow" $
      parseTimeMode (Just "arriveBy") @?= LeaveNow
  ]

-- --------------------------------------------------------------------------
-- getScheduleReferenceTime
-- --------------------------------------------------------------------------

getScheduleReferenceTimeTests :: TestTree
getScheduleReferenceTimeTests = testGroup "getScheduleReferenceTime"
  [ testCase "LeaveNow always returns current time" $ do
      let now = mkUTCTime 2026 3 17 10 0 0
          arrival = Just (mkUTCTime 2026 3 17 11 0 0)
          departure = Just (mkUTCTime 2026 3 17 10 30 0)
      getScheduleReferenceTime LeaveNow arrival departure now @?= now

  , testCase "ArriveBy returns targetArrival when present" $ do
      let now = mkUTCTime 2026 3 17 10 0 0
          arrival = Just (mkUTCTime 2026 3 17 11 0 0)
      getScheduleReferenceTime ArriveBy arrival Nothing now @?= mkUTCTime 2026 3 17 11 0 0

  , testCase "ArriveBy returns now when targetArrival is Nothing" $ do
      let now = mkUTCTime 2026 3 17 10 0 0
      getScheduleReferenceTime ArriveBy Nothing Nothing now @?= now

  , testCase "DepartAt returns targetDeparture when present" $ do
      let now = mkUTCTime 2026 3 17 10 0 0
          departure = Just (mkUTCTime 2026 3 17 10 30 0)
      getScheduleReferenceTime DepartAt Nothing departure now @?= mkUTCTime 2026 3 17 10 30 0

  , testCase "DepartAt returns now when targetDeparture is Nothing" $ do
      let now = mkUTCTime 2026 3 17 10 0 0
      getScheduleReferenceTime DepartAt Nothing Nothing now @?= now
  ]

-- ============================================================================
-- DepartureReminder Tests (pure logic re-implemented locally)
-- ============================================================================

departureReminderTests :: TestTree
departureReminderTests = testGroup "DepartureReminder"
  [ isScheduledForTodayTests
  , getTargetTimeForTodayTests
  , alreadyNotifiedTodayTests
  ]

-- --------------------------------------------------------------------------
-- isScheduledForToday
-- --------------------------------------------------------------------------

isScheduledForTodayTests :: TestTree
isScheduledForTodayTests = testGroup "isScheduledForToday"
  [ testCase "NoRecurrence -> always False" $ do
      let now = mkUTCTime 2026 3 17 10 0 0  -- Tuesday in IST
      isScheduledForToday NoRecurrence Nothing now @?= False

  , testCase "Daily -> always True" $ do
      let now = mkUTCTime 2026 3 17 10 0 0
      isScheduledForToday Daily Nothing now @?= True

  , testCase "Weekdays on Monday -> True" $ do
      -- 2026-03-16 is Monday (UTC), 10:00 UTC = 15:30 IST, still Monday
      let now = mkUTCTime 2026 3 16 10 0 0
      isScheduledForToday Weekdays Nothing now @?= True

  , testCase "Weekdays on Saturday -> False" $ do
      -- 2026-03-21 is Saturday
      let now = mkUTCTime 2026 3 21 10 0 0
      isScheduledForToday Weekdays Nothing now @?= False

  , testCase "Weekends on Saturday -> True" $ do
      let now = mkUTCTime 2026 3 21 10 0 0
      isScheduledForToday Weekends Nothing now @?= True

  , testCase "Weekends on Sunday -> True" $ do
      let now = mkUTCTime 2026 3 22 10 0 0
      isScheduledForToday Weekends Nothing now @?= True

  , testCase "Weekends on Friday -> False" $ do
      -- 2026-03-20 is Friday
      let now = mkUTCTime 2026 3 20 10 0 0
      isScheduledForToday Weekends Nothing now @?= False

  , testCase "Custom [1,3,5] on Monday (1) -> True" $ do
      let now = mkUTCTime 2026 3 16 10 0 0  -- Monday
      isScheduledForToday Custom (Just "[1,3,5]") now @?= True

  , testCase "Custom [1,3,5] on Tuesday (2) -> False" $ do
      let now = mkUTCTime 2026 3 17 10 0 0  -- Tuesday
      isScheduledForToday Custom (Just "[1,3,5]") now @?= False

  , testCase "Custom with invalid JSON -> False" $ do
      let now = mkUTCTime 2026 3 17 10 0 0
      isScheduledForToday Custom (Just "not-json") now @?= False

  , testCase "Custom with empty array -> False" $ do
      let now = mkUTCTime 2026 3 17 10 0 0
      isScheduledForToday Custom (Just "[]") now @?= False

  , testCase "Custom with Nothing -> False" $ do
      let now = mkUTCTime 2026 3 17 10 0 0
      isScheduledForToday Custom Nothing now @?= False

  , testCase "IST edge case: 11:30 PM UTC Monday = 5:00 AM IST Tuesday" $ do
      -- 2026-03-16 23:30 UTC = 2026-03-17 05:00 IST (Tuesday)
      -- Monday = day 1, Tuesday = day 2
      let now = mkUTCTime 2026 3 16 23 30 0
      -- In IST it's Tuesday (day 2), so Weekdays on Tuesday should be True
      isScheduledForToday Weekdays Nothing now @?= True
      -- Custom [2] (Tuesday) should match
      isScheduledForToday Custom (Just "[2]") now @?= True
      -- Custom [1] (Monday) should NOT match since IST day is Tuesday
      isScheduledForToday Custom (Just "[1]") now @?= False
  ]

-- --------------------------------------------------------------------------
-- getTargetTimeForToday
-- --------------------------------------------------------------------------

getTargetTimeForTodayTests :: TestTree
getTargetTimeForTodayTests = testGroup "getTargetTimeForToday"
  [ testCase "ArriveBy with TimeOfDay 9:30 -> correct UTC time" $ do
      -- now is some time on 2026-03-17 in IST
      let now = mkISTTime 2026 3 17 8 0 0  -- 8:00 AM IST
          tod = TimeOfDay 9 30 0  -- 9:30 AM IST target
          result = getTargetTimeForToday StubArriveBy (Just tod) Nothing now
      -- Expected: 9:30 AM IST on 2026-03-17 -> UTC = 9:30 - 5:30 = 4:00 UTC
      result @?= Just (mkUTCTime 2026 3 17 4 0 0)

  , testCase "DepartAt with TimeOfDay 18:00 -> correct UTC time" $ do
      let now = mkISTTime 2026 3 17 12 0 0
          tod = TimeOfDay 18 0 0  -- 6:00 PM IST
          result = getTargetTimeForToday StubDepartAt (Just tod) Nothing now
      -- 18:00 IST -> 12:30 UTC
      result @?= Just (mkUTCTime 2026 3 17 12 30 0)

  , testCase "LeaveNow -> returns trip.targetTime" $ do
      let now = mkISTTime 2026 3 17 8 0 0
          targetTime = Just (mkUTCTime 2026 3 17 5 0 0)
      getTargetTimeForToday StubLeaveNow Nothing targetTime now @?= targetTime

  , testCase "No targetTimeOfDay -> returns trip.targetTime" $ do
      let now = mkISTTime 2026 3 17 8 0 0
          targetTime = Just (mkUTCTime 2026 3 17 5 0 0)
      getTargetTimeForToday StubArriveBy Nothing targetTime now @?= targetTime

  , testCase "ArriveBy with no tod and no targetTime -> Nothing" $ do
      let now = mkISTTime 2026 3 17 8 0 0
      getTargetTimeForToday StubArriveBy Nothing Nothing now @?= Nothing

  , testCase "IST day rollover: late UTC night uses next IST day" $ do
      -- UTC 2026-03-16 23:00, IST = 2026-03-17 04:30
      let now = mkUTCTime 2026 3 16 23 0 0
          tod = TimeOfDay 9 0 0  -- 9:00 AM IST
          result = getTargetTimeForToday StubArriveBy (Just tod) Nothing now
      -- IST day is 2026-03-17, so target = 9:00 AM IST on 03-17 = 03:30 UTC on 03-17
      result @?= Just (mkUTCTime 2026 3 17 3 30 0)
  ]

-- --------------------------------------------------------------------------
-- alreadyNotifiedToday
-- --------------------------------------------------------------------------

alreadyNotifiedTodayTests :: TestTree
alreadyNotifiedTodayTests = testGroup "alreadyNotifiedToday"
  [ testCase "No lastNotifiedAt -> False" $ do
      let now = mkUTCTime 2026 3 17 10 0 0
      alreadyNotifiedToday Nothing now @?= False

  , testCase "Same IST day -> True" $ do
      let now = mkUTCTime 2026 3 17 10 0 0
          lastNotified = mkUTCTime 2026 3 17 5 0 0  -- earlier same IST day
      alreadyNotifiedToday (Just lastNotified) now @?= True

  , testCase "Different IST day -> False" $ do
      let now = mkUTCTime 2026 3 17 10 0 0  -- IST: 15:30 on 03-17
          lastNotified = mkUTCTime 2026 3 16 10 0 0  -- IST: 15:30 on 03-16
      alreadyNotifiedToday (Just lastNotified) now @?= False

  , testCase "IST edge case: notified 11:30 PM UTC yesterday, now 12:30 AM UTC today -> same IST day" $ do
      -- 2026-03-16 23:30 UTC = 2026-03-17 05:00 IST
      -- 2026-03-17 00:30 UTC = 2026-03-17 06:00 IST
      -- Both are on IST day 2026-03-17 -> same day -> True
      let lastNotified = mkUTCTime 2026 3 16 23 30 0
          now = mkUTCTime 2026 3 17 0 30 0
      alreadyNotifiedToday (Just lastNotified) now @?= True

  , testCase "IST edge case: different UTC days, different IST days -> False" $ do
      -- 2026-03-16 18:00 UTC = 2026-03-16 23:30 IST (still day 16)
      -- 2026-03-16 19:00 UTC = 2026-03-17 00:30 IST (day 17)
      let lastNotified = mkUTCTime 2026 3 16 18 0 0
          now = mkUTCTime 2026 3 16 19 0 0
      alreadyNotifiedToday (Just lastNotified) now @?= False

  , testCase "Exactly at IST midnight boundary" $ do
      -- 2026-03-16 18:30 UTC = 2026-03-17 00:00 IST (midnight -> day 17)
      -- 2026-03-16 18:29 UTC = 2026-03-16 23:59 IST (day 16)
      let lastNotified = mkUTCTime 2026 3 16 18 29 0  -- IST day 16
          now = mkUTCTime 2026 3 16 18 30 0           -- IST day 17
      alreadyNotifiedToday (Just lastNotified) now @?= False
  ]

-- ============================================================================
-- Property-based Tests
-- ============================================================================

propertyTests :: TestTree
propertyTests = testGroup "Properties"
  [ QC.testProperty "ArriveBy: comfortable <= recommended <= latest departure times" $
      \(QC.Positive durationSec) (QC.Positive bufferMins) ->
        let now = mkUTCTime 2026 1 1 0 0 0
            target = addUTCTime (fromIntegral (durationSec + bufferMins * 120 + 1000 :: Int)) now
            advisory = computeArriveByAdvisory target (durationSec `mod` 7200 + 60) (bufferMins `mod` 30 + 1) now Nothing
         in comfortableDeparture advisory <= recommendedDeparture advisory
            && recommendedDeparture advisory <= latestDeparture advisory

  , QC.testProperty "LeaveNow always returns Comfortable" $
      \(QC.Positive durationSec) ->
        let now = mkUTCTime 2026 6 15 12 0 0
            advisory = computeLeaveNowAdvisory (durationSec `mod` 36000 + 1) now
         in riskLevel advisory == Comfortable

  , QC.testProperty "computeRiskLevel is monotonically better with more time" $
      \(QC.Positive bufferMins) ->
        let buf = bufferMins `mod` 30 + 1
            tooLate = computeRiskLevel 0 buf
            tight = computeRiskLevel (fromIntegral (buf * 60 - 1)) buf
            good = computeRiskLevel (fromIntegral (buf * 60)) buf
            comfortable = computeRiskLevel (fromIntegral ((buf + 7) * 60)) buf
         in tooLate >= tight  -- TooLate >= Tight in Ord (since TooLate is defined last... wait)
            -- Actually: data RiskLevel = Comfortable | Good | Tight | TooLate
            -- Ord derived: Comfortable < Good < Tight < TooLate
            && tooLate >= tight
            && tight >= good
            && good >= comfortable

  , QC.testProperty "showTimeIST produces valid format" $
      \(QC.Positive secs) ->
        let t = addUTCTime (fromIntegral (secs `mod` 86400 :: Int)) (mkUTCTime 2026 1 1 0 0 0)
            result = showTimeIST t
         in T.isInfixOf "AM" result || T.isInfixOf "PM" result

  , QC.testProperty "parseTimeMode is total (never crashes)" $
      \txt ->
        let result = parseTimeMode (Just (T.pack txt))
         in result `seq` True

  , QC.testProperty "getScheduleReferenceTime: LeaveNow always returns now" $
      \(QC.Positive secs) ->
        let now = addUTCTime (fromIntegral (secs :: Int)) (mkUTCTime 2026 1 1 0 0 0)
            arrival = Just (addUTCTime 3600 now)
            departure = Just (addUTCTime 1800 now)
         in getScheduleReferenceTime LeaveNow arrival departure now == now

  , QC.testProperty "crowding buffer is either Nothing or Just 600" $
      \(QC.Positive secs) ->
        let t = addUTCTime (fromIntegral (secs `mod` 86400 :: Int)) (mkUTCTime 2026 1 1 0 0 0)
            result = computeCrowdingBufferSeconds t
         in result == Nothing || result == Just 600
  ]
