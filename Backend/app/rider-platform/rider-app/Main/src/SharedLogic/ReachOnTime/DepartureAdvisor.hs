module SharedLogic.ReachOnTime.DepartureAdvisor where

import Data.Time (UTCTime, NominalDiffTime, addUTCTime, diffUTCTime, TimeOfDay (..))
import qualified Data.Time as Time
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common

-- | Risk level for departure advisory
data RiskLevel = Comfortable | Good | Tight | TooLate
  deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON)

-- | Safety warning for late-night walking legs
data SafetyWarning = SafetyWarning
  { legOrder :: Int,
    warning :: Text,
    severity :: Text
  }
  deriving (Generic, Show, ToJSON, FromJSON)

-- | Departure advisory for a time-constrained journey
data DepartureAdvisory = DepartureAdvisory
  { latestDeparture :: UTCTime,
    recommendedDeparture :: UTCTime,
    comfortableDeparture :: UTCTime,
    riskLevel :: RiskLevel,
    bufferMinutes :: Int,
    advisoryMessage :: Text,
    safetyWarnings :: [SafetyWarning]
  }
  deriving (Generic, Show, ToJSON, FromJSON)

-- | Compute departure advisory for "Arrive By" mode
computeArriveByAdvisory ::
  UTCTime -> -- target arrival time
  Int -> -- total journey duration in seconds
  Int -> -- buffer minutes
  UTCTime -> -- current time
  Maybe Int -> -- crowding buffer seconds
  DepartureAdvisory
computeArriveByAdvisory targetArrival durationSec bufferMins now mbCrowdingBuffer =
  let crowdingBuffer = fromMaybe 0 mbCrowdingBuffer
      totalDuration = fromIntegral (durationSec + crowdingBuffer)
      latestDep = addUTCTime (negate totalDuration) targetArrival
      bufferSec = fromIntegral (bufferMins * 60) :: NominalDiffTime
      recommendedDep = addUTCTime (negate bufferSec) latestDep
      comfortableDep = addUTCTime (negate (bufferSec + 600)) latestDep -- +10 min generous buffer
      availableTime = diffUTCTime latestDep now
      risk = computeRiskLevel availableTime bufferMins
      msg = generateAdvisoryMessage risk availableTime bufferMins
   in DepartureAdvisory
        { latestDeparture = latestDep,
          recommendedDeparture = recommendedDep,
          comfortableDeparture = comfortableDep,
          riskLevel = risk,
          bufferMinutes = bufferMins,
          advisoryMessage = msg,
          safetyWarnings = []
        }

-- | Compute departure advisory for "Depart At" mode
computeDepartAtAdvisory ::
  UTCTime -> -- planned departure time
  Int -> -- total journey duration in seconds
  UTCTime -> -- current time
  DepartureAdvisory
computeDepartAtAdvisory departTime durationSec now =
  let arrivalTime = addUTCTime (fromIntegral durationSec) departTime
      availableTime = diffUTCTime departTime now
      risk =
        if availableTime <= 0
          then TooLate
          else Comfortable
      msg =
        if availableTime <= 0
          then "Departure time has passed"
          else "Estimated arrival at " <> showTimeIST arrivalTime
   in DepartureAdvisory
        { latestDeparture = departTime,
          recommendedDeparture = departTime,
          comfortableDeparture = departTime,
          riskLevel = risk,
          bufferMinutes = 0,
          advisoryMessage = msg,
          safetyWarnings = []
        }

-- | Compute departure advisory for "Leave Now" mode
computeLeaveNowAdvisory ::
  Int -> -- total journey duration in seconds
  UTCTime -> -- current time
  DepartureAdvisory
computeLeaveNowAdvisory durationSec now =
  let arrivalTime = addUTCTime (fromIntegral durationSec) now
   in DepartureAdvisory
        { latestDeparture = now,
          recommendedDeparture = now,
          comfortableDeparture = now,
          riskLevel = Comfortable,
          bufferMinutes = 0,
          advisoryMessage = "Estimated arrival at " <> showTimeIST arrivalTime,
          safetyWarnings = []
        }

-- | Compute risk level based on available buffer time
computeRiskLevel :: NominalDiffTime -> Int -> RiskLevel
computeRiskLevel availableTime bufferMins
  | availableTime <= 0 = TooLate
  | availableTime < fromIntegral (bufferMins * 60) = Tight
  | availableTime < fromIntegral ((bufferMins + 7) * 60) = Good
  | otherwise = Comfortable

-- | Generate human-readable advisory message
generateAdvisoryMessage :: RiskLevel -> NominalDiffTime -> Int -> Text
generateAdvisoryMessage risk availableTime bufferMins =
  case risk of
    TooLate -> "You can no longer make it on time. Consider alternative options."
    Tight -> "Leave immediately! You have less than " <> show bufferMins <> " minutes of buffer."
    Good -> "Leave soon. You have about " <> show (round (availableTime / 60) :: Int) <> " minutes before you need to depart."
    Comfortable -> "You have plenty of time. Recommended departure in " <> show (round (availableTime / 60) :: Int) <> " minutes."

-- | Compute peak-hour crowding buffer for Chennai bus routes
-- During peak hours (7:30-10:00 AM and 5:00-8:00 PM), buses may pass full.
computeCrowdingBufferSeconds :: UTCTime -> Maybe Int
computeCrowdingBufferSeconds departureTime =
  let -- IST is UTC+5:30
      istOffset = 5 * 3600 + 30 * 60
      istTime = addUTCTime (fromIntegral (istOffset :: Int)) departureTime
      (_, timeOfDay) = Time.utctDayTime istTime `divMod'` (24 * 3600)
      hours = timeOfDay `div` 3600
      mins = (timeOfDay `mod` 3600) `div` 60
      isPeakMorning = hours >= 7 && (hours < 10 || (hours == 7 && mins >= 30))
      isPeakEvening = hours >= 17 && hours < 20
   in if isPeakMorning || isPeakEvening
        then Just 600 -- 10 minutes default crowding buffer
        else Nothing

-- | Check if any walking legs are late-night and potentially unsafe
flagLateNightWalking :: UTCTime -> [(Int, Text, Maybe Double)] -> [SafetyWarning]
flagLateNightWalking departureTime legs =
  let istOffset = 5 * 3600 + 30 * 60
      istTime = addUTCTime (fromIntegral (istOffset :: Int)) departureTime
      (_, timeOfDay) = Time.utctDayTime istTime `divMod'` (24 * 3600)
      hours = timeOfDay `div` 3600
      isLateNight = hours >= 21 || hours < 5
   in if isLateNight
        then mapMaybe checkWalkLeg legs
        else []
  where
    checkWalkLeg (legIdx, mode, mbDistMeters)
      | mode == "Walk" && fromMaybe 0 mbDistMeters > 300 =
          Just
            SafetyWarning
              { legOrder = legIdx,
                warning = "Walking " <> show (round (fromMaybe 0 mbDistMeters) :: Int) <> "m after dark. Consider auto instead.",
                severity = "Medium"
              }
      | otherwise = Nothing

-- | Format UTC time as IST string (HH:MM AM/PM)
showTimeIST :: UTCTime -> Text
showTimeIST utcTime =
  let istOffset = 5 * 3600 + 30 * 60
      istTime = addUTCTime (fromIntegral (istOffset :: Int)) utcTime
      tod = Time.utctDayTime istTime
      totalSecs = floor tod :: Int
      hours = totalSecs `div` 3600
      mins = (totalSecs `mod` 3600) `div` 60
      (displayHours, amPm) =
        if hours >= 12
          then (if hours > 12 then hours - 12 else hours, "PM" :: Text)
          else (if hours == 0 then 12 else hours, "AM" :: Text)
   in show displayHours <> ":" <> (if mins < 10 then "0" else "") <> show mins <> " " <> amPm

-- | Helper for divMod on NominalDiffTime
divMod' :: NominalDiffTime -> NominalDiffTime -> (Int, Int)
divMod' a b =
  let totalSecs = floor a :: Int
      divisor = floor b :: Int
   in if divisor == 0 then (0, totalSecs) else totalSecs `divMod` divisor
