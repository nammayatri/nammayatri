{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module SharedLogic.Scheduler.Jobs.DepartureReminder where

import Data.Time (UTCTime(..), addUTCTime, utctDay)
import qualified Data.Time.Calendar.WeekDate as WeekDate
import qualified Data.Aeson as Aeson
import Kernel.External.Types (SchedulerFlow)
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common
import Lib.Scheduler
import qualified Domain.Types.SavedTrip as DST
import SharedLogic.ReachOnTime.DepartureAdvisor (computeArriveByAdvisory, computeDepartAtAdvisory, istOffsetSeconds)
import qualified Storage.Queries.SavedTripExtra as QSavedTrip

-- | Process all active recurring saved trips and send departure reminders
processDepartureReminders ::
  ( CacheFlow m r,
    MonadFlow m,
    EsqDBFlow m r,
    SchedulerFlow r
  ) =>
  m ()
processDepartureReminders = do
  activeTrips <- QSavedTrip.findAllActiveRecurring
  now <- getCurrentTime
  forM_ activeTrips $ \trip -> do
    when (isScheduledForToday trip.recurrence trip.customDays now) $ do
      let mbTargetTime = getTargetTimeForToday trip now
      case mbTargetTime of
        Nothing -> pure ()
        Just targetTime -> do
          let durationEstimate = 3600 -- default 1 hour estimate; in production, query route
          let advisory = case trip.timeMode of
                DST.DepartAt ->
                  computeDepartAtAdvisory targetTime durationEstimate now
                _ ->
                  computeArriveByAdvisory targetTime durationEstimate trip.bufferMinutes now Nothing
          let notifyTime = addUTCTime (negate $ fromIntegral (trip.notifyBeforeMinutes * 60)) advisory.recommendedDeparture
          when (now >= notifyTime && not (alreadyNotifiedToday trip now)) $ do
            logInfo $ "Sending departure reminder for saved trip: " <> show trip.id
            -- In production: sendPushNotification trip.riderId advisory
            QSavedTrip.updateLastNotified (Just now) (Just advisory.recommendedDeparture) trip.id

-- | Check if a trip's recurrence matches today (using IST day)
isScheduledForToday :: DST.TripRecurrence -> Maybe Text -> UTCTime -> Bool
isScheduledForToday recurrence mbCustomDays now =
  let istDay = utctDay (addUTCTime istOffsetNDT now) -- use IST day, not UTC day
      (_, _, dayOfWeek) = WeekDate.toWeekDate istDay
   in case recurrence of
        DST.NoRecurrence -> False
        DST.Daily -> True
        DST.Weekdays -> dayOfWeek >= 1 && dayOfWeek <= 5
        DST.Weekends -> dayOfWeek >= 6
        DST.Custom -> case mbCustomDays of
          Nothing -> False
          Just daysJson ->
            -- Parse as JSON array of ints for exact matching (avoids isInfixOf false positives)
            case Aeson.decode (encodeUtf8 daysJson) of
              Just (days :: [Int]) -> dayOfWeek `elem` days
              Nothing -> False

-- | Get the target time for today based on saved trip's time-of-day
-- Target time-of-day is in IST; we convert to UTC for internal use
getTargetTimeForToday :: DST.SavedTrip -> UTCTime -> Maybe UTCTime
getTargetTimeForToday trip now =
  case (trip.timeMode, trip.targetTimeOfDay) of
    (DST.ArriveBy, Just tod) ->
      let istDay = utctDay (addUTCTime istOffsetNDT now) -- IST day
          todSecs = timeOfDayToSeconds tod
          -- Construct IST target time, then convert back to UTC
          istTargetUTC = UTCTime istDay (fromIntegral todSecs)
       in Just $ addUTCTime (negate istOffsetNDT) istTargetUTC
    (DST.DepartAt, Just tod) ->
      let istDay = utctDay (addUTCTime istOffsetNDT now) -- IST day
          todSecs = timeOfDayToSeconds tod
          istTargetUTC = UTCTime istDay (fromIntegral todSecs)
       in Just $ addUTCTime (negate istOffsetNDT) istTargetUTC
    _ -> trip.targetTime

-- | Check if we already sent a notification today (using IST day)
alreadyNotifiedToday :: DST.SavedTrip -> UTCTime -> Bool
alreadyNotifiedToday trip now =
  case trip.lastNotifiedAt of
    Nothing -> False
    Just lastNotified ->
      let istDayNow = utctDay (addUTCTime istOffsetNDT now)
          istDayNotified = utctDay (addUTCTime istOffsetNDT lastNotified)
       in istDayNotified == istDayNow

-- | Convert TimeOfDay to seconds since midnight
timeOfDayToSeconds :: TimeOfDay -> Int
timeOfDayToSeconds (TimeOfDay h m s) = h * 3600 + m * 60 + round s

-- | Seconds to TimeOfDay
secondsToTimeOfDay :: Int -> TimeOfDay
secondsToTimeOfDay secs =
  let h = secs `div` 3600
      m = (secs `mod` 3600) `div` 60
      s = secs `mod` 60
   in TimeOfDay h m (fromIntegral s)

-- | IST offset as NominalDiffTime (UTC+5:30 = 19800 seconds)
-- Uses the shared istOffsetSeconds from DepartureAdvisor
istOffsetNDT :: NominalDiffTime
istOffsetNDT = fromIntegral istOffsetSeconds
