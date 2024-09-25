{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module SharedLogic.KaalChakra.Utils
  ( DailySchedulerTime (..),
    WeeklySchedulerTime (..),
    MonthlySchedulerTime (..),
    QuarterlySchedulerTime (..),
    getCurrentDailyJobTime,
    getCurrentWeeklyJobTime,
    getCurrentMonthlyJobTime,
    getCurrentQuarterlyJobTime,
    incrementDay,
    incrementWeek,
    incrementMonth,
    incrementQuarter,
    testTimeDaily,
    testTimeWeekly,
    testTimeMonthly,
    testTimeQuarterly,
  )
where

import qualified Data.Time.Calendar as Time
import qualified Data.Time.Clock as Time
import qualified Data.Time.LocalTime as Time
import Kernel.Prelude
import Kernel.Types.Error
import Kernel.Utils.Common hiding (Hours, Minutes)
import Kernel.Utils.Dhall (FromDhall)

data DailySchedulerTime = DailySchedulerTime
  { minutes :: Int,
    hours :: Int
  }
  deriving (Generic, FromDhall)

data WeeklySchedulerTime = WeeklySchedulerTime
  { minutes :: Int,
    hours :: Int,
    dayOfWeek :: Time.DayOfWeek
  }
  deriving (Generic, FromDhall)

data MonthlySchedulerTime = MonthlySchedulerTime
  { minutes :: Int,
    hours :: Int,
    dayOfMonth :: Int
  }
  deriving (Generic, FromDhall)

data QuarterlySchedulerTime = QuarterlySchedulerTime
  { minutes :: Int,
    hours :: Int,
    dayOfMonth :: Int,
    monthOfQuarter :: Int
  }
  deriving (Generic, FromDhall)

data TimeUnit = Minutes | Hours | DayOfMonth | MonthOfQuarter
  deriving (Show)

timeUnitRange :: TimeUnit -> (Int, Int)
timeUnitRange Minutes = (0, 59)
timeUnitRange Hours = (0, 23)
timeUnitRange DayOfMonth = (1, 28)
timeUnitRange MonthOfQuarter = (1, 3)

validateTimeUnit :: (MonadFlow m) => TimeUnit -> Int -> m ()
validateTimeUnit timeUnit value = do
  let (minLimit, maxLimit) = timeUnitRange timeUnit
  unless (minLimit <= value && value <= maxLimit) $
    throwError (InternalError $ "Invalid time unit value: " <> show value <> "; unit: " <> show timeUnit <> "; range: (" <> show minLimit <> ", " <> show maxLimit <> ")")

getCurrentDailyJobTime :: (MonadFlow m) => DailySchedulerTime -> m UTCTime
getCurrentDailyJobTime schedulerTime@DailySchedulerTime {..} = do
  validateTimeUnit Minutes minutes
  validateTimeUnit Hours hours
  now <- getCurrentTime
  pure $ mkDailyJobTime now schedulerTime

mkDailyJobTime :: UTCTime -> DailySchedulerTime -> UTCTime
mkDailyJobTime now DailySchedulerTime {..} = do
  let scheduledTime = replaceHoursAndMinutes hours minutes now
  if scheduledTime >= now
    then scheduledTime
    else incrementDay scheduledTime

getCurrentWeeklyJobTime :: (MonadFlow m) => WeeklySchedulerTime -> m UTCTime
getCurrentWeeklyJobTime schedulerTime@WeeklySchedulerTime {..} = do
  validateTimeUnit Minutes minutes
  validateTimeUnit Hours hours
  now <- getCurrentTime
  pure $ mkWeeklyJobTime now schedulerTime

mkWeeklyJobTime :: UTCTime -> WeeklySchedulerTime -> UTCTime
mkWeeklyJobTime now WeeklySchedulerTime {..} = do
  let scheduledTime = replaceDayOfWeek dayOfWeek . replaceHoursAndMinutes hours minutes $ now
  if scheduledTime >= now
    then scheduledTime
    else incrementWeek scheduledTime

getCurrentMonthlyJobTime :: (MonadFlow m) => MonthlySchedulerTime -> m UTCTime
getCurrentMonthlyJobTime schedulerTime@MonthlySchedulerTime {..} = do
  validateTimeUnit Minutes minutes
  validateTimeUnit Hours hours
  validateTimeUnit DayOfMonth dayOfMonth
  now <- getCurrentTime
  pure $ mkMonthlyJobTime now schedulerTime

mkMonthlyJobTime :: UTCTime -> MonthlySchedulerTime -> UTCTime
mkMonthlyJobTime now MonthlySchedulerTime {..} = do
  let scheduledTime = replaceDayOfMonth dayOfMonth . replaceHoursAndMinutes hours minutes $ now
  if scheduledTime >= now
    then scheduledTime
    else incrementMonth scheduledTime

getCurrentQuarterlyJobTime :: (MonadFlow m) => QuarterlySchedulerTime -> m UTCTime
getCurrentQuarterlyJobTime schedulerTime@QuarterlySchedulerTime {..} = do
  validateTimeUnit Minutes minutes
  validateTimeUnit Hours hours
  validateTimeUnit DayOfMonth dayOfMonth
  validateTimeUnit MonthOfQuarter monthOfQuarter
  now <- getCurrentTime
  pure $ mkQuarterlyJobTime now schedulerTime

mkQuarterlyJobTime :: UTCTime -> QuarterlySchedulerTime -> UTCTime
mkQuarterlyJobTime now QuarterlySchedulerTime {..} = do
  let scheduledTime = replaceMonthOfQuarter monthOfQuarter . replaceDayOfMonth dayOfMonth . replaceHoursAndMinutes hours minutes $ now
  if scheduledTime >= now
    then scheduledTime
    else incrementQuarter scheduledTime

replaceHoursAndMinutes :: Int -> Int -> UTCTime -> UTCTime
replaceHoursAndMinutes hours minutes time =
  Time.UTCTime
    { utctDay = Time.utctDay time,
      utctDayTime = Time.timeOfDayToTime $ Time.TimeOfDay {todHour = hours, todMin = minutes, todSec = 0}
    }

-- change date to closest required day of week in future (or do not change, if not needed)
replaceDayOfWeek :: Time.DayOfWeek -> UTCTime -> UTCTime
replaceDayOfWeek dayOfWeek time = do
  let daysDiff = Time.dayOfWeekDiff dayOfWeek (Time.dayOfWeek (Time.utctDay time))
  addDays daysDiff time

replaceDayOfMonth :: Int -> UTCTime -> UTCTime
replaceDayOfMonth dayOfMonth time = do
  let (year, month, _oldDay) = Time.toGregorian (Time.utctDay time)
  Time.UTCTime
    { utctDay = Time.fromGregorian year month dayOfMonth,
      utctDayTime = Time.utctDayTime time
    }

replaceMonthOfQuarter :: Int -> UTCTime -> UTCTime
replaceMonthOfQuarter monthOfQuarter time = do
  let (year, oldMonth, day) = Time.toGregorian (Time.utctDay time)
  let quarterNumber = (oldMonth - 1) `div` 3 -- 0, 1, 2 or 3
  let newMonth = quarterNumber * 3 + monthOfQuarter
  Time.UTCTime
    { utctDay = Time.fromGregorian year newMonth day,
      utctDayTime = Time.utctDayTime time
    }

incrementDay :: UTCTime -> UTCTime
incrementDay = addDays 1

addDays :: Int -> UTCTime -> UTCTime
addDays days = addUTCTime $ fromInteger (toEnum days * 86400)

incrementWeek :: UTCTime -> UTCTime
incrementWeek = addDays 7

incrementMonth :: UTCTime -> UTCTime
incrementMonth = addMonth 1

addMonth :: Int -> UTCTime -> UTCTime
addMonth months time = do
  let (oldYear, oldMonth, day) = Time.toGregorian (Time.utctDay time)
  let newMonth = ((oldMonth + months - 1) `mod` 12) + 1
  let newYear = oldYear + toEnum ((oldMonth + months - 1) `div` 12)
  Time.UTCTime
    { utctDay = Time.fromGregorian newYear newMonth day,
      utctDayTime = Time.utctDayTime time
    }

incrementQuarter :: UTCTime -> UTCTime
incrementQuarter = addMonth 3

-- tests

testTimeDaily :: Int -> Int -> IO [UTCTime]
testTimeDaily hours minutes = do
  let seed = DailySchedulerTime {..}
  now <- getCurrentTime
  pure $ testTimeLoop incrementDay 20 $ mkDailyJobTime now seed

testTimeWeekly :: Time.DayOfWeek -> Int -> Int -> IO [UTCTime]
testTimeWeekly dayOfWeek hours minutes = do
  let seed = WeeklySchedulerTime {..}
  now <- getCurrentTime
  pure $ testTimeLoop incrementWeek 20 $ mkWeeklyJobTime now seed

testTimeMonthly :: Int -> Int -> Int -> IO [UTCTime]
testTimeMonthly dayOfMonth hours minutes = do
  let seed = MonthlySchedulerTime {..}
  now <- getCurrentTime
  pure $ testTimeLoop incrementMonth 20 $ mkMonthlyJobTime now seed

testTimeQuarterly :: Int -> Int -> Int -> Int -> IO [UTCTime]
testTimeQuarterly monthOfQuarter dayOfMonth hours minutes = do
  let seed = QuarterlySchedulerTime {..}
  now <- getCurrentTime
  pure $ testTimeLoop incrementQuarter 20 $ mkQuarterlyJobTime now seed

testTimeLoop :: (UTCTime -> UTCTime) -> Int -> UTCTime -> [UTCTime]
testTimeLoop _ 0 _ = []
testTimeLoop f count seed = seed : testTimeLoop f (count -1) (f seed)
