module Domain.Action.UI.EarningProjection
  ( getDriverEarningsProjectionDaily,
    getDriverEarningsProjectionWeekly,
    putDriverEarningsGoal,
    getDriverEarningsGoal,
    getDriverEarningsProjectionAccuracy,
  )
where

import qualified API.Types.UI.EarningProjection as API
import qualified Data.Text as T
import Data.Time (Day, DayOfWeek (..), addDays, dayOfWeek, toGregorian, utctDay)
import qualified Data.Time as T
import Domain.Types.DailyStats (DailyStats (..))
import qualified Domain.Types.DriverEarningGoal as DEG
import Domain.Types.EarningProjectionLog (ProjectionType (..))
import Domain.Types.Merchant (Merchant)
import Domain.Types.MerchantOperatingCity (MerchantOperatingCity)
import Domain.Types.Person (Person)
import Environment
import EulerHS.Prelude hiding (id, map)
import Kernel.Prelude
import Kernel.Types.Common
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified SharedLogic.EarningProjectionEngine as Engine
import qualified Storage.Queries.DailyStats as QDailyStats
import qualified Storage.Queries.DailyStatsExtra as QDailyStatsExtra
import qualified Storage.Queries.DriverEarningGoal as QDriverEarningGoal

getDriverEarningsProjectionDaily ::
  ( Maybe (Id Person),
    Id Merchant,
    Id MerchantOperatingCity
  ) ->
  Flow API.DailyProjectionResp
getDriverEarningsProjectionDaily (mbPersonId, _merchantId, merchantOpCityId) = do
  personId <- fromMaybeM (InvalidRequest "Driver not found") mbPersonId
  now <- getCurrentTime
  let today = utctDay now
  -- Fetch historical daily stats for same day-of-week (last 8 weeks)
  let dayOfWeekToday = dayOfWeek today
      historicalDates = getHistoricalDatesForDayOfWeek dayOfWeekToday today 8
      (fromDate, toDate) = getDateRange historicalDates
  historicalStats <- QDailyStatsExtra.findAllInRangeByDriverId personId fromDate toDate
  let sameDayStats = filterByDayOfWeek dayOfWeekToday historicalStats
  -- Get today's stats
  todayStats <- QDailyStats.findByDriverIdAndDate personId today
  -- Calculate projection
  projection <- Engine.calculateDailyProjection merchantOpCityId sameDayStats todayStats now
  -- Get goal info
  mbGoal <- QDriverEarningGoal.findByDriverId personId
  let currentEarnings = maybe 0 (round . (.totalEarnings)) todayStats
      ridesCompleted = maybe 0 (.numRides) todayStats
      goalProgress = mkGoalProgress mbGoal currentEarnings projection.expected
  -- Generate tips
  tips <- Engine.generateDailyTips merchantOpCityId sameDayStats now
  let accuracy = Engine.calculateHistoricalAccuracy sameDayStats
  pure
    API.DailyProjectionResp
      { projectedEarnings =
          API.ProjectionRange
            { low = projection.low,
              expected = projection.expected,
              high = projection.high,
              currency = "INR"
            },
        currentEarnings = currentEarnings,
        ridesCompleted = ridesCompleted,
        hoursActive = maybe 0.0 (\s -> fromIntegral s.totalRideTime / 3600.0) todayStats,
        factors = projection.factors,
        goal = goalProgress,
        tips = tips,
        historicalAccuracy = accuracy,
        lastUpdated = now
      }

getDriverEarningsProjectionWeekly ::
  ( Maybe (Id Person),
    Id Merchant,
    Id MerchantOperatingCity
  ) ->
  Flow API.WeeklyProjectionResp
getDriverEarningsProjectionWeekly (mbPersonId, _merchantId, merchantOpCityId) = do
  personId <- fromMaybeM (InvalidRequest "Driver not found") mbPersonId
  now <- getCurrentTime
  let today = utctDay now
      weekStart = getWeekStart today
      weekEnd = addDays 7 weekStart
  -- Fetch this week's actual stats
  weekStats <- QDailyStatsExtra.findAllInRangeByDriverId personId weekStart weekEnd
  let currentWeekEarnings = sum $ map (round . (.totalEarnings)) weekStats
  -- Fetch historical data for each day of the week
  dailyBreakdown <- forM [0 .. 6] $ \dayOffset -> do
    let targetDate = addDays dayOffset weekStart
        targetDow = dayOfWeek targetDate
        historicalDates = getHistoricalDatesForDayOfWeek targetDow today 8
        (fromDate, toDate) = getDateRange historicalDates
    historicalForDay <- QDailyStatsExtra.findAllInRangeByDriverId personId fromDate toDate
    let sameDayHistorical = filterByDayOfWeek targetDow historicalForDay
        projected = Engine.weightedAverageEarnings sameDayHistorical
        actualForDate = find (\s -> s.merchantLocalDate == targetDate) weekStats
        actual = fmap (round . (.totalEarnings)) actualForDate
    pure
      API.DayBreakdown
        { day = T.pack $ show targetDow,
          projected = projected,
          actual = actual,
          isToday = targetDate == today
        }
  let daysRemaining = length $ filter (\db -> isNothing db.actual && not db.isToday) dailyBreakdown
      projectedTotal = sum $ map (.projected) dailyBreakdown
      projLow = round (fromIntegral projectedTotal * (0.8 :: Double))
      projHigh = round (fromIntegral projectedTotal * (1.2 :: Double))
  -- Goal info
  mbGoal <- QDriverEarningGoal.findByDriverId personId
  let weeklyGoal = case mbGoal of
        Just goal -> case goal.weeklyGoalAmount of
          Just wga ->
            Just
              API.WeeklyGoalProgress
                { amount = wga,
                  progressPct = if wga > 0 then min 100 (currentWeekEarnings * 100 `div` wga) else 0,
                  onTrack = currentWeekEarnings >= (projectedTotal * (7 - daysRemaining) `div` 7)
                }
          Nothing -> Nothing
        Nothing -> Nothing
  pure
    API.WeeklyProjectionResp
      { projectedEarnings =
          API.ProjectionRange
            { low = projLow,
              expected = projectedTotal,
              high = projHigh,
              currency = "INR"
            },
        currentWeekEarnings = currentWeekEarnings,
        daysRemaining = daysRemaining,
        dailyBreakdown = dailyBreakdown,
        weeklyGoal = weeklyGoal
      }

putDriverEarningsGoal ::
  ( Maybe (Id Person),
    Id Merchant,
    Id MerchantOperatingCity
  ) ->
  API.SetEarningGoalReq ->
  Flow API.SetEarningGoalResp
putDriverEarningsGoal (mbPersonId, _merchantId, merchantOpCityId) req = do
  personId <- fromMaybeM (InvalidRequest "Driver not found") mbPersonId
  now <- getCurrentTime
  let goal =
        DEG.DriverEarningGoal
          { driverId = personId,
            dailyGoalAmount = req.dailyGoalAmount,
            dailyGoalCurrency = "INR",
            weeklyGoalAmount = req.weeklyGoalAmount,
            weeklyGoalCurrency = "INR",
            merchantOperatingCityId = merchantOpCityId,
            createdAt = now,
            updatedAt = now
          }
  QDriverEarningGoal.upsert goal
  pure API.SetEarningGoalResp {result = "Success"}

getDriverEarningsGoal ::
  ( Maybe (Id Person),
    Id Merchant,
    Id MerchantOperatingCity
  ) ->
  Flow API.EarningGoalResp
getDriverEarningsGoal (mbPersonId, _merchantId, merchantOpCityId) = do
  personId <- fromMaybeM (InvalidRequest "Driver not found") mbPersonId
  mbGoal <- QDriverEarningGoal.findByDriverId personId
  now <- getCurrentTime
  let today = utctDay now
      weekStart = getWeekStart today
      weekEnd = addDays 7 weekStart
  todayStats <- QDailyStats.findByDriverIdAndDate personId today
  weekStats <- QDailyStatsExtra.findAllInRangeByDriverId personId weekStart weekEnd
  let dailyEarnings = maybe 0 (round . (.totalEarnings)) todayStats
      weeklyEarnings = sum $ map (round . (.totalEarnings)) weekStats
      dailyGoal = mbGoal >>= (.dailyGoalAmount)
      weeklyGoal = mbGoal >>= (.weeklyGoalAmount)
  pure
    API.EarningGoalResp
      { dailyGoalAmount = dailyGoal,
        weeklyGoalAmount = weeklyGoal,
        dailyProgress = dailyEarnings,
        weeklyProgress = weeklyEarnings,
        dailyProgressPct = maybe 0 (\g -> if g > 0 then min 100 (dailyEarnings * 100 `div` g) else 0) dailyGoal,
        weeklyProgressPct = maybe 0 (\g -> if g > 0 then min 100 (weeklyEarnings * 100 `div` g) else 0) weeklyGoal
      }

getDriverEarningsProjectionAccuracy ::
  ( Maybe (Id Person),
    Id Merchant,
    Id MerchantOperatingCity
  ) ->
  Flow API.ProjectionAccuracyResp
getDriverEarningsProjectionAccuracy (mbPersonId, _merchantId, _merchantOpCityId) = do
  personId <- fromMaybeM (InvalidRequest "Driver not found") mbPersonId
  now <- getCurrentTime
  let today = utctDay now
      last7Days = addDays (-7) today
      last30Days = addDays (-30) today
  -- Fetch recent projection logs
  recentLogs7 <- QDailyStatsExtra.findAllInRangeByDriverId personId last7Days today
  recentLogs30 <- QDailyStatsExtra.findAllInRangeByDriverId personId last30Days today
  -- Use DailyStats as proxy for accuracy tracking since we track projected vs actual
  let avgAccuracy7 = Engine.calculateHistoricalAccuracy recentLogs7
      avgAccuracy30 = Engine.calculateHistoricalAccuracy recentLogs30
  pure
    API.ProjectionAccuracyResp
      { averageAccuracy = avgAccuracy30,
        last7DaysAccuracy = avgAccuracy7,
        last30DaysAccuracy = avgAccuracy30,
        totalProjections = length recentLogs30
      }

-- Helper functions

getHistoricalDatesForDayOfWeek :: DayOfWeek -> Day -> Int -> [Day]
getHistoricalDatesForDayOfWeek dow today numWeeks =
  [addDays (fromIntegral $ -7 * i) today | i <- [1 .. numWeeks], dayOfWeek (addDays (fromIntegral $ -7 * i) today) == dow]

getDateRange :: [Day] -> (Day, Day)
getDateRange [] = (toEnum 0, toEnum 0)
getDateRange dates = (minimum dates, maximum dates)

filterByDayOfWeek :: DayOfWeek -> [DailyStats] -> [DailyStats]
filterByDayOfWeek dow = filter (\s -> dayOfWeek s.merchantLocalDate == dow)

getWeekStart :: Day -> Day
getWeekStart today =
  let dow = dayOfWeek today
      daysBack = case dow of
        Monday -> 0
        Tuesday -> 1
        Wednesday -> 2
        Thursday -> 3
        Friday -> 4
        Saturday -> 5
        Sunday -> 6
   in addDays (negate $ fromIntegral daysBack) today

mkGoalProgress :: Maybe DEG.DriverEarningGoal -> Int -> Int -> Maybe API.GoalProgress
mkGoalProgress mbGoal currentEarnings projectedExpected = do
  goal <- mbGoal
  dailyAmount <- goal.dailyGoalAmount
  let remaining = max 0 (dailyAmount - currentEarnings)
      avgPerRide = if projectedExpected > 0 then max 1 (projectedExpected `div` 15) else 200
  Just
    API.GoalProgress
      { dailyGoalAmount = dailyAmount,
        progressPct = if dailyAmount > 0 then min 100 (currentEarnings * 100 `div` dailyAmount) else 0,
        remainingAmount = remaining,
        estimatedRidesNeeded = if avgPerRide > 0 then (remaining + avgPerRide - 1) `div` avgPerRide else 0
      }
