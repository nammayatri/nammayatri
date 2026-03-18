module SharedLogic.EarningProjectionEngine
  ( calculateDailyProjection,
    calculateWeeklyProjection,
    getDemandMultiplier,
    getTimeOfDayFactor,
    recordProjectionAccuracy,
    weightedAverageEarnings,
    generateDailyTips,
    calculateHistoricalAccuracy,
    DailyProjectionResult (..),
  )
where

import qualified API.Types.UI.EarningProjection as API
import qualified Data.Text as T
import Data.Time (UTCTime (..), addDays, dayOfWeek, timeOfDayToTime, utctDayTime)
import qualified Data.Time as T
import Domain.Types.DailyStats (DailyStats (..))
import qualified Domain.Types.EarningProjectionLog as EPL
import Domain.Types.MerchantOperatingCity (MerchantOperatingCity)
import EulerHS.Prelude hiding (id, map, sum)
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.CacheFlow (CacheFlow)
import Kernel.Types.Common
import Kernel.Types.Id
import Kernel.Utils.Common
import Kernel.Utils.Logging (logDebug, logInfo)

data DailyProjectionResult = DailyProjectionResult
  { low :: Int,
    expected :: Int,
    high :: Int,
    factors :: API.ProjectionFactors
  }

-- | Calculate daily earning projection using weighted historical DailyStats
calculateDailyProjection ::
  ( CacheFlow m r,
    EsqDBFlow m r,
    MonadFlow m
  ) =>
  Id MerchantOperatingCity ->
  [DailyStats] ->
  Maybe DailyStats ->
  UTCTime ->
  m DailyProjectionResult
calculateDailyProjection merchantOpCityId historicalStats todayStats now = do
  let baseProjection = weightedAverageEarnings historicalStats
  -- Get demand multiplier from Redis (same pattern as DemandHotspots)
  demandMult <- getDemandMultiplier merchantOpCityId
  -- Calculate time-of-day factor (how much of productive day remains)
  let timeFactor = getTimeOfDayFactor now
  -- Calculate current day earnings
  let currentEarnings = maybe 0 (round . (.totalEarnings)) todayStats
  -- Combine factors
  let adjustedBase = round (fromIntegral baseProjection * demandMult * timeFactor :: Double)
      surgeBonus = round (fromIntegral baseProjection * max 0 (demandMult - 1.0) * 0.3 :: Double)
      streakBonus = if maybe 0 (.numRides) todayStats >= 5 then round (fromIntegral baseProjection * (0.05 :: Double)) else 0
      expected = max currentEarnings (adjustedBase + surgeBonus + streakBonus)
      low = round (fromIntegral expected * (0.75 :: Double))
      high = round (fromIntegral expected * (1.25 :: Double))
  logDebug $ "Earning projection - base: " <> show baseProjection <> " demand: " <> show demandMult <> " timeFactor: " <> show timeFactor
  pure
    DailyProjectionResult
      { low = low,
        expected = expected,
        high = high,
        factors =
          API.ProjectionFactors
            { baseProjection = baseProjection,
              demandMultiplier = demandMult,
              surgeBonus = surgeBonus,
              streakBonus = streakBonus,
              timeRemainingFactor = timeFactor
            }
      }

-- | Calculate weekly projection by aggregating daily projections
calculateWeeklyProjection ::
  ( CacheFlow m r,
    EsqDBFlow m r,
    MonadFlow m
  ) =>
  Id MerchantOperatingCity ->
  [[DailyStats]] ->
  UTCTime ->
  m (Int, Int, Int)
calculateWeeklyProjection merchantOpCityId dailyHistories now = do
  dailyProjections <- forM dailyHistories $ \dayStats -> do
    projection <- calculateDailyProjection merchantOpCityId dayStats Nothing now
    pure projection.expected
  let total = sum dailyProjections
      low = round (fromIntegral total * (0.8 :: Double))
      high = round (fromIntegral total * (1.2 :: Double))
  pure (low, total, high)

-- | Weighted average of historical earnings, with more recent weeks weighted higher
weightedAverageEarnings :: [DailyStats] -> Int
weightedAverageEarnings [] = 0
weightedAverageEarnings stats =
  let indexed = zip [1 ..] (reverse stats) -- oldest first
      weights = map (\(i, _) -> i :: Double) indexed
      totalWeight = sum weights
      weightedSum = sum $ map (\(w, s) -> w * fromIntegral (round (s.totalEarnings) :: Int)) indexed
   in if totalWeight > 0
        then round (weightedSum / totalWeight)
        else 0

-- | Get demand multiplier from Redis (pattern from DemandHotspots)
getDemandMultiplier ::
  ( CacheFlow m r,
    MonadFlow m
  ) =>
  Id MerchantOperatingCity ->
  m Double
getDemandMultiplier merchantOpCityId = do
  let key = mkDemandMultiplierKey merchantOpCityId
  mbMultiplier :: Maybe Double <- Redis.safeGet key
  pure $ fromMaybe 1.0 mbMultiplier

-- | Calculate time-of-day factor based on remaining productive hours
-- Productive hours assumed 6 AM to 11 PM (17 hours)
getTimeOfDayFactor :: UTCTime -> Double
getTimeOfDayFactor now =
  let secondsInDay = utctDayTime now
      hourOfDay = realToFrac secondsInDay / 3600.0 :: Double
      -- Productive window: 6 AM to 23 PM = 17 hours
      productiveStart = 6.0
      productiveEnd = 23.0
      totalProductiveHours = productiveEnd - productiveStart
      remainingHours = max 0 (productiveEnd - max productiveStart hourOfDay)
   in if totalProductiveHours > 0
        then remainingHours / totalProductiveHours
        else 0.0

-- | Record projection accuracy at end of day (to be called by allocator job)
recordProjectionAccuracy ::
  ( CacheFlow m r,
    EsqDBFlow m r,
    MonadFlow m
  ) =>
  Id MerchantOperatingCity ->
  m ()
recordProjectionAccuracy _merchantOpCityId = do
  logInfo "Recording end-of-day projection accuracy"
  -- This would be implemented as part of the allocator job
  -- For now, accuracy is calculated on-the-fly from historical data
  pure ()

-- | Generate contextual tips based on historical data and current conditions
generateDailyTips ::
  ( CacheFlow m r,
    MonadFlow m
  ) =>
  Id MerchantOperatingCity ->
  [DailyStats] ->
  UTCTime ->
  m [T.Text]
generateDailyTips merchantOpCityId historicalStats now = do
  demandMult <- getDemandMultiplier merchantOpCityId
  let tips = catMaybes
        [ if demandMult > 1.1
            then Just $ "Demand is " <> T.pack (show (round ((demandMult - 1.0) * 100) :: Int)) <> "% higher than usual in your area"
            else Nothing,
          if length historicalStats >= 4
            then
              let avgEarnings = weightedAverageEarnings historicalStats
               in if avgEarnings > 2000
                    then Just "Your earnings on this day of the week are typically above average"
                    else Nothing
            else Just "We're building your projection model - keep driving to improve accuracy",
          let hourOfDay = realToFrac (utctDayTime now) / 3600.0 :: Double
           in if hourOfDay < 10.0
                then Just "Morning hours tend to have steady demand for commuter rides"
                else
                  if hourOfDay > 17.0 && hourOfDay < 20.0
                    then Just "Evening peak hours - higher earning potential right now"
                    else Nothing
        ]
  pure $ take 3 tips

-- | Calculate historical accuracy from DailyStats trends
calculateHistoricalAccuracy :: [DailyStats] -> Double
calculateHistoricalAccuracy [] = 0.0
calculateHistoricalAccuracy stats =
  let earnings = map (round . (.totalEarnings) :: DailyStats -> Int) stats
      avg = if null earnings then 0 else sum earnings `div` length earnings
      deviations = map (\e -> abs (e - avg)) earnings
      avgDeviation = if null deviations then 0 else sum deviations `div` length deviations
      accuracy = if avg > 0 then max 0 (100.0 - (fromIntegral avgDeviation / fromIntegral avg * 100.0)) else 0.0
   in min 100.0 accuracy

-- Redis key helpers

mkDemandMultiplierKey :: Id MerchantOperatingCity -> Text
mkDemandMultiplierKey cityId = "EarningProjection:DemandMult:cityId:" <> cityId.getId

mkProjectionCacheKey :: Text -> Text -> Text
mkProjectionCacheKey driverId projType = "EarningProjection:Cache:" <> projType <> ":driver:" <> driverId
