{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module SharedLogic.Allocator.Jobs.BatchPipelineTuner where

import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import EulerHS.Prelude hiding (id)
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Id
import Kernel.Utils.Common
import Lib.Scheduler
import qualified SharedLogic.Allocator as SAllocator

-- | Metrics snapshot read from Redis for tuning decisions
data PipelineMetrics = PipelineMetrics
  { avgBatchLatencyMs :: Double,
    p99BatchLatencyMs :: Double,
    snapToRoadFailureRate :: Double,
    activeRideCount :: Int,
    locUpdatesPerSec :: Double
  }
  deriving (Generic, Show, Eq, FromJSON, ToJSON)

-- | Safety limits for auto-tuning: max 20% change per cycle
maxChangePercent :: Double
maxChangePercent = 0.20

-- | Minimum cooldown between tuning actions (seconds)
tuningCooldownSec :: Int
tuningCooldownSec = 900 -- 15 minutes

-- | Runs every 5 minutes per merchant-city.
-- Reads pipeline metrics, applies tuning rules, updates config.
-- Safety: max 20% change per cycle, 15-min cooldown, manual override flag.
-- Initially runs in shadow mode (logs only).
runBatchPipelineTuner ::
  ( CacheFlow m r,
    EsqDBFlow m r,
    MonadFlow m
  ) =>
  Job 'SAllocator.BatchPipelineTuner ->
  m ExecutionResult
runBatchPipelineTuner Job {id, jobInfo} = withLogTag ("JobId-" <> id.getId) $ do
  let jobData = jobInfo.jobData
      merchantId = jobData.merchantId
      merchantOperatingCityId = jobData.merchantOperatingCityId
  logInfo $ "BatchPipelineTuner running for merchant " <> merchantId.getId <> " city " <> merchantOperatingCityId.getId

  -- Check manual override flag
  let overrideKey = mkManualOverrideKey merchantId merchantOperatingCityId
  manualOverride :: Maybe Bool <- Redis.safeGet overrideKey
  if manualOverride == Just True
    then do
      logInfo "BatchPipelineTuner: manual override active, skipping tuning"
      reschedule
    else do
      -- Check cooldown
      let cooldownKey = mkTuningCooldownKey merchantId merchantOperatingCityId
      lastTuning :: Maybe UTCTime <- Redis.safeGet cooldownKey
      now <- getCurrentTime
      let cooldownElapsed = case lastTuning of
            Just lastTime -> diffUTCTime now lastTime > fromIntegral tuningCooldownSec
            Nothing -> True

      if not cooldownElapsed
        then do
          logInfo "BatchPipelineTuner: cooldown period active, skipping"
          reschedule
        else do
          -- Read pipeline metrics from Redis
          let metricsKey = mkPipelineMetricsKey merchantId merchantOperatingCityId
          mbMetrics :: Maybe PipelineMetrics <- Redis.safeGet metricsKey
          case mbMetrics of
            Nothing -> do
              logInfo "BatchPipelineTuner: no metrics available, skipping"
              reschedule
            Just metrics -> do
              -- Shadow mode: log recommendations without applying
              let recommendations = computeTuningRecommendations metrics
              logInfo $ "BatchPipelineTuner [SHADOW MODE]: recommendations = " <> show recommendations
              -- Set cooldown
              Redis.setExp cooldownKey now tuningCooldownSec
              reschedule
  where
    reschedule = do
      -- Reschedule in 5 minutes
      ReSchedule . addUTCTime 300 <$> getCurrentTime

-- | Tuning recommendation (shadow mode only for now)
data TuningRecommendation = TuningRecommendation
  { field :: Text,
    currentValue :: Double,
    recommendedValue :: Double,
    reason :: Text
  }
  deriving (Generic, Show, Eq, FromJSON, ToJSON)

-- | Compute tuning recommendations based on pipeline metrics.
-- Applies the 20% max change constraint.
computeTuningRecommendations :: PipelineMetrics -> [TuningRecommendation]
computeTuningRecommendations metrics =
  catMaybes
    [ -- If p99 latency is high, recommend increasing batch interval
      if metrics.p99BatchLatencyMs > 5000
        then
          Just
            TuningRecommendation
              { field = "clientBatchIntervalSec",
                currentValue = 3.0,
                recommendedValue = clampChange 3.0 (3.0 * 1.15),
                reason = "p99 latency exceeds 5000ms"
              }
        else Nothing,
      -- If snap-to-road failure rate is high, recommend reducing batch size
      if metrics.snapToRoadFailureRate > 0.1
        then
          Just
            TuningRecommendation
              { field = "maxSnapToRoadReqPoints",
                currentValue = 100.0,
                recommendedValue = clampChange 100.0 (100.0 * 0.85),
                reason = "snap-to-road failure rate exceeds 10%"
              }
        else Nothing,
      -- If load is high, recommend increasing LTS drainer batch size
      if metrics.locUpdatesPerSec > 1000
        then
          Just
            TuningRecommendation
              { field = "ltsBatchSize",
                currentValue = 20.0,
                recommendedValue = clampChange 20.0 (20.0 * 1.15),
                reason = "high location update rate"
              }
        else Nothing
    ]

-- | Clamp a value change to at most maxChangePercent from the current value
clampChange :: Double -> Double -> Double
clampChange current recommended =
  let maxDelta = current * maxChangePercent
      delta = recommended - current
      clampedDelta = max (-maxDelta) (min maxDelta delta)
   in current + clampedDelta

mkManualOverrideKey :: Id DM.Merchant -> Id DMOC.MerchantOperatingCity -> Text
mkManualOverrideKey merchantId cityId =
  "BatchTuner:ManualOverride:merchantId:" <> merchantId.getId <> ":cityId:" <> cityId.getId

mkTuningCooldownKey :: Id DM.Merchant -> Id DMOC.MerchantOperatingCity -> Text
mkTuningCooldownKey merchantId cityId =
  "BatchTuner:Cooldown:merchantId:" <> merchantId.getId <> ":cityId:" <> cityId.getId

mkPipelineMetricsKey :: Id DM.Merchant -> Id DMOC.MerchantOperatingCity -> Text
mkPipelineMetricsKey merchantId cityId =
  "BatchTuner:Metrics:merchantId:" <> merchantId.getId <> ":cityId:" <> cityId.getId
