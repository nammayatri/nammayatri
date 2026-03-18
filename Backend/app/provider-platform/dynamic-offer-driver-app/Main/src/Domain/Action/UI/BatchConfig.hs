{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.UI.BatchConfig
  ( getBatchConfig,
  )
where

import qualified Domain.Types.BatchPipelineConfig as DBPC
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import EulerHS.Prelude hiding (id)
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Id
import Kernel.Utils.Common

-- | GET endpoint that returns batch config for a merchant/city.
-- Also writes LTS-relevant subset to Redis for LTS polling.
getBatchConfig ::
  ( MonadFlow m,
    CacheFlow m r,
    EsqDBFlow m r
  ) =>
  Id DM.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  m DBPC.BatchPipelineConfig
getBatchConfig merchantId merchantOperatingCityId = do
  let redisKey = mkBatchConfigRedisKey merchantId merchantOperatingCityId
  cachedConfig :: Maybe DBPC.BatchPipelineConfig <- Redis.safeGet redisKey
  case cachedConfig of
    Just config -> pure config
    Nothing -> do
      -- Build default config when no DB record exists yet
      now <- getCurrentTime
      configId <- generateGUID
      let config =
            DBPC.BatchPipelineConfig
              { id = configId,
                clientBatchSize = 10,
                clientBatchIntervalSec = 3,
                clientMinDisplacementMeters = 25.0,
                clientLocationUpdateIntervalSec = 1,
                ltsDrainerSize = 50,
                ltsDrainerDelaySec = 2,
                ltsBatchSize = 20,
                normalRideBulkLocUpdateBatchSize = 5,
                maxSnapToRoadReqPoints = 100,
                telemetrySampleRate = 0.1,
                telemetryEnabled = False,
                autoTuningEnabled = False,
                merchantId = merchantId,
                merchantOperatingCityId = merchantOperatingCityId,
                createdAt = now,
                updatedAt = now
              }
      -- Cache in Redis with TTL 300s for LTS polling
      Redis.setExp redisKey config 300
      -- Also store LTS-relevant subset separately
      let ltsKey = mkLtsBatchConfigRedisKey merchantId merchantOperatingCityId
      let ltsSubset =
            LtsBatchConfig
              { ltsDrainerSize = config.ltsDrainerSize,
                ltsDrainerDelaySec = config.ltsDrainerDelaySec,
                ltsBatchSize = config.ltsBatchSize
              }
      Redis.setExp ltsKey ltsSubset 300
      pure config

data LtsBatchConfig = LtsBatchConfig
  { ltsDrainerSize :: Int,
    ltsDrainerDelaySec :: Int,
    ltsBatchSize :: Int
  }
  deriving (Generic, Show, Eq, ToJSON, FromJSON)

mkBatchConfigRedisKey :: Id DM.Merchant -> Id DMOC.MerchantOperatingCity -> Text
mkBatchConfigRedisKey merchantId cityId =
  "BatchPipelineConfig:merchantId:" <> merchantId.getId <> ":cityId:" <> cityId.getId

mkLtsBatchConfigRedisKey :: Id DM.Merchant -> Id DMOC.MerchantOperatingCity -> Text
mkLtsBatchConfigRedisKey merchantId cityId =
  "LtsBatchConfig:merchantId:" <> merchantId.getId <> ":cityId:" <> cityId.getId
