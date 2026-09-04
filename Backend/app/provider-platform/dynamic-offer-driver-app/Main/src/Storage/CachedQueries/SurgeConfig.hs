{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | City+tier cache over surge_config. Deliberately no per-version stickiness:
-- clearing this one key on any status change makes activation and rollback
-- take effect on the next search.
module Storage.CachedQueries.SurgeConfig
  ( findAllByCityAndServiceTier,
    clearCache,
  )
where

import Domain.Types.Common (ServiceTierType)
import qualified Domain.Types.MerchantOperatingCity as DMOC
import Domain.Types.SurgeConfig (SurgeConfig)
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.Queries.SurgeConfig as Queries

findAllByCityAndServiceTier :: (CacheFlow m r, EsqDBFlow m r) => Id DMOC.MerchantOperatingCity -> ServiceTierType -> m [SurgeConfig]
findAllByCityAndServiceTier merchantOpCityId serviceTier =
  Hedis.withCrossAppRedis (Hedis.safeGet $ makeCityTierKey merchantOpCityId serviceTier) >>= \case
    Just a -> pure a
    Nothing -> do
      configs <- Queries.findAllByCityAndServiceTier merchantOpCityId serviceTier
      expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
      Hedis.withCrossAppRedis $ Hedis.setExp (makeCityTierKey merchantOpCityId serviceTier) configs expTime
      pure configs

clearCache :: Hedis.HedisFlow m r => Id DMOC.MerchantOperatingCity -> ServiceTierType -> m ()
clearCache merchantOpCityId serviceTier =
  Hedis.runInMultiCloudRedisWrite $
    Hedis.withCrossAppRedis $
      Hedis.del (makeCityTierKey merchantOpCityId serviceTier)

makeCityTierKey :: Id DMOC.MerchantOperatingCity -> ServiceTierType -> Text
makeCityTierKey merchantOpCityId serviceTier = "driver-offer:CachedQueries:SurgeConfig:CityId-" <> merchantOpCityId.getId <> ":Tier-" <> show serviceTier
