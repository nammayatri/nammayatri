{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-deprecations #-}

module Storage.CachedQueries.RideRelatedNotificationConfig
  ( create,
    findAllByMerchantOperatingCityId,
    findAllByMerchantOperatingCityIdAndTimeDiffEvent,
    clearCache,
  )
where

import Domain.Types.MerchantOperatingCity
import Domain.Types.RideRelatedNotificationConfig
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.Queries.RideRelatedNotificationConfig as Queries

create :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => RideRelatedNotificationConfig -> m ()
create = Queries.create

findAllByMerchantOperatingCityId :: (CacheFlow m r, EsqDBFlow m r) => Id MerchantOperatingCity -> m [RideRelatedNotificationConfig]
findAllByMerchantOperatingCityId id =
  Hedis.withCrossAppRedis (Hedis.safeGet $ makeMerchantOpCityIdKey id) >>= \case
    Just a -> return a
    Nothing -> cacheRideRelatedNotificationConfigForCity id /=<< Queries.findAllByMerchantOperatingCityId id

cacheRideRelatedNotificationConfigForCity :: CacheFlow m r => Id MerchantOperatingCity -> [RideRelatedNotificationConfig] -> m ()
cacheRideRelatedNotificationConfigForCity merchantOperatingCityId cfg = do
  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
  let merchantIdKey = makeMerchantOpCityIdKey merchantOperatingCityId
  Hedis.withCrossAppRedis $ Hedis.setExp merchantIdKey cfg expTime

makeMerchantOpCityIdKey :: Id MerchantOperatingCity -> Text
makeMerchantOpCityIdKey id = "CachedQueries:RideRelatedNotificationConfig:MerchantOperatingCityId-" <> id.getId

findAllByMerchantOperatingCityIdAndTimeDiffEvent :: (CacheFlow m r, EsqDBFlow m r) => Id MerchantOperatingCity -> TimeDiffEvent -> m [RideRelatedNotificationConfig]
findAllByMerchantOperatingCityIdAndTimeDiffEvent id timeDiffEvent =
  Hedis.safeGet (makeMerchantOpCityIdAndTimeDiffEventKey id timeDiffEvent) >>= \case
    Just a -> return a
    Nothing -> cacheRideRelatedNotificationConfig (makeMerchantOpCityIdAndTimeDiffEventKey id timeDiffEvent) /=<< Queries.findAllByMerchantOperatingCityIdAndTimeDiffEvent id timeDiffEvent

cacheRideRelatedNotificationConfig :: CacheFlow m r => Text -> [RideRelatedNotificationConfig] -> m ()
cacheRideRelatedNotificationConfig key rideRelatedNotificationConfigs = do
  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
  Hedis.setExp key rideRelatedNotificationConfigs expTime

makeMerchantOpCityIdAndTimeDiffEventKey :: Id MerchantOperatingCity -> TimeDiffEvent -> Text
makeMerchantOpCityIdAndTimeDiffEventKey id timeDiffEvent = "CachedQueries:RideRelatedNotificationConfig:MerchantOperatingCityId-" <> id.getId <> ":TimeDiffEvent-" <> show timeDiffEvent

-- Call it after any update
clearCache :: Hedis.HedisFlow m r => Id MerchantOperatingCity -> TimeDiffEvent -> m ()
clearCache merchantOpCityId timeDiffEvent = do
  Hedis.del (makeMerchantOpCityIdAndTimeDiffEventKey merchantOpCityId timeDiffEvent)
