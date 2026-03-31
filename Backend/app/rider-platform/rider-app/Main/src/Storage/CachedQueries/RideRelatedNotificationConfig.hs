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
    findAllByMerchantOperatingCityIdInRideFlow,
    findAllByMerchantOperatingCityIdAndTimeDiffEventInRideFlow,
    updateByPrimaryKey,
  )
where

import Domain.Types.MerchantOperatingCity
import Domain.Types.RideRelatedNotificationConfig
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Lib.Yudhishthira.Types as LYT
import Storage.Beam.Yudhishthira ()
import qualified Storage.Queries.RideRelatedNotificationConfig as Queries

create :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => RideRelatedNotificationConfig -> m ()
create val = do
  Queries.create val
  clearCache val.merchantOperatingCityId val.timeDiffEvent

findAllByMerchantOperatingCityId :: (CacheFlow m r, EsqDBFlow m r) => Id MerchantOperatingCity -> Maybe [LYT.ConfigVersionMap] -> m [RideRelatedNotificationConfig]
findAllByMerchantOperatingCityId id _mbConfigVersionMap =
  Hedis.safeGet (makeMerchantOpCityIdAllKey id) >>= \case
    Just configs -> return configs
    Nothing -> cacheAllRideRelatedNotificationConfigs id /=<< Queries.findAllByMerchantOperatingCityId id

findAllByMerchantOperatingCityIdInRideFlow :: (CacheFlow m r, EsqDBFlow m r) => Id MerchantOperatingCity -> [LYT.ConfigVersionMap] -> m [RideRelatedNotificationConfig]
findAllByMerchantOperatingCityIdInRideFlow id configInExperimentVersions =
  findAllByMerchantOperatingCityId id (Just configInExperimentVersions)

findAllByMerchantOperatingCityIdAndTimeDiffEvent :: (CacheFlow m r, EsqDBFlow m r) => Id MerchantOperatingCity -> TimeDiffEvent -> Maybe [LYT.ConfigVersionMap] -> m [RideRelatedNotificationConfig]
findAllByMerchantOperatingCityIdAndTimeDiffEvent id timeDiffEvent _mbConfigVersionMap =
  Hedis.safeGet (makeMerchantOpCityIdAndTimeDiffEventKey id timeDiffEvent) >>= \case
    Just configs -> return configs
    Nothing ->
      cacheRideRelatedNotificationConfigsByEvent id timeDiffEvent
        /=<< Queries.findAllByMerchantOperatingCityIdAndTimeDiffEvent id timeDiffEvent

findAllByMerchantOperatingCityIdAndTimeDiffEventInRideFlow :: (CacheFlow m r, EsqDBFlow m r) => Id MerchantOperatingCity -> TimeDiffEvent -> [LYT.ConfigVersionMap] -> m [RideRelatedNotificationConfig]
findAllByMerchantOperatingCityIdAndTimeDiffEventInRideFlow id timeDiffEvent configInExperimentVersions =
  findAllByMerchantOperatingCityIdAndTimeDiffEvent id timeDiffEvent (Just configInExperimentVersions)

cacheAllRideRelatedNotificationConfigs :: (CacheFlow m r) => Id MerchantOperatingCity -> [RideRelatedNotificationConfig] -> m ()
cacheAllRideRelatedNotificationConfigs cityId configs = do
  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
  Hedis.setExp (makeMerchantOpCityIdAllKey cityId) configs expTime

cacheRideRelatedNotificationConfigsByEvent :: (CacheFlow m r) => Id MerchantOperatingCity -> TimeDiffEvent -> [RideRelatedNotificationConfig] -> m ()
cacheRideRelatedNotificationConfigsByEvent cityId timeDiffEvent configs = do
  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
  Hedis.setExp (makeMerchantOpCityIdAndTimeDiffEventKey cityId timeDiffEvent) configs expTime

makeMerchantOpCityIdAllKey :: Id MerchantOperatingCity -> Text
makeMerchantOpCityIdAllKey id = "CachedQueries:RideRelatedNotificationConfig:MerchantOperatingCityId-" <> id.getId <> "-all"

makeMerchantOpCityIdAndTimeDiffEventKey :: Id MerchantOperatingCity -> TimeDiffEvent -> Text
makeMerchantOpCityIdAndTimeDiffEventKey id timeDiffEvent = "CachedQueries:RideRelatedNotificationConfig:MerchantOperatingCityId-" <> id.getId <> ":TimeDiffEvent-" <> show timeDiffEvent

clearCache :: (CacheFlow m r, EsqDBFlow m r) => Id MerchantOperatingCity -> TimeDiffEvent -> m ()
clearCache merchantOpCityId timeDiffEvent = do
  Hedis.del (makeMerchantOpCityIdAndTimeDiffEventKey merchantOpCityId timeDiffEvent)
  Hedis.del (makeMerchantOpCityIdAllKey merchantOpCityId)

updateByPrimaryKey :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => RideRelatedNotificationConfig -> m ()
updateByPrimaryKey cfg = do
  Queries.updateByPrimaryKey cfg
  clearCache cfg.merchantOperatingCityId cfg.timeDiffEvent
