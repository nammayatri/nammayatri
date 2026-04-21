{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-deprecations #-}

module Storage.CachedQueries.MerchantConfig
  ( create,
    findAllByMerchantOperatingCityId,
    findAllByMerchantOperatingCityIdInRideFlow,
    clearCache,
    updateByPrimaryKey,
  )
where

import Domain.Types.MerchantConfig
import Domain.Types.MerchantOperatingCity
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Lib.Yudhishthira.Types as LYT
import Storage.Beam.Yudhishthira ()
import qualified Storage.Queries.MerchantConfig as Queries

create :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => MerchantConfig -> m ()
create val = do
  Queries.create val
  clearCache val.merchantOperatingCityId

findAllByMerchantOperatingCityIdInRideFlow :: (CacheFlow m r, EsqDBFlow m r, MonadFlow m) => Id MerchantOperatingCity -> [LYT.ConfigVersionMap] -> m [MerchantConfig]
findAllByMerchantOperatingCityIdInRideFlow id configInExperimentVersions =
  findAllByMerchantOperatingCityId id (Just configInExperimentVersions)

findAllByMerchantOperatingCityId :: (CacheFlow m r, EsqDBFlow m r, MonadFlow m) => Id MerchantOperatingCity -> Maybe [LYT.ConfigVersionMap] -> m [MerchantConfig]
findAllByMerchantOperatingCityId id _mbConfigInExperimentVersions =
  Hedis.safeGet (makeMerchantOperatingCityIdKey id) >>= \case
    Just configs -> return configs
    Nothing -> cacheAllMerchantConfigs id /=<< Queries.findAllByMerchantOperatingCityId id True

cacheAllMerchantConfigs :: (CacheFlow m r) => Id MerchantOperatingCity -> [MerchantConfig] -> m ()
cacheAllMerchantConfigs cityId configs = do
  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
  Hedis.setExp (makeMerchantOperatingCityIdKey cityId) configs expTime

makeMerchantOperatingCityIdKey :: Id MerchantOperatingCity -> Text
makeMerchantOperatingCityIdKey id = "CachedQueries:MerchantConfig:MerchantOperatingCityId-" <> id.getId

clearCache :: (CacheFlow m r, EsqDBFlow m r, MonadFlow m) => Id MerchantOperatingCity -> m ()
clearCache id = Hedis.runInMultiCloudRedisWrite $ Hedis.del (makeMerchantOperatingCityIdKey id)

updateByPrimaryKey :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => MerchantConfig -> m ()
updateByPrimaryKey cfg = do
  Queries.updateByPrimaryKey cfg
  clearCache cfg.merchantOperatingCityId
