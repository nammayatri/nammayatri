{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Storage.CachedQueries.FRFSConfig
  ( create,
    findByMerchantOperatingCityId,
    findByMerchantOperatingCityIdInRideFlow,
    clearCache,
    updateByPrimaryKey,
  )
where

import Domain.Types.FRFSConfig
import qualified Domain.Types.MerchantOperatingCity
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Lib.Yudhishthira.Types as LYT
import Storage.Beam.Yudhishthira ()
import qualified Storage.Queries.FRFSConfig as Queries

create :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Domain.Types.FRFSConfig.FRFSConfig -> m ()
create val = do
  Queries.create val
  clearCache val.merchantOperatingCityId

findByMerchantOperatingCityIdInRideFlow :: (CacheFlow m r, EsqDBFlow m r) => Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity -> [LYT.ConfigVersionMap] -> m (Maybe Domain.Types.FRFSConfig.FRFSConfig)
findByMerchantOperatingCityIdInRideFlow merchantOperatingCityId configInExperimentVersions = do
  findByMerchantOperatingCityId merchantOperatingCityId (Just configInExperimentVersions)

findByMerchantOperatingCityId :: (CacheFlow m r, EsqDBFlow m r) => Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity -> Maybe [LYT.ConfigVersionMap] -> m (Maybe Domain.Types.FRFSConfig.FRFSConfig)
findByMerchantOperatingCityId merchantOperatingCityId _mbConfigInExperimentVersions =
  Hedis.safeGet (makeMerchantOperatingCityIdKey merchantOperatingCityId) >>= \case
    Just config -> return $ Just config
    Nothing -> flip whenJust (cacheFRFSConfig merchantOperatingCityId) /=<< Queries.findByMerchantOperatingCityId merchantOperatingCityId

cacheFRFSConfig :: (CacheFlow m r) => Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity -> Domain.Types.FRFSConfig.FRFSConfig -> m ()
cacheFRFSConfig cityId config = do
  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
  Hedis.setExp (makeMerchantOperatingCityIdKey cityId) config expTime

makeMerchantOperatingCityIdKey :: Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity -> Text
makeMerchantOperatingCityIdKey id = "CachedQueries:FRFSConfig:MerchantOperatingCityId-" <> id.getId

clearCache :: (CacheFlow m r, EsqDBFlow m r) => Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity -> m ()
clearCache merchantOperatingCityId = Hedis.del (makeMerchantOperatingCityIdKey merchantOperatingCityId)

updateByPrimaryKey :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Domain.Types.FRFSConfig.FRFSConfig -> m ()
updateByPrimaryKey cfg = do
  Queries.updateByPrimaryKey cfg
  clearCache cfg.merchantOperatingCityId
