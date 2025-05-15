{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Storage.CachedQueries.FRFSConfig
  ( findByMerchantOperatingCityIdAndRouteId,
  )
where

import Domain.Types.FRFSConfig
import qualified Domain.Types.MerchantOperatingCity
import Domain.Types.Route
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Types.Id
import Kernel.Utils.Common
import Storage.Beam.Yudhishthira ()
import qualified Storage.Queries.FRFSConfig as Queries

findByMerchantOperatingCityIdAndRouteId :: (CacheFlow m r, EsqDBFlow m r, MonadFlow m) => (Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity -> Maybe (Id Route) -> m (Maybe Domain.Types.FRFSConfig.FRFSConfig))
findByMerchantOperatingCityIdAndRouteId merchantOperatingCityId routeId = do
  Hedis.safeGet (makeIdKey merchantOperatingCityId routeId) >>= \case
    Just a -> return $ Just a
    Nothing -> flip whenJust cacheFRFSConfig /=<< Queries.findByMerchantOperatingCityIdAndRouteId merchantOperatingCityId routeId

cacheFRFSConfig :: (CacheFlow m r) => Domain.Types.FRFSConfig.FRFSConfig -> m ()
cacheFRFSConfig frfsConfig = do
  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
  let idKey = makeIdKey frfsConfig.merchantOperatingCityId frfsConfig.routeId
  Hedis.setExp idKey frfsConfig expTime

makeIdKey :: Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity -> Maybe (Id Route) -> Text
makeIdKey merchantOperatingCityId routeId = "CachedQueries:FRFSConfig:MerchantOperatingCity-" <> merchantOperatingCityId.getId <> ":OndcSubscriberIdAndUniqueKeyId-" <> maybe "Nothing" getId routeId
