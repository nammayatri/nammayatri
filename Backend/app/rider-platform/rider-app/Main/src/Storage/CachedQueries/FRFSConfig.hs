{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Storage.CachedQueries.FRFSConfig
  ( findByMerchantOperatingCityId,
  )
where

import Domain.Types.FRFSConfig
import qualified Domain.Types.MerchantOperatingCity
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Hedis
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.Queries.FRFSConfig as Queries

findByMerchantOperatingCityId :: KvDbFlow m r => (Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity -> m (Maybe Domain.Types.FRFSConfig.FRFSConfig))
findByMerchantOperatingCityId merchantOperatingCityId = do
  Hedis.safeGet (makeIdKey merchantOperatingCityId) >>= \case
    Just a -> return $ Just a
    Nothing -> flip whenJust cacheFRFSConfig /=<< Queries.findByMerchantOperatingCityId merchantOperatingCityId

cacheFRFSConfig :: (CacheFlow m r) => Domain.Types.FRFSConfig.FRFSConfig -> m ()
cacheFRFSConfig frfsConfig = do
  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
  let idKey = makeIdKey frfsConfig.merchantOperatingCityId
  Hedis.setExp idKey frfsConfig expTime

makeIdKey :: Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity -> Text
makeIdKey merchantOperatingCityId = "CachedQueries:FRFSConfig:MerchantOperatingCity-" <> show merchantOperatingCityId
