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
  ( findAllByMerchantOperatingCityId,
  )
where

import Domain.Types.Merchant.MerchantOperatingCity
import Domain.Types.MerchantConfig
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.Queries.MerchantConfig as Queries

findAllByMerchantOperatingCityId :: (CacheFlow m r, EsqDBFlow m r, MonadFlow m) => Id MerchantOperatingCity -> m [MerchantConfig]
findAllByMerchantOperatingCityId id =
  Hedis.safeGet (makeMerchantOperatingCityIdKey id) >>= \case
    Just a -> return a
    Nothing -> cacheMerchantOperatingCity id /=<< Queries.findAllByMerchantOperatingCityId id

cacheMerchantOperatingCity :: (CacheFlow m r) => Id MerchantOperatingCity -> [MerchantConfig] -> m ()
cacheMerchantOperatingCity merchantOperatingCityId merchantConfig = do
  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
  Hedis.setExp (makeMerchantOperatingCityIdKey merchantOperatingCityId) merchantConfig expTime

makeMerchantOperatingCityIdKey :: Id MerchantOperatingCity -> Text
makeMerchantOperatingCityIdKey id = "CachedQueries:MerchantConfig:MerchantOperatingCityId-" <> id.getId
