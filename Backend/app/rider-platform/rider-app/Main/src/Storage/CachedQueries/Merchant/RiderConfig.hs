{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-deprecations #-}

module Storage.CachedQueries.Merchant.RiderConfig
  ( findByMerchantOperatingCityId,
    clearCache,
  )
where

import Domain.Types.Merchant.RiderConfig
import Domain.Types.MerchantOperatingCity (MerchantOperatingCity)
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.Queries.Merchant.RiderConfig as Queries

findByMerchantOperatingCityId :: (CacheFlow m r, EsqDBFlow m r, MonadFlow m) => Id MerchantOperatingCity -> m (Maybe RiderConfig)
findByMerchantOperatingCityId id =
  Hedis.safeGet (makeMerchantOperatingCityIdKey id) >>= \case
    Just a -> return a
    Nothing -> flip whenJust cacheRiderConfig /=<< Queries.findByMerchantOperatingCityId id

cacheRiderConfig :: (CacheFlow m r) => RiderConfig -> m ()
cacheRiderConfig riderConfig = do
  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
  let riderConfigKey = makeMerchantOperatingCityIdKey riderConfig.merchantOperatingCityId
  Hedis.setExp riderConfigKey riderConfig expTime

makeMerchantOperatingCityIdKey :: Id MerchantOperatingCity -> Text
makeMerchantOperatingCityIdKey id = "CachedQueries:RiderConfig:MerchantOperatingCityId-" <> id.getId

clearCache :: Hedis.HedisFlow m r => Id MerchantOperatingCity -> m ()
clearCache merchanOperatingCityId = do
  Hedis.del (makeMerchantOperatingCityIdKey merchanOperatingCityId)
