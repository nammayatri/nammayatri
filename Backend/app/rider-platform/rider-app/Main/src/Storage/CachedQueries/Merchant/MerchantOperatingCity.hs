{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-deprecations #-}

module Storage.CachedQueries.Merchant.MerchantOperatingCity where

import Domain.Types.Merchant (Merchant)
import Domain.Types.Merchant.MerchantOperatingCity
import Kernel.Prelude
-- import qualified Kernel.Storage.Esqueleto as Esq
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Types.Id
import Kernel.Utils.Common
import Storage.CachedQueries.CacheConfig
import qualified Storage.Queries.Merchant.MerchantOperatingCity as Queries

findByMerchantIdAndCity :: (CacheFlow m r, EsqDBFlow m r) => Id Merchant -> City -> m (Maybe MerchantOperatingCity)
findByMerchantIdAndCity merchantId city =
  Hedis.safeGet (makeMerchantIdAndCityKey merchantId city) >>= \case
    Just a -> return a
    Nothing -> flip whenJust cachedMerchantIdAndCity /=<< Queries.findByMerchantIdAndCity merchantId city

findByMerchantId :: (CacheFlow m r, EsqDBFlow m r) => Id Merchant -> m (Maybe MerchantOperatingCity)
findByMerchantId merchantId =
  Hedis.safeGet (makeMerchantIdKey merchantId) >>= \case
    Just a -> return a
    Nothing -> flip whenJust cachedMerchantIdAndCity /=<< Queries.findByMerchantId merchantId

cachedMerchantIdAndCity :: CacheFlow m r => MerchantOperatingCity -> m ()
cachedMerchantIdAndCity merchantOperatingCity = do
  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
  let merchantIdAndCityKey = makeMerchantIdAndCityKey merchantOperatingCity.merchantId merchantOperatingCity.city
  Hedis.setExp merchantIdAndCityKey merchantOperatingCity expTime

makeMerchantIdAndCityKey :: Id Merchant -> City -> Text
makeMerchantIdAndCityKey merchantId city = "CachedQueries:MerchantOperatingCity:MerchantId-" <> merchantId.getId <> ":City-" <> show city

makeMerchantIdKey :: Id Merchant -> Text
makeMerchantIdKey merchantId = "CachedQueries:MerchantOperatingCity:MerchantId-" <> merchantId.getId
