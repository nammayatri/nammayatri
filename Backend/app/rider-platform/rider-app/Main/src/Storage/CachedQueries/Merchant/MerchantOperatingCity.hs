{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-deprecations #-}

module Storage.CachedQueries.Merchant.MerchantOperatingCity
  ( findById,
    findByMerchantIdAndCity,
    findByMerchantShortIdAndCity,
  )
where

import Domain.Types.Merchant (Merchant)
import Domain.Types.Merchant.MerchantOperatingCity (MerchantOperatingCity)
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Hedis
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.Queries.Merchant.MerchantOperatingCity as Queries

findById :: (CacheFlow m r, EsqDBFlow m r) => Id MerchantOperatingCity -> m (Maybe MerchantOperatingCity)
findById id =
  Hedis.safeGet (makeMerchantOperatingCityIdKey id) >>= \case
    Just a -> return a
    Nothing -> flip whenJust cachedMerchantOperatingCityId /=<< Queries.findById id

findByMerchantIdAndCity :: (CacheFlow m r, EsqDBFlow m r) => Id Merchant -> Context.City -> m (Maybe MerchantOperatingCity)
findByMerchantIdAndCity merchantId city =
  Hedis.safeGet (makeMerchantIdAndCityKey merchantId city) >>= \case
    Just a -> return a
    Nothing -> flip whenJust cachedMerchantIdAndCity /=<< Queries.findByMerchantIdAndCity merchantId city

findByMerchantShortIdAndCity :: (CacheFlow m r, EsqDBFlow m r) => ShortId Merchant -> Context.City -> m (Maybe MerchantOperatingCity)
findByMerchantShortIdAndCity merchantShortId city =
  Hedis.safeGet (makeMerchantShortIdAndCityKey merchantShortId city) >>= \case
    Just a -> return a
    Nothing -> flip whenJust cachedMerchantShortIdAndCity /=<< Queries.findByMerchantShortIdAndCity merchantShortId city

cachedMerchantOperatingCityId :: CacheFlow m r => MerchantOperatingCity -> m ()
cachedMerchantOperatingCityId merchantOperatingCity = do
  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
  let merchantOperatingCityIdKey = makeMerchantOperatingCityIdKey merchantOperatingCity.id
  Hedis.setExp merchantOperatingCityIdKey merchantOperatingCity expTime

cachedMerchantIdAndCity :: CacheFlow m r => MerchantOperatingCity -> m ()
cachedMerchantIdAndCity merchantOperatingCity = do
  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
  let merchantIdAndCityKey = makeMerchantIdAndCityKey merchantOperatingCity.merchantId merchantOperatingCity.city
  Hedis.setExp merchantIdAndCityKey merchantOperatingCity expTime

cachedMerchantShortIdAndCity :: CacheFlow m r => MerchantOperatingCity -> m ()
cachedMerchantShortIdAndCity merchantOperatingCity = do
  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
  let merchantShortIdAndCityKey = makeMerchantShortIdAndCityKey merchantOperatingCity.merchantShortId merchantOperatingCity.city
  Hedis.setExp merchantShortIdAndCityKey merchantOperatingCity expTime

makeMerchantOperatingCityIdKey :: Id MerchantOperatingCity -> Text
makeMerchantOperatingCityIdKey merchantOperatingCityId = "CachedQueries:MerchantOperatingCity:Id-" <> merchantOperatingCityId.getId

makeMerchantIdAndCityKey :: Id Merchant -> Context.City -> Text
makeMerchantIdAndCityKey merchantId city = "CachedQueries:MerchantOperatingCity:MerchantId-" <> merchantId.getId <> ":City-" <> show city

makeMerchantShortIdAndCityKey :: ShortId Merchant -> Context.City -> Text
makeMerchantShortIdAndCityKey merchantShortId city = "CachedQueries:MerchantOperatingCity:MerchantShortId-" <> merchantShortId.getShortId <> ":City-" <> show city
