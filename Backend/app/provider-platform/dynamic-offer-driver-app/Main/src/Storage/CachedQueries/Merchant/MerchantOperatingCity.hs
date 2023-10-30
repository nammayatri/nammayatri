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
import Domain.Types.Merchant.MerchantOperatingCity (MerchantOperatingCity)
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Hedis
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.Queries.Merchant.MerchantOperatingCity as Queries
import Tools.Error

getMerchantOpCityId :: (CacheFlow m r, EsqDBFlow m r) => Maybe (Id MerchantOperatingCity) -> Merchant -> Maybe Context.City -> m (Id MerchantOperatingCity)
getMerchantOpCityId mbMerchantOpCityId merchant mbCity =
  case mbMerchantOpCityId of
    Just moCityId -> pure moCityId
    Nothing -> do
      let city = fromMaybe merchant.city mbCity
      (.id)
        <$> ( findByMerchantIdAndCity merchant.id (fromMaybe merchant.city mbCity)
                >>= fromMaybeM (MerchantOperatingCityNotFound $ "merchant-Id-" <> merchant.id.getId <> "-city-" <> show city)
            )

findById :: (CacheFlow m r, EsqDBFlow m r) => Id MerchantOperatingCity -> m (Maybe MerchantOperatingCity)
findById merchantOpCityId =
  Hedis.safeGet (makeMerchantOpCityIdKey merchantOpCityId) >>= \case
    Just moCity -> pure moCity
    Nothing -> flip whenJust cacheMerchantOpCityById /=<< Queries.findById merchantOpCityId

findByMerchantIdAndCity :: (CacheFlow m r, EsqDBFlow m r) => Id Merchant -> Context.City -> m (Maybe MerchantOperatingCity)
findByMerchantIdAndCity merchantId city =
  Hedis.safeGet (makeMerchantIdAndCityKey merchantId city) >>= \case
    Just a -> return a
    Nothing -> flip whenJust cachedMerchantIdAndCity /=<< Queries.findByMerchantIdAndCity merchantId city

cacheMerchantOpCityById :: CacheFlow m r => MerchantOperatingCity -> m ()
cacheMerchantOpCityById merchantOpCity = do
  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
  let merchantOpCityIdKey = makeMerchantOpCityIdKey merchantOpCity.id
  Hedis.setExp merchantOpCityIdKey merchantOpCity expTime

cachedMerchantIdAndCity :: CacheFlow m r => MerchantOperatingCity -> m ()
cachedMerchantIdAndCity merchantOperatingCity = do
  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
  let merchantIdAndCityKey = makeMerchantIdAndCityKey merchantOperatingCity.merchantId merchantOperatingCity.city
  Hedis.setExp merchantIdAndCityKey merchantOperatingCity expTime

makeMerchantOpCityIdKey :: Id MerchantOperatingCity -> Text
makeMerchantOpCityIdKey merchantOpCityId = "CachedQueries:MerchantOperatingCity:MerchantOpCityId-" <> merchantOpCityId.getId

makeMerchantIdAndCityKey :: Id Merchant -> Context.City -> Text
makeMerchantIdAndCityKey merchantId city = "CachedQueries:MerchantOperatingCity:MerchantId-" <> merchantId.getId <> ":City-" <> show city

makeMerchantIdKey :: Id Merchant -> Text
makeMerchantIdKey merchantId = "CachedQueries:MerchantOperatingCity:MerchantId-" <> merchantId.getId
