{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-deprecations #-}

module Storage.CachedQueries.FareProduct where

import Domain.Types.FareProduct
import Domain.Types.Merchant.MerchantOperatingCity (MerchantOperatingCity)
import Domain.Types.Vehicle.Variant (Variant (..))
import Kernel.Prelude
import qualified Kernel.Storage.Esqueleto as Esq
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow)
import qualified Storage.Queries.FareProduct as Queries

findAllFareProductForVariants :: (CacheFlow m r, Esq.EsqDBFlow m r) => Id MerchantOperatingCity -> Area -> m [FareProduct]
findAllFareProductForVariants merchantOpCityId area =
  Hedis.withCrossAppRedis (Hedis.safeGet $ makeFareProductForVariantsByMerchantIdAndAreaKey merchantOpCityId area) >>= \case
    Just a -> pure a
    Nothing -> cacheAllFareProductForVariantsByMerchantIdAndArea merchantOpCityId area /=<< Queries.findAllFareProductForVariants merchantOpCityId area

cacheAllFareProductForVariantsByMerchantIdAndArea :: (CacheFlow m r) => Id MerchantOperatingCity -> Area -> [FareProduct] -> m ()
cacheAllFareProductForVariantsByMerchantIdAndArea merchantOpCityId area fareProducts = do
  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
  Hedis.withCrossAppRedis $ Hedis.setExp (makeFareProductForVariantsByMerchantIdAndAreaKey merchantOpCityId area) fareProducts expTime

makeFareProductForVariantsByMerchantIdAndAreaKey :: Id MerchantOperatingCity -> Area -> Text
makeFareProductForVariantsByMerchantIdAndAreaKey merchantOpCityId area = "driver-offer:CachedQueries:FareProduct:MerchantOpCityId-" <> getId merchantOpCityId <> ":Area-" <> show area

findByMerchantVariantArea :: (CacheFlow m r, Esq.EsqDBFlow m r) => Id MerchantOperatingCity -> Variant -> Area -> m (Maybe FareProduct)
findByMerchantVariantArea merchantOpCityId vehicleVariant area =
  Hedis.withCrossAppRedis (Hedis.safeGet $ makeFareProductByMerchantVariantAreaKey merchantOpCityId vehicleVariant area) >>= \case
    Just a -> pure a
    Nothing -> flip whenJust (cacheFareProductByMerchantVariantArea merchantOpCityId vehicleVariant area) /=<< Queries.findByMerchantOpCityIdVariantArea merchantOpCityId vehicleVariant area

cacheFareProductByMerchantVariantArea :: (CacheFlow m r) => Id MerchantOperatingCity -> Variant -> Area -> FareProduct -> m ()
cacheFareProductByMerchantVariantArea merchantOpCityId vehicleVariant area fareProduct = do
  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
  Hedis.withCrossAppRedis $ Hedis.setExp (makeFareProductByMerchantVariantAreaKey merchantOpCityId vehicleVariant area) fareProduct expTime

makeFareProductByMerchantVariantAreaKey :: Id MerchantOperatingCity -> Variant -> Area -> Text
makeFareProductByMerchantVariantAreaKey merchantOpCityId vehicleVariant area = "driver-offer:CachedQueries:FareProduct:MerchantOpCityId-" <> getId merchantOpCityId <> ":Variant-" <> show vehicleVariant <> ":Area-" <> show area
