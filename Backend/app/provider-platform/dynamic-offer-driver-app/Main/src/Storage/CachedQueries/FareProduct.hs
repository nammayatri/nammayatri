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

import qualified Domain.Types.Common as DTC
import Domain.Types.FareProduct
import Domain.Types.Merchant.MerchantOperatingCity (MerchantOperatingCity)
import Domain.Types.Vehicle.Variant (Variant (..))
import Kernel.Prelude
import qualified Kernel.Storage.Esqueleto as Esq
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow)
import qualified Storage.Queries.FareProduct as Queries

findAllFareProductForVariants :: (CacheFlow m r, Esq.EsqDBFlow m r) => Id MerchantOperatingCity -> DTC.TripCategory -> Area -> m [FareProduct]
findAllFareProductForVariants merchantOpCityId tripCategory area =
  Hedis.withCrossAppRedis (Hedis.safeGet $ makeFareProductForVariantsByMerchantIdAndAreaKey merchantOpCityId tripCategory area) >>= \case
    Just a -> pure a
    Nothing -> cacheAllFareProductForVariantsByMerchantIdAndArea merchantOpCityId tripCategory area /=<< Queries.findAllFareProductForVariants merchantOpCityId tripCategory area

cacheAllFareProductForVariantsByMerchantIdAndArea :: (CacheFlow m r) => Id MerchantOperatingCity -> DTC.TripCategory -> Area -> [FareProduct] -> m ()
cacheAllFareProductForVariantsByMerchantIdAndArea merchantOpCityId tripCategory area fareProducts = do
  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
  Hedis.withCrossAppRedis $ Hedis.setExp (makeFareProductForVariantsByMerchantIdAndAreaKey merchantOpCityId tripCategory area) fareProducts expTime

makeFareProductForVariantsByMerchantIdAndAreaKey :: Id MerchantOperatingCity -> DTC.TripCategory -> Area -> Text
makeFareProductForVariantsByMerchantIdAndAreaKey merchantOpCityId tripCategory area = "driver-offer:CachedQueries:FareProduct:MerchantOpCityId-" <> getId merchantOpCityId <> ":Area-" <> show area <> ":TripCategory-" <> show tripCategory

findByMerchantVariantArea :: (CacheFlow m r, Esq.EsqDBFlow m r) => Id MerchantOperatingCity -> DTC.TripCategory -> Variant -> Area -> m (Maybe FareProduct)
findByMerchantVariantArea merchantOpCityId tripCategory vehicleVariant area =
  Hedis.withCrossAppRedis (Hedis.safeGet $ makeFareProductByMerchantVariantAreaKey merchantOpCityId tripCategory vehicleVariant area) >>= \case
    Just a -> pure a
    Nothing -> flip whenJust (cacheFareProductByMerchantVariantArea merchantOpCityId tripCategory vehicleVariant area) /=<< Queries.findByMerchantOpCityIdVariantArea merchantOpCityId tripCategory vehicleVariant area

cacheFareProductByMerchantVariantArea :: (CacheFlow m r) => Id MerchantOperatingCity -> DTC.TripCategory -> Variant -> Area -> FareProduct -> m ()
cacheFareProductByMerchantVariantArea merchantOpCityId tripCategory vehicleVariant area fareProduct = do
  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
  Hedis.withCrossAppRedis $ Hedis.setExp (makeFareProductByMerchantVariantAreaKey merchantOpCityId tripCategory vehicleVariant area) fareProduct expTime

makeFareProductByMerchantVariantAreaKey :: Id MerchantOperatingCity -> DTC.TripCategory -> Variant -> Area -> Text
makeFareProductByMerchantVariantAreaKey merchantOpCityId tripCategory vehicleVariant area = "driver-offer:CachedQueries:FareProduct:MerchantOpCityId-" <> getId merchantOpCityId <> ":Variant-" <> show vehicleVariant <> ":Area-" <> show area <> ":TripCategory-" <> show tripCategory
