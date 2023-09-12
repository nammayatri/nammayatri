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
import Domain.Types.Merchant (Merchant)
import Domain.Types.Vehicle.Variant (Variant (..))
import Kernel.Prelude
import qualified Kernel.Storage.Esqueleto as Esq
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow)
import qualified Storage.Queries.FareProduct as Queries

findAllFareProductForVariants :: (CacheFlow m r, Esq.EsqDBFlow m r) => Id Merchant -> Area -> m [FareProduct]
findAllFareProductForVariants merchantId area =
  Hedis.withCrossAppRedis (Hedis.safeGet $ makeFareProductForVariantsByMerchantIdAndAreaKey merchantId area) >>= \case
    Just a -> pure a
    Nothing -> cacheAllFareProductForVariantsByMerchantIdAndArea merchantId area /=<< Queries.findAllFareProductForVariants merchantId area

cacheAllFareProductForVariantsByMerchantIdAndArea :: (CacheFlow m r) => Id Merchant -> Area -> [FareProduct] -> m ()
cacheAllFareProductForVariantsByMerchantIdAndArea merchantId area fareProducts = do
  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
  Hedis.withCrossAppRedis $ Hedis.setExp (makeFareProductForVariantsByMerchantIdAndAreaKey merchantId area) fareProducts expTime

makeFareProductForVariantsByMerchantIdAndAreaKey :: Id Merchant -> Area -> Text
makeFareProductForVariantsByMerchantIdAndAreaKey merchantId area = "driver-offer:CachedQueries:FareProduct:MerchantId-" <> getId merchantId <> ":Area-" <> show area

findByMerchantVariantArea :: (CacheFlow m r, Esq.EsqDBFlow m r) => Id Merchant -> Variant -> Area -> m (Maybe FareProduct)
findByMerchantVariantArea merchantId vehicleVariant area =
  Hedis.withCrossAppRedis (Hedis.safeGet $ makeFareProductByMerchantVariantAreaKey merchantId vehicleVariant area) >>= \case
    Just a -> pure a
    Nothing -> flip whenJust (cacheFareProductByMerchantVariantArea merchantId vehicleVariant area) /=<< Queries.findByMerchantVariantArea merchantId vehicleVariant area

cacheFareProductByMerchantVariantArea :: (CacheFlow m r) => Id Merchant -> Variant -> Area -> FareProduct -> m ()
cacheFareProductByMerchantVariantArea merchantId vehicleVariant area fareProduct = do
  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
  Hedis.withCrossAppRedis $ Hedis.setExp (makeFareProductByMerchantVariantAreaKey merchantId vehicleVariant area) fareProduct expTime

makeFareProductByMerchantVariantAreaKey :: Id Merchant -> Variant -> Area -> Text
makeFareProductByMerchantVariantAreaKey merchantId vehicleVariant area = "driver-offer:CachedQueries:FareProduct:MerchantId-" <> getId merchantId <> ":Variant-" <> show vehicleVariant <> ":Area-" <> show area
