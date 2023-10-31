{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-deprecations #-}

module Storage.CachedQueries.FareProduct
  ( findAllFareProductForVariants,
    findByMerchantVariantAreaFlow,
  )
where

import Domain.Types.FareProduct
import Domain.Types.Merchant.MerchantOperatingCity (MerchantOperatingCity)
import Domain.Types.Vehicle.Variant (Variant (..))
import Kernel.Prelude
import qualified Kernel.Storage.Esqueleto as Esq
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow)
import qualified Storage.Queries.FareProduct as Queries

findAllFareProductForVariants :: (CacheFlow m r, Esq.EsqDBFlow m r) => Id MerchantOperatingCity -> Area -> FlowType -> m [FareProduct]
findAllFareProductForVariants merchantOpCityId area flow =
  Hedis.withCrossAppRedis (Hedis.safeGet $ makeFareProductForVariantsByMerchantIdAndAreaAndFlowKey merchantOpCityId area flow) >>= \case
    Just a -> pure a
    Nothing -> cacheAllFareProductForVariantsByMerchantIdAndAreaAndFlow merchantOpCityId area flow /=<< Queries.findAllFareProductForVariants merchantOpCityId area flow

cacheAllFareProductForVariantsByMerchantIdAndAreaAndFlow :: (CacheFlow m r) => Id MerchantOperatingCity -> Area -> FlowType -> [FareProduct] -> m ()
cacheAllFareProductForVariantsByMerchantIdAndAreaAndFlow merchantOpCityId area flow fareProducts = do
  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
  Hedis.withCrossAppRedis $ Hedis.setExp (makeFareProductForVariantsByMerchantIdAndAreaAndFlowKey merchantOpCityId area flow) fareProducts expTime

makeFareProductForVariantsByMerchantIdAndAreaAndFlowKey :: Id MerchantOperatingCity -> Area -> FlowType -> Text
makeFareProductForVariantsByMerchantIdAndAreaAndFlowKey merchantOpCityId area flow = "driver-offer:CachedQueries:FareProduct:MerchantId-" <> getId merchantOpCityId <> ":Area-" <> show area <> ":Flow-" <> show flow

findByMerchantVariantAreaFlow :: (CacheFlow m r, Esq.EsqDBFlow m r) => Id MerchantOperatingCity -> Variant -> Area -> FlowType -> m (Maybe FareProduct)
findByMerchantVariantAreaFlow merchantOpCityId vehicleVariant area flow =
  Hedis.withCrossAppRedis (Hedis.safeGet $ makeFareProductByMerchantVariantAreaFlowKey merchantOpCityId vehicleVariant area flow) >>= \case
    Just a -> pure a
    Nothing -> flip whenJust (cacheFareProductByMerchantVariantAreaFlow merchantOpCityId vehicleVariant area flow) /=<< Queries.findByMerchantVariantAreaFlow merchantOpCityId vehicleVariant area flow

cacheFareProductByMerchantVariantAreaFlow :: (CacheFlow m r) => Id MerchantOperatingCity -> Variant -> Area -> FlowType -> FareProduct -> m ()
cacheFareProductByMerchantVariantAreaFlow merchantOpCityId vehicleVariant area flow fareProduct = do
  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
  Hedis.withCrossAppRedis $ Hedis.setExp (makeFareProductByMerchantVariantAreaFlowKey merchantOpCityId vehicleVariant area flow) fareProduct expTime

makeFareProductByMerchantVariantAreaFlowKey :: Id MerchantOperatingCity -> Variant -> Area -> FlowType -> Text
makeFareProductByMerchantVariantAreaFlowKey merchantOpCityId vehicleVariant area flow = "driver-offer:CachedQueries:FareProduct:MerchantId-" <> getId merchantOpCityId <> ":Variant-" <> show vehicleVariant <> ":Area-" <> show area <> ":Flow-" <> show flow
