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
import Domain.Types.Merchant (Merchant)
import Domain.Types.Vehicle.Variant (Variant (..))
import Kernel.Prelude
import qualified Kernel.Storage.Esqueleto as Esq
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow)
import qualified Storage.Queries.FareProduct as Queries

findAllFareProductForVariants :: (CacheFlow m r, Esq.EsqDBFlow m r) => Id Merchant -> Area -> FlowType -> m [FareProduct]
findAllFareProductForVariants merchantId area flow =
  Hedis.withCrossAppRedis (Hedis.safeGet $ makeFareProductForVariantsByMerchantIdAndAreaAndFlowKey merchantId area flow) >>= \case
    Just a -> pure a
    Nothing -> cacheAllFareProductForVariantsByMerchantIdAndAreaAndFlow merchantId area flow /=<< Queries.findAllFareProductForVariants merchantId area flow

cacheAllFareProductForVariantsByMerchantIdAndAreaAndFlow :: (CacheFlow m r) => Id Merchant -> Area -> FlowType -> [FareProduct] -> m ()
cacheAllFareProductForVariantsByMerchantIdAndAreaAndFlow merchantId area flow fareProducts = do
  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
  Hedis.withCrossAppRedis $ Hedis.setExp (makeFareProductForVariantsByMerchantIdAndAreaAndFlowKey merchantId area flow) fareProducts expTime

makeFareProductForVariantsByMerchantIdAndAreaAndFlowKey :: Id Merchant -> Area -> FlowType -> Text
makeFareProductForVariantsByMerchantIdAndAreaAndFlowKey merchantId area flow = "driver-offer:CachedQueries:FareProduct:MerchantId-" <> getId merchantId <> ":Area-" <> show area <> ":Flow-" <> show flow

findByMerchantVariantAreaFlow :: (CacheFlow m r, Esq.EsqDBFlow m r) => Id Merchant -> Variant -> Area -> FlowType -> m (Maybe FareProduct)
findByMerchantVariantAreaFlow merchantId vehicleVariant area flow =
  Hedis.withCrossAppRedis (Hedis.safeGet $ makeFareProductByMerchantVariantAreaFlowKey merchantId vehicleVariant area flow) >>= \case
    Just a -> pure a
    Nothing -> flip whenJust (cacheFareProductByMerchantVariantAreaFlow merchantId vehicleVariant area flow) /=<< Queries.findByMerchantVariantAreaFlow merchantId vehicleVariant area flow

cacheFareProductByMerchantVariantAreaFlow :: (CacheFlow m r) => Id Merchant -> Variant -> Area -> FlowType -> FareProduct -> m ()
cacheFareProductByMerchantVariantAreaFlow merchantId vehicleVariant area flow fareProduct = do
  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
  Hedis.withCrossAppRedis $ Hedis.setExp (makeFareProductByMerchantVariantAreaFlowKey merchantId vehicleVariant area flow) fareProduct expTime

makeFareProductByMerchantVariantAreaFlowKey :: Id Merchant -> Variant -> Area -> FlowType -> Text
makeFareProductByMerchantVariantAreaFlowKey merchantId vehicleVariant area flow = "driver-offer:CachedQueries:FareProduct:MerchantId-" <> getId merchantId <> ":Variant-" <> show vehicleVariant <> ":Area-" <> show area <> ":Flow-" <> show flow
