{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingStrategies #-}
{-# OPTIONS_GHC -Wno-deprecations #-}

module Storage.CachedQueries.FarePolicy.Discount
  ( findById,
    findAllByMerchantIdAndVariant,
    create,
    update,
    delete,
    clearCache,
  )
where

import Data.Coerce (coerce)
import Domain.Types.Common
import Domain.Types.FarePolicy.Discount
import Domain.Types.Merchant (Merchant)
import qualified Domain.Types.Vehicle as Vehicle
import Kernel.Prelude
import qualified Kernel.Storage.Esqueleto as Esq
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Types.Id
import Kernel.Utils.Common
import Storage.CachedQueries.CacheConfig
import qualified Storage.CachedQueries.FarePolicy.OneWayFarePolicy as OWFP
import qualified Storage.Queries.FarePolicy.Discount as Queries

findById :: forall m r. (CacheFlow m r, EsqDBFlow m r) => Id Discount -> m (Maybe Discount)
findById id =
  Hedis.safeGet (makeIdKey id) >>= \case
    Just a -> return . Just $ coerce @(DiscountD 'Unsafe) @Discount a
    Nothing -> flip whenJust cacheDiscount /=<< Queries.findById id (Proxy @m)

findAllByMerchantIdAndVariant :: forall m r. (CacheFlow m r, EsqDBFlow m r) => Id Merchant -> Vehicle.Variant -> m [Discount]
findAllByMerchantIdAndVariant merchantId vehVar =
  Hedis.safeGet (makeAllMerchantIdVehVarKey merchantId vehVar) >>= \case
    Just a -> return $ fmap (coerce @(DiscountD 'Unsafe) @Discount) a
    Nothing -> cacheRes /=<< Queries.findAllByMerchantIdAndVariant merchantId vehVar (Proxy @m)
  where
    cacheRes discounts = do
      expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
      Hedis.setExp (makeAllMerchantIdVehVarKey merchantId vehVar) (coerce @[Discount] @[DiscountD 'Unsafe] discounts) expTime

cacheDiscount :: (CacheFlow m r) => Discount -> m ()
cacheDiscount discount = do
  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
  let idKey = makeIdKey discount.id
  Hedis.setExp idKey (coerce @Discount @(DiscountD 'Unsafe) discount) expTime

baseKey :: Text
baseKey = "CachedQueries:Discount"

makeIdKey :: Id Discount -> Text
makeIdKey id = baseKey <> ":Id-" <> id.getId

makeAllMerchantIdVehVarKey :: Id Merchant -> Vehicle.Variant -> Text
makeAllMerchantIdVehVarKey merchantId vehVar = baseKey <> ":MerchantId-" <> merchantId.getId <> ":VehVar-" <> show vehVar <> ":All"

-- Call it after any update
clearCache :: (CacheFlow m r, EsqDBFlow m r) => Discount -> m ()
clearCache discount = do
  Hedis.del (makeIdKey discount.id)
  Hedis.del (makeAllMerchantIdVehVarKey discount.merchantId discount.vehicleVariant)
  owFP <- OWFP.findByMerchantIdAndVariant discount.merchantId discount.vehicleVariant
  whenJust owFP OWFP.clearCache

create ::
  Discount ->
  Esq.SqlDB m ()
create = Queries.create

update ::
  (CacheFlow m r, EsqDBFlow m r) =>
  Discount ->
  Esq.SqlDB m ()
update discount = do
  Queries.update discount
  Esq.finalize $ clearCache discount

delete :: (CacheFlow m r, EsqDBFlow m r) => Discount -> Esq.SqlDB m ()
delete discount = do
  Queries.deleteById discount.id
  Esq.finalize $ clearCache discount
