{-# LANGUAGE DerivingStrategies #-}
{-# OPTIONS_GHC -Wno-deprecations #-}

module Storage.CachedQueries.FarePolicy.Discount
  ( findById,
    findAllByMerchantIdAndVariant,
    create,
    update,
    deleteById,
    clearCache,
  )
where

import Beckn.Prelude
import qualified Beckn.Storage.Esqueleto as Esq
import qualified Beckn.Storage.Hedis as Hedis
import Beckn.Types.Id
import Beckn.Utils.Common
import Data.Coerce (coerce)
import Domain.Types.Common
import Domain.Types.FarePolicy.Discount
import Domain.Types.Merchant (Merchant)
import qualified Domain.Types.Vehicle as Vehicle
import Storage.CachedQueries.CacheConfig
import qualified Storage.CachedQueries.FarePolicy.OneWayFarePolicy as OWFP
import qualified Storage.Queries.FarePolicy.Discount as Queries

findById :: (CacheFlow m r, EsqDBFlow m r) => Id Discount -> m (Maybe Discount)
findById id =
  Hedis.get (makeIdKey id) >>= \case
    Just a -> return . Just $ coerce @(DiscountD 'Unsafe) @Discount a
    Nothing -> flip whenJust cacheDiscount /=<< Queries.findById id

findAllByMerchantIdAndVariant :: (CacheFlow m r, EsqDBFlow m r) => Id Merchant -> Vehicle.Variant -> m [Discount]
findAllByMerchantIdAndVariant merchantId vehVar =
  Hedis.get (makeAllMerchantIdVehVarKey merchantId vehVar) >>= \case
    Just a -> return $ fmap (coerce @(DiscountD 'Unsafe) @Discount) a
    Nothing -> cacheRes /=<< Queries.findAllByMerchantIdAndVariant merchantId vehVar
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
  Esq.SqlDB ()
create = Queries.create

update ::
  Discount ->
  Esq.SqlDB ()
update = Queries.update

deleteById :: Id Discount -> Esq.SqlDB ()
deleteById = Queries.deleteById
