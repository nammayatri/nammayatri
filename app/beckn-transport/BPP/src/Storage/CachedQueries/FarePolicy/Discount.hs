{-# LANGUAGE DerivingStrategies #-}
{-# OPTIONS_GHC -Wno-deprecations #-}

module Storage.CachedQueries.FarePolicy.Discount
  ( findById,
    findAllByOrgIdAndVariant,
    create,
    update,
    deleteById,
    clearCache,
  )
where

import Beckn.Prelude
import qualified Beckn.Storage.Esqueleto as Esq
import Beckn.Storage.Hedis
import qualified Beckn.Storage.Hedis as Hedis
import Beckn.Types.Id
import Beckn.Utils.Common
import Data.Coerce (coerce)
import Domain.Types.Common
import Domain.Types.FarePolicy.Discount
import Domain.Types.Organization (Organization)
import qualified Domain.Types.Vehicle as Vehicle
import Storage.CachedQueries.CacheConfig
import qualified Storage.CachedQueries.FarePolicy.OneWayFarePolicy as OWFP
import qualified Storage.Queries.FarePolicy.Discount as Queries

findById :: (HasCacheConfig r, HedisFlow m r, EsqDBFlow m r) => Id Discount -> m (Maybe Discount)
findById id =
  Hedis.get (makeIdKey id) >>= \case
    Just a -> return . Just $ coerce @(DiscountD 'Unsafe) @Discount a
    Nothing -> flip whenJust cacheDiscount /=<< Queries.findById id

findAllByOrgIdAndVariant :: (HasCacheConfig r, HedisFlow m r, EsqDBFlow m r) => Id Organization -> Vehicle.Variant -> m [Discount]
findAllByOrgIdAndVariant orgId vehVar =
  Hedis.get (makeAllOrgIdVehVarKey orgId vehVar) >>= \case
    Just a -> return $ fmap (coerce @(DiscountD 'Unsafe) @Discount) a
    Nothing -> cacheRes /=<< Queries.findAllByOrgIdAndVariant orgId vehVar
  where
    cacheRes discounts = do
      expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
      Hedis.setExp (makeAllOrgIdVehVarKey orgId vehVar) (coerce @[Discount] @[DiscountD 'Unsafe] discounts) expTime

cacheDiscount :: (HasCacheConfig r, HedisFlow m r) => Discount -> m ()
cacheDiscount discount = do
  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
  let idKey = makeIdKey discount.id
  Hedis.setExp idKey (coerce @Discount @(DiscountD 'Unsafe) discount) expTime
  Hedis.setExp (makeAllOrgIdVehVarKey discount.organizationId discount.vehicleVariant) idKey expTime

baseKey :: Text
baseKey = "CachedQueries:Discount"

makeIdKey :: Id Discount -> Text
makeIdKey id = baseKey <> ":Id-" <> id.getId

makeAllOrgIdVehVarKey :: Id Organization -> Vehicle.Variant -> Text
makeAllOrgIdVehVarKey orgId vehVar = baseKey <> ":OrgId-" <> orgId.getId <> ":VehVar-" <> show vehVar <> ":All"

-- Call it after any update
clearCache :: (HasCacheConfig r, HedisFlow m r, EsqDBFlow m r) => Discount -> m ()
clearCache discount = do
  Hedis.del (makeIdKey discount.id)
  Hedis.del (makeAllOrgIdVehVarKey discount.organizationId discount.vehicleVariant)
  owFP <- OWFP.findByOrgIdAndVariant discount.organizationId discount.vehicleVariant
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
