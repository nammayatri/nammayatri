{-# LANGUAGE DerivingStrategies #-}
{-# OPTIONS_GHC -Wno-deprecations #-}

module Storage.CachedQueries.FarePolicy
  ( findById,
    findByOrgIdAndVariant,
    findAllByOrgId,
    clearCache,
    update,
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
import Domain.Types.FarePolicy
import Domain.Types.Organization (Organization)
import qualified Domain.Types.Vehicle as Vehicle
import Storage.CachedQueries.CacheConfig
import qualified Storage.Queries.FarePolicy as Queries

findById :: (HasCacheConfig r, HedisFlow m r, EsqDBFlow m r) => Id FarePolicy -> m (Maybe FarePolicy)
findById id =
  Hedis.get (makeIdKey id) >>= \case
    Just a -> return . Just $ coerce @(FarePolicyD 'Unsafe) @FarePolicy a
    Nothing -> flip whenJust cacheFarePolicy /=<< Queries.findById id

findByOrgIdAndVariant ::
  (HasCacheConfig r, HedisFlow m r, EsqDBFlow m r) =>
  Id Organization ->
  Vehicle.Variant ->
  m (Maybe FarePolicy)
findByOrgIdAndVariant orgId vehVar =
  Hedis.get (makeOrgIdVehVarKey orgId vehVar) >>= \case
    Nothing -> findAndCache
    Just id ->
      Hedis.get (makeIdKey id) >>= \case
        Just a -> return . Just $ coerce @(FarePolicyD 'Unsafe) @FarePolicy a
        Nothing -> findAndCache
  where
    findAndCache = flip whenJust cacheFarePolicy /=<< Queries.findByOrgIdAndVariant orgId vehVar

findAllByOrgId :: (HasCacheConfig r, HedisFlow m r, EsqDBFlow m r) => Id Organization -> m [FarePolicy]
findAllByOrgId orgId =
  Hedis.get (makeAllOrgIdKey orgId) >>= \case
    Just a -> return $ fmap (coerce @(FarePolicyD 'Unsafe) @FarePolicy) a
    Nothing -> cacheRes /=<< Queries.findAllByOrgId orgId
  where
    cacheRes fps = do
      expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
      Hedis.setExp (makeAllOrgIdKey orgId) (coerce @[FarePolicy] @[FarePolicyD 'Unsafe] fps) expTime

cacheFarePolicy :: (HasCacheConfig r, HedisFlow m r) => FarePolicy -> m ()
cacheFarePolicy fp = do
  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
  let idKey = makeIdKey fp.id
  Hedis.setExp idKey (coerce @FarePolicy @(FarePolicyD 'Unsafe) fp) expTime
  Hedis.setExp (makeOrgIdVehVarKey fp.organizationId fp.vehicleVariant) idKey expTime

makeIdKey :: Id FarePolicy -> Text
makeIdKey id = "CachedQueries:FarePolicy:Id-" <> id.getId

makeOrgIdVehVarKey :: Id Organization -> Vehicle.Variant -> Text
makeOrgIdVehVarKey orgId vehVar = "CachedQueries:FarePolicy:OrgId-" <> orgId.getId <> ":VehVar-" <> show vehVar

makeAllOrgIdKey :: Id Organization -> Text
makeAllOrgIdKey orgId = "CachedQueries:FarePolicy:OrgId-" <> orgId.getId <> ":All"

-- Call it after any update
clearCache :: HedisFlow m r => FarePolicy -> m ()
clearCache fp = do
  Hedis.del (makeIdKey fp.id)
  Hedis.del (makeOrgIdVehVarKey fp.organizationId fp.vehicleVariant)
  Hedis.del (makeAllOrgIdKey fp.organizationId)

update :: FarePolicy -> Esq.SqlDB ()
update = Queries.update
