{-# LANGUAGE DerivingStrategies #-}
{-# OPTIONS_GHC -Wno-deprecations #-}

module Storage.CachedQueries.FarePolicy.OneWayFarePolicy
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
import Domain.Types.FarePolicy.OneWayFarePolicy
import Domain.Types.Organization (Organization)
import qualified Domain.Types.Vehicle as Vehicle
import Storage.CachedQueries.CacheConfig
import qualified Storage.Queries.FarePolicy.OneWayFarePolicy as Queries

findById :: (HasCacheConfig r, HedisFlow m r, EsqDBFlow m r) => Id OneWayFarePolicy -> m (Maybe OneWayFarePolicy)
findById id =
  Hedis.get (makeIdKey id) >>= \case
    Just a -> return . Just $ coerce @(OneWayFarePolicyD 'Unsafe) @OneWayFarePolicy a
    Nothing -> flip whenJust cacheOneWayFarePolicy /=<< Queries.findById id

findByOrgIdAndVariant ::
  (HasCacheConfig r, HedisFlow m r, EsqDBFlow m r) =>
  Id Organization ->
  Vehicle.Variant ->
  m (Maybe OneWayFarePolicy)
findByOrgIdAndVariant orgId vehVar =
  Hedis.get (makeOrgIdVehVarKey orgId vehVar) >>= \case
    Nothing -> findAndCache
    Just id ->
      Hedis.get (makeIdKey id) >>= \case
        Just a -> return . Just $ coerce @(OneWayFarePolicyD 'Unsafe) @OneWayFarePolicy a
        Nothing -> findAndCache
  where
    findAndCache = flip whenJust cacheOneWayFarePolicy /=<< Queries.findByOrgIdAndVariant orgId vehVar

findAllByOrgId :: (HasCacheConfig r, HedisFlow m r, EsqDBFlow m r) => Id Organization -> m [OneWayFarePolicy]
findAllByOrgId orgId =
  Hedis.get (makeAllOrgIdKey orgId) >>= \case
    Just a -> return $ fmap (coerce @(OneWayFarePolicyD 'Unsafe) @OneWayFarePolicy) a
    Nothing -> cacheRes /=<< Queries.findAllByOrgId orgId
  where
    cacheRes owFPs = do
      expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
      Hedis.setExp (makeAllOrgIdKey orgId) (coerce @[OneWayFarePolicy] @[OneWayFarePolicyD 'Unsafe] owFPs) expTime

cacheOneWayFarePolicy :: (HasCacheConfig r, HedisFlow m r) => OneWayFarePolicy -> m ()
cacheOneWayFarePolicy owFP = do
  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
  let idKey = makeIdKey owFP.id
  Hedis.setExp idKey (coerce @OneWayFarePolicy @(OneWayFarePolicyD 'Unsafe) owFP) expTime
  Hedis.setExp (makeOrgIdVehVarKey owFP.organizationId owFP.vehicleVariant) idKey expTime

makeIdKey :: Id OneWayFarePolicy -> Text
makeIdKey id = "CachedQueries:OneWayFarePolicy:Id-" <> id.getId

makeOrgIdVehVarKey :: Id Organization -> Vehicle.Variant -> Text
makeOrgIdVehVarKey orgId vehVar = "CachedQueries:OneWayFarePolicy:OrgId-" <> orgId.getId <> ":VehVar-" <> show vehVar

makeAllOrgIdKey :: Id Organization -> Text
makeAllOrgIdKey orgId = "CachedQueries:OneWayFarePolicy:OrgId-" <> orgId.getId <> ":All"

-- Call it after any update
clearCache :: HedisFlow m r => OneWayFarePolicy -> m ()
clearCache owFP = do
  Hedis.del (makeIdKey owFP.id)
  Hedis.del (makeOrgIdVehVarKey owFP.organizationId owFP.vehicleVariant)
  Hedis.del (makeAllOrgIdKey owFP.organizationId)

update :: OneWayFarePolicy -> Esq.SqlDB ()
update = Queries.update
