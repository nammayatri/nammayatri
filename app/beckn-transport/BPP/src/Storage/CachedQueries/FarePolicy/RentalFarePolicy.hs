{-# LANGUAGE DerivingStrategies #-}
{-# OPTIONS_GHC -Wno-deprecations #-}

module Storage.CachedQueries.FarePolicy.RentalFarePolicy
  ( findById,
    findByOffer,
    findAllByOrgId,
    markAllAsDeleted,
    create,
    clearCache,
    clearAllCacheByOrgId,
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
import Domain.Types.FarePolicy.RentalFarePolicy
import Domain.Types.Organization (Organization)
import qualified Domain.Types.Vehicle as Vehicle
import qualified Storage.Queries.FarePolicy.RentalFarePolicy as Queries

findById :: (HedisFlow m r, EsqDBFlow m r) => Id RentalFarePolicy -> m (Maybe RentalFarePolicy)
findById id =
  Hedis.get (makeIdKey id) >>= \case
    Just a -> return . Just $ coerce @(RentalFarePolicyD 'Unsafe) @RentalFarePolicy a
    Nothing -> flip whenJust cacheRentalFarePolicy /=<< Queries.findById id

findByOffer ::
  (HedisFlow m r, EsqDBFlow m r) =>
  Id Organization ->
  Vehicle.Variant ->
  Kilometers ->
  Hours ->
  m (Maybe RentalFarePolicy)
findByOffer orgId vehVar kms hours =
  Hedis.get (makeOrgIdVehVarKmHoursKey orgId vehVar kms hours) >>= \case
    Nothing -> findAndCache
    Just id ->
      Hedis.get (makeIdKey id) >>= \case
        Just a -> return . Just $ coerce @(RentalFarePolicyD 'Unsafe) @RentalFarePolicy a
        Nothing -> findAndCache
  where
    findAndCache = flip whenJust cacheRentalFarePolicy /=<< Queries.findByOffer orgId vehVar kms hours

findAllByOrgId :: (HedisFlow m r, EsqDBFlow m r) => Id Organization -> m [RentalFarePolicy]
findAllByOrgId orgId =
  Hedis.get (makeAllOrgIdKey orgId) >>= \case
    Just a -> return $ fmap (coerce @(RentalFarePolicyD 'Unsafe) @RentalFarePolicy) a
    Nothing -> cacheRes /=<< Queries.findAllByOrgId orgId
  where
    cacheRes rFPs = do
      Hedis.setExp (makeAllOrgIdKey orgId) (coerce @[RentalFarePolicy] @[RentalFarePolicyD 'Unsafe] rFPs) expTime
    expTime = 60 * 60 * 24

cacheRentalFarePolicy :: HedisFlow m r => RentalFarePolicy -> m ()
cacheRentalFarePolicy rFP = do
  let idKey = makeIdKey rFP.id
  Hedis.setExp idKey (coerce @RentalFarePolicy @(RentalFarePolicyD 'Unsafe) rFP) expTime
  Hedis.setExp (makeOrgIdVehVarKmHoursKey rFP.organizationId rFP.vehicleVariant rFP.baseDistance rFP.baseDuration) idKey expTime
  where
    expTime = 60 * 60 * 24

baseKey :: Text
baseKey = "CachedQueries:RentalFarePolicy"

makeIdKey :: Id RentalFarePolicy -> Text
makeIdKey id = baseKey <> ":Id-" <> id.getId

makeOrgIdVehVarKmHoursKey :: Id Organization -> Vehicle.Variant -> Kilometers -> Hours -> Text
makeOrgIdVehVarKmHoursKey orgId vehVar kms hrs =
  baseKey <> ":OrgId-" <> orgId.getId
    <> ":VehVar-"
    <> show vehVar
    <> ":Killometers-"
    <> show kms
    <> ":Hours-"
    <> show hrs

makeAllOrgIdKey :: Id Organization -> Text
makeAllOrgIdKey orgId = baseKey <> ":OrgId-" <> orgId.getId <> ":All"

-- Call it after any update
clearCache :: HedisFlow m r => RentalFarePolicy -> m ()
clearCache rFP = do
  Hedis.del (makeIdKey rFP.id)
  Hedis.del (makeOrgIdVehVarKmHoursKey rFP.organizationId rFP.vehicleVariant rFP.baseDistance rFP.baseDuration)
  Hedis.del (makeAllOrgIdKey rFP.organizationId)

-- Call it after any mass delete
clearAllCacheByOrgId :: HedisFlow m r => Id Organization -> m ()
clearAllCacheByOrgId orgId =
  Hedis.delByPattern (makeAllOrgIdKey orgId <> "*")

create ::
  RentalFarePolicy ->
  Esq.SqlDB ()
create = Queries.create

markAllAsDeleted ::
  Id Organization ->
  Esq.SqlDB ()
markAllAsDeleted = Queries.markAllAsDeleted
