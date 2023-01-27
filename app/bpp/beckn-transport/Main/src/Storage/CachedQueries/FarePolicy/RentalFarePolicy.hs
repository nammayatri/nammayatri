{-# LANGUAGE DerivingStrategies #-}
{-# OPTIONS_GHC -Wno-deprecations #-}

module Storage.CachedQueries.FarePolicy.RentalFarePolicy
  ( findById,
    findByOffer,
    findAllByMerchantId,
    markAllAsDeleted,
    create,
    clearCache,
    clearAllCacheByMerchantId,
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
import Domain.Types.Merchant (Merchant)
import qualified Domain.Types.Vehicle as Vehicle
import Storage.CachedQueries.CacheConfig
import qualified Storage.Queries.FarePolicy.RentalFarePolicy as Queries

findById :: (CacheFlow m r, EsqDBFlow m r) => Id RentalFarePolicy -> m (Maybe RentalFarePolicy)
findById id =
  Hedis.safeGet (makeIdKey id) >>= \case
    Just a -> return . Just $ coerce @(RentalFarePolicyD 'Unsafe) @RentalFarePolicy a
    Nothing -> flip whenJust cacheRentalFarePolicy /=<< Queries.findById id

findByOffer ::
  (CacheFlow m r, EsqDBFlow m r) =>
  Id Merchant ->
  Vehicle.Variant ->
  Kilometers ->
  Hours ->
  m (Maybe RentalFarePolicy)
findByOffer merchantId vehVar kms hours =
  Hedis.safeGet (makeMerchantIdVehVarKmHoursKey merchantId vehVar kms hours) >>= \case
    Nothing -> findAndCache
    Just id ->
      Hedis.safeGet (makeIdKey id) >>= \case
        Just a -> return . Just $ coerce @(RentalFarePolicyD 'Unsafe) @RentalFarePolicy a
        Nothing -> findAndCache
  where
    findAndCache = flip whenJust cacheRentalFarePolicy /=<< Queries.findByOffer merchantId vehVar kms hours

findAllByMerchantId :: (CacheFlow m r, EsqDBFlow m r) => Id Merchant -> m [RentalFarePolicy]
findAllByMerchantId merchantId =
  Hedis.safeGet (makeAllMerchantIdKey merchantId) >>= \case
    Just a -> return $ fmap (coerce @(RentalFarePolicyD 'Unsafe) @RentalFarePolicy) a
    Nothing -> cacheRes /=<< Queries.findAllByMerchantId merchantId
  where
    cacheRes rFPs = do
      expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
      Hedis.setExp (makeAllMerchantIdKey merchantId) (coerce @[RentalFarePolicy] @[RentalFarePolicyD 'Unsafe] rFPs) expTime

cacheRentalFarePolicy :: (CacheFlow m r) => RentalFarePolicy -> m ()
cacheRentalFarePolicy rFP = do
  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
  let idKey = makeIdKey rFP.id
  Hedis.setExp idKey (coerce @RentalFarePolicy @(RentalFarePolicyD 'Unsafe) rFP) expTime
  Hedis.setExp (makeMerchantIdVehVarKmHoursKey rFP.merchantId rFP.vehicleVariant rFP.baseDistance rFP.baseDuration) idKey expTime

baseKey :: Text
baseKey = "CachedQueries:RentalFarePolicy"

makeIdKey :: Id RentalFarePolicy -> Text
makeIdKey id = baseKey <> ":Id-" <> id.getId

makeMerchantIdVehVarKmHoursKey :: Id Merchant -> Vehicle.Variant -> Kilometers -> Hours -> Text
makeMerchantIdVehVarKmHoursKey merchantId vehVar kms hrs =
  baseKey <> ":MerchantId-" <> merchantId.getId
    <> ":VehVar-"
    <> show vehVar
    <> ":Killometers-"
    <> show kms
    <> ":Hours-"
    <> show hrs

makeAllMerchantIdKey :: Id Merchant -> Text
makeAllMerchantIdKey merchantId = baseKey <> ":MerchantId-" <> merchantId.getId <> ":All"

-- Call it after any update
clearCache :: HedisFlow m r => RentalFarePolicy -> m ()
clearCache rFP = do
  Hedis.del (makeIdKey rFP.id)
  Hedis.del (makeMerchantIdVehVarKmHoursKey rFP.merchantId rFP.vehicleVariant rFP.baseDistance rFP.baseDuration)
  Hedis.del (makeAllMerchantIdKey rFP.merchantId)

-- Call it after any mass delete
clearAllCacheByMerchantId :: HedisFlow m r => Id Merchant -> m ()
clearAllCacheByMerchantId merchantId =
  Hedis.delByPattern (makeAllMerchantIdKey merchantId <> "*")

create ::
  RentalFarePolicy ->
  Esq.SqlDB ()
create = Queries.create

markAllAsDeleted ::
  Id Merchant ->
  Esq.SqlDB ()
markAllAsDeleted = Queries.markAllAsDeleted
