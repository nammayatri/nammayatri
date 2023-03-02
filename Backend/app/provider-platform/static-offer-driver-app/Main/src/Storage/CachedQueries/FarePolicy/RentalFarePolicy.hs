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

import Data.Coerce (coerce)
import Domain.Types.Common
import Domain.Types.FarePolicy.RentalFarePolicy
import Domain.Types.Merchant (Merchant)
import qualified Domain.Types.Vehicle as Vehicle
import Kernel.Prelude
import qualified Kernel.Storage.Esqueleto as Esq
import Kernel.Storage.Hedis
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Types.Id
import Kernel.Utils.Common
import Storage.CachedQueries.CacheConfig
import qualified Storage.Queries.FarePolicy.RentalFarePolicy as Queries

findById :: forall m r. (CacheFlow m r, EsqDBFlow m r) => Id RentalFarePolicy -> m (Maybe RentalFarePolicy)
findById id =
  Hedis.safeGet (makeIdKey id) >>= \case
    Just a -> return . Just $ coerce @(RentalFarePolicyD 'Unsafe) @RentalFarePolicy a
    Nothing -> flip whenJust cacheRentalFarePolicy /=<< Queries.findById id (Proxy @m)

findByOffer ::
  forall m r.
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
    findAndCache = flip whenJust cacheRentalFarePolicy /=<< Queries.findByOffer merchantId vehVar kms hours (Proxy @m)

findAllByMerchantId :: forall m r. (CacheFlow m r, EsqDBFlow m r) => Id Merchant -> m [RentalFarePolicy]
findAllByMerchantId merchantId =
  Hedis.safeGet (makeAllMerchantIdKey merchantId) >>= \case
    Just a -> return $ fmap (coerce @(RentalFarePolicyD 'Unsafe) @RentalFarePolicy) a
    Nothing -> cacheRes /=<< Queries.findAllByMerchantId merchantId (Proxy @m)
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
  Esq.SqlDB m ()
create = Queries.create

markAllAsDeleted ::
  Id Merchant ->
  Esq.SqlDB m ()
markAllAsDeleted = Queries.markAllAsDeleted
