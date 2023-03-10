{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-deprecations #-}

module Storage.CachedQueries.FarePolicy
  ( findById,
    findByMerchantIdAndVariant,
    findAllByMerchantId,
    clearCache,
    update,
  )
where

import Data.Coerce (coerce)
import Domain.Types.Common
import Domain.Types.FarePolicy
import Domain.Types.Merchant (Merchant)
import qualified Domain.Types.Vehicle as Vehicle
import Kernel.Prelude
import qualified Kernel.Storage.Esqueleto as Esq
import Kernel.Storage.Hedis
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Types.Id
import Kernel.Utils.Common
import Storage.CachedQueries.CacheConfig
import Storage.CachedQueries.FarePolicy.RestrictedExtraFare (findRestrictedFareListByMerchant, findRestrictedFareListByMerchantAndVehicle)
import qualified Storage.Queries.FarePolicy as Queries

getUpdatedFarePolicy :: (CacheFlow m r, EsqDBFlow m r) => Id Merchant -> Maybe Vehicle.Variant -> Meters -> FarePolicy -> m FarePolicy
getUpdatedFarePolicy mId varId distance farePolicy = do
  restrictedPolicy <-
    case varId of
      Just var -> findRestrictedFareListByMerchantAndVehicle mId var
      Nothing -> findRestrictedFareListByMerchant mId
  pure $ maybe farePolicy (updateMaxExtraFare farePolicy) (restrictedFair restrictedPolicy)
  where
    updateMaxExtraFare FarePolicy {..} maxFee = FarePolicy {driverExtraFee = driverExtraFee {maxFee}, ..}
    restrictedFair fares =
      case find (\fare -> fare.minTripDistance <= distance) fares of
        Just fare -> Just (fare.driverMaxExtraFare)
        Nothing -> Nothing

findById :: (CacheFlow m r, EsqDBFlow m r) => Id FarePolicy -> m (Maybe FarePolicy)
findById id =
  Hedis.withCrossAppRedis (Hedis.safeGet $ makeIdKey id) >>= \case
    Just a -> return . Just $ coerce @(FarePolicyD 'Unsafe) @FarePolicy a
    Nothing -> flip whenJust cacheFarePolicy /=<< Queries.findById id

findByMerchantIdAndVariant ::
  (CacheFlow m r, EsqDBFlow m r) =>
  Id Merchant ->
  Vehicle.Variant ->
  Maybe Meters ->
  m (Maybe FarePolicy)
findByMerchantIdAndVariant merchantId vehVar mRideDistance =
  case mRideDistance of
    Just distance -> mapM (getUpdatedFarePolicy merchantId (Just vehVar) distance) =<< getFarePolicy
    Nothing -> getFarePolicy
  where
    getFarePolicy = do
      Hedis.withCrossAppRedis (Hedis.safeGet $ makeMerchantIdVehVarKey merchantId vehVar) >>= \case
        Nothing -> findAndCache
        Just id ->
          Hedis.withCrossAppRedis (Hedis.safeGet $ makeIdKey id) >>= \case
            Just a -> return . Just $ coerce @(FarePolicyD 'Unsafe) @FarePolicy a
            Nothing -> findAndCache
    findAndCache = flip whenJust cacheFarePolicy /=<< Queries.findByMerchantIdAndVariant merchantId vehVar

findAllByMerchantId :: (CacheFlow m r, EsqDBFlow m r) => Id Merchant -> Maybe Meters -> m [FarePolicy]
findAllByMerchantId merchantId mRideDistance =
  case mRideDistance of
    Just distance -> traverse (getUpdatedFarePolicy merchantId Nothing distance) =<< getFarePolicy
    Nothing -> getFarePolicy
  where
    getFarePolicy = do
      Hedis.withCrossAppRedis (Hedis.safeGet $ makeAllMerchantIdKey merchantId) >>= \case
        Just a -> return $ fmap (coerce @(FarePolicyD 'Unsafe) @FarePolicy) a
        Nothing -> cacheRes /=<< Queries.findAllByMerchantId merchantId
    cacheRes fps = do
      expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
      Hedis.withCrossAppRedis $ Hedis.setExp (makeAllMerchantIdKey merchantId) (coerce @[FarePolicy] @[FarePolicyD 'Unsafe] fps) expTime

cacheFarePolicy :: (CacheFlow m r) => FarePolicy -> m ()
cacheFarePolicy fp = do
  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
  let idKey = makeIdKey fp.id
  Hedis.withCrossAppRedis $ do
    Hedis.setExp idKey (coerce @FarePolicy @(FarePolicyD 'Unsafe) fp) expTime
    Hedis.setExp (makeMerchantIdVehVarKey fp.merchantId fp.vehicleVariant) idKey expTime

makeIdKey :: Id FarePolicy -> Text
makeIdKey id = "driver-offer:CachedQueries:FarePolicy:Id-" <> id.getId

makeMerchantIdVehVarKey :: Id Merchant -> Vehicle.Variant -> Text
makeMerchantIdVehVarKey merchantId vehVar = "driver-offer:CachedQueries:FarePolicy:MerchantId-" <> merchantId.getId <> ":VehVar-" <> show vehVar

makeAllMerchantIdKey :: Id Merchant -> Text
makeAllMerchantIdKey merchantId = "driver-offer:CachedQueries:FarePolicy:MerchantId-" <> merchantId.getId <> ":All"

-- Call it after any update
clearCache :: HedisFlow m r => FarePolicy -> m ()
clearCache fp = Hedis.withCrossAppRedis $ do
  Hedis.del (makeIdKey fp.id)
  Hedis.del (makeMerchantIdVehVarKey fp.merchantId fp.vehicleVariant)
  Hedis.del (makeAllMerchantIdKey fp.merchantId)

update :: FarePolicy -> Esq.SqlDB ()
update = Queries.update
