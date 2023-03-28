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

module Storage.CachedQueries.SlabFarePolicy
  ( findById,
    findByMerchantIdAndVariant,
    findAllByMerchantId,
    clearCache,
  )
where

import Data.Coerce (coerce)
import Domain.Types.Common
import Domain.Types.Merchant (Merchant)
import Domain.Types.SlabFarePolicy
import qualified Domain.Types.Vehicle as Vehicle
import Kernel.Prelude
import Kernel.Storage.Hedis
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Types.Id
import Kernel.Utils.Common
import Storage.CachedQueries.CacheConfig
import qualified Storage.Queries.SlabFarePolicy as Queries

findById :: (CacheFlow m r, EsqDBFlow m r) => Id SlabFarePolicy -> m (Maybe SlabFarePolicy)
findById id =
  Hedis.withCrossAppRedis (Hedis.safeGet $ makeIdKey id) >>= \case
    Just a -> return . Just $ coerce @(SlabFarePolicyD 'Unsafe) @SlabFarePolicy a
    Nothing -> flip whenJust cacheSlabFarePolicy /=<< Queries.findById id

findByMerchantIdAndVariant ::
  (CacheFlow m r, EsqDBFlow m r) =>
  Id Merchant ->
  Vehicle.Variant ->
  m (Maybe SlabFarePolicy)
findByMerchantIdAndVariant merchantId vehVar = do
  Hedis.withCrossAppRedis (Hedis.safeGet $ makeMerchantIdVehVarKey merchantId vehVar) >>= \case
    Nothing -> findAndCache
    Just id ->
      Hedis.withCrossAppRedis (Hedis.safeGet $ makeIdKey id) >>= \case
        Just a -> return . Just $ coerce @(SlabFarePolicyD 'Unsafe) @SlabFarePolicy a
        Nothing -> findAndCache
  where
    findAndCache = flip whenJust cacheSlabFarePolicy /=<< Queries.findByMerchantIdAndVariant merchantId vehVar

findAllByMerchantId :: (CacheFlow m r, EsqDBFlow m r) => Id Merchant -> m [SlabFarePolicy]
findAllByMerchantId merchantId = do
  Hedis.withCrossAppRedis (Hedis.safeGet $ makeAllMerchantIdKey merchantId) >>= \case
    Just a -> return $ fmap (coerce @(SlabFarePolicyD 'Unsafe) @SlabFarePolicy) a
    Nothing -> cacheRes /=<< Queries.findAllByMerchantId merchantId
  where
    cacheRes fps = do
      expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
      Hedis.withCrossAppRedis $ Hedis.setExp (makeAllMerchantIdKey merchantId) (coerce @[SlabFarePolicy] @[SlabFarePolicyD 'Unsafe] fps) expTime

cacheSlabFarePolicy :: (CacheFlow m r) => SlabFarePolicy -> m ()
cacheSlabFarePolicy fp = do
  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
  let idKey = makeIdKey fp.id
  Hedis.withCrossAppRedis $ do
    Hedis.setExp idKey (coerce @SlabFarePolicy @(SlabFarePolicyD 'Unsafe) fp) expTime
    Hedis.setExp (makeMerchantIdVehVarKey fp.merchantId fp.vehicleVariant) idKey expTime

makeIdKey :: Id SlabFarePolicy -> Text
makeIdKey id = "driver-offer:CachedQueries:SlabFarePolicy:Id-" <> id.getId

makeMerchantIdVehVarKey :: Id Merchant -> Vehicle.Variant -> Text
makeMerchantIdVehVarKey merchantId vehVar = "driver-offer:CachedQueries:SlabFarePolicy:MerchantId-" <> merchantId.getId <> ":VehVar-" <> show vehVar

makeAllMerchantIdKey :: Id Merchant -> Text
makeAllMerchantIdKey merchantId = "driver-offer:CachedQueries:SlabFarePolicy:MerchantId-" <> merchantId.getId <> ":All"

-- Call it after any update
clearCache :: HedisFlow m r => SlabFarePolicy -> m ()
clearCache fp = Hedis.withCrossAppRedis $ do
  Hedis.del (makeIdKey fp.id)
  Hedis.del (makeMerchantIdVehVarKey fp.merchantId fp.vehicleVariant)
  Hedis.del (makeAllMerchantIdKey fp.merchantId)
