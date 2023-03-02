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

module Storage.CachedQueries.FarePolicy.OneWayFarePolicy
  ( findById,
    findByMerchantIdAndVariant,
    findAllByMerchantId,
    clearCache,
    update,
  )
where

import Data.Coerce (coerce)
import Domain.Types.Common
import Domain.Types.FarePolicy.OneWayFarePolicy
import Domain.Types.Merchant (Merchant)
import qualified Domain.Types.Vehicle as Vehicle
import Kernel.Prelude
import qualified Kernel.Storage.Esqueleto as Esq
import Kernel.Storage.Hedis
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Types.Id
import Kernel.Utils.Common
import Storage.CachedQueries.CacheConfig
import qualified Storage.Queries.FarePolicy.OneWayFarePolicy as Queries

findById :: forall m r. (CacheFlow m r, EsqDBFlow m r) => Id OneWayFarePolicy -> m (Maybe OneWayFarePolicy)
findById id =
  Hedis.safeGet (makeIdKey id) >>= \case
    Just a -> return . Just $ coerce @(OneWayFarePolicyD 'Unsafe) @OneWayFarePolicy a
    Nothing -> flip whenJust cacheOneWayFarePolicy /=<< Queries.findById id (Proxy @m)

findByMerchantIdAndVariant ::
  forall m r.
  (CacheFlow m r, EsqDBFlow m r) =>
  Id Merchant ->
  Vehicle.Variant ->
  m (Maybe OneWayFarePolicy)
findByMerchantIdAndVariant merchantId vehVar =
  Hedis.safeGet (makeMerchantIdVehVarKey merchantId vehVar) >>= \case
    Nothing -> findAndCache
    Just id ->
      Hedis.safeGet (makeIdKey id) >>= \case
        Just a -> return . Just $ coerce @(OneWayFarePolicyD 'Unsafe) @OneWayFarePolicy a
        Nothing -> findAndCache
  where
    findAndCache = flip whenJust cacheOneWayFarePolicy /=<< Queries.findByMerchantIdAndVariant merchantId vehVar (Proxy @m)

findAllByMerchantId :: forall m r. (CacheFlow m r, EsqDBFlow m r) => Id Merchant -> m [OneWayFarePolicy]
findAllByMerchantId merchantId =
  Hedis.safeGet (makeAllMerchantIdKey merchantId) >>= \case
    Just a -> return $ fmap (coerce @(OneWayFarePolicyD 'Unsafe) @OneWayFarePolicy) a
    Nothing -> cacheRes /=<< Queries.findAllByMerchantId merchantId (Proxy @m)
  where
    cacheRes owFPs = do
      expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
      Hedis.setExp (makeAllMerchantIdKey merchantId) (coerce @[OneWayFarePolicy] @[OneWayFarePolicyD 'Unsafe] owFPs) expTime

cacheOneWayFarePolicy :: (CacheFlow m r) => OneWayFarePolicy -> m ()
cacheOneWayFarePolicy owFP = do
  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
  let idKey = makeIdKey owFP.id
  Hedis.setExp idKey (coerce @OneWayFarePolicy @(OneWayFarePolicyD 'Unsafe) owFP) expTime
  Hedis.setExp (makeMerchantIdVehVarKey owFP.merchantId owFP.vehicleVariant) idKey expTime

makeIdKey :: Id OneWayFarePolicy -> Text
makeIdKey id = "CachedQueries:OneWayFarePolicy:Id-" <> id.getId

makeMerchantIdVehVarKey :: Id Merchant -> Vehicle.Variant -> Text
makeMerchantIdVehVarKey merchantId vehVar = "CachedQueries:OneWayFarePolicy:MerchantId-" <> merchantId.getId <> ":VehVar-" <> show vehVar

makeAllMerchantIdKey :: Id Merchant -> Text
makeAllMerchantIdKey merchantId = "CachedQueries:OneWayFarePolicy:MerchantId-" <> merchantId.getId <> ":All"

-- Call it after any update
clearCache :: HedisFlow m r => OneWayFarePolicy -> m ()
clearCache owFP = do
  Hedis.del (makeIdKey owFP.id)
  Hedis.del (makeMerchantIdVehVarKey owFP.merchantId owFP.vehicleVariant)
  Hedis.del (makeAllMerchantIdKey owFP.merchantId)

update :: HedisFlow m r => OneWayFarePolicy -> Esq.SqlDB m ()
update owFP = do
  Queries.update owFP
  Esq.finalize $ clearCache owFP
