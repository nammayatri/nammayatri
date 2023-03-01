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

module Storage.CachedQueries.FarePolicy.RestrictedExtraFare where

import Domain.Types.FarePolicy.RestrictedExtraFare
import Domain.Types.Merchant (Merchant)
import qualified Domain.Types.Vehicle as Vehicle
import Kernel.Prelude
import qualified Kernel.Storage.Esqueleto as Esq
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Types.Id
import Kernel.Utils.Common ()
import Storage.CachedQueries.CacheConfig
import Storage.Queries.FarePolicy.RestrictedExtraFare as Queries

findRestrictedFareListByMerchantAndVehicle ::
  forall m r.
  (CacheFlow m r, Esq.EsqDBFlow m r) =>
  Id Merchant ->
  Vehicle.Variant ->
  m [RestrictedExtraFare]
findRestrictedFareListByMerchantAndVehicle merchantId vehVar =
  Hedis.withCrossAppRedis (Hedis.safeGet $ makeMerchantIdVehVarKey merchantId vehVar) >>= \case
    Just a -> pure a
    Nothing -> do
      val <- Queries.findMaxExtraFareByMerchantAndVehicle merchantId vehVar (Proxy @m)
      _ <- cacheRestrictedFareListByMerchantAndVehicle merchantId vehVar val
      pure val

findRestrictedFareListByMerchant ::
  forall m r.
  (CacheFlow m r, Esq.EsqDBFlow m r) =>
  Id Merchant ->
  m [RestrictedExtraFare]
findRestrictedFareListByMerchant merchantId =
  Hedis.withCrossAppRedis (Hedis.safeGet $ makeMerchantIdKey merchantId) >>= \case
    Just a -> pure a
    Nothing -> do
      val <- Queries.findMaxExtraFareByMerchant merchantId (Proxy @m)
      _ <- cacheRestrictedFareListByMerchant merchantId val
      pure val

makeMerchantIdVehVarKey :: Id Merchant -> Vehicle.Variant -> Text
makeMerchantIdVehVarKey merchantId vehVar = "driver-offer:CachedQueries:RestrictExtraFee:MerchantId-" <> merchantId.getId <> ":VehVar-" <> show vehVar

makeMerchantIdKey :: Id Merchant -> Text
makeMerchantIdKey merchantId = "driver-offer:CachedQueries:RestrictExtraFee:MerchantId-" <> merchantId.getId

cacheRestrictedFareListByMerchantAndVehicle :: (CacheFlow m r) => Id Merchant -> Vehicle.Variant -> [RestrictedExtraFare] -> m ()
cacheRestrictedFareListByMerchantAndVehicle merchantId vehVar resFare = do
  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
  Hedis.withCrossAppRedis $ Hedis.setExp (makeMerchantIdVehVarKey merchantId vehVar) resFare expTime

cacheRestrictedFareListByMerchant :: (CacheFlow m r) => Id Merchant -> [RestrictedExtraFare] -> m ()
cacheRestrictedFareListByMerchant merchantId resFare = do
  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
  Hedis.withCrossAppRedis $ Hedis.setExp (makeMerchantIdKey merchantId) resFare expTime
