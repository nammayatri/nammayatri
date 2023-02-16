{-# LANGUAGE DerivingStrategies #-}
{-# OPTIONS_GHC -Wno-deprecations #-}

module Storage.CachedQueries.RestrictedExtraFare where

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
  (CacheFlow m r, Esq.EsqDBFlow m r) =>
  Id Merchant ->
  Vehicle.Variant ->
  m [RestrictedExtraFare]
findRestrictedFareListByMerchantAndVehicle merchantId vehVar =
  Hedis.withCrossAppRedis (Hedis.safeGet $ makeMerchantIdVehVarKey merchantId vehVar) >>= \case
    Just a -> pure a
    Nothing -> do
      val <- Queries.findMaxExtraFareByMerchantAndVehicle merchantId vehVar
      _ <- cacheRestrictedFareListByMerchantAndVehicle merchantId vehVar val
      pure val

findRestrictedFareListByMerchant ::
  (CacheFlow m r, Esq.EsqDBFlow m r) =>
  Id Merchant ->
  m [RestrictedExtraFare]
findRestrictedFareListByMerchant merchantId =
  Hedis.withCrossAppRedis (Hedis.safeGet $ makeMerchantIdKey merchantId) >>= \case
    Just a -> pure a
    Nothing -> do
      val <- Queries.findMaxExtraFareByMerchant merchantId
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
