{-# OPTIONS_GHC -Wno-deprecations #-}

module Storage.CachedQueries.SubscriptionConfig where

import qualified Domain.Types.Merchant.MerchantOperatingCity as MerchantOperatingCity
import Domain.Types.Plan
import Domain.Types.SubscriptionConfig
import qualified Domain.Types.Vehicle as Vehicle
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.Queries.SubscriptionConfig as Queries

findSubscriptionConfigsByMerchantOpCityIdAndServiceNameWithVehicleVariant ::
  (CacheFlow m r, MonadFlow m, EsqDBFlow m r) =>
  Id MerchantOperatingCity.MerchantOperatingCity ->
  ServiceNames ->
  Vehicle.Variant ->
  m (Maybe SubscriptionConfig)
findSubscriptionConfigsByMerchantOpCityIdAndServiceNameWithVehicleVariant (Id merchantOpCity) serviceName vehicleVariant =
  Hedis.withCrossAppRedis (Hedis.safeGet $ makeMerchantOpCityIdAndServiceWithVehicleKey (Id merchantOpCity) serviceName vehicleVariant) >>= \case
    Just a -> pure a
    Nothing -> cacheByMerchantOpCityAndServiceNameWithVariant (Id merchantOpCity) serviceName vehicleVariant /=<< Queries.findSubscriptionConfigsByMerchantOpCityIdAndServiceNameWithVehicleVariant (Just $ Id merchantOpCity) serviceName vehicleVariant

findSubscriptionConfigsByMerchantOpCityIdAndServiceName ::
  (CacheFlow m r, MonadFlow m, EsqDBFlow m r) =>
  Id MerchantOperatingCity.MerchantOperatingCity ->
  ServiceNames ->
  m [SubscriptionConfig]
findSubscriptionConfigsByMerchantOpCityIdAndServiceName (Id merchantOpCity) serviceName =
  Hedis.withCrossAppRedis (Hedis.safeGet $ makeMerchantOpCityIdAndServiceKey (Id merchantOpCity) serviceName) >>= \case
    Just a -> pure a
    Nothing -> cacheByMerchantOpCityAndServiceName (Id merchantOpCity) serviceName /=<< Queries.findSubscriptionConfigsByMerchantOpCityIdAndServiceName (Just $ Id merchantOpCity) serviceName

cacheByMerchantOpCityAndServiceName ::
  CacheFlow m r =>
  Id MerchantOperatingCity.MerchantOperatingCity ->
  ServiceNames ->
  [SubscriptionConfig] ->
  m ()
cacheByMerchantOpCityAndServiceName (Id merchantOpCity) serviceName configs = do
  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
  Hedis.withCrossAppRedis $ Hedis.setExp (makeMerchantOpCityIdAndServiceKey (Id merchantOpCity) serviceName) configs expTime

cacheByMerchantOpCityAndServiceNameWithVariant ::
  CacheFlow m r =>
  Id MerchantOperatingCity.MerchantOperatingCity ->
  ServiceNames ->
  Vehicle.Variant ->
  Maybe SubscriptionConfig ->
  m ()
cacheByMerchantOpCityAndServiceNameWithVariant (Id merchantOpCity) serviceName variant config = do
  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
  Hedis.withCrossAppRedis $ Hedis.setExp (makeMerchantOpCityIdAndServiceWithVehicleKey (Id merchantOpCity) serviceName variant) config expTime

makeMerchantOpCityIdAndServiceWithVehicleKey :: Id MerchantOperatingCity.MerchantOperatingCity -> ServiceNames -> Vehicle.Variant -> Text
makeMerchantOpCityIdAndServiceWithVehicleKey merchantOpCityId serviceName variant = "driver-offer:CachedQueries:SubscriptionConfig:MerchantOpCityId-" <> merchantOpCityId.getId <> ":ServiceName-" <> show serviceName <> ":VehicleVariant-" <> show variant

makeMerchantOpCityIdAndServiceKey :: Id MerchantOperatingCity.MerchantOperatingCity -> ServiceNames -> Text
makeMerchantOpCityIdAndServiceKey merchantOpCityId serviceName = "driver-offer:CachedQueries:SubscriptionConfig:MerchantOpCityId-" <> merchantOpCityId.getId <> ":ServiceName-" <> show serviceName
