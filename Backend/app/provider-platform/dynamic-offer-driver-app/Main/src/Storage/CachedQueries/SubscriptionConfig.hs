{-# OPTIONS_GHC -Wno-deprecations #-}

module Storage.CachedQueries.SubscriptionConfig where

import qualified Domain.Types.Merchant.MerchantOperatingCity as MerchantOperatingCity
import Domain.Types.Plan
import Domain.Types.SubscriptionConfig
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.Queries.SubscriptionConfig as Queries

findSubscriptionConfigsByMerchantOpCityIdAndServiceName ::
  (CacheFlow m r, MonadFlow m, EsqDBFlow m r) =>
  Id MerchantOperatingCity.MerchantOperatingCity ->
  ServiceNames ->
  m (Maybe SubscriptionConfig)
findSubscriptionConfigsByMerchantOpCityIdAndServiceName (Id merchantOpCity) serviceName =
  Hedis.withCrossAppRedis (Hedis.safeGet $ makeMerchantOpCityIdAndServiceKey (Id merchantOpCity) serviceName) >>= \case
    Just a -> pure a
    Nothing -> cacheByMerchantOpCityAndServiceName (Id merchantOpCity) serviceName /=<< Queries.findSubscriptionConfigsByMerchantOpCityIdAndServiceName (Just $ Id merchantOpCity) serviceName

cacheByMerchantOpCityAndServiceName :: CacheFlow m r => Id MerchantOperatingCity.MerchantOperatingCity -> ServiceNames -> Maybe SubscriptionConfig -> m ()
cacheByMerchantOpCityAndServiceName (Id merchantOpCity) serviceName config = do
  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
  Hedis.withCrossAppRedis $ Hedis.setExp (makeMerchantOpCityIdAndServiceKey (Id merchantOpCity) serviceName) config expTime

makeMerchantOpCityIdAndServiceKey :: Id MerchantOperatingCity.MerchantOperatingCity -> ServiceNames -> Text
makeMerchantOpCityIdAndServiceKey merchantOpCityId serviceName = "driver-offer:CachedQueries:SubscriptionConfig:MerchantOpCityId-" <> merchantOpCityId.getId <> ":ServiceName-" <> show serviceName
