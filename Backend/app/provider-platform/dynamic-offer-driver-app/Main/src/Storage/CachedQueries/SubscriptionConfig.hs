{-# OPTIONS_GHC -Wno-deprecations #-}

module Storage.CachedQueries.SubscriptionConfig where

import qualified Domain.Types.MerchantOperatingCity as MerchantOperatingCity
import Domain.Types.Plan
import Domain.Types.SubscriptionConfig
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Lib.Yudhishthira.Types as LYT
import Storage.Beam.Yudhishthira ()
import qualified Storage.Queries.SubscriptionConfig as Queries
import qualified Tools.DynamicLogic as DynamicLogic

findSubscriptionConfigsByMerchantOpCityIdAndServiceName ::
  (CacheFlow m r, MonadFlow m, EsqDBFlow m r) =>
  Id MerchantOperatingCity.MerchantOperatingCity ->
  Maybe [LYT.ConfigVersionMap] ->
  ServiceNames ->
  m (Maybe SubscriptionConfig)
findSubscriptionConfigsByMerchantOpCityIdAndServiceName merchantOpCityId mbConfigVersionMap serviceName = do
  let cacheKey = makeMerchantOpCityIdAndServiceKey merchantOpCityId serviceName
  DynamicLogic.findOneConfigWithCacheKey
    (cast merchantOpCityId)
    (LYT.DRIVER_CONFIG LYT.SubscriptionConfig)
    mbConfigVersionMap
    Nothing
    (Queries.findSubscriptionConfigsByMerchantOpCityIdAndServiceName (Just merchantOpCityId) serviceName)
    cacheKey

makeMerchantOpCityIdAndServiceKey :: Id MerchantOperatingCity.MerchantOperatingCity -> ServiceNames -> Text
makeMerchantOpCityIdAndServiceKey merchantOpCityId serviceName = "driver-offer:CachedQueries:SubscriptionConfig:MerchantOpCityId-" <> merchantOpCityId.getId <> ":ServiceName-" <> show serviceName

makeMerchantOpCityIdAndUIEnabledKey :: Id MerchantOperatingCity.MerchantOperatingCity -> Bool -> Text
makeMerchantOpCityIdAndUIEnabledKey merchantOpCityId isUiEnabled = "driver-offer:CachedQueries:SubscriptionConfig:MerchantOpCityId-" <> merchantOpCityId.getId <> ":IsUIEnabled-" <> show isUiEnabled

findAllServicesUIEnabledByCity ::
  (CacheFlow m r, MonadFlow m, EsqDBFlow m r) =>
  Id MerchantOperatingCity.MerchantOperatingCity ->
  Bool ->
  Maybe [LYT.ConfigVersionMap] ->
  m [SubscriptionConfig]
findAllServicesUIEnabledByCity merchantOpCityId isUiEnabled mbConfigVersionMap = do
  let cacheKey = makeMerchantOpCityIdAndUIEnabledKey merchantOpCityId isUiEnabled
  DynamicLogic.findAllConfigsWithCacheKey
    (cast merchantOpCityId)
    (LYT.DRIVER_CONFIG LYT.SubscriptionConfig)
    mbConfigVersionMap
    Nothing
    (Queries.findAllServicesUIEnabledByCity (Just merchantOpCityId) isUiEnabled)
    cacheKey

clearCache :: (CacheFlow m r, EsqDBFlow m r) => Id MerchantOperatingCity.MerchantOperatingCity -> m ()
clearCache merchantOpCityId =
  DynamicLogic.clearConfigCache
    (cast merchantOpCityId)
    (LYT.DRIVER_CONFIG LYT.SubscriptionConfig)
    Nothing

clearCacheByServiceName :: (CacheFlow m r, EsqDBFlow m r) => Id MerchantOperatingCity.MerchantOperatingCity -> ServiceNames -> m ()
clearCacheByServiceName merchantOpCityId serviceName =
  DynamicLogic.clearConfigCacheWithPrefix
    (makeMerchantOpCityIdAndServiceKey merchantOpCityId serviceName)
    (cast merchantOpCityId)
    (LYT.DRIVER_CONFIG LYT.SubscriptionConfig)
    Nothing

clearCacheByUIEnabled :: (CacheFlow m r, EsqDBFlow m r) => Id MerchantOperatingCity.MerchantOperatingCity -> Bool -> m ()
clearCacheByUIEnabled merchantOpCityId isUiEnabled =
  DynamicLogic.clearConfigCacheWithPrefix
    (makeMerchantOpCityIdAndUIEnabledKey merchantOpCityId isUiEnabled)
    (cast merchantOpCityId)
    (LYT.DRIVER_CONFIG LYT.SubscriptionConfig)
    Nothing
