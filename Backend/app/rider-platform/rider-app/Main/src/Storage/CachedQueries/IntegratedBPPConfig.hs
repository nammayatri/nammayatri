{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.CachedQueries.IntegratedBPPConfig where

import BecknV2.OnDemand.Enums (VehicleCategory)
import Domain.Types.IntegratedBPPConfig (IntegratedBPPConfig, PlatformType)
import Domain.Types.MerchantOperatingCity (MerchantOperatingCity)
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Hedis
import qualified Kernel.Storage.InMem as IM
import Kernel.Types.Id (Id, getId)
import Kernel.Utils.Common
import qualified Storage.Queries.IntegratedBPPConfig as Queries

findAllByDomainAndCityAndVehicleCategory ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  Text ->
  Id MerchantOperatingCity ->
  VehicleCategory ->
  PlatformType ->
  m [IntegratedBPPConfig]
findAllByDomainAndCityAndVehicleCategory domain merchantOperatingCityId vehicleCategory platformType = do
  let cacheKey = buildDomainCacheKey domain merchantOperatingCityId vehicleCategory platformType ":All"
  IM.withInMemCache [cacheKey] 3600 $ do
    Hedis.safeGet cacheKey
      >>= ( \case
              Just a -> pure a
              Nothing -> do
                dataToBeCached <- Queries.findAllByDomainAndCityAndVehicleCategory domain merchantOperatingCityId vehicleCategory platformType
                unless (null dataToBeCached) $ do
                  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
                  Hedis.setExp cacheKey dataToBeCached expTime
                pure dataToBeCached
          )

findByDomainAndCityAndVehicleCategory :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => Text -> Id MerchantOperatingCity -> VehicleCategory -> PlatformType -> m (Maybe IntegratedBPPConfig)
findByDomainAndCityAndVehicleCategory domain merchantOperatingCityId vehicleCategory platformType = do
  let cacheKey = buildDomainCacheKey domain merchantOperatingCityId vehicleCategory platformType ""
  IM.withInMemCache [cacheKey] 3600 $ do
    Hedis.safeGet cacheKey
      >>= ( \case
              Just a -> pure a
              Nothing -> do
                dataToBeCached <- Queries.findByDomainAndCityAndVehicleCategory domain merchantOperatingCityId vehicleCategory platformType
                when (isJust dataToBeCached) $ do
                  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
                  Hedis.setExp cacheKey dataToBeCached expTime
                pure dataToBeCached
          )

findById :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => Id IntegratedBPPConfig -> m (Maybe IntegratedBPPConfig)
findById integratedBPPConfigId = do
  let cacheKey = buildIdCacheKey integratedBPPConfigId
  IM.withInMemCache [cacheKey] 3600 $ do
    Hedis.safeGet cacheKey
      >>= ( \case
              Just a -> pure a
              Nothing -> do
                dataToBeCached <- Queries.findById integratedBPPConfigId
                when (isJust dataToBeCached) $ do
                  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
                  Hedis.setExp cacheKey dataToBeCached expTime
                pure dataToBeCached
          )

findByAgencyId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => Text -> m (Maybe IntegratedBPPConfig)
findByAgencyId agencyKey = do
  let cacheKey = buildAgencyCacheKey agencyKey
  IM.withInMemCache [cacheKey] 3600 $ do
    Hedis.safeGet cacheKey
      >>= ( \case
              Just a -> pure a
              Nothing -> do
                dataToBeCached <- Queries.findByAgencyId agencyKey
                when (isJust dataToBeCached) $ do
                  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
                  Hedis.setExp cacheKey dataToBeCached expTime
                pure dataToBeCached
          )

findAllByPlatformAndVehicleCategory :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => Text -> VehicleCategory -> PlatformType -> m [IntegratedBPPConfig]
findAllByPlatformAndVehicleCategory domain vehicleCategory platformType = do
  let cacheKey = buildPlatformCacheKey domain vehicleCategory platformType
  IM.withInMemCache [cacheKey] 3600 $ do
    Hedis.safeGet cacheKey
      >>= ( \case
              Just a -> pure a
              Nothing -> do
                dataToBeCached <- Queries.findAllByPlatformAndVehicleCategory domain vehicleCategory platformType
                unless (null dataToBeCached) $ do
                  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
                  Hedis.setExp cacheKey dataToBeCached expTime
                pure dataToBeCached
          )

-- Helper functions for cache key construction
buildDomainCacheKey :: Text -> Id MerchantOperatingCity -> VehicleCategory -> PlatformType -> Text -> Text
buildDomainCacheKey domain merchantOperatingCityId vehicleCategory platformType suffix =
  "CachedQueries:IntegratedBPPConfig"
    <> ":Domain-"
    <> domain
    <> ":MerchantOperatingCityId-"
    <> getId merchantOperatingCityId
    <> ":VehicleCategory-"
    <> show vehicleCategory
    <> ":PlatformType-"
    <> show platformType
    <> suffix

buildIdCacheKey :: Id IntegratedBPPConfig -> Text
buildIdCacheKey integratedBPPConfigId = "CachedQueries:IntegratedBPPConfig:Id-" <> getId integratedBPPConfigId

buildAgencyCacheKey :: Text -> Text
buildAgencyCacheKey agencyKey = "CachedQueries:IntegratedBPPConfig:AgencyId-" <> agencyKey

buildPlatformCacheKey :: Text -> VehicleCategory -> PlatformType -> Text
buildPlatformCacheKey domain vehicleCategory platformType =
  "CachedQueries:IntegratedBPPConfig"
    <> ":Platform-"
    <> domain
    <> ":VehicleCategory-"
    <> show vehicleCategory
    <> ":PlatformType-"
    <> show platformType
    <> ":AllCities"
