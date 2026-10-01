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
import qualified Storage.Queries.IntegratedBPPConfigExtra as QueriesExtra

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

-- | The row of an agency key for the caller's platform type (journeys want MULTIMODAL, the driver proxy APPLICATION).
findByAgencyId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => Text -> PlatformType -> m (Maybe IntegratedBPPConfig)
findByAgencyId agencyKey platformType = do
  let cacheKey = buildAgencyCacheKey agencyKey platformType
  -- Redis only: an in-memory layer would pin a miss for an hour, and a Nothing is never cached anywhere
  Hedis.safeGet cacheKey
    >>= ( \case
            Just a -> pure a
            Nothing -> do
              dataToBeCached <- QueriesExtra.findByAgencyIdDeterministic agencyKey platformType
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

buildAgencyCacheKey :: Text -> PlatformType -> Text
buildAgencyCacheKey agencyKey platformType = "CachedQueries:IntegratedBPPConfig:AgencyId-" <> agencyKey <> ":" <> show platformType

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
