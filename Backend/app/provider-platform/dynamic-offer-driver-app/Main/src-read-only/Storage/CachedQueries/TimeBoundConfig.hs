{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.CachedQueries.TimeBoundConfig (module Storage.CachedQueries.TimeBoundConfig, module ReExport) where

import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.TimeBoundConfig
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Storage.Hedis as Hedis
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import qualified Lib.Yudhishthira.Types
import Storage.CachedQueries.TimeBoundConfigExtra as ReExport
import qualified Storage.Queries.TimeBoundConfig as Queries

findByCityAndDomain ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity -> Lib.Yudhishthira.Types.LogicDomain -> m ([Domain.Types.TimeBoundConfig.TimeBoundConfig]))
findByCityAndDomain merchantOperatingCityId timeBoundDomain = do
  Hedis.withCrossAppRedis (Hedis.safeGet $ "driverOfferCachedQueries:TimeBoundConfig:" <> ":MerchantOperatingCityId-" <> Kernel.Types.Id.getId merchantOperatingCityId <> ":TimeBoundDomain-" <> show timeBoundDomain)
    >>= ( \case
            Just a -> pure a
            Nothing ->
              ( \dataToBeCached -> do
                  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
                  Hedis.withCrossAppRedis $ Hedis.setExp ("driverOfferCachedQueries:TimeBoundConfig:" <> ":MerchantOperatingCityId-" <> Kernel.Types.Id.getId merchantOperatingCityId <> ":TimeBoundDomain-" <> show timeBoundDomain) dataToBeCached expTime
              )
                /=<< Queries.findByCityAndDomain merchantOperatingCityId timeBoundDomain
        )

findByPrimaryKey ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity -> Kernel.Prelude.Text -> Lib.Yudhishthira.Types.LogicDomain -> m (Kernel.Prelude.Maybe Domain.Types.TimeBoundConfig.TimeBoundConfig))
findByPrimaryKey merchantOperatingCityId name timeBoundDomain = do
  (Hedis.safeGet $ "driverOfferCachedQueries:TimeBoundConfig:" <> ":MerchantOperatingCityId-" <> Kernel.Types.Id.getId merchantOperatingCityId <> ":Name-" <> show name <> ":TimeBoundDomain-" <> show timeBoundDomain)
    >>= ( \case
            Just a -> pure (Just a)
            Nothing ->
              flip
                whenJust
                ( \dataToBeCached -> do
                    expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
                    Hedis.setExp ("driverOfferCachedQueries:TimeBoundConfig:" <> ":MerchantOperatingCityId-" <> Kernel.Types.Id.getId merchantOperatingCityId <> ":Name-" <> show name <> ":TimeBoundDomain-" <> show timeBoundDomain) dataToBeCached expTime
                )
                /=<< Queries.findByPrimaryKey merchantOperatingCityId name timeBoundDomain
        )
