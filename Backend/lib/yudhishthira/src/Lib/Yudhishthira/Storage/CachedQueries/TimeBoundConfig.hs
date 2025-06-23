{-# OPTIONS_GHC -Wno-orphans #-}

module Lib.Yudhishthira.Storage.CachedQueries.TimeBoundConfig where

import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Types.Id
import qualified Lib.Yudhishthira.Storage.Beam.BeamFlow as BeamFlow
import qualified Lib.Yudhishthira.Storage.Queries.TimeBoundConfig as Queries
import Lib.Yudhishthira.Types
import qualified Lib.Yudhishthira.Types.TimeBoundConfig

create :: BeamFlow.BeamFlow m r => Lib.Yudhishthira.Types.TimeBoundConfig.TimeBoundConfig -> m ()
create = Queries.create

findByCityAndDomain ::
  BeamFlow.BeamFlow m r =>
  (Kernel.Types.Id.Id Lib.Yudhishthira.Types.MerchantOperatingCity -> Lib.Yudhishthira.Types.LogicDomain -> m ([Lib.Yudhishthira.Types.TimeBoundConfig.TimeBoundConfig]))
findByCityAndDomain merchantOperatingCityId timeBoundDomain = do
  Hedis.withCrossAppRedis (Hedis.safeGet $ cityAndDomainCacheKey merchantOperatingCityId timeBoundDomain)
    >>= ( \case
            Just a -> pure a
            Nothing ->
              ( \dataToBeCached -> do
                  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
                  Hedis.withCrossAppRedis $ Hedis.setExp (cityAndDomainCacheKey merchantOperatingCityId timeBoundDomain) dataToBeCached expTime
              )
                /=<< Queries.findByCityAndDomain merchantOperatingCityId timeBoundDomain
        )

findByPrimaryKey ::
  BeamFlow.BeamFlow m r =>
  (Kernel.Types.Id.Id Lib.Yudhishthira.Types.MerchantOperatingCity -> Kernel.Prelude.Text -> Lib.Yudhishthira.Types.LogicDomain -> m (Kernel.Prelude.Maybe Lib.Yudhishthira.Types.TimeBoundConfig.TimeBoundConfig))
findByPrimaryKey merchantOperatingCityId name timeBoundDomain = do
  (Hedis.safeGet $ primaryKeyCacheKey merchantOperatingCityId name timeBoundDomain)
    >>= ( \case
            Just a -> pure (Just a)
            Nothing ->
              flip
                whenJust
                ( \dataToBeCached -> do
                    expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
                    Hedis.setExp (primaryKeyCacheKey merchantOperatingCityId name timeBoundDomain) dataToBeCached expTime
                )
                /=<< Queries.findByPrimaryKey merchantOperatingCityId name timeBoundDomain
        )

delete :: BeamFlow.BeamFlow m r => Id MerchantOperatingCity -> LogicDomain -> Text -> m ()
delete = Queries.delete

clearCache :: BeamFlow.BeamFlow m r => Id MerchantOperatingCity -> LogicDomain -> Text -> m ()
clearCache merchantOperatingCityId timeBoundDomain name = do
  Hedis.withCrossAppRedis $ Hedis.del (cityAndDomainCacheKey merchantOperatingCityId timeBoundDomain)
  Hedis.withCrossAppRedis $ Hedis.del (primaryKeyCacheKey merchantOperatingCityId name timeBoundDomain)

-- Cache key functions
cityAndDomainCacheKey :: Kernel.Types.Id.Id Lib.Yudhishthira.Types.MerchantOperatingCity -> Lib.Yudhishthira.Types.LogicDomain -> Text
cityAndDomainCacheKey merchantOperatingCityId timeBoundDomain = "yudhishthira-CachedQueries:TimeBoundConfig:" <> ":MerchantOperatingCityId-" <> Kernel.Types.Id.getId merchantOperatingCityId <> ":TimeBoundDomain-" <> show timeBoundDomain

primaryKeyCacheKey :: Kernel.Types.Id.Id Lib.Yudhishthira.Types.MerchantOperatingCity -> Kernel.Prelude.Text -> Lib.Yudhishthira.Types.LogicDomain -> Text
primaryKeyCacheKey merchantOperatingCityId name timeBoundDomain = "yudhishthira-CachedQueries:TimeBoundConfig:" <> ":MerchantOperatingCityId-" <> Kernel.Types.Id.getId merchantOperatingCityId <> ":Name-" <> show name <> ":TimeBoundDomain-" <> show timeBoundDomain
