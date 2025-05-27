{-# OPTIONS_GHC -Wno-orphans #-}

module Lib.Yudhishthira.Storage.CachedQueries.AppDynamicLogicElement where

import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Hedis
import qualified Lib.Yudhishthira.Storage.Beam.BeamFlow as BeamFlow
import qualified Lib.Yudhishthira.Storage.Queries.AppDynamicLogicElement as Queries
import qualified Lib.Yudhishthira.Types
import qualified Lib.Yudhishthira.Types.AppDynamicLogicElement

findByDomain :: (BeamFlow.BeamFlow m r) => (Lib.Yudhishthira.Types.LogicDomain -> m ([Lib.Yudhishthira.Types.AppDynamicLogicElement.AppDynamicLogicElement]))
findByDomain domain = do
  Hedis.withCrossAppRedis (Hedis.safeGet $ domainCacheKey domain)
    >>= ( \case
            Just a -> if null a then fetchAndCase else pure a
            Nothing -> fetchAndCase
        )
  where
    fetchAndCase =
      ( \dataToBeCached -> do
          expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
          Hedis.withCrossAppRedis $ Hedis.setExp (domainCacheKey domain) dataToBeCached expTime
      )
        /=<< Queries.findByDomain domain

findByDomainAndVersion ::
  (BeamFlow.BeamFlow m r) =>
  (Lib.Yudhishthira.Types.LogicDomain -> Kernel.Prelude.Int -> m ([Lib.Yudhishthira.Types.AppDynamicLogicElement.AppDynamicLogicElement]))
findByDomainAndVersion domain version = do
  Hedis.withCrossAppRedis (Hedis.safeGet $ domainAndVersionCacheKey domain version)
    >>= ( \case
            Just a -> if null a then fetchAndCase else pure a
            Nothing -> fetchAndCase
        )
  where
    fetchAndCase =
      ( \dataToBeCached -> do
          expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
          Hedis.withCrossAppRedis $ Hedis.setExp (domainAndVersionCacheKey domain version) dataToBeCached expTime
      )
        /=<< Queries.findByDomainAndVersion Nothing Nothing domain version

clearCache :: BeamFlow.BeamFlow m r => Lib.Yudhishthira.Types.LogicDomain -> m ()
clearCache domain =
  Hedis.withCrossAppRedis $ Hedis.del $ domainCacheKey domain

createMany :: (BeamFlow.BeamFlow m r) => [Lib.Yudhishthira.Types.AppDynamicLogicElement.AppDynamicLogicElement] -> m ()
createMany = Queries.createMany

create :: BeamFlow.BeamFlow m r => Lib.Yudhishthira.Types.AppDynamicLogicElement.AppDynamicLogicElement -> m ()
create = Queries.create

-- Cache key functions
domainCacheKey :: Lib.Yudhishthira.Types.LogicDomain -> Text
domainCacheKey domain = "yudhishthira-CachedQueries:AppDynamicLogicElement:" <> ":Domain-" <> show domain

domainAndVersionCacheKey :: Lib.Yudhishthira.Types.LogicDomain -> Kernel.Prelude.Int -> Text
domainAndVersionCacheKey domain version = "yudhishthira-CachedQueries:AppDynamicLogicElement:" <> ":Domain-" <> show domain <> ":Version-" <> show version
