{-# OPTIONS_GHC -Wno-deprecations #-}

module Storage.CachedQueries.PlanTranslation where

import Domain.Types.Plan
import Domain.Types.PlanTranslation
import Kernel.External.Types (Language)
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.Queries.PlanTranslation as Queries

findByPlanIdAndLanguage :: (CacheFlow m r, MonadFlow m) => Id Plan -> Language -> m (Maybe PlanTranslation)
findByPlanIdAndLanguage (Id planId) language =
  Hedis.withCrossAppRedis (Hedis.safeGet $ makePlanIdAndLanguageKey (Id planId) language) >>= \case
    Just a -> pure a
    Nothing -> cacheByPlanIdAndLanguage (Id planId) language /=<< Queries.findByPlanIdAndLanguage (Id planId) language

cacheByPlanIdAndLanguage :: (CacheFlow m r) => Id Plan -> Language -> Maybe PlanTranslation -> m ()
cacheByPlanIdAndLanguage (Id planId) language planTranslation = do
  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
  Hedis.withCrossAppRedis $ Hedis.setExp (makePlanIdAndLanguageKey (Id planId) language) planTranslation expTime

makePlanIdAndLanguageKey :: Id Plan -> Language -> Text
makePlanIdAndLanguageKey id language = "driver-offer:CachedQueries:PlanTranslation:PlanId-" <> id.getId <> ":Language-" <> show language
