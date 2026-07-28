{-# OPTIONS_GHC -Wno-deprecations #-}

module Storage.CachedQueries.IncentiveJourneyMilestone
  ( findByJourneyId,
    clearCacheByJourneyId,
  )
where

import Data.List (sortOn)
import qualified Domain.Types.IncentiveJourney as DIJ
import qualified Domain.Types.IncentiveJourneyMilestone as DIJM
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.Queries.IncentiveJourneyMilestone as Queries

findByJourneyId ::
  (CacheFlow m r, EsqDBFlow m r) =>
  Id DIJ.IncentiveJourney ->
  m [DIJM.IncentiveJourneyMilestone]
findByJourneyId journeyId =
  Hedis.withCrossAppRedis (Hedis.safeGet (makeByJourneyIdKey journeyId)) >>= \case
    Just milestones -> pure milestones
    Nothing -> do
      milestones <- sortOn (.order) <$> Queries.findByJourneyId Nothing Nothing journeyId
      cacheByJourneyId journeyId milestones
      pure milestones

cacheByJourneyId :: (MonadFlow m, CacheFlow m r) => Id DIJ.IncentiveJourney -> [DIJM.IncentiveJourneyMilestone] -> m ()
cacheByJourneyId journeyId milestones = do
  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
  Hedis.withCrossAppRedis $ Hedis.setExp (makeByJourneyIdKey journeyId) milestones expTime

clearCacheByJourneyId :: (CacheFlow m r) => Id DIJ.IncentiveJourney -> m ()
clearCacheByJourneyId journeyId =
  Hedis.runInMultiCloudRedisWrite $
    Hedis.withCrossAppRedis $
      void $
        Hedis.del (makeByJourneyIdKey journeyId)

makeByJourneyIdKey :: Id DIJ.IncentiveJourney -> Text
makeByJourneyIdKey journeyId = "driver-offer:CachedQueries:IncentiveJourneyMilestone:JourneyId-" <> journeyId.getId
