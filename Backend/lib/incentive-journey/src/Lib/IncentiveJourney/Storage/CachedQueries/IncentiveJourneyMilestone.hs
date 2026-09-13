{-# OPTIONS_GHC -Wno-deprecations #-}

module Lib.IncentiveJourney.Storage.CachedQueries.IncentiveJourneyMilestone
  ( findByJourneyId,
    clearCacheByJourneyId,
  )
where

import Data.List (sortOn)
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Lib.IncentiveJourney.Domain.Types.IncentiveJourney as DIJ
import qualified Lib.IncentiveJourney.Domain.Types.IncentiveJourneyMilestone as DIJM
import Lib.IncentiveJourney.Storage.Beam.BeamFlow (BeamFlow)
import qualified Lib.IncentiveJourney.Storage.Queries.IncentiveJourneyMilestone as Queries
import Lib.IncentiveJourney.Types.Actor (JourneyActor, actorCachePrefix)

findByJourneyId ::
  (BeamFlow m r) =>
  JourneyActor ->
  Id DIJ.IncentiveJourney ->
  m [DIJM.IncentiveJourneyMilestone]
findByJourneyId actor journeyId =
  Hedis.withCrossAppRedis (Hedis.safeGet (makeByJourneyIdKey actor journeyId)) >>= \case
    Just milestones -> pure milestones
    Nothing -> do
      milestones <- sortOn (.order) <$> Queries.findByJourneyId Nothing Nothing journeyId
      cacheByJourneyId actor journeyId milestones
      pure milestones

cacheByJourneyId ::
  (MonadFlow m, CacheFlow m r) =>
  JourneyActor ->
  Id DIJ.IncentiveJourney ->
  [DIJM.IncentiveJourneyMilestone] ->
  m ()
cacheByJourneyId actor journeyId milestones = do
  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
  Hedis.withCrossAppRedis $ Hedis.setExp (makeByJourneyIdKey actor journeyId) milestones expTime

clearCacheByJourneyId :: (CacheFlow m r) => JourneyActor -> Id DIJ.IncentiveJourney -> m ()
clearCacheByJourneyId actor journeyId =
  Hedis.runInMultiCloudRedisWrite $
    Hedis.withCrossAppRedis $
      void $
        Hedis.del (makeByJourneyIdKey actor journeyId)

makeByJourneyIdKey :: JourneyActor -> Id DIJ.IncentiveJourney -> Text
makeByJourneyIdKey actor journeyId =
  actorCachePrefix actor <> ":CachedQueries:IncentiveJourneyMilestone:JourneyId-" <> journeyId.getId
