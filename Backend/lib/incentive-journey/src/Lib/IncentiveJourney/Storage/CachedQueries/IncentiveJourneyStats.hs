{-# OPTIONS_GHC -Wno-deprecations #-}

module Lib.IncentiveJourney.Storage.CachedQueries.IncentiveJourneyStats
  ( findByPersonIdAndCjmAndPeriodKey,
    findByPersonIdAndCjmAndMilestonePeriod,
    upsertJourneyStats,
    clearCacheForStats,
    clearCacheByPersonCjmPeriod,
  )
where

import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Lib.IncentiveJourney.Domain.Types.CohortJourneyMapping as DCJM
import qualified Lib.IncentiveJourney.Domain.Types.Common as Common
import qualified Lib.IncentiveJourney.Domain.Types.IncentiveJourneyMilestone as DIJM
import qualified Lib.IncentiveJourney.Domain.Types.IncentiveJourneyStats as DIJS
import Lib.IncentiveJourney.Storage.Beam.BeamFlow (BeamFlow)
import qualified Lib.IncentiveJourney.Storage.Queries.IncentiveJourneyStatsExtra as Queries
import Lib.IncentiveJourney.Types.Actor (JourneyActor (..), actorCachePrefix)

findByPersonIdAndCjmAndPeriodKey ::
  (BeamFlow m r) =>
  JourneyActor ->
  Id Common.Person ->
  Id DCJM.CohortJourneyMapping ->
  Text ->
  m [DIJS.IncentiveJourneyStats]
findByPersonIdAndCjmAndPeriodKey actor personId cohortJourneyMappingId periodKey =
  Hedis.withCrossAppRedis (Hedis.safeGet (makeByPersonCjmPeriodKey actor personId cohortJourneyMappingId periodKey)) >>= \case
    Just rows -> pure rows
    Nothing -> do
      rows <- Queries.findStatsByPersonCjmAndPeriod personId cohortJourneyMappingId periodKey
      expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
      Hedis.withCrossAppRedis $ Hedis.setExp (makeByPersonCjmPeriodKey actor personId cohortJourneyMappingId periodKey) rows expTime
      pure rows

findByPersonIdAndCjmAndMilestonePeriod ::
  (BeamFlow m r) =>
  JourneyActor ->
  Id Common.Person ->
  Id DCJM.CohortJourneyMapping ->
  Id DIJM.IncentiveJourneyMilestone ->
  Text ->
  m (Maybe DIJS.IncentiveJourneyStats)
findByPersonIdAndCjmAndMilestonePeriod actor personId cohortJourneyMappingId milestoneId periodKey =
  Hedis.withCrossAppRedis (Hedis.safeGet (makeByPersonCjmMilestonePeriodKey actor personId cohortJourneyMappingId milestoneId periodKey)) >>= \case
    Just row -> pure row
    Nothing -> do
      mbRow <- Queries.findStatsByPersonCjmAndMilestonePeriod personId cohortJourneyMappingId milestoneId periodKey
      expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
      Hedis.withCrossAppRedis $ Hedis.setExp (makeByPersonCjmMilestonePeriodKey actor personId cohortJourneyMappingId milestoneId periodKey) mbRow expTime
      pure mbRow

upsertJourneyStats ::
  (BeamFlow m r) =>
  JourneyActor ->
  DIJS.IncentiveJourneyStats ->
  m DIJS.IncentiveJourneyStats
upsertJourneyStats actor stats = do
  updated <- Queries.upsertJourneyStats stats
  clearCacheForStats actor updated
  pure updated

clearCacheForStats :: (CacheFlow m r) => JourneyActor -> DIJS.IncentiveJourneyStats -> m ()
clearCacheForStats actor stats =
  Hedis.runInMultiCloudRedisWrite $
    Hedis.withCrossAppRedis $ do
      whenJust stats.cohortJourneyMappingId $ \cohortJourneyMappingId -> do
        void $ Hedis.del (makeByPersonCjmPeriodKey actor stats.personId cohortJourneyMappingId stats.periodKey)
        void $
          Hedis.del
            ( makeByPersonCjmMilestonePeriodKey
                actor
                stats.personId
                cohortJourneyMappingId
                stats.milestoneId
                stats.periodKey
            )

clearCacheByPersonCjmPeriod ::
  (CacheFlow m r) =>
  JourneyActor ->
  Id Common.Person ->
  Id DCJM.CohortJourneyMapping ->
  Text ->
  m ()
clearCacheByPersonCjmPeriod actor personId cohortJourneyMappingId periodKey =
  Hedis.runInMultiCloudRedisWrite $
    Hedis.withCrossAppRedis $
      void $
        Hedis.del (makeByPersonCjmPeriodKey actor personId cohortJourneyMappingId periodKey)

-- | Keep historical driver Redis segment "DriverId-" so live caches stay valid.
personIdKeySegment :: JourneyActor -> Text
personIdKeySegment = \case
  DriverActor -> "DriverId"
  RiderActor -> "PersonId"

makeByPersonCjmPeriodKey ::
  JourneyActor ->
  Id Common.Person ->
  Id DCJM.CohortJourneyMapping ->
  Text ->
  Text
makeByPersonCjmPeriodKey actor personId cohortJourneyMappingId periodKey =
  actorCachePrefix actor
    <> ":CachedQueries:IncentiveJourneyStats:"
    <> personIdKeySegment actor
    <> "-"
    <> personId.getId
    <> ":CohortJourneyMappingId-"
    <> cohortJourneyMappingId.getId
    <> ":PeriodKey-"
    <> periodKey

makeByPersonCjmMilestonePeriodKey ::
  JourneyActor ->
  Id Common.Person ->
  Id DCJM.CohortJourneyMapping ->
  Id DIJM.IncentiveJourneyMilestone ->
  Text ->
  Text
makeByPersonCjmMilestonePeriodKey actor personId cohortJourneyMappingId milestoneId periodKey =
  actorCachePrefix actor
    <> ":CachedQueries:IncentiveJourneyStats:"
    <> personIdKeySegment actor
    <> "-"
    <> personId.getId
    <> ":CohortJourneyMappingId-"
    <> cohortJourneyMappingId.getId
    <> ":MilestoneId-"
    <> milestoneId.getId
    <> ":PeriodKey-"
    <> periodKey
