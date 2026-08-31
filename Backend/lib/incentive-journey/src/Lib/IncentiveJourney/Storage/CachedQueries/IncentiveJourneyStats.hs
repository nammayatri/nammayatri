{-# OPTIONS_GHC -Wno-deprecations #-}

module Lib.IncentiveJourney.Storage.CachedQueries.IncentiveJourneyStats
  ( findByPersonIdAndJourneyIdAndPeriodKey,
    findByPersonIdAndJourneyIdAndMilestoneIdAndPeriodKey,
    upsertJourneyStats,
    clearCacheForStats,
    clearCacheByPersonJourneyPeriod,
  )
where

import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Lib.IncentiveJourney.Domain.Types.Common as Common
import qualified Lib.IncentiveJourney.Domain.Types.IncentiveJourney as DIJ
import qualified Lib.IncentiveJourney.Domain.Types.IncentiveJourneyMilestone as DIJM
import qualified Lib.IncentiveJourney.Domain.Types.IncentiveJourneyStats as DIJS
import Lib.IncentiveJourney.Storage.Beam.BeamFlow (BeamFlow)
import qualified Lib.IncentiveJourney.Storage.Queries.IncentiveJourneyStatsExtra as Queries
import Lib.IncentiveJourney.Types.Actor (JourneyActor (..), actorCachePrefix)

findByPersonIdAndJourneyIdAndPeriodKey ::
  (BeamFlow m r) =>
  JourneyActor ->
  Id Common.Person ->
  Id DIJ.IncentiveJourney ->
  Text ->
  m [DIJS.IncentiveJourneyStats]
findByPersonIdAndJourneyIdAndPeriodKey actor personId journeyId periodKey =
  Hedis.withCrossAppRedis (Hedis.safeGet (makeByPersonJourneyPeriodKey actor personId journeyId periodKey)) >>= \case
    Just rows -> pure rows
    Nothing -> do
      rows <- Queries.findStatsByPersonJourneyAndPeriod personId journeyId periodKey
      expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
      Hedis.withCrossAppRedis $ Hedis.setExp (makeByPersonJourneyPeriodKey actor personId journeyId periodKey) rows expTime
      pure rows

findByPersonIdAndJourneyIdAndMilestoneIdAndPeriodKey ::
  (BeamFlow m r) =>
  JourneyActor ->
  Id Common.Person ->
  Id DIJ.IncentiveJourney ->
  Id DIJM.IncentiveJourneyMilestone ->
  Text ->
  m (Maybe DIJS.IncentiveJourneyStats)
findByPersonIdAndJourneyIdAndMilestoneIdAndPeriodKey actor personId journeyId milestoneId periodKey =
  Hedis.withCrossAppRedis (Hedis.safeGet (makeByPersonJourneyMilestonePeriodKey actor personId journeyId milestoneId periodKey)) >>= \case
    Just row -> pure row
    Nothing -> do
      mbRow <- Queries.findStatsByPersonAndMilestonePeriod personId journeyId milestoneId periodKey
      expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
      Hedis.withCrossAppRedis $ Hedis.setExp (makeByPersonJourneyMilestonePeriodKey actor personId journeyId milestoneId periodKey) mbRow expTime
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
      void $ Hedis.del (makeByPersonJourneyPeriodKey actor stats.personId stats.journeyId stats.periodKey)
      void $
        Hedis.del
          ( makeByPersonJourneyMilestonePeriodKey
              actor
              stats.personId
              stats.journeyId
              stats.milestoneId
              stats.periodKey
          )

clearCacheByPersonJourneyPeriod ::
  (CacheFlow m r) =>
  JourneyActor ->
  Id Common.Person ->
  Id DIJ.IncentiveJourney ->
  Text ->
  m ()
clearCacheByPersonJourneyPeriod actor personId journeyId periodKey =
  Hedis.runInMultiCloudRedisWrite $
    Hedis.withCrossAppRedis $
      void $
        Hedis.del (makeByPersonJourneyPeriodKey actor personId journeyId periodKey)

-- | Keep historical driver Redis segment "DriverId-" so live caches stay valid.
personIdKeySegment :: JourneyActor -> Text
personIdKeySegment = \case
  DriverActor -> "DriverId"
  RiderActor -> "PersonId"

makeByPersonJourneyPeriodKey ::
  JourneyActor ->
  Id Common.Person ->
  Id DIJ.IncentiveJourney ->
  Text ->
  Text
makeByPersonJourneyPeriodKey actor personId journeyId periodKey =
  actorCachePrefix actor
    <> ":CachedQueries:IncentiveJourneyStats:"
    <> personIdKeySegment actor
    <> "-"
    <> personId.getId
    <> ":JourneyId-"
    <> journeyId.getId
    <> ":PeriodKey-"
    <> periodKey

makeByPersonJourneyMilestonePeriodKey ::
  JourneyActor ->
  Id Common.Person ->
  Id DIJ.IncentiveJourney ->
  Id DIJM.IncentiveJourneyMilestone ->
  Text ->
  Text
makeByPersonJourneyMilestonePeriodKey actor personId journeyId milestoneId periodKey =
  actorCachePrefix actor
    <> ":CachedQueries:IncentiveJourneyStats:"
    <> personIdKeySegment actor
    <> "-"
    <> personId.getId
    <> ":JourneyId-"
    <> journeyId.getId
    <> ":MilestoneId-"
    <> milestoneId.getId
    <> ":PeriodKey-"
    <> periodKey
