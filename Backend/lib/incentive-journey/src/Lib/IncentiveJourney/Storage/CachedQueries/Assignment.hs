{-# OPTIONS_GHC -Wno-deprecations #-}

module Lib.IncentiveJourney.Storage.CachedQueries.Assignment
  ( findAssignmentsByUserId,
    clearCacheByPersonId,
    clearCacheByCohortMappingId,
  )
where

import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Lib.IncentiveJourney.Assignment as Assignment
import qualified Lib.IncentiveJourney.Domain.Types.CohortJourneyMapping as DCJM
import qualified Lib.IncentiveJourney.Domain.Types.Common as Common
import qualified Lib.IncentiveJourney.Domain.Types.UserCohortMapping as DUCM
import Lib.IncentiveJourney.Storage.Beam.BeamFlow (BeamFlow)
import qualified Lib.IncentiveJourney.Storage.Queries.CohortJourneyMappingExtra as QCJMExtra
import qualified Lib.IncentiveJourney.Storage.Queries.UserCohortMappingExtra as QUCMExtra
import Lib.IncentiveJourney.Types.Actor (JourneyActor (..), actorCachePrefix)

findAssignmentsByUserId ::
  (BeamFlow m r) =>
  JourneyActor ->
  Id Common.Person ->
  m [Assignment.JourneyAssignment]
findAssignmentsByUserId actor personId = do
  ucms <- findUserCohortMappingsByPersonId actor personId
  cjms <- mapM (findCohortJourneyMappingById actor . (.cohortMappingId)) ucms
  pure $
    [ Assignment.JourneyAssignment {userCohortMapping = ucm, cohortJourneyMapping = cjm}
      | (ucm, Just cjm) <- zip ucms cjms
    ]

findUserCohortMappingsByPersonId ::
  (BeamFlow m r) =>
  JourneyActor ->
  Id Common.Person ->
  m [DUCM.UserCohortMapping]
findUserCohortMappingsByPersonId actor personId =
  Hedis.withCrossAppRedis (Hedis.safeGet (makeByPersonIdKey actor personId)) >>= \case
    Just cached -> do
      now <- getCurrentTime
      let active = filter (\ucm -> ucm.validTill >= now) cached
      when (length active /= length cached) $ clearCacheByPersonId actor personId
      pure active
    Nothing -> do
      fetched <- QUCMExtra.findActiveByUserId personId
      expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
      Hedis.withCrossAppRedis $ Hedis.setExp (makeByPersonIdKey actor personId) fetched expTime
      pure fetched

findCohortJourneyMappingById ::
  (BeamFlow m r) =>
  JourneyActor ->
  Id DCJM.CohortJourneyMapping ->
  m (Maybe DCJM.CohortJourneyMapping)
findCohortJourneyMappingById actor cohortMappingId =
  Hedis.withCrossAppRedis (Hedis.safeGet (makeByCohortMappingIdKey actor cohortMappingId)) >>= \case
    Just cjm -> pure (Just cjm)
    Nothing -> do
      mbCjm <- QCJMExtra.findCohortJourneyMappingById cohortMappingId
      whenJust mbCjm $ \cjm -> do
        expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
        Hedis.withCrossAppRedis $ Hedis.setExp (makeByCohortMappingIdKey actor cohortMappingId) cjm expTime
      pure mbCjm

clearCacheByPersonId ::
  (CacheFlow m r) =>
  JourneyActor ->
  Id Common.Person ->
  m ()
clearCacheByPersonId actor personId =
  Hedis.runInMultiCloudRedisWrite $
    Hedis.withCrossAppRedis $
      void $
        Hedis.del (makeByPersonIdKey actor personId)

clearCacheByCohortMappingId ::
  (CacheFlow m r) =>
  JourneyActor ->
  Id DCJM.CohortJourneyMapping ->
  m ()
clearCacheByCohortMappingId actor cohortMappingId =
  Hedis.runInMultiCloudRedisWrite $
    Hedis.withCrossAppRedis $
      void $
        Hedis.del (makeByCohortMappingIdKey actor cohortMappingId)

personIdKeySegment :: JourneyActor -> Text
personIdKeySegment = \case
  DriverActor -> "DriverId"
  RiderActor -> "PersonId"

makeByPersonIdKey ::
  JourneyActor ->
  Id Common.Person ->
  Text
makeByPersonIdKey actor personId =
  actorCachePrefix actor
    <> ":CachedQueries:UserCohortMapping:"
    <> personIdKeySegment actor
    <> "-"
    <> personId.getId

makeByCohortMappingIdKey ::
  JourneyActor ->
  Id DCJM.CohortJourneyMapping ->
  Text
makeByCohortMappingIdKey actor cohortMappingId =
  actorCachePrefix actor
    <> ":CachedQueries:CohortJourneyMapping:Id-"
    <> cohortMappingId.getId
