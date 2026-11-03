{-# OPTIONS_GHC -Wno-deprecations #-}

module Lib.IncentiveJourney.Storage.CachedQueries.Assignment
  ( findAssignmentsByUserId,
    findMembershipsByUserId,
    findAssignmentsForDriver,
    findMembershipsForDriver,
    clearCacheByPersonId,
    clearCacheByCohortMappingId,
  )
where

import Data.List (nubBy)
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Lib.IncentiveJourney.Assignment as Assignment
import qualified Lib.IncentiveJourney.Domain.Types.CohortJourneyMapping as DCJM
import qualified Lib.IncentiveJourney.Domain.Types.Common as Common
import qualified Lib.IncentiveJourney.Domain.Types.UserCohortMapping as DUCM
import Lib.IncentiveJourney.Storage.Beam.BeamFlow (BeamFlow)
import qualified Lib.IncentiveJourney.Storage.CachedQueries.AutoApplyCohortMapping as CQAuto
import qualified Lib.IncentiveJourney.Storage.Queries.CohortJourneyMappingExtra as QCJMExtra
import qualified Lib.IncentiveJourney.Storage.Queries.UserCohortMappingExtra as QUCMExtra
import Lib.IncentiveJourney.Types.Actor (JourneyActor (..), actorCachePrefix)

-- | Eval / driver UI: OptIn rows only when optedInAt is set.
findAssignmentsByUserId ::
  (BeamFlow m r) =>
  JourneyActor ->
  Id Common.Person ->
  m [Assignment.JourneyAssignment]
findAssignmentsByUserId actor personId = findUcmAssignments actor personId True

-- | Dashboard membership listing: include pending OptIn invites (optedInAt null).
findMembershipsByUserId ::
  (BeamFlow m r) =>
  JourneyActor ->
  Id Common.Person ->
  m [Assignment.JourneyAssignment]
findMembershipsByUserId actor personId = findUcmAssignments actor personId False

findUcmAssignments ::
  (BeamFlow m r) =>
  JourneyActor ->
  Id Common.Person ->
  Bool -> -- requireOptInForOptInCampaigns
  m [Assignment.JourneyAssignment]
findUcmAssignments actor personId requireOptIn = do
  ucms <- findUserCohortMappingsByPersonId actor personId
  cjms <- mapM (findCohortJourneyMappingById actor . (.cohortMappingId)) ucms
  let pairs = [(ucm, cjm) | (ucm, Just cjm) <- zip ucms cjms]
      selectedPairs = if requireOptIn then filterOptInActive pairs else pairs
  pure $
    [ Assignment.JourneyAssignment
        { userCohortMapping = Just ucm,
          cohortJourneyMapping = cjm,
          assignedAt = ucm.createdAt
        }
      | (ucm, cjm) <- selectedPairs
    ]

-- | UCM assignments plus enabled city auto-apply rows.
-- A null vehicle category on the auto row covers every category in the city.
-- allowIfNoMapping rows apply only when the driver has no active user_cohort_mapping.
-- The same cohort journey mapping is kept once, so a driver is not rewarded twice.
-- row.enabled turns off only this auto-apply. cjm.enabled still has to be true.
-- OptIn campaigns are never auto-applied; they require an opted-in UCM row.
findAssignmentsForDriver ::
  (BeamFlow m r) =>
  JourneyActor ->
  Id Common.Person ->
  Id Common.Merchant ->
  Id Common.MerchantOperatingCity ->
  Maybe Text ->
  m [Assignment.JourneyAssignment]
findAssignmentsForDriver actor personId merchantId merchantOperatingCityId mbVehicleCategory =
  findDriverAssignments actor personId merchantId merchantOperatingCityId mbVehicleCategory True

-- | Dashboard listing for drivers: same as findAssignmentsForDriver but keeps pending OptIn invites.
findMembershipsForDriver ::
  (BeamFlow m r) =>
  JourneyActor ->
  Id Common.Person ->
  Id Common.Merchant ->
  Id Common.MerchantOperatingCity ->
  Maybe Text ->
  m [Assignment.JourneyAssignment]
findMembershipsForDriver actor personId merchantId merchantOperatingCityId mbVehicleCategory =
  findDriverAssignments actor personId merchantId merchantOperatingCityId mbVehicleCategory False

findDriverAssignments ::
  (BeamFlow m r) =>
  JourneyActor ->
  Id Common.Person ->
  Id Common.Merchant ->
  Id Common.MerchantOperatingCity ->
  Maybe Text ->
  Bool -> -- requireOptInForOptInCampaigns
  m [Assignment.JourneyAssignment]
findDriverAssignments actor personId merchantId merchantOperatingCityId mbVehicleCategory requireOptIn = do
  ucms <- findUserCohortMappingsByPersonId actor personId
  ucmCjms <- mapM (findCohortJourneyMappingById actor . (.cohortMappingId)) ucms
  let ucmPairs = [(ucm, cjm) | (ucm, Just cjm) <- zip ucms ucmCjms]
      activeUcmPairs = filterOptInActive ucmPairs
      -- Listing may include pending OptIn invites; allowIfNoMapping still keys off opted-in / Assigned only.
      selectedUcmPairs = if requireOptIn then activeUcmPairs else ucmPairs
      ucmAssignments =
        [ Assignment.JourneyAssignment
            { userCohortMapping = Just ucm,
              cohortJourneyMapping = cjm,
              assignedAt = ucm.createdAt
            }
          | (ucm, cjm) <- selectedUcmPairs
        ]
      hasActiveUcm = not (null activeUcmPairs)
  rows <- CQAuto.findApplicableByMerchantAndCity actor merchantId merchantOperatingCityId mbVehicleCategory
  autoAssignments <-
    catMaybes <$> forM rows \row -> do
      mbCjm <- findCohortJourneyMappingById actor row.cohortJourneyMappingId
      case mbCjm of
        Just cjm
          | row.enabled
              && cjm.enabled
              && cjm.merchantId == merchantId
              && cjm.merchantOperatingCityId == merchantOperatingCityId
              && (not row.allowIfNoMapping || not hasActiveUcm) -> do
            let isAssignedCampaign = fromMaybe DCJM.Assigned cjm.campaignUserLinkType /= DCJM.OptIn
            pure $
              if isAssignedCampaign
                then
                  Just
                    Assignment.JourneyAssignment
                      { userCohortMapping = Nothing,
                        cohortJourneyMapping = cjm,
                        assignedAt = row.createdAt
                      }
                else Nothing
        _ -> pure Nothing
  pure $ nubBy sameCohortJourneyMapping (ucmAssignments <> autoAssignments)

-- | OptIn campaigns require optedInAt; Assigned (or missing / null campaignUserLinkType) stay active.
filterOptInActive ::
  [(DUCM.UserCohortMapping, DCJM.CohortJourneyMapping)] ->
  [(DUCM.UserCohortMapping, DCJM.CohortJourneyMapping)]
filterOptInActive =
  filter
    ( \(ucm, cjm) ->
        case fromMaybe DCJM.Assigned cjm.campaignUserLinkType of
          DCJM.OptIn -> isJust ucm.optedInAt
          DCJM.Assigned -> True
    )

sameCohortJourneyMapping :: Assignment.JourneyAssignment -> Assignment.JourneyAssignment -> Bool
sameCohortJourneyMapping assignment other = assignment.cohortJourneyMapping.id == other.cohortJourneyMapping.id

findUserCohortMappingsByPersonId ::
  (BeamFlow m r) =>
  JourneyActor ->
  Id Common.Person ->
  m [DUCM.UserCohortMapping]
findUserCohortMappingsByPersonId actor personId =
  Hedis.withCrossAppRedis (Hedis.safeGet (makeByPersonIdKey actor personId)) >>= \case
    Just cached -> do
      now <- getCurrentTime
      let active = filter (\ucm -> ucm.enabled && ucm.validTill >= now) cached
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
