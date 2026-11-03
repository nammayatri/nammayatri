{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
-}

-- | Shared dashboard logic for rider and provider.
-- Platform modules only adapt request fields (personId vs driverId) and response records.
module Lib.IncentiveJourney.Domain.Action.Dashboard.Core
  ( resolveMerchant,
    listJourneys,
    createJourney,
    listMilestones,
    statsHistory,
    createCohort,
    listCohortJourneyRows,
    AssignmentView (..),
    AssignmentOptInStatus (..),
    listAssignmentViews,
    assignUser,
    unassignUser,
    optInUser,
    assertControlFeaturesSlotAvailable,
    assertNoExistingCohortJourneyMapping,
    rollCohortJourneyMapping,
  )
where

import Control.Monad.Extra (mapMaybeM)
import qualified Data.HashMap.Strict as HM
import Data.List (nub, sortOn)
import Data.Maybe (listToMaybe)
import qualified Data.Text as T
import Data.Time (Day)
import EulerHS.Prelude hiding (id, sortOn)
import qualified Kernel.Types.Beckn.Context
import Kernel.Types.Error (GenericError (InvalidRequest))
import qualified Kernel.Types.Id as ID
import Kernel.Utils.Common
import qualified Lib.IncentiveJourney as IJ
import qualified Lib.IncentiveJourney.CampaignSync as CampaignSync
import qualified Lib.IncentiveJourney.Common as IJC
import Lib.IncentiveJourney.Domain.Action.Dashboard.ServiceHandle (ServiceHandle (..))
import qualified Lib.IncentiveJourney.Domain.Types.AutoApplyCohortMapping as DAuto
import qualified Lib.IncentiveJourney.Domain.Types.CohortDetails as DCD
import qualified Lib.IncentiveJourney.Domain.Types.CohortJourneyMapping as DCJM
import qualified Lib.IncentiveJourney.Domain.Types.Common as DIJC
import qualified Lib.IncentiveJourney.Domain.Types.IncentiveJourney as DIJ
import qualified Lib.IncentiveJourney.Domain.Types.IncentiveJourneyMilestone as DIJM
import qualified Lib.IncentiveJourney.Domain.Types.IncentiveJourneyStats as DIJS
import qualified Lib.IncentiveJourney.Domain.Types.UserCohortMapping as DUCM
import qualified Lib.IncentiveJourney.MgCampaign as Mg
import Lib.IncentiveJourney.Storage.Beam.BeamFlow (BeamFlow)
import qualified Lib.IncentiveJourney.Storage.Queries.AutoApplyCohortMapping as QAuto
import qualified Lib.IncentiveJourney.Storage.Queries.AutoApplyCohortMappingExtra as QAutoExtra
import qualified Lib.IncentiveJourney.Storage.Queries.CohortDetailsExtra as QCDExtra
import qualified Lib.IncentiveJourney.Storage.Queries.CohortJourneyMappingExtra as QCJMExtra
import qualified Lib.IncentiveJourney.Storage.Queries.IncentiveJourney as QJourney
import qualified Lib.IncentiveJourney.Storage.Queries.IncentiveJourneyStatsExtra as QStats
import qualified Lib.IncentiveJourney.Storage.Queries.UserCohortMappingExtra as QUCMExtra

resolveMerchant ::
  BeamFlow m r =>
  ServiceHandle m ->
  ID.ShortId DIJC.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  m (IJC.MerchantInfo, ID.Id DIJC.MerchantOperatingCity)
resolveMerchant handle merchantShortId opCity = do
  merchant <- handle.findMerchantByShortId merchantShortId
  merchantOpCityId <- handle.getMerchantOpCityId merchant opCity
  pure (merchant, merchantOpCityId)

listJourneys ::
  BeamFlow m r =>
  ServiceHandle m ->
  ID.ShortId DIJC.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  Maybe Int ->
  Maybe Int ->
  Maybe (ID.Id DIJ.IncentiveJourney) ->
  Maybe DIJ.IncentiveJourneyType ->
  m [DIJ.IncentiveJourney]
listJourneys handle merchantShortId opCity mbLimit mbOffset mbJourneyId mbJourneyType = do
  void $ resolveMerchant handle merchantShortId opCity
  let limitVal = fromMaybe 20 mbLimit
      offsetVal = fromMaybe 0 mbOffset
  handle.getJourneys (Just limitVal) (Just offsetVal) mbJourneyId mbJourneyType

createJourney ::
  BeamFlow m r =>
  ServiceHandle m ->
  ID.ShortId DIJC.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  Text ->
  Maybe Text ->
  DIJ.IncentiveJourneyType ->
  m (ID.Id DIJ.IncentiveJourney)
createJourney handle merchantShortId opCity name description journeyType = do
  void $ resolveMerchant handle merchantShortId opCity
  now <- getCurrentTime
  journeyId <- generateGUID
  let journey =
        DIJ.IncentiveJourney
          { id = journeyId,
            name = name,
            description = description,
            journeyType = journeyType,
            createdAt = now,
            updatedAt = now
          }
  QJourney.create journey
  handle.clearJourneyCache journey
  pure journeyId

listMilestones ::
  BeamFlow m r =>
  ServiceHandle m ->
  ID.ShortId DIJC.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  ID.Id DIJ.IncentiveJourney ->
  Maybe Int ->
  Maybe Int ->
  m [DIJM.IncentiveJourneyMilestone]
listMilestones handle merchantShortId opCity journeyId mbLimit mbOffset = do
  void $ resolveMerchant handle merchantShortId opCity
  void $ QJourney.findById journeyId >>= fromMaybeM (InvalidRequest "Incentive journey not found")
  milestones <- sortOn (.order) <$> handle.getMilestonesByJourneyId journeyId
  let limitVal = fromMaybe 20 mbLimit
      offsetVal = fromMaybe 0 mbOffset
  pure $ take limitVal . drop offsetVal $ milestones

statsHistory ::
  BeamFlow m r =>
  ServiceHandle m ->
  ID.ShortId DIJC.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  ID.Id DIJC.Person ->
  Maybe (ID.Id DIJ.IncentiveJourney) ->
  Maybe Int ->
  Maybe Int ->
  Day ->
  Day ->
  m [DIJS.IncentiveJourneyStats]
statsHistory handle merchantShortId opCity personId mbJourneyId mbLimit mbOffset fromDate toDate = do
  (_merchant, merchantOpCityId) <- resolveMerchant handle merchantShortId opCity
  timeDiffFromUtc <- handle.getTimeDiffFromUtc merchantOpCityId
  when (toDate < fromDate) $ throwError (InvalidRequest "toDate must be >= fromDate")
  let (dayStart, _) = QStats.mkLocalDayUtcBounds fromDate timeDiffFromUtc
      (_, dayEndExclusive) = QStats.mkLocalDayUtcBounds toDate timeDiffFromUtc
  rows <- handle.findStatsHistoryByPersonId personId dayStart dayEndExclusive mbLimit mbOffset
  pure $ case mbJourneyId of
    Nothing -> rows
    Just journeyId -> filter (\stats -> stats.journeyId == journeyId) rows

createCohort ::
  BeamFlow m r =>
  Text ->
  Text ->
  Maybe Text ->
  Maybe Value ->
  m DCD.CohortDetails
createCohort name category description cohortRule = do
  when (T.null name) $ throwError (InvalidRequest "cohort name must be non-empty")
  when (T.null category) $ throwError (InvalidRequest "cohort category must be non-empty")
  mbExisting <- QCDExtra.findByNameAndCategory name category
  when (isJust mbExisting) $
    throwError (InvalidRequest "cohort with the same name and category already exists")
  QCDExtra.createCohortDetails name category description cohortRule

listCohortJourneyRows ::
  BeamFlow m r =>
  ServiceHandle m ->
  ID.ShortId DIJC.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  Maybe Int ->
  Maybe Int ->
  Maybe (ID.Id DCD.CohortDetails) ->
  Maybe Text ->
  Maybe Text ->
  Maybe DIJ.IncentiveJourneyType ->
  Maybe Bool ->
  m [(DCJM.CohortJourneyMapping, DCD.CohortDetails, DIJ.IncentiveJourney)]
listCohortJourneyRows handle merchantShortId opCity mbLimit mbOffset mbCohortId mbCohortName mbCohortCategory mbJourneyType mbIsActive = do
  (_merchant, merchantOpCityId) <- resolveMerchant handle merchantShortId opCity
  let limitVal = fromMaybe 20 mbLimit
      offsetVal = fromMaybe 0 mbOffset
  mbCohortIds <- resolveCohortIdFilter mbCohortId mbCohortName mbCohortCategory
  mbJourneyIds <- resolveJourneyIdFilter handle mbJourneyType
  mappings <-
    QCJMExtra.findByMerchantOperatingCityIdWithOptionalFilters
      (Just limitVal)
      (Just offsetVal)
      (ID.cast merchantOpCityId)
      mbCohortIds
      mbJourneyIds
      mbIsActive
  cohorts <- QCDExtra.findByIds (map (.cohortId) mappings)
  journeys <- QJourney.findByIds (map (.journeyId) mappings)
  let cohortById = HM.fromList $ map (\cohort -> (cohort.id, cohort)) cohorts
      journeyById = HM.fromList $ map (\journey -> (journey.id, journey)) journeys
  pure $
    mapMaybe
      ( \cjm -> do
          cohort <- HM.lookup cjm.cohortId cohortById
          journey <- HM.lookup cjm.journeyId journeyById
          pure (cjm, cohort, journey)
      )
      mappings

resolveCohortIdFilter ::
  BeamFlow m r =>
  Maybe (ID.Id DCD.CohortDetails) ->
  Maybe Text ->
  Maybe Text ->
  m (Maybe [ID.Id DCD.CohortDetails])
resolveCohortIdFilter mbCohortId mbCohortName mbCohortCategory =
  case mbCohortId of
    Just cohortId -> pure $ Just [cohortId]
    Nothing ->
      let mbName = case T.strip <$> mbCohortName of
            Just name | not (T.null name) -> Just name
            _ -> Nothing
          mbCategory = case T.strip <$> mbCohortCategory of
            Just cat | not (T.null cat) -> Just cat
            _ -> Nothing
       in case (mbName, mbCategory) of
            (Nothing, Nothing) -> pure Nothing
            _ -> do
              cohorts <- QCDExtra.findMatchingByOptionalNameAndCategory mbName mbCategory
              pure $ Just (map (.id) cohorts)

resolveJourneyIdFilter ::
  BeamFlow m r =>
  ServiceHandle m ->
  Maybe DIJ.IncentiveJourneyType ->
  m (Maybe [ID.Id DIJ.IncentiveJourney])
resolveJourneyIdFilter handle mbJourneyType =
  case mbJourneyType of
    Nothing -> pure Nothing
    Just journeyType -> do
      journeys <- handle.getJourneys Nothing Nothing Nothing (Just journeyType)
      pure $ Just (map (.id) journeys)

-- | Dashboard-facing opt-in membership. Distinct from CampaignUserLinkType so
-- Pending vs OptedIn is visible (userEnabled alone cannot tell them apart).
-- | NotRequired = Assigned campaign (no opt-in). Pending / OptedIn = OptIn invite state.
data AssignmentOptInStatus = NotRequired | Pending | OptedIn
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

data AssignmentView = AssignmentView
  { cohortId :: ID.Id DCD.CohortDetails,
    cohortName :: Text,
    cohortCategory :: Text,
    cohortJourneyMappingId :: ID.Id DCJM.CohortJourneyMapping,
    journeyId :: ID.Id DIJ.IncentiveJourney,
    journeyName :: Text,
    journeyType :: DIJ.IncentiveJourneyType,
    controlFeatures :: Maybe Value,
    enabled :: Bool,
    userEnabled :: Bool,
    campaignUserLinkType :: DCJM.CampaignUserLinkType,
    optInStatus :: AssignmentOptInStatus,
    optedInAt :: Maybe UTCTime,
    startDate :: UTCTime,
    endDate :: UTCTime,
    streakRange :: Int,
    assignedAt :: UTCTime
  }

mkAssignmentOptInStatus :: DCJM.CohortJourneyMapping -> Maybe DUCM.UserCohortMapping -> AssignmentOptInStatus
mkAssignmentOptInStatus cjm mbUcm =
  case fromMaybe DCJM.Assigned cjm.campaignUserLinkType of
    DCJM.Assigned -> NotRequired
    DCJM.OptIn ->
      if isJust (mbUcm >>= (.optedInAt)) then OptedIn else Pending

listAssignmentViews ::
  BeamFlow m r =>
  ServiceHandle m ->
  ID.ShortId DIJC.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  ID.Id DIJC.Person ->
  m [AssignmentView]
listAssignmentViews handle merchantShortId opCity personId = do
  (merchant, merchantOpCityId) <- resolveMerchant handle merchantShortId opCity
  -- Pending OptIn invites must appear here; eval / driver UI keep using
  -- findAssignmentsByUserId (opt-in filtered).
  assignments <- handle.findDashboardAssignmentsByUserId personId
  items <- mapMaybeM (toAssignmentView (ID.cast merchant.id) (ID.cast merchantOpCityId)) assignments
  pure $ sortOn (Down . (.assignedAt)) items

toAssignmentView ::
  BeamFlow m r =>
  ID.Id DIJC.Merchant ->
  ID.Id DIJC.MerchantOperatingCity ->
  IJ.JourneyAssignment ->
  m (Maybe AssignmentView)
toAssignmentView merchantId merchantOpCityId assignment = do
  let cjm = assignment.cohortJourneyMapping
  mbJourney <- QJourney.findById cjm.journeyId
  case mbJourney of
    Nothing -> pure Nothing
    Just journey
      | cjm.merchantId == merchantId
          && cjm.merchantOperatingCityId == merchantOpCityId -> do
        cohort <-
          QCDExtra.findCohortDetailsById cjm.cohortId
            >>= fromMaybeM (InvalidRequest "Cohort not found")
        let journeyType = journey.journeyType
            linkType = fromMaybe DCJM.Assigned cjm.campaignUserLinkType
            mbUcm = assignment.userCohortMapping
        pure $
          Just
            AssignmentView
              { cohortId = cohort.id,
                cohortName = cohort.name,
                cohortCategory = cohort.category,
                cohortJourneyMappingId = cjm.id,
                journeyId = journey.id,
                journeyName = journey.name,
                journeyType = journeyType,
                controlFeatures = cjm.controlFeatures,
                enabled = cjm.enabled,
                userEnabled = maybe True (.enabled) mbUcm,
                campaignUserLinkType = linkType,
                optInStatus = mkAssignmentOptInStatus cjm mbUcm,
                optedInAt = mbUcm >>= (.optedInAt),
                startDate = cjm.startDate,
                endDate = IJ.displayStreakEndDate cjm.startDate cjm.streakRange journeyType,
                streakRange = cjm.streakRange,
                assignedAt = assignment.assignedAt
              }
    _ -> pure Nothing

assignUser ::
  BeamFlow m r =>
  ServiceHandle m ->
  ID.ShortId DIJC.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  ID.Id DIJC.Person ->
  ID.Id DCJM.CohortJourneyMapping ->
  Bool ->
  UTCTime ->
  Text ->
  Text ->
  m ()
assignUser handle merchantShortId opCity personId cjmId enabled validTill notFoundMessage cityMessage = do
  (merchant, merchantOpCityId) <- resolveMerchant handle merchantShortId opCity
  person <- handle.findPersonById personId >>= fromMaybeM (InvalidRequest notFoundMessage)
  unless (person.merchantId == merchant.id && person.merchantOperatingCityId == merchantOpCityId) $
    throwError (InvalidRequest cityMessage)
  cjm <- QCJMExtra.findCohortJourneyMappingById cjmId >>= fromMaybeM (InvalidRequest "Cohort journey mapping not found")
  unless (cjm.merchantId == ID.cast merchant.id && cjm.merchantOperatingCityId == ID.cast merchantOpCityId) $
    throwError (InvalidRequest "Cohort journey mapping does not belong to this merchant/city")
  journey <- QJourney.findById cjm.journeyId >>= fromMaybeM (InvalidRequest "Incentive journey not found")
  timeDiffFromUtc <- handle.getTimeDiffFromUtc merchantOpCityId
  localTime <- getLocalCurrentTime timeDiffFromUtc
  let endDate = IJ.computeStreakEndDate cjm.startDate cjm.streakRange journey.journeyType
      linkType = fromMaybe DCJM.Assigned cjm.campaignUserLinkType
  when (localTime >= endDate) $
    throwError (InvalidRequest "Cohort journey mapping has expired")
  -- OptIn only: same bound as optInUser. Assigned keeps long validTill (eval still
  -- streak-gated; list-ensure tags use min(ucmTill, streakEnd)).
  when (linkType == DCJM.OptIn && validTill > endDate) $
    throwError (InvalidRequest "validTill must be on or before the cohort journey end")
  assertControlFeaturesSlotAvailable handle personId cjm
  ucm <- QUCMExtra.upsertUserCohortMapping personId cjmId enabled validTill
  handle.clearAssignmentCacheByPersonId personId
  -- Disabled UCM is excluded from assignment queries but can still leave an IJCampaign
  -- tag (optedInAt is preserved on reassign). Clear the tag so perks stop; re-enable
  -- with enabled=True syncs again from retained optedInAt.
  if not ucm.enabled
    then whenJust handle.clearCampaignMembership $ \clear -> clear personId cjmId
    else do
      -- Preserve existing OptIn state on reassign; first invite stays optedOut (optedInAt unset).
      -- Cap tag expiry at campaign end so a stale UCM.validTill cannot extend perks.
      let snapshotOptedIn = isJust ucm.optedInAt
          snapshotValidTill =
            if snapshotOptedIn
              then Just (min ucm.validTill endDate)
              else Nothing
      syncMgMembershipSnapshot handle personId journey cjm snapshotOptedIn snapshotValidTill

-- | Opt-in for OptIn cohorts. Requires an existing invite UCM row.
-- validTill should be the period end (caller computes from terms / daily|weekly).
optInUser ::
  BeamFlow m r =>
  ServiceHandle m ->
  ID.ShortId DIJC.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  ID.Id DIJC.Person ->
  ID.Id DCJM.CohortJourneyMapping ->
  DUCM.OptInPeriod ->
  UTCTime ->
  Text ->
  Text ->
  Text ->
  m ()
optInUser handle merchantShortId opCity personId cjmId optInPeriod validTill optedInBy notFoundMessage cityMessage = do
  (merchant, merchantOpCityId) <- resolveMerchant handle merchantShortId opCity
  person <- handle.findPersonById personId >>= fromMaybeM (InvalidRequest notFoundMessage)
  unless (person.merchantId == merchant.id && person.merchantOperatingCityId == merchantOpCityId) $
    throwError (InvalidRequest cityMessage)
  cjm <- QCJMExtra.findCohortJourneyMappingById cjmId >>= fromMaybeM (InvalidRequest "Cohort journey mapping not found")
  unless (cjm.merchantId == ID.cast merchant.id && cjm.merchantOperatingCityId == ID.cast merchantOpCityId) $
    throwError (InvalidRequest "Cohort journey mapping does not belong to this merchant/city")
  unless (fromMaybe DCJM.Assigned cjm.campaignUserLinkType == DCJM.OptIn) $
    throwError (InvalidRequest "Opt-in is only supported for OptIn campaigns")
  unless cjm.enabled $
    throwError (InvalidRequest "Cohort journey mapping is disabled")
  journey <- QJourney.findById cjm.journeyId >>= fromMaybeM (InvalidRequest "Incentive journey not found")
  timeDiffFromUtc <- handle.getTimeDiffFromUtc merchantOpCityId
  localTime <- getLocalCurrentTime timeDiffFromUtc
  let endDate = IJ.computeStreakEndDate cjm.startDate cjm.streakRange journey.journeyType
  when (localTime >= endDate) $
    throwError (InvalidRequest "Cohort journey mapping has expired")
  when (validTill > endDate) $
    throwError (InvalidRequest "validTill must be on or before the cohort journey end")
  when (validTill <= localTime) $
    throwError (InvalidRequest "validTill must be in the future")
  assertControlFeaturesSlotAvailable handle personId cjm
  void $ QUCMExtra.markUserCohortMappingOptedIn personId cjmId optInPeriod validTill optedInBy
  handle.clearAssignmentCacheByPersonId personId
  syncMgMembershipSnapshot handle personId journey cjm True (Just validTill)

unassignUser ::
  BeamFlow m r =>
  ServiceHandle m ->
  ID.ShortId DIJC.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  ID.Id DIJC.Person ->
  ID.Id DCJM.CohortJourneyMapping ->
  Text ->
  Text ->
  m ()
unassignUser handle merchantShortId opCity personId cjmId notFoundMessage cityMessage = do
  (merchant, merchantOpCityId) <- resolveMerchant handle merchantShortId opCity
  person <- handle.findPersonById personId >>= fromMaybeM (InvalidRequest notFoundMessage)
  unless (person.merchantId == merchant.id && person.merchantOperatingCityId == merchantOpCityId) $
    throwError (InvalidRequest cityMessage)
  QUCMExtra.disableUserCohortMapping personId cjmId
  handle.clearAssignmentCacheByPersonId personId
  whenJust handle.clearCampaignMembership $ \clear -> clear personId cjmId

-- | Reject second controlFeatures campaign (Assigned occupant or opted-in OptIn).
-- Not-started and ended campaigns are ignored even when UCM.validTill is still open.
assertControlFeaturesSlotAvailable ::
  BeamFlow m r =>
  ServiceHandle m ->
  ID.Id DIJC.Person ->
  DCJM.CohortJourneyMapping ->
  m ()
assertControlFeaturesSlotAvailable handle personId cjm =
  when (Mg.cjmHasControlFeatures cjm) $ do
    timeDiffFromUtc <- handle.getTimeDiffFromUtc (ID.cast cjm.merchantOperatingCityId)
    localTime <- getLocalCurrentTime timeDiffFromUtc
    assignments <- handle.findAssignmentsByUserId personId
    rows <- forM assignments $ \a -> do
      mbJourney <- handle.getOneJourney a.cohortJourneyMapping.journeyId
      let journeyType = maybe DIJ.Daily (.journeyType) mbJourney
      pure (a.cohortJourneyMapping, a.userCohortMapping, journeyType)
    whenJust (CampaignSync.findControlFeaturesSlotConflict localTime cjm.id.getId rows) $ \otherCjmId ->
      throwError $
        InvalidRequest $
          "Driver already has an active campaign with control features: " <> otherCjmId

-- | Write / clear IJCampaign person tag (and AA reconcile via driver-app callback).
-- OptIn: tag only after optedIn. Assigned: never on assign/bulk — list fork ensures tags.
syncMgMembershipSnapshot ::
  BeamFlow m r =>
  ServiceHandle m ->
  ID.Id DIJC.Person ->
  DIJ.IncentiveJourney ->
  DCJM.CohortJourneyMapping ->
  Bool ->
  Maybe UTCTime ->
  m ()
syncMgMembershipSnapshot handle personId journey cjm optedIn mbValidTill =
  whenJust handle.syncCampaignMembership $ \sync -> do
    milestones <- sortOn (.order) <$> handle.getMilestonesByJourneyId journey.id
    now <- getCurrentTime
    case CampaignSync.mkHotPathSync now journey cjm milestones optedIn mbValidTill of
      Nothing ->
        -- Clear this CJM from the tag when invite-only OptIn or journey no longer needs tracking.
        when (fromMaybe DCJM.Assigned cjm.campaignUserLinkType == DCJM.OptIn && not optedIn) $
          whenJust handle.clearCampaignMembership $ \clear -> clear personId cjm.id
      Just payload
        | CampaignSync.shouldDeferAssignedTag cjm ->
          pure ()
        | otherwise -> sync personId (Just payload)

-- | First create only. Later windows for the same city/cohort/journey must use roll.
assertNoExistingCohortJourneyMapping ::
  BeamFlow m r =>
  ID.Id DIJC.MerchantOperatingCity ->
  ID.Id DCD.CohortDetails ->
  ID.Id DIJ.IncentiveJourney ->
  m ()
assertNoExistingCohortJourneyMapping merchantOpCityId cohortId journeyId = do
  existing <-
    QCJMExtra.findAllByMerchantOperatingCityIdAndCohortIdAndJourneyId
      merchantOpCityId
      cohortId
      journeyId
  unless (null existing) $
    throwError
      ( InvalidRequest
          "Cohort journey mapping already exists for this city/cohort/journey; use /cohortJourney/roll to start the next campaign window"
      )

-- | Insert a new CJM row for the next window (same cohort/journey/streak/rewards).
-- Requires previousId to be the latest city/cohort/journey mapping (by startDate) so
-- auto-apply clone copies the live tip’s enabled rows — rolling an ancestor would copy
-- disabled rows and leave the new window never auto-applying. Also requires
-- newStartDate >= previous exclusive endDate and no overlap with any sibling window.
-- Clones auto-apply onto the new mapping and disables the previous auto-apply rows.
rollCohortJourneyMapping ::
  BeamFlow m r =>
  ServiceHandle m ->
  ID.Id DIJC.Merchant ->
  ID.Id DIJC.MerchantOperatingCity ->
  ID.Id DCJM.CohortJourneyMapping ->
  UTCTime ->
  m DCJM.CohortJourneyMapping
rollCohortJourneyMapping handle merchantId merchantOpCityId previousId newStartDate = do
  previous <-
    QCJMExtra.findCohortJourneyMappingById previousId
      >>= fromMaybeM (InvalidRequest "Cohort journey mapping not found")
  unless (previous.merchantId == merchantId && previous.merchantOperatingCityId == merchantOpCityId) $
    throwError (InvalidRequest "Cohort journey mapping does not belong to this merchant/city")
  journey <-
    QJourney.findById previous.journeyId
      >>= fromMaybeM (InvalidRequest "Incentive journey not found")
  let previousEnd = IJ.computeStreakEndDate previous.startDate previous.streakRange journey.journeyType
      newEnd = IJ.computeStreakEndDate newStartDate previous.streakRange journey.journeyType
  when (newStartDate < previousEnd) $
    throwError $
      InvalidRequest $
        "Roll startDate must be >= previous exclusive endDate ("
          <> show previousEnd
          <> ")"
  mbExact <-
    QCJMExtra.findByMerchantOperatingCityIdAndCohortIdAndJourneyIdAndStartDate
      previous.merchantOperatingCityId
      previous.cohortId
      previous.journeyId
      newStartDate
  whenJust mbExact $ \_ ->
    throwError (InvalidRequest "A cohort journey mapping already exists for this city/cohort/journey/startDate")
  siblings <-
    QCJMExtra.findAllByMerchantOperatingCityIdAndCohortIdAndJourneyId
      previous.merchantOperatingCityId
      previous.cohortId
      previous.journeyId
  -- Tip-only: A→B disables A's auto-apply; a later A→C would clone enabled=False onto C.
  case listToMaybe (sortOn (Down . (.startDate)) siblings) of
    Just latest
      | latest.id /= previous.id ->
        throwError $
          InvalidRequest $
            "Roll previous must be the latest cohort journey mapping for this city/cohort/journey (latest id "
              <> latest.id.getId
              <> ", startDate "
              <> show latest.startDate
              <> ")"
    _ -> pure ()
  -- Reject a second roll onto a different startDate that still overlaps an existing successor.
  let overlapping =
        [ cjm
          | cjm <- siblings,
            let otherEnd = IJ.computeStreakEndDate cjm.startDate cjm.streakRange journey.journeyType,
            -- Half-open windows [start, end): overlap iff startA < endB && startB < endA.
            cjm.startDate < newEnd && newStartDate < otherEnd
        ]
  case overlapping of
    [] -> pure ()
    (conflict : _) ->
      throwError $
        InvalidRequest $
          "Roll window overlaps an existing cohort journey mapping for this city/cohort/journey (conflicting startDate "
            <> show conflict.startDate
            <> ")"
  rolled <-
    QCJMExtra.createCohortJourneyMapping
      previous.merchantId
      previous.merchantOperatingCityId
      previous.cohortId
      previous.journeyId
      newStartDate
      previous.streakRange
      previous.enabled
      (Just previous.id)
      previous.maxWaiveOffCount
      previous.streakEndRewardType
      previous.streakEndRewardValue
      previous.streakEndRewardExpirationAt
      previous.streakEndRewardMetadata
      previous.campaignUserLinkType
      previous.controlFeatures
  rolloverAutoApplyMappings handle previous rolled
  handle.clearAssignmentCacheByCohortMappingId previous.id
  handle.clearAssignmentCacheByCohortMappingId rolled.id
  pure rolled

rolloverAutoApplyMappings ::
  BeamFlow m r =>
  ServiceHandle m ->
  DCJM.CohortJourneyMapping ->
  DCJM.CohortJourneyMapping ->
  m ()
rolloverAutoApplyMappings handle previous rolled = do
  oldRows <- QAuto.findByCohortJourneyMappingId previous.id
  forM_ oldRows $ \row -> do
    void $
      QAutoExtra.createAutoApplyCohortMapping
        rolled.id
        row.merchantId
        row.merchantOperatingCityId
        row.vehicleCategory
        row.allowIfNoMapping
        row.enabled
    void $ QAutoExtra.updateAutoApplyCohortMapping row {DAuto.enabled = False}
  let categoriesToClear = nub $ map (.vehicleCategory) oldRows
  unless (null oldRows) $
    handle.clearAutoApplyCacheByMerchantAndCity previous.merchantId previous.merchantOperatingCityId categoriesToClear
