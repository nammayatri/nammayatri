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
    listAssignmentViews,
    assignUser,
    unassignUser,
  )
where

import Control.Monad.Extra (mapMaybeM)
import qualified Data.HashMap.Strict as HM
import Data.List (sortOn)
import qualified Data.Text as T
import Data.Time (Day)
import EulerHS.Prelude hiding (id, sortOn)
import qualified Kernel.Types.Beckn.Context
import Kernel.Types.Error (GenericError (InvalidRequest))
import qualified Kernel.Types.Id as ID
import Kernel.Utils.Common
import qualified Lib.IncentiveJourney as IJ
import qualified Lib.IncentiveJourney.Common as IJC
import Lib.IncentiveJourney.Domain.Action.Dashboard.ServiceHandle (ServiceHandle (..))
import qualified Lib.IncentiveJourney.Domain.Types.CohortDetails as DCD
import qualified Lib.IncentiveJourney.Domain.Types.CohortJourneyMapping as DCJM
import qualified Lib.IncentiveJourney.Domain.Types.Common as DIJC
import qualified Lib.IncentiveJourney.Domain.Types.IncentiveJourney as DIJ
import qualified Lib.IncentiveJourney.Domain.Types.IncentiveJourneyMilestone as DIJM
import qualified Lib.IncentiveJourney.Domain.Types.IncentiveJourneyStats as DIJS
import Lib.IncentiveJourney.Storage.Beam.BeamFlow (BeamFlow)
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

data AssignmentView = AssignmentView
  { cohortId :: ID.Id DCD.CohortDetails,
    cohortName :: Text,
    cohortJourneyMappingId :: ID.Id DCJM.CohortJourneyMapping,
    journeyId :: ID.Id DIJ.IncentiveJourney,
    journeyName :: Text,
    journeyType :: DIJ.IncentiveJourneyType,
    enabled :: Bool,
    userEnabled :: Bool,
    startDate :: UTCTime,
    endDate :: UTCTime,
    streakRange :: Int,
    assignedAt :: UTCTime
  }

listAssignmentViews ::
  BeamFlow m r =>
  ServiceHandle m ->
  ID.ShortId DIJC.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  ID.Id DIJC.Person ->
  m [AssignmentView]
listAssignmentViews handle merchantShortId opCity personId = do
  (merchant, merchantOpCityId) <- resolveMerchant handle merchantShortId opCity
  assignments <- handle.findAssignmentsByUserId personId
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
        pure $
          Just
            AssignmentView
              { cohortId = cohort.id,
                cohortName = cohort.name,
                cohortJourneyMappingId = cjm.id,
                journeyId = journey.id,
                journeyName = journey.name,
                journeyType = journeyType,
                enabled = cjm.enabled,
                userEnabled = maybe True (.enabled) assignment.userCohortMapping,
                startDate = cjm.startDate,
                endDate = IJ.computeStreakEndDate cjm.startDate cjm.streakRange journeyType,
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
  void $ QJourney.findById cjm.journeyId >>= fromMaybeM (InvalidRequest "Incentive journey not found")
  void $ QUCMExtra.upsertUserCohortMapping personId cjmId enabled validTill
  handle.clearAssignmentCacheByPersonId personId

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
  -- CJM may already be deleted; still remove the user_cohort_mapping row.
  QUCMExtra.deleteUserCohortMapping personId cjmId
  handle.clearAssignmentCacheByPersonId personId
