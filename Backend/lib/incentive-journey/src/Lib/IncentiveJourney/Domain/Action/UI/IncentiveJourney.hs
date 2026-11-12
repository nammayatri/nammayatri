module Lib.IncentiveJourney.Domain.Action.UI.IncentiveJourney
  ( getIncentiveJourneyList,
    getIncentiveJourneyHistory,
    postIncentiveJourneyOptIn,
  )
where

import Data.List (find, nub, nubBy, sortOn)
import Data.Maybe (listToMaybe)
import qualified Data.Text as T
import Data.Time (Day, UTCTime (UTCTime), addDays, defaultTimeLocale, parseTimeM, utctDay)
import EulerHS.Prelude hiding (find, id, sortOn)
import Kernel.Types.APISuccess (APISuccess (Success))
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Lib.IncentiveJourney as IJ
import qualified Lib.IncentiveJourney.CampaignSync as CampaignSync
import qualified Lib.IncentiveJourney.Common as IJC
import qualified Lib.IncentiveJourney.Common.UI.IncentiveJourney as API
import Lib.IncentiveJourney.Domain.Action.Dashboard.ServiceHandle (ServiceHandle (..))
import qualified Lib.IncentiveJourney.Domain.Types.CohortJourneyMapping as DCJM
import qualified Lib.IncentiveJourney.Domain.Types.Common as DIJC
import qualified Lib.IncentiveJourney.Domain.Types.IncentiveJourney as DIJ
import qualified Lib.IncentiveJourney.Domain.Types.IncentiveJourneyMilestone as DIJM
import qualified Lib.IncentiveJourney.Domain.Types.IncentiveJourneyStats as DIJS
import qualified Lib.IncentiveJourney.Domain.Types.UserCohortMapping as DUCM
import qualified Lib.IncentiveJourney.MgCampaign as Mg
import qualified Lib.IncentiveJourney.RewardMetadata as RM
import Lib.IncentiveJourney.Storage.Beam.BeamFlow (BeamFlow)
import qualified Lib.IncentiveJourney.Storage.Queries.CohortJourneyMappingExtra as QCJMExtra
import qualified Lib.IncentiveJourney.Storage.Queries.UserCohortMapping as QUCM
import qualified Lib.IncentiveJourney.Storage.Queries.UserCohortMappingExtra as QUCMExtra

buildSpecialLocationNames :: BeamFlow m r => ServiceHandle m -> [DIJM.IncentiveJourneyMilestone] -> m [(Text, Text)]
buildSpecialLocationNames handle milestones = do
  let locationIds =
        nub $
          concatMap
            (\milestone -> fromMaybe [] milestone.specialLocationIds)
            milestones
  case handle.findSpecialLocationNameById of
    Nothing -> pure [(lid, lid) | lid <- locationIds]
    Just findName -> do
      pairs <- forM locationIds $ \lid -> do
        mbName <- findName lid
        pure (lid, fromMaybe lid mbName)
      pure pairs

toSpecialLocationNames :: [(Text, Text)] -> Maybe [Text] -> Maybe [Text]
toSpecialLocationNames specialLocationNames =
  fmap (map resolveName)
  where
    resolveName locationId = maybe locationId snd (find ((== locationId) . fst) specialLocationNames)

resolveDisplayRewardValue :: DIJM.IncentiveJourneyMilestone -> Maybe Int
resolveDisplayRewardValue = (.rewardValue)

-- | Driver/rider self opt-in for OptIn cohorts. Requires invite UCM row.
postIncentiveJourneyOptIn ::
  BeamFlow m r =>
  ServiceHandle m ->
  ( Maybe (Id IJC.Person),
    Id IJC.Merchant,
    Id IJC.MerchantOperatingCity
  ) ->
  API.OptInIncentiveJourneyReq ->
  m APISuccess
postIncentiveJourneyOptIn handle (mbPersonId, merchantId, merchantOpCityId) req = do
  personId <- mbPersonId & fromMaybeM (PersonNotFound "No person id passed")
  cjm <-
    QCJMExtra.findCohortJourneyMappingById req.cohortJourneyMappingId
      >>= fromMaybeM (InvalidRequest "Cohort journey mapping not found")
  unless (cjm.merchantId == merchantId && cjm.merchantOperatingCityId == merchantOpCityId) $
    throwError (InvalidRequest "Cohort journey mapping does not belong to this merchant/city")
  unless (fromMaybe DCJM.Assigned cjm.campaignUserLinkType == DCJM.OptIn) $
    throwError (InvalidRequest "Opt-in is only supported for OptIn campaigns")
  unless cjm.enabled $
    throwError (InvalidRequest "Cohort journey mapping is disabled")
  journey <-
    handle.getOneJourney cjm.journeyId
      >>= fromMaybeM (InvalidRequest "Incentive journey not found")
  timeDiffFromUtc <- handle.getTimeDiffFromUtc merchantOpCityId
  localTime <- getLocalCurrentTime timeDiffFromUtc
  let endDate = IJ.computeStreakEndDate cjm.startDate cjm.streakRange journey.journeyType
  when (localTime >= endDate) $
    throwError (InvalidRequest "Cohort journey mapping has expired")
  -- Exclusive endDate: validTill == endDate is OK (same as dashboard opt-in).
  when (req.validTill > endDate) $
    throwError (InvalidRequest "validTill must be on or before the cohort journey end")
  when (req.validTill <= localTime) $
    throwError (InvalidRequest "validTill must be in the future")
  when (Mg.cjmHasControlFeatures cjm) $ do
    assignments <- handle.findAssignmentsByUserId personId
    rows <- forM assignments $ \a -> do
      mbJourney <- handle.getOneJourney a.cohortJourneyMapping.journeyId
      let journeyType = maybe DIJ.Daily (.journeyType) mbJourney
      pure (a.cohortJourneyMapping, a.userCohortMapping, journeyType)
    whenJust (CampaignSync.findControlFeaturesSlotConflict localTime cjm.id.getId rows) $ \otherCjmId ->
      throwError $
        InvalidRequest $
          "Driver already has an active campaign with control features: " <> otherCjmId
  -- Driver self-opt-in only: reject unassigned / disabled invites (dashboard may re-enable).
  inviteUcm <-
    QUCM.findByUserIdAndCohortMappingId personId cjm.id
      >>= fromMaybeM (InvalidRequest "User cohort mapping invite not found; assign/invite first")
  unless inviteUcm.enabled $
    throwError (InvalidRequest "Invite is disabled; contact support or wait for a new invite")
  let optInPeriod = fromMaybe DUCM.Daily req.optInPeriod
  void $ QUCMExtra.markUserCohortMappingOptedIn personId cjm.id optInPeriod req.validTill "driver"
  handle.clearAssignmentCacheByPersonId personId
  whenJust handle.syncCampaignMembership $ \sync -> do
    milestones <- sortOn (.order) <$> handle.getMilestonesByJourneyId journey.id
    case CampaignSync.mkHotPathSync localTime journey cjm milestones True (Just req.validTill) of
      Nothing -> pure ()
      Just payload -> sync personId (Just payload)
  pure Success

getIncentiveJourneyList ::
  BeamFlow m r =>
  ServiceHandle m ->
  ( Maybe (Id IJC.Person),
    Id IJC.Merchant,
    Id IJC.MerchantOperatingCity
  ) ->
  Maybe Int ->
  Maybe Int ->
  m API.IncentiveJourneyListRes
getIncentiveJourneyList handle (mbPersonId, merchantId, merchantOpCityId) mbLimit mbOffset = do
  driverId <- mbPersonId & fromMaybeM (PersonNotFound "No person id passed")
  timeDiffFromUtc <- handle.getTimeDiffFromUtc merchantOpCityId
  localTime <- getLocalCurrentTime timeDiffFromUtc
  assignments <- handle.findAssignmentsByUserId driverId
  let mappedJourneyIds = map ((.journeyId) . (.cohortJourneyMapping)) assignments
  if null mappedJourneyIds
    then pure API.IncentiveJourneyListRes {journeys = []}
    else do
      allJourneys <- handle.getJourneysByIds mappedJourneyIds
      let matching = allJourneys
          journeysWithMapping =
            [ (j, cjm)
              | j <- matching,
                assignment <- assignments,
                let cjm = assignment.cohortJourneyMapping,
                cjm.journeyId == j.id,
                cjm.enabled,
                cjm.merchantId == merchantId,
                cjm.merchantOperatingCityId == merchantOpCityId,
                IJ.isJourneyWindowActiveFor localTime j cjm
            ]
          orderedPairs = IJ.orderJourneyAssignmentPairsForDisplay localTime journeysWithMapping
          pagedPairs =
            take (fromMaybe (length orderedPairs) mbLimit)
              . drop (fromMaybe 0 mbOffset)
              $ orderedPairs
      if null pagedPairs
        then pure API.IncentiveJourneyListRes {journeys = []}
        else do
          journeys <-
            forM pagedPairs $ \(journey, cjm) -> do
              milestones <- handle.loadJourneyMilestones journey.id
              let periodKey = IJ.mkJourneyPeriodKeyFor localTime journey cjm.startDate
                  -- Inclusive last day for drivers (backend window uses exclusive computeStreakEndDate).
                  endDate =
                    IJ.displayStreakEndDate
                      cjm.startDate
                      cjm.streakRange
                      journey.journeyType
              statsRows <- handle.findStatsByPersonIdCjmAndPeriodKey driverId cjm.id periodKey
              specialLocationNames <- buildSpecialLocationNames handle milestones
              items <- mapM (toMilestoneItem specialLocationNames statsRows) milestones
              pure $
                API.IncentiveJourneyListItem
                  { cohortJourneyMappingId = cjm.id,
                    journeyId = journey.id,
                    name = journey.name,
                    description = journey.description,
                    journeyType = journey.journeyType,
                    controlFeatures = cjm.controlFeatures,
                    periodDays = RM.streakMetaPeriodDays cjm.streakEndRewardMetadata,
                    startDate = cjm.startDate,
                    endDate = endDate,
                    streakRange = cjm.streakRange,
                    enabled = cjm.enabled,
                    milestones = items
                  }
          pure API.IncentiveJourneyListRes {journeys = journeys}

toMilestoneItem :: BeamFlow m r => [(Text, Text)] -> [DIJS.IncentiveJourneyStats] -> DIJM.IncentiveJourneyMilestone -> m API.IncentiveJourneyMilestoneItem
toMilestoneItem specialLocationNames statsRows milestone = do
  let mbStats = find (\s -> s.milestoneId == milestone.id) statsRows
      displayRewardValue = resolveDisplayRewardValue milestone
  pure $
    API.IncentiveJourneyMilestoneItem
      { milestoneId = milestone.id,
        name = milestone.name,
        description = milestone.description,
        order = milestone.order,
        conditionType = milestone.conditionType,
        conditionOperator = milestone.conditionOperator,
        conditionValue = milestone.conditionValue,
        evaluation = milestone.evaluation,
        areaType = milestone.areaType,
        specialLocationNames = toSpecialLocationNames specialLocationNames milestone.specialLocationIds,
        vehicleCategory = milestone.vehicleCategory,
        serviceTierType = milestone.serviceTierType,
        rewardType = milestone.rewardType,
        rewardValue = displayRewardValue,
        subscriptionWaiveOff =
          case milestone.rewardType of
            DIJC.SubscriptionWaiveOff ->
              ( \spec ->
                  API.SubscriptionWaiveOffConfig
                    { percentage = spec.percentage,
                      daysValidFor = spec.daysValidFor,
                      serviceName = spec.serviceName,
                      waiveOffMode = spec.waiveOffMode
                    }
              )
                <$> IJ.mkSubscriptionWaiveOffSpec
                  milestone.rewardValue
                  milestone.rewardExpirationAt
                  milestone.rewardMetadata
            _ -> Nothing,
        timeBounds = milestone.timeBounds,
        status = maybe DIJS.NotStarted (.status) mbStats,
        currentValue = maybe 0 (.currentValue) mbStats,
        currentMetrics = mbStats >>= (.currentMetrics)
      }

getIncentiveJourneyHistory ::
  BeamFlow m r =>
  ServiceHandle m ->
  ( Maybe (Id IJC.Person),
    Id IJC.Merchant,
    Id IJC.MerchantOperatingCity
  ) ->
  Maybe Text ->
  Maybe Int ->
  Maybe Int ->
  m API.IncentiveJourneyHistoryRes
getIncentiveJourneyHistory handle (mbPersonId, merchantId, merchantOpCityId) mbDate mbLimit mbOffset = do
  driverId <- mbPersonId & fromMaybeM (PersonNotFound "No person id passed")
  timeDiffFromUtc <- handle.getTimeDiffFromUtc merchantOpCityId
  localTime <- getLocalCurrentTime timeDiffFromUtc
  let historyDay = fromMaybe (utctDay localTime) (mbDate >>= parseDateText)
      historyDayUtc = UTCTime historyDay 0
      dailyPeriodKey = IJ.mkDailyPeriodKey historyDayUtc
      limitVal = fromMaybe 30 mbLimit
      offsetVal = fromMaybe 0 mbOffset
  assignments <- handle.findAssignmentsByUserId driverId
  -- Include expired/disabled UCMs so Week:/Month: history keys stay findable after opt-out.
  allUcms <- QUCM.findByUserId driverId
  let assignmentJourneyIds = nub $ map ((.journeyId) . (.cohortJourneyMapping)) assignments
      assignmentCjmIds = map ((.id) . (.cohortJourneyMapping)) assignments
      inactiveUcmCjmIds =
        nub
          [ ucm.cohortMappingId
            | ucm <- allUcms,
              ucm.cohortMappingId `notElem` assignmentCjmIds
          ]
  assignmentJourneys <-
    if null assignmentJourneyIds
      then pure []
      else handle.getJourneysByIds assignmentJourneyIds
  inactiveCjms <- QCJMExtra.findByIds inactiveUcmCjmIds
  let inactiveCjmFiltered =
        [ cjm
          | cjm <- inactiveCjms,
            cjm.merchantId == merchantId,
            cjm.merchantOperatingCityId == merchantOpCityId
        ]
      inactiveJourneyIds = nub $ map (.journeyId) inactiveCjmFiltered
  inactiveJourneys <-
    if null inactiveJourneyIds
      then pure []
      else handle.getJourneysByIds inactiveJourneyIds
  let dayStart = historyDayUtc
      dayEnd = UTCTime (addDays 1 historyDay) 0
      -- Daily key + anchored Week:/Month: keys from active assignments and past UCMs.
      anchoredPeriodKeys =
        nub $
          [ IJ.mkJourneyPeriodKeyFor historyDayUtc journey cjm.startDate
            | assignment <- assignments,
              let cjm = assignment.cohortJourneyMapping,
              journey <- assignmentJourneys,
              journey.id == cjm.journeyId,
              cjm.enabled,
              cjm.merchantId == merchantId,
              cjm.merchantOperatingCityId == merchantOpCityId
          ]
            <> [ IJ.mkJourneyPeriodKeyFor historyDayUtc journey cjm.startDate
                 | cjm <- inactiveCjmFiltered,
                   journey <- inactiveJourneys,
                   journey.id == cjm.journeyId,
                   historyDayOverlapsJourney dayStart dayEnd journey cjm
               ]
      periodKeysToFetch = nub (dailyPeriodKey : anchoredPeriodKeys)
  statsRowsNested <- forM periodKeysToFetch $ handle.findStatsByPersonIdAndPeriodKey driverId
  let statsRows = nubBy (\a b -> a.id == b.id) (concat statsRowsNested)
      statsCampaigns = nub [(stats.journeyId, stats.cohortJourneyMappingId) | stats <- statsRows]
      statsCjmIds = mapMaybe snd statsCampaigns
  statsExpanded <-
    concat
      <$> forM
        statsCampaigns
        ( \(journeyId, mbCohortJourneyMappingId) -> do
            mbJourney <- handle.getOneJourney journeyId
            let journeyStats =
                  filter
                    (\s -> s.journeyId == journeyId && s.cohortJourneyMappingId == mbCohortJourneyMappingId)
                    statsRows
            expandHistoryJourney handle historyDayUtc journeyId mbCohortJourneyMappingId mbJourney journeyStats assignments
        )
  -- Eligible assignments with no stats for this date still belong in history.
  -- Another campaign of the same journey is included when its own rows are absent.
  -- The app shows those untouched milestones as missed. Status stays NotStarted.
  let candidateCjms =
        [ cjm
          | assignment <- assignments,
            let cjm = assignment.cohortJourneyMapping,
            cjm.enabled,
            cjm.merchantId == merchantId,
            cjm.merchantOperatingCityId == merchantOpCityId,
            cjm.id `notElem` statsCjmIds
        ]
      candidateJourneyIds = nub (map (.journeyId) candidateCjms)
  assignedJourneys <-
    if null candidateJourneyIds
      then pure []
      else handle.getJourneysByIds candidateJourneyIds
  let eligiblePairs =
        [ (journey, cjm)
          | cjm <- candidateCjms,
            journey <- assignedJourneys,
            journey.id == cjm.journeyId,
            historyDayOverlapsJourney dayStart dayEnd journey cjm
        ]
  eligibleExpanded <-
    concat
      <$> forM eligiblePairs (\(journey, cjm) -> expandHistoryJourney handle historyDayUtc journey.id (Just cjm.id) (Just journey) [] assignments)
  let history =
        take limitVal
          . drop offsetVal
          $ statsExpanded <> eligibleExpanded
  pure API.IncentiveJourneyHistoryRes {history = history}

expandHistoryJourney ::
  BeamFlow m r =>
  ServiceHandle m ->
  UTCTime ->
  Id DIJ.IncentiveJourney ->
  Maybe (Id DCJM.CohortJourneyMapping) ->
  Maybe DIJ.IncentiveJourney ->
  [DIJS.IncentiveJourneyStats] ->
  [IJ.JourneyAssignment] ->
  m [API.IncentiveJourneyHistoryItem]
expandHistoryJourney handle historyDayUtc journeyId mbCohortJourneyMappingId mbJourney journeyStats assignments = do
  milestones <- handle.loadJourneyMilestones journeyId
  specialLocationNames <- buildSpecialLocationNames handle milestones
  -- Fall back to DB when the mapping is no longer in active assignments (expired UCM).
  mbCjm <-
    case mbCohortJourneyMappingId of
      Just cjmId ->
        case find (\a -> a.cohortJourneyMapping.id == cjmId) assignments of
          Just assignment -> pure (Just assignment.cohortJourneyMapping)
          Nothing -> QCJMExtra.findCohortJourneyMappingById cjmId
      Nothing -> pure Nothing
  let journeyName = maybe journeyId.getId (.name) mbJourney
      journeyType = maybe DIJ.Daily (.journeyType) mbJourney
      controlFeatures = mbCjm >>= (.controlFeatures)
      periodDays = RM.streakMetaPeriodDays (mbCjm >>= (.streakEndRewardMetadata))
      mbStartDate = (.startDate) <$> mbCjm
      defaultPeriodKey =
        case listToMaybe journeyStats of
          Just stats -> stats.periodKey
          Nothing ->
            case (mbJourney, mbStartDate) of
              (Just journey, Just startDate) -> IJ.mkJourneyPeriodKeyFor historyDayUtc journey startDate
              _ -> IJ.mkDailyPeriodKey historyDayUtc
  forM milestones $ \milestone -> do
    let mbStats = find (\s -> s.milestoneId == milestone.id) journeyStats
        displayRewardValue = resolveDisplayRewardValue milestone
    pure $ mkHistoryItem specialLocationNames journeyId mbCohortJourneyMappingId journeyName journeyType controlFeatures periodDays milestones milestone defaultPeriodKey displayRewardValue mbStats

-- | The history date overlaps the cohort mapping window. End is exclusive, same as the list check.
historyDayOverlapsJourney :: UTCTime -> UTCTime -> DIJ.IncentiveJourney -> DCJM.CohortJourneyMapping -> Bool
historyDayOverlapsJourney dayStart dayEnd journey cjm =
  let endDate = IJ.computeStreakEndDate cjm.startDate cjm.streakRange journey.journeyType
   in cjm.startDate < dayEnd && endDate > dayStart

mkHistoryItem ::
  [(Text, Text)] ->
  Id DIJ.IncentiveJourney ->
  Maybe (Id DCJM.CohortJourneyMapping) ->
  Text ->
  DIJ.IncentiveJourneyType ->
  Maybe Value ->
  Maybe Int ->
  [DIJM.IncentiveJourneyMilestone] ->
  DIJM.IncentiveJourneyMilestone ->
  Text ->
  Maybe Int ->
  Maybe DIJS.IncentiveJourneyStats ->
  API.IncentiveJourneyHistoryItem
mkHistoryItem specialLocationNames journeyId mbCohortJourneyMappingId journeyName journeyType controlFeatures periodDays journeyMilestones milestone defaultPeriodKey displayRewardValue mbStats =
  let milestoneOrder = IJ.milestoneOrderInJourney milestone journeyMilestones
   in case mbStats of
        Just stats ->
          API.IncentiveJourneyHistoryItem
            { journeyId = journeyId,
              cohortJourneyMappingId = mbCohortJourneyMappingId,
              journeyName = journeyName,
              journeyType = journeyType,
              controlFeatures = controlFeatures,
              periodDays = periodDays,
              currentMetrics = stats.currentMetrics,
              evaluation = milestone.evaluation,
              milestoneId = milestone.id,
              milestoneName = milestone.name,
              milestoneDescription = milestone.description,
              milestoneOrder = milestoneOrder,
              conditionType = stats.conditionType,
              conditionOperator = stats.conditionOperator,
              conditionValue = stats.conditionValue,
              areaType = milestone.areaType,
              specialLocationNames = toSpecialLocationNames specialLocationNames milestone.specialLocationIds,
              currentValue = stats.currentValue,
              status = stats.status,
              rewardType = stats.rewardType,
              rewardValue = displayRewardValue,
              periodKey = stats.periodKey,
              completedAt = completedAtForStatus stats.status stats.updatedAt
            }
        Nothing ->
          API.IncentiveJourneyHistoryItem
            { journeyId = journeyId,
              cohortJourneyMappingId = mbCohortJourneyMappingId,
              journeyName = journeyName,
              journeyType = journeyType,
              controlFeatures = controlFeatures,
              periodDays = periodDays,
              currentMetrics = Nothing,
              evaluation = milestone.evaluation,
              milestoneId = milestone.id,
              milestoneName = milestone.name,
              milestoneDescription = milestone.description,
              milestoneOrder = milestoneOrder,
              conditionType = milestone.conditionType,
              conditionOperator = milestone.conditionOperator,
              conditionValue = milestone.conditionValue,
              areaType = milestone.areaType,
              specialLocationNames = toSpecialLocationNames specialLocationNames milestone.specialLocationIds,
              currentValue = 0,
              status = DIJS.NotStarted,
              rewardType = milestone.rewardType,
              rewardValue = displayRewardValue,
              periodKey = defaultPeriodKey,
              completedAt = Nothing
            }

completedAtForStatus :: DIJS.JourneyMilestoneStatus -> UTCTime -> Maybe UTCTime
completedAtForStatus status updatedAt =
  case status of
    DIJS.Completed -> Just updatedAt
    DIJS.Rewarded -> Just updatedAt
    DIJS.WaivedOff -> Just updatedAt
    _ -> Nothing

parseDateText :: Text -> Maybe Day
parseDateText =
  parseTimeM True defaultTimeLocale "%Y-%m-%d" . T.unpack
