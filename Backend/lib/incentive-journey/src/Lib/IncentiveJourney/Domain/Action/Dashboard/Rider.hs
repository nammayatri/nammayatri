module Lib.IncentiveJourney.Domain.Action.Dashboard.Rider
  ( getIncentiveJourneyList,
    postIncentiveJourneyCreate,
    getIncentiveJourneyMilestoneList,
    postIncentiveJourneyMilestoneCreate,
    getIncentiveJourneyStatsHistory,
    getIncentiveJourneyPersonAssignments,
    postIncentiveJourneyStatsWaiveOff,
    postIncentiveJourneyCohortCreate,
    postIncentiveJourneyCohortJourneyCreate,
    putIncentiveJourneyCohortJourneyUpdate,
    getIncentiveJourneyCohortJourneyList,
    postIncentiveJourneyAssign,
    deleteIncentiveJourneyUnassign,
  )
where

import qualified API.Types.RiderPlatform.IncentiveJourney.IncentiveJourney as Common
import qualified Dashboard.Common
import Data.Time (Day)
import EulerHS.Prelude hiding (id, sortOn)
import Kernel.Types.APISuccess (APISuccess (Success))
import qualified Kernel.Types.Beckn.Context
import Kernel.Types.Error (GenericError (InvalidRequest))
import qualified Kernel.Types.Id as ID
import Kernel.Utils.Common
import qualified Lib.IncentiveJourney as IJ
import qualified Lib.IncentiveJourney.Common as IJC
import qualified Lib.IncentiveJourney.Domain.Action.Dashboard.Core as Core
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
import qualified Lib.IncentiveJourney.Storage.Queries.IncentiveJourneyMilestone as QMilestone
import qualified Lib.IncentiveJourney.Streak as IJStreak

resolveMerchant ::
  BeamFlow m r =>
  ServiceHandle m ->
  ID.ShortId DIJC.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  m (IJC.MerchantInfo, ID.Id DIJC.MerchantOperatingCity)
resolveMerchant = Core.resolveMerchant

getIncentiveJourneyList ::
  BeamFlow m r =>
  ServiceHandle m ->
  ID.ShortId DIJC.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  Maybe Int ->
  Maybe Int ->
  Maybe (ID.Id Dashboard.Common.IncentiveJourney) ->
  Maybe Common.IncentiveJourneyType ->
  m Common.IncentiveJourneyListRes
getIncentiveJourneyList handle merchantShortId opCity mbLimit mbOffset mbJourneyId mbJourneyType = do
  allJourneys <-
    Core.listJourneys
      handle
      merchantShortId
      opCity
      mbLimit
      mbOffset
      (ID.cast @Dashboard.Common.IncentiveJourney @DIJ.IncentiveJourney <$> mbJourneyId)
      (toDomainJourneyType <$> mbJourneyType)
  pure Common.IncentiveJourneyListRes {journeys = map toJourneyListItem allJourneys}

postIncentiveJourneyCreate ::
  BeamFlow m r =>
  ServiceHandle m ->
  ID.ShortId DIJC.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  Common.CreateIncentiveJourneyReq ->
  m Common.CreateIncentiveJourneyRes
postIncentiveJourneyCreate handle merchantShortId opCity req = do
  journeyId <- Core.createJourney handle merchantShortId opCity req.name req.description (toDomainJourneyType req.journeyType)
  pure Common.CreateIncentiveJourneyRes {journeyId = ID.cast journeyId}

getIncentiveJourneyMilestoneList ::
  BeamFlow m r =>
  ServiceHandle m ->
  ID.ShortId DIJC.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  ID.Id Dashboard.Common.IncentiveJourney ->
  Maybe Int ->
  Maybe Int ->
  m Common.IncentiveJourneyMilestoneListRes
getIncentiveJourneyMilestoneList handle merchantShortId opCity dashboardJourneyId mbLimit mbOffset = do
  page <-
    Core.listMilestones
      handle
      merchantShortId
      opCity
      (ID.cast @Dashboard.Common.IncentiveJourney @DIJ.IncentiveJourney dashboardJourneyId)
      mbLimit
      mbOffset
  pure Common.IncentiveJourneyMilestoneListRes {milestones = map toMilestoneListItem page}

postIncentiveJourneyMilestoneCreate ::
  BeamFlow m r =>
  ServiceHandle m ->
  ID.ShortId DIJC.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  Common.CreateIncentiveJourneyMilestoneReq ->
  m Common.CreateIncentiveJourneyMilestoneRes
postIncentiveJourneyMilestoneCreate handle merchantShortId opCity req = do
  (merchant, merchantOpCityId) <- resolveMerchant handle merchantShortId opCity
  when (req.conditionValue < 0) $
    throwError (InvalidRequest "conditionValue must be >= 0")
  when (req.order < 0) $
    throwError (InvalidRequest "order must be >= 0")
  let journeyId = ID.cast @Dashboard.Common.IncentiveJourney @DIJ.IncentiveJourney req.journeyId
  void $ QJourney.findById journeyId >>= fromMaybeM (InvalidRequest "Incentive journey not found")
  now <- getCurrentTime
  milestoneId <- generateGUID
  let milestone =
        DIJM.IncentiveJourneyMilestone
          { id = milestoneId,
            journeyId = journeyId,
            name = req.name,
            description = req.description,
            order = req.order,
            conditionType = toDomainConditionType req.conditionType,
            conditionOperator = toDomainConditionOperator req.conditionOperator,
            conditionValue = req.conditionValue,
            areaType = toDomainAreaType <$> req.areaType,
            specialLocationIds = req.specialLocationIds,
            vehicleCategory = req.vehicleCategory,
            serviceTierType = req.serviceTierType,
            rewardType = toDomainRewardType req.rewardType,
            rewardValue = req.rewardValue,
            rewardExpirationAt = req.rewardExpirationAt,
            rewardMetadata = Nothing,
            timeBounds = req.timeBounds,
            createdAt = now,
            updatedAt = now,
            merchantId = Just (ID.cast merchant.id),
            merchantOperatingCityId = Just (ID.cast merchantOpCityId)
          }
  validateMilestoneCondition milestone
  validateMilestoneReward milestone
  QMilestone.create milestone
  handle.clearMilestoneCacheByJourneyId journeyId
  pure Common.CreateIncentiveJourneyMilestoneRes {milestoneId = ID.cast milestoneId}

getIncentiveJourneyStatsHistory ::
  BeamFlow m r =>
  ServiceHandle m ->
  ID.ShortId DIJC.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  ID.Id Dashboard.Common.Person ->
  Maybe (ID.Id Dashboard.Common.IncentiveJourney) ->
  Maybe Int ->
  Maybe Int ->
  Day ->
  Day ->
  m Common.IncentiveJourneyStatsHistoryRes
getIncentiveJourneyStatsHistory handle merchantShortId opCity personId mbJourneyId mbLimit mbOffset fromDate toDate = do
  rows <-
    Core.statsHistory
      handle
      merchantShortId
      opCity
      (ID.cast @Dashboard.Common.Person @DIJC.Person personId)
      (ID.cast @Dashboard.Common.IncentiveJourney @DIJ.IncentiveJourney <$> mbJourneyId)
      mbLimit
      mbOffset
      fromDate
      toDate
  pure Common.IncentiveJourneyStatsHistoryRes {stats = map toStatsHistoryItem rows}

getIncentiveJourneyPersonAssignments ::
  BeamFlow m r =>
  ServiceHandle m ->
  ID.ShortId DIJC.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  ID.Id Dashboard.Common.Person ->
  m Common.IncentiveJourneyPersonAssignmentListRes
getIncentiveJourneyPersonAssignments handle merchantShortId opCity personId = do
  views <- Core.listAssignmentViews handle merchantShortId opCity (ID.cast @Dashboard.Common.Person @DIJC.Person personId)
  pure Common.IncentiveJourneyPersonAssignmentListRes {assignments = map toPersonAssignmentItem views}

postIncentiveJourneyStatsWaiveOff ::
  BeamFlow m r =>
  ServiceHandle m ->
  ID.ShortId DIJC.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  Common.WaiveIncentiveJourneyMilestoneReq ->
  m APISuccess
postIncentiveJourneyStatsWaiveOff handle merchantShortId opCity req = do
  (merchant, merchantOpCityId) <- resolveMerchant handle merchantShortId opCity
  let journeyId = ID.cast @Dashboard.Common.IncentiveJourney @DIJ.IncentiveJourney req.journeyId
      milestoneId = ID.cast @Dashboard.Common.IncentiveJourneyMilestone @DIJM.IncentiveJourneyMilestone req.milestoneId
      personId = ID.cast @Dashboard.Common.Person @DIJC.Person req.personId
  journey <- QJourney.findById journeyId >>= fromMaybeM (InvalidRequest "Incentive journey not found")
  when (null req.periodKey) $ throwError (InvalidRequest "periodKey must be non-empty")
  waiveFn <- handle.waiveRiderMilestone & fromMaybeM (InvalidRequest "waiveRiderMilestone not configured")
  waiveFn personId merchant.id merchantOpCityId journey milestoneId req.periodKey
  pure Success

postIncentiveJourneyCohortCreate ::
  BeamFlow m r =>
  ServiceHandle m ->
  ID.ShortId DIJC.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  Common.CreateCohortDetailsReq ->
  m Common.CreateCohortDetailsRes
postIncentiveJourneyCohortCreate _handle _merchantShortId _opCity req = do
  cohort <- Core.createCohort req.name req.category req.description req.cohortRule
  pure Common.CreateCohortDetailsRes {cohortId = ID.cast cohort.id}

postIncentiveJourneyCohortJourneyCreate ::
  BeamFlow m r =>
  ServiceHandle m ->
  ID.ShortId DIJC.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  Common.CreateCohortJourneyMappingReq ->
  m Common.CreateCohortJourneyMappingRes
postIncentiveJourneyCohortJourneyCreate handle merchantShortId opCity req = do
  (merchant, merchantOpCityId) <- resolveMerchant handle merchantShortId opCity
  when (req.streakRange <= 0) $ throwError (InvalidRequest "streakRange must be > 0")
  let journeyId = ID.cast @Dashboard.Common.IncentiveJourney @DIJ.IncentiveJourney req.journeyId
      cohortId = ID.cast @Dashboard.Common.CohortDetails @DCD.CohortDetails req.cohortId
  void $ QCDExtra.findCohortDetailsById cohortId >>= fromMaybeM (InvalidRequest "Cohort not found")
  journey <- QJourney.findById journeyId >>= fromMaybeM (InvalidRequest "Incentive journey not found")
  case IJStreak.validateMappingStartDate journey.journeyType req.startDate of
    Left err -> throwError (InvalidRequest err)
    Right () -> pure ()
  let enabled = fromMaybe True req.enabled
      maxWaiveOffCount = Just (fromMaybe 1 req.maxWaiveOffCount)
  whenJust maxWaiveOffCount $ \n ->
    when (n < 0) $ throwError (InvalidRequest "maxWaiveOffCount must be >= 0")
  validateStreakEndRewardOnMapping (toDomainRewardType <$> req.streakEndRewardType) req.streakEndRewardValue
  cjm <-
    QCJMExtra.createCohortJourneyMapping
      (ID.cast merchant.id)
      (ID.cast merchantOpCityId)
      cohortId
      journeyId
      req.startDate
      req.streakRange
      enabled
      maxWaiveOffCount
      (toDomainRewardType <$> req.streakEndRewardType)
      req.streakEndRewardValue
      req.streakEndRewardExpirationAt
      Nothing
  pure Common.CreateCohortJourneyMappingRes {cohortJourneyMappingId = ID.cast cjm.id}

putIncentiveJourneyCohortJourneyUpdate ::
  BeamFlow m r =>
  ServiceHandle m ->
  ID.ShortId DIJC.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  Common.UpdateCohortJourneyMappingReq ->
  m APISuccess
putIncentiveJourneyCohortJourneyUpdate handle merchantShortId opCity req = do
  (merchant, merchantOpCityId) <- resolveMerchant handle merchantShortId opCity
  let cjmId = ID.cast @Dashboard.Common.CohortJourneyMapping @DCJM.CohortJourneyMapping req.cohortJourneyMappingId
  cjm <- QCJMExtra.findCohortJourneyMappingById cjmId >>= fromMaybeM (InvalidRequest "Cohort journey mapping not found")
  unless (cjm.merchantId == ID.cast merchant.id && cjm.merchantOperatingCityId == ID.cast merchantOpCityId) $
    throwError (InvalidRequest "Cohort journey mapping does not belong to this merchant/city")
  journey <- QJourney.findById cjm.journeyId >>= fromMaybeM (InvalidRequest "Incentive journey not found")
  let startDate = fromMaybe cjm.startDate req.startDate
      streakRange = fromMaybe cjm.streakRange req.streakRange
      enabled = fromMaybe cjm.enabled req.enabled
      maxWaiveOffCount = maybe cjm.maxWaiveOffCount Just req.maxWaiveOffCount
      streakEndRewardType = (toDomainRewardType <$> req.streakEndRewardType) <|> cjm.streakEndRewardType
      streakEndRewardValue = req.streakEndRewardValue <|> cjm.streakEndRewardValue
      streakEndRewardExpirationAt = req.streakEndRewardExpirationAt <|> cjm.streakEndRewardExpirationAt
  whenJust maxWaiveOffCount $ \n ->
    when (n < 0) $ throwError (InvalidRequest "maxWaiveOffCount must be >= 0")
  when (streakRange <= 0) $ throwError (InvalidRequest "streakRange must be > 0")
  case IJStreak.validateMappingStartDate journey.journeyType startDate of
    Left err -> throwError (InvalidRequest err)
    Right () -> pure ()
  validateStreakEndRewardOnMapping streakEndRewardType streakEndRewardValue
  void $
    QCJMExtra.updateCohortJourneyMappingFields
      cjm{DCJM.startDate = startDate,
          DCJM.streakRange = streakRange,
          DCJM.enabled = enabled,
          DCJM.maxWaiveOffCount = maxWaiveOffCount,
          DCJM.streakEndRewardType = streakEndRewardType,
          DCJM.streakEndRewardValue = streakEndRewardValue,
          DCJM.streakEndRewardExpirationAt = streakEndRewardExpirationAt,
          DCJM.streakEndRewardMetadata = cjm.streakEndRewardMetadata
         }
  handle.clearAssignmentCacheByCohortMappingId cjmId
  pure Success

getIncentiveJourneyCohortJourneyList ::
  BeamFlow m r =>
  ServiceHandle m ->
  ID.ShortId DIJC.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  Maybe Int ->
  Maybe Int ->
  Maybe Text ->
  Maybe Text ->
  Maybe Bool ->
  Maybe Common.IncentiveJourneyType ->
  m Common.CohortJourneyMappingListRes
getIncentiveJourneyCohortJourneyList handle merchantShortId opCity mbLimit mbOffset mbCohortName mbCohortCategory mbIsActive mbJourneyType = do
  rows <-
    Core.listCohortJourneyRows
      handle
      merchantShortId
      opCity
      mbLimit
      mbOffset
      Nothing
      mbCohortName
      mbCohortCategory
      (toDomainJourneyType <$> mbJourneyType)
      mbIsActive
  pure Common.CohortJourneyMappingListRes {mappings = map (\(cjm, cohort, journey) -> toCohortJourneyMappingListItem cjm cohort journey) rows}

toCohortJourneyMappingListItem :: DCJM.CohortJourneyMapping -> DCD.CohortDetails -> DIJ.IncentiveJourney -> Common.CohortJourneyMappingListItem
toCohortJourneyMappingListItem cjm cohort journey =
  let journeyType = journey.journeyType
   in Common.CohortJourneyMappingListItem
        { cohortJourneyMappingId = ID.cast cjm.id,
          merchantId = cjm.merchantId.getId,
          merchantOperatingCityId = cjm.merchantOperatingCityId.getId,
          cohortId = ID.cast cjm.cohortId,
          cohortName = cohort.name,
          cohortCategory = cohort.category,
          cohortRule = cohort.cohortRule,
          journeyId = ID.cast cjm.journeyId,
          journeyName = journey.name,
          isActive = cjm.enabled,
          journeyType = toApiJourneyType journeyType,
          maxWaiveOffCount = cjm.maxWaiveOffCount,
          startDate = cjm.startDate,
          endDate = IJ.computeStreakEndDate cjm.startDate cjm.streakRange journeyType,
          streakRange = cjm.streakRange,
          streakEndRewardType = toApiRewardType <$> cjm.streakEndRewardType,
          streakEndRewardValue = cjm.streakEndRewardValue,
          streakEndRewardExpirationAt = cjm.streakEndRewardExpirationAt,
          streakEndSubscriptionWaiveOff = Nothing,
          createdAt = cjm.createdAt,
          updatedAt = cjm.updatedAt
        }

postIncentiveJourneyAssign ::
  BeamFlow m r =>
  ServiceHandle m ->
  ID.ShortId DIJC.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  Common.AssignUserToIncentiveJourneyReq ->
  m APISuccess
postIncentiveJourneyAssign handle merchantShortId opCity req = do
  Core.assignUser
    handle
    merchantShortId
    opCity
    (ID.cast @Dashboard.Common.Person @DIJC.Person req.personId)
    (ID.cast @Dashboard.Common.CohortJourneyMapping @DCJM.CohortJourneyMapping req.cohortJourneyMappingId)
    req.enabled
    req.validTill
    "Person not found"
    "Person does not belong to this merchant/city"
  pure Success

deleteIncentiveJourneyUnassign ::
  BeamFlow m r =>
  ServiceHandle m ->
  ID.ShortId DIJC.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  Common.UnassignUserFromIncentiveJourneyReq ->
  m APISuccess
deleteIncentiveJourneyUnassign handle merchantShortId opCity req = do
  Core.unassignUser
    handle
    merchantShortId
    opCity
    (ID.cast @Dashboard.Common.Person @DIJC.Person req.personId)
    (ID.cast @Dashboard.Common.CohortJourneyMapping @DCJM.CohortJourneyMapping req.cohortJourneyMappingId)
    "Person not found"
    "Person does not belong to this merchant/city"
  pure Success

---------------------------------------------------------------------------
-- Helpers
---------------------------------------------------------------------------

toJourneyListItem :: DIJ.IncentiveJourney -> Common.IncentiveJourneyListItem
toJourneyListItem journey =
  Common.IncentiveJourneyListItem
    { journeyId = ID.cast journey.id,
      name = journey.name,
      description = journey.description,
      journeyType = toApiJourneyType journey.journeyType,
      createdAt = journey.createdAt,
      updatedAt = journey.updatedAt
    }

toMilestoneListItem :: DIJM.IncentiveJourneyMilestone -> Common.IncentiveJourneyMilestoneListItem
toMilestoneListItem milestone =
  Common.IncentiveJourneyMilestoneListItem
    { milestoneId = ID.cast milestone.id,
      journeyId = ID.cast milestone.journeyId,
      name = milestone.name,
      description = milestone.description,
      order = milestone.order,
      conditionType = toApiConditionType milestone.conditionType,
      conditionOperator = toApiConditionOperator milestone.conditionOperator,
      conditionValue = milestone.conditionValue,
      areaType = toApiAreaType <$> milestone.areaType,
      specialLocationIds = milestone.specialLocationIds,
      vehicleCategory = milestone.vehicleCategory,
      serviceTierType = milestone.serviceTierType,
      rewardType = toApiRewardType milestone.rewardType,
      rewardValue = milestone.rewardValue,
      rewardExpirationAt = milestone.rewardExpirationAt,
      subscriptionWaiveOff = Nothing,
      timeBounds = milestone.timeBounds,
      createdAt = milestone.createdAt,
      updatedAt = milestone.updatedAt
    }

validateArea :: BeamFlow m r => Maybe DIJM.MilestoneAreaType -> Maybe [Text] -> m ()
validateArea mbAreaType mbSpecialLocationIds =
  case mbAreaType of
    Just DIJM.Pickup -> requireNonEmptySpecialLocationIds
    Just DIJM.Drop -> requireNonEmptySpecialLocationIds
    Just DIJM.PickupDrop -> requireNonEmptySpecialLocationIds
    Just DIJM.Default -> rejectSpecialLocationIds
    Nothing -> rejectSpecialLocationIds
  where
    requireNonEmptySpecialLocationIds =
      when (maybe True null mbSpecialLocationIds) $
        throwError (InvalidRequest "specialLocationIds must be non-empty for Pickup/Drop/PickupDrop areaType")
    rejectSpecialLocationIds =
      when (maybe False (not . null) mbSpecialLocationIds) $
        throwError (InvalidRequest "specialLocationIds must be empty or omitted for Default/Nothing areaType")

validateMilestoneCondition :: BeamFlow m r => DIJM.IncentiveJourneyMilestone -> m ()
validateMilestoneCondition milestone =
  validateArea milestone.areaType milestone.specialLocationIds

-- Same rules as before. Rider payout is stubbed today; Coins configs are still allowed.
validateMilestoneReward :: BeamFlow m r => DIJM.IncentiveJourneyMilestone -> m ()
validateMilestoneReward milestone =
  case milestone.rewardType of
    DIJC.Coins ->
      case milestone.rewardValue of
        Just coins | coins > 0 -> pure ()
        Just _ -> throwError (InvalidRequest "Coins milestone requires rewardValue > 0")
        Nothing -> throwError (InvalidRequest "Coins milestone requires rewardValue > 0")
    DIJC.Cash ->
      throwError (InvalidRequest "Cash reward type is not supported")
    DIJC.Coupons ->
      throwError (InvalidRequest "Coupons reward type is not supported")
    DIJC.WalletMoney ->
      throwError (InvalidRequest "WalletMoney reward type is not supported")
    DIJC.SubscriptionWaiveOff ->
      throwError (InvalidRequest "SubscriptionWaiveOff reward type is not supported")
    DIJC.PoolingPriority ->
      throwError (InvalidRequest "PoolingPriority reward type is not supported")
    DIJC.NoReward -> pure ()

validateStreakEndRewardOnMapping ::
  BeamFlow m r =>
  Maybe DIJC.MilestoneRewardType ->
  Maybe Int ->
  m ()
validateStreakEndRewardOnMapping mbRewardType mbRewardValue =
  case mbRewardType of
    Nothing -> pure ()
    Just DIJC.Coins ->
      case mbRewardValue of
        Just coins | coins > 0 -> pure ()
        _ -> throwError (InvalidRequest "Streak-end Coins reward requires streakEndRewardValue > 0")
    Just DIJC.NoReward -> pure ()
    Just other -> throwError (InvalidRequest $ show other <> " streak-end reward is not supported yet")

toPersonAssignmentItem :: Core.AssignmentView -> Common.IncentiveJourneyPersonAssignmentItem
toPersonAssignmentItem assignmentView =
  Common.IncentiveJourneyPersonAssignmentItem
    { cohortId = ID.cast assignmentView.cohortId,
      cohortName = assignmentView.cohortName,
      cohortJourneyMappingId = ID.cast assignmentView.cohortJourneyMappingId,
      journeyId = ID.cast assignmentView.journeyId,
      journeyName = assignmentView.journeyName,
      journeyType = toApiJourneyType assignmentView.journeyType,
      enabled = assignmentView.enabled,
      userEnabled = assignmentView.userEnabled,
      startDate = assignmentView.startDate,
      endDate = assignmentView.endDate,
      streakRange = assignmentView.streakRange,
      assignedAt = assignmentView.assignedAt
    }

toStatsHistoryItem :: DIJS.IncentiveJourneyStats -> Common.IncentiveJourneyStatsHistoryItem
toStatsHistoryItem stats =
  Common.IncentiveJourneyStatsHistoryItem
    { statsId = stats.id.getId,
      personId = ID.cast stats.personId,
      journeyId = ID.cast stats.journeyId,
      milestoneId = ID.cast stats.milestoneId,
      periodKey = stats.periodKey,
      conditionType = toApiConditionType stats.conditionType,
      conditionValue = stats.conditionValue,
      currentValue = stats.currentValue,
      status = toApiStatus stats.status,
      rewardType = toApiRewardType stats.rewardType,
      rewardValue = stats.rewardValue,
      createdAt = stats.createdAt,
      updatedAt = stats.updatedAt
    }

toDomainJourneyType :: Common.IncentiveJourneyType -> DIJ.IncentiveJourneyType
toDomainJourneyType = \case
  Common.Daily -> DIJ.Daily
  Common.Weekly -> DIJ.Weekly
  Common.Monthly -> DIJ.Monthly

toApiJourneyType :: DIJ.IncentiveJourneyType -> Common.IncentiveJourneyType
toApiJourneyType = \case
  DIJ.Daily -> Common.Daily
  DIJ.Weekly -> Common.Weekly
  DIJ.Monthly -> Common.Monthly

toApiStatus :: DIJS.JourneyMilestoneStatus -> Common.JourneyMilestoneStatus
toApiStatus = \case
  DIJS.NotStarted -> Common.NotStarted
  DIJS.InProgress -> Common.InProgress
  DIJS.Completed -> Common.Completed
  DIJS.Rewarded -> Common.Rewarded
  DIJS.WaivedOff -> Common.WaivedOff

toDomainConditionType :: Common.MilestoneConditionType -> DIJM.MilestoneConditionType
toDomainConditionType = \case
  Common.RideCompleted -> DIJM.RideCompleted
  Common.Earnings -> DIJM.Earnings
  Common.Distance -> DIJM.Distance
  Common.RideDuration -> DIJM.RideDuration
  Common.BookingTicket -> DIJM.BookingTicket

toApiConditionType :: DIJM.MilestoneConditionType -> Common.MilestoneConditionType
toApiConditionType = \case
  DIJM.RideCompleted -> Common.RideCompleted
  DIJM.Earnings -> Common.Earnings
  DIJM.Distance -> Common.Distance
  DIJM.RideDuration -> Common.RideDuration
  DIJM.BookingTicket -> Common.BookingTicket

toDomainAreaType :: Common.MilestoneAreaType -> DIJM.MilestoneAreaType
toDomainAreaType = \case
  Common.Default -> DIJM.Default
  Common.Pickup -> DIJM.Pickup
  Common.Drop -> DIJM.Drop
  Common.PickupDrop -> DIJM.PickupDrop

toApiAreaType :: DIJM.MilestoneAreaType -> Common.MilestoneAreaType
toApiAreaType = \case
  DIJM.Default -> Common.Default
  DIJM.Pickup -> Common.Pickup
  DIJM.Drop -> Common.Drop
  DIJM.PickupDrop -> Common.PickupDrop

toDomainConditionOperator :: Common.MilestoneConditionOperator -> DIJM.MilestoneConditionOperator
toDomainConditionOperator = \case
  Common.GTE -> DIJM.GTE
  Common.GT -> DIJM.GT
  Common.EQ -> DIJM.EQ
  Common.LTE -> DIJM.LTE
  Common.LT -> DIJM.LT

toApiConditionOperator :: DIJM.MilestoneConditionOperator -> Common.MilestoneConditionOperator
toApiConditionOperator = \case
  DIJM.GTE -> Common.GTE
  DIJM.GT -> Common.GT
  DIJM.EQ -> Common.EQ
  DIJM.LTE -> Common.LTE
  DIJM.LT -> Common.LT

toDomainRewardType :: Common.MilestoneRewardType -> DIJC.MilestoneRewardType
toDomainRewardType = \case
  Common.Coins -> DIJC.Coins
  Common.Cash -> DIJC.Cash
  Common.Coupons -> DIJC.Coupons
  Common.WalletMoney -> DIJC.WalletMoney
  Common.SubscriptionWaiveOff -> DIJC.SubscriptionWaiveOff
  Common.PoolingPriority -> DIJC.PoolingPriority
  Common.NoReward -> DIJC.NoReward

toApiRewardType :: DIJC.MilestoneRewardType -> Common.MilestoneRewardType
toApiRewardType = \case
  DIJC.Coins -> Common.Coins
  DIJC.Cash -> Common.Cash
  DIJC.Coupons -> Common.Coupons
  DIJC.WalletMoney -> Common.WalletMoney
  DIJC.SubscriptionWaiveOff -> Common.SubscriptionWaiveOff
  DIJC.PoolingPriority -> Common.PoolingPriority
  DIJC.NoReward -> Common.NoReward
