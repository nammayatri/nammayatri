module Lib.IncentiveJourney.Domain.Action.Dashboard.Provider
  ( getIncentiveJourneyList,
    postIncentiveJourneyCreate,
    getIncentiveJourneyMilestoneList,
    postIncentiveJourneyMilestoneCreate,
    getIncentiveJourneyStatsHistory,
    getIncentiveJourneyDriverAssignments,
    postIncentiveJourneyStatsWaiveOff,
    postIncentiveJourneyCohortCreate,
    getIncentiveJourneyCohortList,
    postIncentiveJourneyCohortJourneyCreate,
    putIncentiveJourneyCohortJourneyUpdate,
    getIncentiveJourneyCohortJourneyList,
    postIncentiveJourneyAssign,
    deleteIncentiveJourneyUnassign,
    postIncentiveJourneyAssignBulkFromS3,
    getIncentiveJourneyAssignBulkFromS3List,
    postIncentiveJourneyAutoApplyCohortCreate,
    putIncentiveJourneyAutoApplyCohortUpdate,
    getIncentiveJourneyAutoApplyCohortList,
  )
where

import qualified API.Types.ProviderPlatform.IncentiveJourney.IncentiveJourney as Common
import qualified Dashboard.Common
import qualified Data.Text as T
import Data.Time (Day)
import qualified Domain.Types.VehicleCategory as DTV
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
import qualified Lib.IncentiveJourney.Domain.Action.Dashboard.ServiceHandle as SH
import qualified Lib.IncentiveJourney.Domain.Types.AutoApplyCohortMapping as DAuto
import qualified Lib.IncentiveJourney.Domain.Types.BulkUserCohortMappingRun as DBulkRun
import qualified Lib.IncentiveJourney.Domain.Types.CohortDetails as DCD
import qualified Lib.IncentiveJourney.Domain.Types.CohortJourneyMapping as DCJM
import qualified Lib.IncentiveJourney.Domain.Types.Common as DIJC
import qualified Lib.IncentiveJourney.Domain.Types.IncentiveJourney as DIJ
import qualified Lib.IncentiveJourney.Domain.Types.IncentiveJourneyMilestone as DIJM
import qualified Lib.IncentiveJourney.Domain.Types.IncentiveJourneyStats as DIJS
import Lib.IncentiveJourney.Storage.Beam.BeamFlow (BeamFlow)
import qualified Lib.IncentiveJourney.Storage.Queries.AutoApplyCohortMappingExtra as QAuto
import qualified Lib.IncentiveJourney.Storage.Queries.BulkUserCohortMappingRunExtra as QBulkRunExtra
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
  when (req.rewardType == Common.SubscriptionWaiveOff) $
    requireSubscriptionWaiveOffConfig req.subscriptionWaiveOff
  let journeyId = ID.cast @Dashboard.Common.IncentiveJourney @DIJ.IncentiveJourney req.journeyId
  void $ QJourney.findById journeyId >>= fromMaybeM (InvalidRequest "Incentive journey not found")
  let (rewardValue, rewardExpirationAt, rewardMetadata) =
        resolveRewardStorage
          (toDomainRewardType req.rewardType)
          req.rewardValue
          req.rewardExpirationAt
          req.subscriptionWaiveOff
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
            rewardValue = rewardValue,
            rewardExpirationAt = rewardExpirationAt,
            rewardMetadata = rewardMetadata,
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
      subscriptionWaiveOff =
        case milestone.rewardType of
          DIJC.SubscriptionWaiveOff ->
            toApiSubscriptionWaiveOffFromStored
              milestone.rewardValue
              milestone.rewardExpirationAt
              milestone.rewardMetadata
          _ -> Nothing,
      timeBounds = milestone.timeBounds,
      createdAt = milestone.createdAt,
      updatedAt = milestone.updatedAt
    }

validateArea ::
  BeamFlow m r => Maybe DIJM.MilestoneAreaType -> Maybe [Text] -> m ()
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

validateMilestoneCondition ::
  BeamFlow m r => DIJM.IncentiveJourneyMilestone -> m ()
validateMilestoneCondition milestone =
  validateArea milestone.areaType milestone.specialLocationIds

validateMilestoneReward ::
  BeamFlow m r => DIJM.IncentiveJourneyMilestone -> m ()
validateMilestoneReward milestone =
  case milestone.rewardType of
    DIJC.Coins ->
      case milestone.rewardValue of
        Just coins | coins > 0 -> pure ()
        Just _ -> throwError (InvalidRequest "Coins milestone requires rewardValue > 0")
        Nothing -> throwError (InvalidRequest "Coins milestone requires rewardValue > 0")
    DIJC.Cash ->
      case milestone.rewardValue of
        Just amount | amount > 0 -> pure ()
        _ -> throwError (InvalidRequest "Cash milestone requires rewardValue > 0")
    DIJC.Coupons ->
      throwError (InvalidRequest "Coupons reward type is not supported")
    DIJC.WalletMoney ->
      throwError (InvalidRequest "WalletMoney reward type is not supported")
    DIJC.SubscriptionWaiveOff ->
      validateStoredSubscriptionWaiveOff
        milestone.rewardValue
        milestone.rewardExpirationAt
        milestone.rewardMetadata
    DIJC.PoolingPriority ->
      throwError (InvalidRequest "PoolingPriority reward type is not supported")
    DIJC.NoReward -> pure ()

-- | Validate streak-end fields when streakEndRewardType is set on cohort_journey_mapping.
validateStreakEndRewardOnMapping ::
  BeamFlow m r =>
  Maybe DIJC.MilestoneRewardType ->
  Maybe Int ->
  Maybe Int ->
  Maybe Value ->
  m ()
validateStreakEndRewardOnMapping mbRewardType mbRewardValue mbRewardExpirationAt mbRewardMetadata =
  case mbRewardType of
    Nothing -> pure ()
    Just DIJC.Coins ->
      case mbRewardValue of
        Just coins | coins > 0 -> pure ()
        _ -> throwError (InvalidRequest "Streak-end Coins reward requires streakEndRewardValue > 0")
    Just DIJC.Cash ->
      case mbRewardValue of
        Just amount | amount > 0 -> pure ()
        _ -> throwError (InvalidRequest "Streak-end Cash reward requires streakEndRewardValue > 0")
    Just DIJC.SubscriptionWaiveOff ->
      validateStoredSubscriptionWaiveOff mbRewardValue mbRewardExpirationAt mbRewardMetadata
    Just DIJC.NoReward -> pure ()
    Just other -> throwError (InvalidRequest $ show other <> " streak-end reward is not supported yet")

getIncentiveJourneyStatsHistory ::
  BeamFlow m r =>
  ServiceHandle m ->
  ID.ShortId DIJC.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  ID.Id Dashboard.Common.Driver ->
  Maybe (ID.Id Dashboard.Common.IncentiveJourney) ->
  Maybe Int ->
  Maybe Int ->
  Day ->
  Day ->
  m Common.IncentiveJourneyStatsHistoryRes
getIncentiveJourneyStatsHistory handle merchantShortId opCity driverId mbJourneyId mbLimit mbOffset fromDate toDate = do
  rows <-
    Core.statsHistory
      handle
      merchantShortId
      opCity
      (ID.cast @Dashboard.Common.Driver @DIJC.Person driverId)
      (ID.cast @Dashboard.Common.IncentiveJourney @DIJ.IncentiveJourney <$> mbJourneyId)
      mbLimit
      mbOffset
      fromDate
      toDate
  pure Common.IncentiveJourneyStatsHistoryRes {stats = map toStatsHistoryItem rows}

getIncentiveJourneyDriverAssignments ::
  BeamFlow m r =>
  ServiceHandle m ->
  ID.ShortId DIJC.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  ID.Id Dashboard.Common.Driver ->
  m Common.IncentiveJourneyDriverAssignmentListRes
getIncentiveJourneyDriverAssignments handle merchantShortId opCity driverId = do
  views <- Core.listAssignmentViews handle merchantShortId opCity (ID.cast @Dashboard.Common.Driver @DIJC.Person driverId)
  pure Common.IncentiveJourneyDriverAssignmentListRes {assignments = map toDriverAssignmentItem views}

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
      driverId = ID.cast @Dashboard.Common.Driver @DIJC.Person req.driverId
  journey <- QJourney.findById journeyId >>= fromMaybeM (InvalidRequest "Incentive journey not found")
  when (T.null req.periodKey) $ throwError (InvalidRequest "periodKey must be non-empty")
  waiveFn <- handle.waiveDriverMilestone & fromMaybeM (InvalidRequest "waiveDriverMilestone not configured")
  waiveFn driverId merchant.id merchantOpCityId journey milestoneId req.periodKey
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

getIncentiveJourneyCohortList ::
  BeamFlow m r =>
  ServiceHandle m ->
  ID.ShortId DIJC.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  Maybe Int ->
  Maybe Int ->
  Maybe Text ->
  Maybe Text ->
  m Common.CohortDetailsListRes
getIncentiveJourneyCohortList _handle _merchantShortId _opCity mbLimit mbOffset mbCohortName mbCohortCategory = do
  cohorts <- QCDExtra.listWithOptionalNameAndCategory mbLimit mbOffset mbCohortName mbCohortCategory
  pure
    Common.CohortDetailsListRes
      { cohorts =
          map
            ( \c ->
                Common.CohortDetailsListItem
                  { cohortId = ID.cast c.id,
                    name = c.name,
                    category = c.category,
                    description = c.description,
                    cohortRule = c.cohortRule,
                    createdAt = c.createdAt,
                    updatedAt = c.updatedAt
                  }
            )
            cohorts
      }

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
  when ((toDomainRewardType <$> req.streakEndRewardType) == Just DIJC.SubscriptionWaiveOff && isNothing req.streakEndSubscriptionWaiveOff) $
    throwError (InvalidRequest "streakEndSubscriptionWaiveOff is required when streakEndRewardType is SubscriptionWaiveOff")
  let enabled = fromMaybe True req.enabled
      maxWaiveOffCount = Just (fromMaybe 1 req.maxWaiveOffCount)
  whenJust maxWaiveOffCount $ \n ->
    when (n < 0) $ throwError (InvalidRequest "maxWaiveOffCount must be >= 0")
  let (streakEndRewardValue, streakEndRewardExpirationAt, streakEndRewardMetadata) =
        resolveRewardStorage
          (maybe DIJC.NoReward toDomainRewardType req.streakEndRewardType)
          req.streakEndRewardValue
          req.streakEndRewardExpirationAt
          req.streakEndSubscriptionWaiveOff
  validateStreakEndRewardOnMapping
    (toDomainRewardType <$> req.streakEndRewardType)
    streakEndRewardValue
    streakEndRewardExpirationAt
    streakEndRewardMetadata
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
      streakEndRewardValue
      streakEndRewardExpirationAt
      streakEndRewardMetadata
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
  whenJust maxWaiveOffCount $ \n ->
    when (n < 0) $ throwError (InvalidRequest "maxWaiveOffCount must be >= 0")
  when (streakEndRewardType == Just DIJC.SubscriptionWaiveOff) $
    requireSubscriptionWaiveOffConfig
      ( req.streakEndSubscriptionWaiveOff
          <|> toApiSubscriptionWaiveOffFromStored
            cjm.streakEndRewardValue
            cjm.streakEndRewardExpirationAt
            cjm.streakEndRewardMetadata
      )
  let (streakEndRewardValue, streakEndRewardExpirationAt, streakEndRewardMetadata) =
        resolveRewardStorageUpdate
          (fromMaybe DIJC.NoReward streakEndRewardType)
          cjm.streakEndRewardValue
          cjm.streakEndRewardExpirationAt
          cjm.streakEndRewardMetadata
          req.streakEndRewardValue
          req.streakEndRewardExpirationAt
          req.streakEndSubscriptionWaiveOff
  when (streakRange <= 0) $ throwError (InvalidRequest "streakRange must be > 0")
  case IJStreak.validateMappingStartDate journey.journeyType startDate of
    Left err -> throwError (InvalidRequest err)
    Right () -> pure ()
  validateStreakEndRewardOnMapping streakEndRewardType streakEndRewardValue streakEndRewardExpirationAt streakEndRewardMetadata
  void $
    QCJMExtra.updateCohortJourneyMappingFields
      cjm{DCJM.startDate = startDate,
          DCJM.streakRange = streakRange,
          DCJM.enabled = enabled,
          DCJM.maxWaiveOffCount = maxWaiveOffCount,
          DCJM.streakEndRewardType = streakEndRewardType,
          DCJM.streakEndRewardValue = streakEndRewardValue,
          DCJM.streakEndRewardExpirationAt = streakEndRewardExpirationAt,
          DCJM.streakEndRewardMetadata = streakEndRewardMetadata
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
  Maybe (ID.Id Dashboard.Common.CohortDetails) ->
  Maybe Text ->
  Maybe Bool ->
  Maybe Common.IncentiveJourneyType ->
  m Common.CohortJourneyMappingListRes
getIncentiveJourneyCohortJourneyList handle merchantShortId opCity mbLimit mbOffset mbCohortName mbCohortId mbCohortCategory mbIsActive mbJourneyType = do
  rows <-
    Core.listCohortJourneyRows
      handle
      merchantShortId
      opCity
      mbLimit
      mbOffset
      (ID.cast @Dashboard.Common.CohortDetails @DCD.CohortDetails <$> mbCohortId)
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
          streakEndSubscriptionWaiveOff =
            case cjm.streakEndRewardType of
              Just DIJC.SubscriptionWaiveOff ->
                toApiSubscriptionWaiveOffFromStored
                  cjm.streakEndRewardValue
                  cjm.streakEndRewardExpirationAt
                  cjm.streakEndRewardMetadata
              _ -> Nothing,
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
    (ID.cast @Dashboard.Common.Driver @DIJC.Person req.driverId)
    (ID.cast @Dashboard.Common.CohortJourneyMapping @DCJM.CohortJourneyMapping req.cohortJourneyMappingId)
    req.enabled
    req.validTill
    "Driver not found"
    "Driver does not belong to this merchant/city"
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
    (ID.cast @Dashboard.Common.Driver @DIJC.Person req.driverId)
    (ID.cast @Dashboard.Common.CohortJourneyMapping @DCJM.CohortJourneyMapping req.cohortJourneyMappingId)
    "Driver not found"
    "Driver does not belong to this merchant/city"
  pure Success

postIncentiveJourneyAssignBulkFromS3 ::
  BeamFlow m r =>
  ServiceHandle m ->
  ID.ShortId DIJC.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  Common.BulkAssignUserCohortFromS3Req ->
  m Common.BulkAssignUserCohortFromS3Res
postIncentiveJourneyAssignBulkFromS3 handle merchantShortId opCity req = do
  (merchant, merchantOpCityId) <- resolveMerchant handle merchantShortId opCity
  when (T.null (T.strip req.s3FilePath)) $
    throwError (InvalidRequest "s3FilePath must not be empty")
  now <- getCurrentTime
  when (req.scheduledAt < now) $
    throwError (InvalidRequest "scheduledAt must be now or in the future")
  let batchSize = SH.clampBatchSize $ fromMaybe SH.defaultBatchSize req.batchSize
      delaySecs = max 0 $ fromMaybe SH.defaultRescheduleDelaySeconds req.rescheduleDelaySeconds
      s3Path = T.strip req.s3FilePath
  runIdText <- generateGUIDText
  let runId = ID.Id runIdText
  -- Domain run row first so list API can show the schedule even before the first tick.
  QBulkRunExtra.createRun $
    DBulkRun.BulkUserCohortMappingRun
      { id = runId,
        merchantId = ID.cast merchant.id,
        merchantOperatingCityId = ID.cast merchantOpCityId,
        s3FilePath = s3Path,
        scheduledAt = req.scheduledAt,
        status = DBulkRun.Scheduled,
        fileOffset = 0,
        batchSize = batchSize,
        rescheduleDelaySeconds = delaySecs,
        totalRows = Nothing,
        rowsInserted = 0,
        rowsSkipped = 0,
        currentSchedulerJobId = Nothing,
        errorMessage = Nothing,
        createdAt = now,
        updatedAt = now
      }
  scheduleFn <- handle.scheduleBulkUpload & fromMaybeM (InvalidRequest "scheduleBulkUpload not configured")
  scheduleFn merchant.id merchantOpCityId s3Path req.scheduledAt batchSize delaySecs runIdText
  pure
    Common.BulkAssignUserCohortFromS3Res
      { runId = runIdText,
        scheduledAt = req.scheduledAt,
        status = Common.Scheduled,
        offset = 0,
        batchSize = batchSize,
        rescheduleDelaySeconds = delaySecs
      }

getIncentiveJourneyAssignBulkFromS3List ::
  BeamFlow m r =>
  ServiceHandle m ->
  ID.ShortId DIJC.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  Maybe Int ->
  Maybe Int ->
  Maybe Common.BulkUserCohortMappingRunStatus ->
  m Common.BulkAssignUserCohortFromS3ListRes
getIncentiveJourneyAssignBulkFromS3List handle merchantShortId opCity mbLimit mbOffset mbStatus = do
  (_merchant, merchantOpCityId) <- resolveMerchant handle merchantShortId opCity
  let limitVal = Just $ fromMaybe 20 mbLimit
      offsetVal = Just $ fromMaybe 0 mbOffset
  runs <-
    QBulkRunExtra.findByMerchantOperatingCityIdAndMaybeStatus
      limitVal
      offsetVal
      (ID.cast merchantOpCityId)
      (toDomainBulkRunStatus <$> mbStatus)
  pure
    Common.BulkAssignUserCohortFromS3ListRes
      { runs = map toBulkAssignListItem runs
      }

toBulkAssignListItem :: DBulkRun.BulkUserCohortMappingRun -> Common.BulkAssignUserCohortFromS3ListItem
toBulkAssignListItem run =
  Common.BulkAssignUserCohortFromS3ListItem
    { runId = ID.getId run.id,
      s3FilePath = run.s3FilePath,
      scheduledAt = run.scheduledAt,
      status = toApiBulkRunStatus run.status,
      offset = run.fileOffset,
      batchSize = run.batchSize,
      rescheduleDelaySeconds = run.rescheduleDelaySeconds,
      totalRows = run.totalRows,
      rowsInserted = run.rowsInserted,
      rowsSkipped = run.rowsSkipped,
      currentSchedulerJobId = run.currentSchedulerJobId,
      errorMessage = run.errorMessage,
      createdAt = run.createdAt,
      updatedAt = run.updatedAt
    }

toApiBulkRunStatus :: DBulkRun.BulkUserCohortMappingRunStatus -> Common.BulkUserCohortMappingRunStatus
toApiBulkRunStatus = \case
  DBulkRun.Scheduled -> Common.Scheduled
  DBulkRun.Running -> Common.Running
  DBulkRun.Succeeded -> Common.Succeeded
  DBulkRun.Failed -> Common.Failed
  DBulkRun.Cancelled -> Common.Cancelled

toDomainBulkRunStatus :: Common.BulkUserCohortMappingRunStatus -> DBulkRun.BulkUserCohortMappingRunStatus
toDomainBulkRunStatus status = case status of
  Common.Scheduled -> DBulkRun.Scheduled
  Common.Running -> DBulkRun.Running
  Common.Succeeded -> DBulkRun.Succeeded
  Common.Failed -> DBulkRun.Failed
  Common.Cancelled -> DBulkRun.Cancelled

toDriverAssignmentItem :: Core.AssignmentView -> Common.IncentiveJourneyDriverAssignmentItem
toDriverAssignmentItem assignmentView =
  Common.IncentiveJourneyDriverAssignmentItem
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
      driverId = ID.cast stats.personId,
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

requireSubscriptionWaiveOffConfig ::
  BeamFlow m r => Maybe Common.SubscriptionWaiveOffConfig -> m ()
requireSubscriptionWaiveOffConfig = \case
  Nothing -> throwError (InvalidRequest "subscriptionWaiveOff is required when rewardType is SubscriptionWaiveOff")
  Just cfg -> validateSubscriptionWaiveOffConfig cfg

validateSubscriptionWaiveOffConfig ::
  BeamFlow m r => Common.SubscriptionWaiveOffConfig -> m ()
validateSubscriptionWaiveOffConfig cfg = do
  when (cfg.percentage <= 0 || cfg.percentage > 100) $
    throwError (InvalidRequest "subscription waive-off percentage must be between 1 and 100")
  when (cfg.daysValidFor <= 0) $
    throwError (InvalidRequest "subscription waive-off daysValidFor must be > 0")
  unless (isValidSubscriptionServiceName cfg.serviceName) $
    throwError (InvalidRequest "subscription waive-off serviceName must be YATRI_SUBSCRIPTION, PREPAID_SUBSCRIPTION, YATRI_RENTAL, or DASHCAM_RENTAL_<provider>")
  unless (cfg.waiveOffMode `elem` ["WITH_OFFER", "WITHOUT_OFFER"]) $
    throwError (InvalidRequest "subscription waive-off waiveOffMode must be WITH_OFFER or WITHOUT_OFFER")

-- | Validate SWO stored as rewardValue + rewardExpirationAt + rewardMetadata.
validateStoredSubscriptionWaiveOff ::
  BeamFlow m r => Maybe Int -> Maybe Int -> Maybe Value -> m ()
validateStoredSubscriptionWaiveOff mbPercentage mbDaysValidFor mbMetadata =
  case IJ.mkSubscriptionWaiveOffSpec mbPercentage mbDaysValidFor mbMetadata of
    Nothing ->
      throwError (InvalidRequest "SubscriptionWaiveOff requires rewardValue (percentage), rewardExpirationAt (daysValidFor), and rewardMetadata {serviceName, waiveOffMode}")
    Just spec ->
      validateSubscriptionWaiveOffConfig
        Common.SubscriptionWaiveOffConfig
          { percentage = spec.percentage,
            daysValidFor = spec.daysValidFor,
            serviceName = spec.serviceName,
            waiveOffMode = spec.waiveOffMode
          }

toApiSubscriptionWaiveOffFromStored :: Maybe Int -> Maybe Int -> Maybe Value -> Maybe Common.SubscriptionWaiveOffConfig
toApiSubscriptionWaiveOffFromStored mbPercentage mbDaysValidFor mbMetadata =
  fmap
    ( \spec ->
        Common.SubscriptionWaiveOffConfig
          { percentage = spec.percentage,
            daysValidFor = spec.daysValidFor,
            serviceName = spec.serviceName,
            waiveOffMode = spec.waiveOffMode
          }
    )
    (IJ.mkSubscriptionWaiveOffSpec mbPercentage mbDaysValidFor mbMetadata)

resolveRewardStorage ::
  DIJC.MilestoneRewardType ->
  Maybe Int ->
  Maybe Int ->
  Maybe Common.SubscriptionWaiveOffConfig ->
  (Maybe Int, Maybe Int, Maybe Value)
resolveRewardStorage rewardType mbValue mbExpiration mbWaive =
  case rewardType of
    DIJC.SubscriptionWaiveOff ->
      case mbWaive of
        Just cfg ->
          ( Just cfg.percentage,
            Just cfg.daysValidFor,
            Just $ IJ.encodeSubscriptionWaiveOffMetadata cfg.serviceName cfg.waiveOffMode
          )
        Nothing -> (mbValue, mbExpiration, Nothing)
    _ -> (mbValue, mbExpiration, Nothing)

resolveRewardStorageUpdate ::
  DIJC.MilestoneRewardType ->
  Maybe Int ->
  Maybe Int ->
  Maybe Value ->
  Maybe Int ->
  Maybe Int ->
  Maybe Common.SubscriptionWaiveOffConfig ->
  (Maybe Int, Maybe Int, Maybe Value)
resolveRewardStorageUpdate rewardType existingValue existingExpiration existingMetadata mbValue mbExpiration mbWaive =
  case rewardType of
    DIJC.SubscriptionWaiveOff ->
      case mbWaive of
        Just cfg ->
          ( Just cfg.percentage,
            Just cfg.daysValidFor,
            Just $ IJ.encodeSubscriptionWaiveOffMetadata cfg.serviceName cfg.waiveOffMode
          )
        Nothing ->
          ( mbValue <|> existingValue,
            mbExpiration <|> existingExpiration,
            existingMetadata
          )
    _ ->
      ( mbValue <|> existingValue,
        mbExpiration <|> existingExpiration,
        Nothing
      )

postIncentiveJourneyAutoApplyCohortCreate ::
  BeamFlow m r =>
  ServiceHandle m ->
  ID.ShortId DIJC.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  Common.CreateAutoApplyCohortMappingReq ->
  m Common.CreateAutoApplyCohortMappingRes
postIncentiveJourneyAutoApplyCohortCreate handle merchantShortId opCity req = do
  (merchant, merchantOpCityId) <- resolveMerchant handle merchantShortId opCity
  let cjmId = ID.cast @Dashboard.Common.CohortJourneyMapping @DCJM.CohortJourneyMapping req.cohortJourneyMappingId
  cjm <- QCJMExtra.findCohortJourneyMappingById cjmId >>= fromMaybeM (InvalidRequest "Cohort journey mapping not found")
  unless (cjm.merchantId == ID.cast merchant.id && cjm.merchantOperatingCityId == ID.cast merchantOpCityId) $
    throwError (InvalidRequest "Cohort journey mapping does not belong to this merchant/city")
  mbExisting <- QAuto.findByCohortJourneyMappingIdAndVehicleCategory cjmId req.vehicleCategory
  when (isJust mbExisting) $
    throwError (InvalidRequest "Auto apply cohort mapping already exists for this cohort journey mapping and vehicle category")
  row <-
    QAuto.createAutoApplyCohortMapping
      cjmId
      (ID.cast merchant.id)
      (ID.cast merchantOpCityId)
      req.vehicleCategory
      req.allowIfNoMapping
      req.enabled
  handle.clearAutoApplyCacheByMerchantAndCity (ID.cast merchant.id) (ID.cast merchantOpCityId) [req.vehicleCategory]
  pure Common.CreateAutoApplyCohortMappingRes {autoApplyCohortMappingId = ID.cast row.id}

putIncentiveJourneyAutoApplyCohortUpdate ::
  BeamFlow m r =>
  ServiceHandle m ->
  ID.ShortId DIJC.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  Common.UpdateAutoApplyCohortMappingReq ->
  m APISuccess
putIncentiveJourneyAutoApplyCohortUpdate handle merchantShortId opCity req = do
  (merchant, merchantOpCityId) <- resolveMerchant handle merchantShortId opCity
  let rowId = ID.cast @Dashboard.Common.AutoApplyCohortMapping @DAuto.AutoApplyCohortMapping req.autoApplyCohortMappingId
  row <- QAuto.findAutoApplyCohortMappingById rowId >>= fromMaybeM (InvalidRequest "Auto apply cohort mapping not found")
  unless (row.merchantId == ID.cast merchant.id && row.merchantOperatingCityId == ID.cast merchantOpCityId) $
    throwError (InvalidRequest "Auto apply cohort mapping does not belong to this merchant/city")
  cjm <-
    QCJMExtra.findCohortJourneyMappingById row.cohortJourneyMappingId
      >>= fromMaybeM (InvalidRequest "Cohort journey mapping not found")
  unless (cjm.merchantId == ID.cast merchant.id && cjm.merchantOperatingCityId == ID.cast merchantOpCityId) $
    throwError (InvalidRequest "Cohort journey mapping does not belong to this merchant/city")
  vehicleCategory <- case (req.allVehicleCategories, req.vehicleCategory) of
    (Just True, Just _) ->
      throwError (InvalidRequest "Set either vehicleCategory or allVehicleCategories, not both")
    (Just True, _) -> pure Nothing
    (_, Just category) -> pure (Just category)
    _ -> pure row.vehicleCategory
  let allowIfNoMapping = fromMaybe row.allowIfNoMapping req.allowIfNoMapping
      enabled = fromMaybe row.enabled req.enabled
  when (vehicleCategory /= row.vehicleCategory) $ do
    mbExisting <- QAuto.findByCohortJourneyMappingIdAndVehicleCategory row.cohortJourneyMappingId vehicleCategory
    when (maybe False (\existing -> existing.id /= row.id) mbExisting) $
      throwError (InvalidRequest "Auto apply cohort mapping already exists for this cohort journey mapping and vehicle category")
  void $
    QAuto.updateAutoApplyCohortMapping
      row{DAuto.vehicleCategory = vehicleCategory,
          DAuto.allowIfNoMapping = allowIfNoMapping,
          DAuto.enabled = enabled
         }
  let categoriesToClear =
        if row.vehicleCategory == vehicleCategory
          then [vehicleCategory]
          else [row.vehicleCategory, vehicleCategory]
  handle.clearAutoApplyCacheByMerchantAndCity row.merchantId row.merchantOperatingCityId categoriesToClear
  pure Success

getIncentiveJourneyAutoApplyCohortList ::
  BeamFlow m r =>
  ServiceHandle m ->
  ID.ShortId DIJC.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  Maybe Int ->
  Maybe Int ->
  Maybe DTV.VehicleCategory ->
  m Common.AutoApplyCohortMappingListRes
getIncentiveJourneyAutoApplyCohortList handle merchantShortId opCity mbLimit mbOffset mbVehicleCategory = do
  (merchant, merchantOpCityId) <- resolveMerchant handle merchantShortId opCity
  rows <-
    QAuto.findByMerchantAndCity
      (Just $ fromMaybe 20 mbLimit)
      (Just $ fromMaybe 0 mbOffset)
      (ID.cast merchant.id)
      (ID.cast merchantOpCityId)
      mbVehicleCategory
  pure
    Common.AutoApplyCohortMappingListRes
      { mappings =
          map
            ( \row ->
                Common.AutoApplyCohortMappingListItem
                  { autoApplyCohortMappingId = ID.cast row.id,
                    cohortJourneyMappingId = ID.cast row.cohortJourneyMappingId,
                    merchantId = row.merchantId.getId,
                    merchantOperatingCityId = row.merchantOperatingCityId.getId,
                    vehicleCategory = row.vehicleCategory,
                    allowIfNoMapping = row.allowIfNoMapping,
                    enabled = row.enabled,
                    createdAt = row.createdAt,
                    updatedAt = row.updatedAt
                  }
            )
            rows
      }

isValidSubscriptionServiceName :: Text -> Bool
isValidSubscriptionServiceName serviceName =
  serviceName `elem` ["YATRI_SUBSCRIPTION", "PREPAID_SUBSCRIPTION", "YATRI_RENTAL"]
    || T.isPrefixOf "DASHCAM_RENTAL_" serviceName
