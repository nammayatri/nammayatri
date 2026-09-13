module Domain.Action.Dashboard.IncentiveJourney
  ( getIncentiveJourneyList,
    postIncentiveJourneyCreate,
    putIncentiveJourneyUpdate,
    getIncentiveJourneyMilestoneList,
    postIncentiveJourneyMilestoneCreate,
    putIncentiveJourneyMilestoneUpdate,
    getIncentiveJourneyStatsHistory,
    postIncentiveJourneyStatsWaiveOff,
    postIncentiveJourneyCohortCreate,
    postIncentiveJourneyCohortJourneyCreate,
    putIncentiveJourneyCohortJourneyUpdate,
    postIncentiveJourneyAssign,
    deleteIncentiveJourneyUnassign,
  )
where

import qualified API.Types.RiderPlatform.Management.IncentiveJourney as Common
import qualified Dashboard.Common
import Data.List (sortOn)
import qualified Data.Text as T
import Data.Time (Day)
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.Person as DP
import qualified Environment
import EulerHS.Prelude hiding (id, sortOn)
import Kernel.Types.APISuccess (APISuccess (Success))
import qualified Kernel.Types.Beckn.Context
import Kernel.Types.Error (GenericError (InvalidRequest))
import qualified Kernel.Types.Id as ID
import Kernel.Utils.Common
import Lib.ConfigPilot.Interface.Getter (invalidateConfigInMem)
import Lib.ConfigPilot.Interface.Types (getConfig)
import qualified Lib.IncentiveJourney.Domain.Types.CohortDetails as DCD
import qualified Lib.IncentiveJourney.Domain.Types.CohortJourneyMapping as DCJM
import qualified Lib.IncentiveJourney.Domain.Types.Common as DIJC
import qualified Lib.IncentiveJourney.Domain.Types.IncentiveJourney as DIJ
import qualified Lib.IncentiveJourney.Domain.Types.IncentiveJourneyMilestone as DIJM
import qualified Lib.IncentiveJourney.Domain.Types.IncentiveJourneyStats as DIJS
import qualified Lib.IncentiveJourney.Storage.Queries.CohortDetailsExtra as QCDExtra
import qualified Lib.IncentiveJourney.Storage.Queries.CohortJourneyMappingExtra as QCJMExtra
import qualified Lib.IncentiveJourney.Storage.Queries.IncentiveJourney as QJourney
import qualified Lib.IncentiveJourney.Storage.Queries.IncentiveJourneyMilestone as QMilestone
import qualified Lib.IncentiveJourney.Storage.Queries.UserCohortMappingExtra as QUCMExtra
import qualified Lib.IncentiveJourney.Streak as IJStreak
import Lib.Yudhishthira.Types.ConfigPilot (ConfigType (..))
import qualified SharedLogic.IncentiveJourney as SLJourney
import SharedLogic.Merchant (findMerchantByShortId)
import Storage.Beam.IncentiveJourney ()
import Storage.Beam.Yudhishthira ()
import qualified Storage.CachedQueries.IncentiveJourney as CQJourney
import qualified Storage.CachedQueries.IncentiveJourneyMilestone as CQMilestone
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import Storage.ConfigPilot.Config.IncentiveJourney (IncentiveJourneyDimensions (..))
import Storage.ConfigPilot.Config.IncentiveJourneyMilestone (IncentiveJourneyMilestoneDimensions (..))
import Storage.ConfigPilot.Config.RiderConfig (RiderConfigDimensions (..))
import qualified Storage.Queries.IncentiveJourneyStatsExtra as QStats

getIncentiveJourneyList ::
  ID.ShortId DM.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  Maybe Int ->
  Maybe Int ->
  Maybe Bool ->
  Environment.Flow Common.IncentiveJourneyListRes
getIncentiveJourneyList merchantShortId opCity mbLimit mbOffset mbEnabled = do
  merchant <- findMerchantByShortId merchantShortId
  merchantOpCityId <- CQMOC.getMerchantOpCityId merchant (Just opCity)
  journeys <-
    case mbEnabled of
      Just True ->
        getConfig
          ( IncentiveJourneyDimensions
              { merchantOperatingCityId = merchantOpCityId.getId,
                journeyId = Nothing,
                enabled = Just True
              }
          )
          (Just $ CQJourney.findEnabledByMerchantOperatingCityId merchantOpCityId)
      Just False ->
        filter (not . (.enabled))
          <$> getConfig
            ( IncentiveJourneyDimensions
                { merchantOperatingCityId = merchantOpCityId.getId,
                  journeyId = Nothing,
                  enabled = Nothing
                }
            )
            (Just $ CQJourney.findByMerchantOperatingCityId merchantOpCityId)
      Nothing ->
        getConfig
          ( IncentiveJourneyDimensions
              { merchantOperatingCityId = merchantOpCityId.getId,
                journeyId = Nothing,
                enabled = Nothing
              }
          )
          (Just $ CQJourney.findByMerchantOperatingCityId merchantOpCityId)
  let limitVal = fromMaybe 20 mbLimit
      offsetVal = fromMaybe 0 mbOffset
      page = take limitVal . drop offsetVal $ journeys
  pure Common.IncentiveJourneyListRes {journeys = map toJourneyListItem page}

postIncentiveJourneyCreate ::
  ID.ShortId DM.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  Common.CreateIncentiveJourneyReq ->
  Environment.Flow Common.CreateIncentiveJourneyRes
postIncentiveJourneyCreate merchantShortId opCity req = do
  merchant <- findMerchantByShortId merchantShortId
  merchantOpCityId <- CQMOC.getMerchantOpCityId merchant (Just opCity)
  now <- getCurrentTime
  journeyId <- generateGUID
  let journey =
        DIJ.IncentiveJourney
          { id = journeyId,
            merchantId = ID.cast merchant.id,
            merchantOperatingCityId = ID.cast merchantOpCityId,
            name = req.name,
            description = req.description,
            journeyType = Just (toDomainJourneyType req.journeyType),
            enabled = req.enabled,
            maxWaiveOffCount = Just (fromMaybe 1 req.maxWaiveOffCount),
            createdAt = now,
            updatedAt = now
          }
  whenJust journey.maxWaiveOffCount $ \n ->
    when (n < 0) $ throwError (InvalidRequest "maxWaiveOffCount must be >= 0")
  QJourney.create journey
  CQJourney.clearCache journey
  invalidateConfigInMem IncentiveJourneyConfigRider
  pure Common.CreateIncentiveJourneyRes {journeyId = ID.cast journeyId}

putIncentiveJourneyUpdate ::
  ID.ShortId DM.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  Common.UpdateIncentiveJourneyReq ->
  Environment.Flow APISuccess
putIncentiveJourneyUpdate merchantShortId opCity req = do
  merchant <- findMerchantByShortId merchantShortId
  merchantOpCityId <- CQMOC.getMerchantOpCityId merchant (Just opCity)
  let journeyId = ID.cast @Dashboard.Common.IncentiveJourney @DIJ.IncentiveJourney req.journeyId
  journey <- QJourney.findById journeyId >>= fromMaybeM (InvalidRequest "Incentive journey not found")
  unless (journey.merchantOperatingCityId == ID.cast merchantOpCityId && journey.merchantId == ID.cast merchant.id) $
    throwError (InvalidRequest "Incentive journey does not belong to this merchant/city")
  let updated =
        journey
          { DIJ.name = fromMaybe journey.name req.name,
            DIJ.description = maybe journey.description Just req.description,
            DIJ.journeyType = maybe journey.journeyType (Just . toDomainJourneyType) req.journeyType,
            DIJ.enabled = fromMaybe journey.enabled req.enabled,
            DIJ.maxWaiveOffCount = maybe journey.maxWaiveOffCount Just req.maxWaiveOffCount
          }
  whenJust updated.maxWaiveOffCount $ \n ->
    when (n < 0) $ throwError (InvalidRequest "maxWaiveOffCount must be >= 0")
  QJourney.updateByPrimaryKey updated
  CQJourney.clearCache updated
  invalidateConfigInMem IncentiveJourneyConfigRider
  pure Success

getIncentiveJourneyMilestoneList ::
  ID.ShortId DM.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  Maybe Int ->
  Maybe Int ->
  ID.Id Dashboard.Common.IncentiveJourney ->
  Environment.Flow Common.IncentiveJourneyMilestoneListRes
getIncentiveJourneyMilestoneList merchantShortId opCity mbLimit mbOffset dashboardJourneyId = do
  merchant <- findMerchantByShortId merchantShortId
  merchantOpCityId <- CQMOC.getMerchantOpCityId merchant (Just opCity)
  let journeyId = ID.cast @Dashboard.Common.IncentiveJourney @DIJ.IncentiveJourney dashboardJourneyId
  journey <- QJourney.findById journeyId >>= fromMaybeM (InvalidRequest "Incentive journey not found")
  unless (journey.merchantOperatingCityId == ID.cast merchantOpCityId && journey.merchantId == ID.cast merchant.id) $
    throwError (InvalidRequest "Incentive journey does not belong to this merchant/city")
  milestones <-
    getConfig
      ( IncentiveJourneyMilestoneDimensions
          { merchantOperatingCityId = merchantOpCityId.getId,
            journeyId = Just journeyId,
            milestoneId = Nothing
          }
      )
      (Just $ CQMilestone.findByJourneyId journeyId)
  let limitVal = fromMaybe 20 mbLimit
      offsetVal = fromMaybe 0 mbOffset
      page = take limitVal . drop offsetVal $ sortOn (.order) milestones
  pure Common.IncentiveJourneyMilestoneListRes {milestones = map toMilestoneListItem page}

postIncentiveJourneyMilestoneCreate ::
  ID.ShortId DM.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  Common.CreateIncentiveJourneyMilestoneReq ->
  Environment.Flow Common.CreateIncentiveJourneyMilestoneRes
postIncentiveJourneyMilestoneCreate merchantShortId opCity req = do
  merchant <- findMerchantByShortId merchantShortId
  merchantOpCityId <- CQMOC.getMerchantOpCityId merchant (Just opCity)
  let journeyId = ID.cast @Dashboard.Common.IncentiveJourney @DIJ.IncentiveJourney req.journeyId
  journey <- QJourney.findById journeyId >>= fromMaybeM (InvalidRequest "Incentive journey not found")
  unless (journey.merchantOperatingCityId == ID.cast merchantOpCityId && journey.merchantId == ID.cast merchant.id) $
    throwError (InvalidRequest "Incentive journey does not belong to this merchant/city")
  when (req.conditionValue < 0) $
    throwError (InvalidRequest "conditionValue must be >= 0")
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
            conditionOperator = Just (toDomainConditionOperator req.conditionOperator),
            conditionValue = req.conditionValue,
            areaType = toDomainAreaType <$> req.areaType,
            specialLocationIds = req.specialLocationIds,
            vehicleCategory = req.vehicleCategory,
            serviceTierType = req.serviceTierType,
            rewardType = toDomainRewardType req.rewardType,
            rewardValue = req.rewardValue,
            rewardExpirationAt = req.rewardExpirationAt,
            timeBounds = req.timeBounds,
            createdAt = now,
            updatedAt = now,
            merchantId = Just (ID.cast merchant.id),
            merchantOperatingCityId = Just (ID.cast merchantOpCityId)
          }
  validateMilestoneCondition milestone
  validateMilestoneReward journey milestone
  QMilestone.create milestone
  CQMilestone.clearCacheByJourneyId journeyId
  invalidateConfigInMem IncentiveJourneyMilestoneConfigRider
  pure Common.CreateIncentiveJourneyMilestoneRes {milestoneId = ID.cast milestoneId}

putIncentiveJourneyMilestoneUpdate ::
  ID.ShortId DM.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  Common.UpdateIncentiveJourneyMilestoneReq ->
  Environment.Flow APISuccess
putIncentiveJourneyMilestoneUpdate merchantShortId opCity req = do
  merchant <- findMerchantByShortId merchantShortId
  merchantOpCityId <- CQMOC.getMerchantOpCityId merchant (Just opCity)
  let milestoneId = ID.cast @Dashboard.Common.IncentiveJourneyMilestone @DIJM.IncentiveJourneyMilestone req.milestoneId
  milestone <- QMilestone.findById milestoneId >>= fromMaybeM (InvalidRequest "Incentive journey milestone not found")
  journey <- QJourney.findById milestone.journeyId >>= fromMaybeM (InvalidRequest "Incentive journey not found")
  unless (journey.merchantOperatingCityId == ID.cast merchantOpCityId && journey.merchantId == ID.cast merchant.id) $
    throwError (InvalidRequest "Incentive journey does not belong to this merchant/city")
  whenJust req.conditionValue $ \v ->
    when (v < 0) $ throwError (InvalidRequest "conditionValue must be >= 0")
  let updated =
        milestone
          { DIJM.name = maybe milestone.name Just req.name,
            DIJM.description = maybe milestone.description Just req.description,
            DIJM.order = fromMaybe milestone.order req.order,
            DIJM.conditionType = maybe milestone.conditionType toDomainConditionType req.conditionType,
            DIJM.conditionOperator = maybe milestone.conditionOperator (Just . toDomainConditionOperator) req.conditionOperator,
            DIJM.conditionValue = fromMaybe milestone.conditionValue req.conditionValue,
            DIJM.areaType = maybe milestone.areaType (Just . toDomainAreaType) req.areaType,
            DIJM.specialLocationIds = maybe milestone.specialLocationIds Just req.specialLocationIds,
            DIJM.vehicleCategory = maybe milestone.vehicleCategory Just req.vehicleCategory,
            DIJM.serviceTierType = maybe milestone.serviceTierType Just req.serviceTierType,
            DIJM.rewardType = maybe milestone.rewardType toDomainRewardType req.rewardType,
            DIJM.rewardValue = maybe milestone.rewardValue Just req.rewardValue,
            DIJM.rewardExpirationAt = maybe milestone.rewardExpirationAt Just req.rewardExpirationAt,
            DIJM.timeBounds = maybe milestone.timeBounds Just req.timeBounds
          }
  validateMilestoneCondition updated
  validateMilestoneReward journey updated
  QMilestone.updateByPrimaryKey updated
  CQMilestone.clearCacheByJourneyId milestone.journeyId
  invalidateConfigInMem IncentiveJourneyMilestoneConfigRider
  pure Success

getIncentiveJourneyStatsHistory ::
  ID.ShortId DM.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  Maybe (ID.Id Dashboard.Common.IncentiveJourney) ->
  Maybe Int ->
  Maybe Int ->
  Day ->
  ID.Id Dashboard.Common.Person ->
  Day ->
  Environment.Flow Common.IncentiveJourneyStatsHistoryRes
getIncentiveJourneyStatsHistory merchantShortId opCity mbJourneyId mbLimit mbOffset fromDate personId toDate = do
  merchant <- findMerchantByShortId merchantShortId
  merchantOpCityId <- CQMOC.getMerchantOpCityId merchant (Just opCity)
  riderConfig <-
    getConfig (RiderConfigDimensions {merchantOperatingCityId = merchantOpCityId.getId}) Nothing
      >>= fromMaybeM (InvalidRequest "RiderConfig not found")
  when (toDate < fromDate) $ throwError (InvalidRequest "toDate must be >= fromDate")
  let (dayStart, _) = QStats.mkLocalDayUtcBounds fromDate riderConfig.timeDiffFromUtc
      (_, dayEndExclusive) = QStats.mkLocalDayUtcBounds toDate riderConfig.timeDiffFromUtc
  rows <-
    QStats.findHistoryByPersonIdAndCreatedAtRange
      (ID.cast @Dashboard.Common.Person @DIJC.Person personId)
      dayStart
      dayEndExclusive
      mbLimit
      mbOffset
  let filtered =
        case mbJourneyId of
          Nothing -> rows
          Just jId -> filter (\s -> s.journeyId == ID.cast jId) rows
  pure Common.IncentiveJourneyStatsHistoryRes {stats = map toStatsHistoryItem filtered}

postIncentiveJourneyStatsWaiveOff ::
  ID.ShortId DM.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  Common.WaiveIncentiveJourneyMilestoneReq ->
  Environment.Flow APISuccess
postIncentiveJourneyStatsWaiveOff merchantShortId opCity req = do
  merchant <- findMerchantByShortId merchantShortId
  merchantOpCityId <- CQMOC.getMerchantOpCityId merchant (Just opCity)
  let journeyId = ID.cast @Dashboard.Common.IncentiveJourney @DIJ.IncentiveJourney req.journeyId
      milestoneId = ID.cast @Dashboard.Common.IncentiveJourneyMilestone @DIJM.IncentiveJourneyMilestone req.milestoneId
      personId = ID.cast @Dashboard.Common.Person @DP.Person req.personId
  journey <- QJourney.findById journeyId >>= fromMaybeM (InvalidRequest "Incentive journey not found")
  unless (journey.merchantOperatingCityId == ID.cast merchantOpCityId && journey.merchantId == ID.cast merchant.id) $
    throwError (InvalidRequest "Incentive journey does not belong to this merchant/city")
  when (null req.periodKey) $ throwError (InvalidRequest "periodKey must be non-empty")
  SLJourney.waiveRiderMilestone
    personId
    merchant.id
    merchantOpCityId
    journey
    milestoneId
    req.periodKey
  pure Success

postIncentiveJourneyCohortCreate ::
  ID.ShortId DM.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  Common.CreateCohortDetailsReq ->
  Environment.Flow Common.CreateCohortDetailsRes
postIncentiveJourneyCohortCreate _merchantShortId _opCity req = do
  when (T.null req.name) $ throwError (InvalidRequest "cohort name must be non-empty")
  cohort <- QCDExtra.createCohortDetails req.name
  pure Common.CreateCohortDetailsRes {cohortId = ID.cast cohort.id}

postIncentiveJourneyCohortJourneyCreate ::
  ID.ShortId DM.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  Common.CreateCohortJourneyMappingReq ->
  Environment.Flow Common.CreateCohortJourneyMappingRes
postIncentiveJourneyCohortJourneyCreate merchantShortId opCity req = do
  merchant <- findMerchantByShortId merchantShortId
  merchantOpCityId <- CQMOC.getMerchantOpCityId merchant (Just opCity)
  when (req.streakRange <= 0) $ throwError (InvalidRequest "streakRange must be > 0")
  let journeyId = ID.cast @Dashboard.Common.IncentiveJourney @DIJ.IncentiveJourney req.journeyId
      cohortId = ID.cast @Dashboard.Common.CohortDetails @DCD.CohortDetails req.cohortId
  void $ QCDExtra.findCohortDetailsById cohortId >>= fromMaybeM (InvalidRequest "Cohort not found")
  journey <- QJourney.findById journeyId >>= fromMaybeM (InvalidRequest "Incentive journey not found")
  unless (journey.merchantOperatingCityId == ID.cast merchantOpCityId && journey.merchantId == ID.cast merchant.id) $
    throwError (InvalidRequest "Incentive journey does not belong to this merchant/city")
  case IJStreak.validateMappingStartDate (SLJourney.journeyTypeOrDefault journey.journeyType) req.startDate of
    Left err -> throwError (InvalidRequest err)
    Right () -> pure ()
  validateStreakEndRewardOnMapping (toDomainRewardType <$> req.streakEndRewardType) req.streakEndRewardValue
  cjm <-
    QCJMExtra.createCohortJourneyMapping
      cohortId
      journeyId
      req.startDate
      req.streakRange
      (toDomainRewardType <$> req.streakEndRewardType)
      req.streakEndRewardValue
      req.streakEndRewardExpirationAt
  pure Common.CreateCohortJourneyMappingRes {cohortJourneyMappingId = ID.cast cjm.id}

putIncentiveJourneyCohortJourneyUpdate ::
  ID.ShortId DM.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  Common.UpdateCohortJourneyMappingReq ->
  Environment.Flow APISuccess
putIncentiveJourneyCohortJourneyUpdate merchantShortId opCity req = do
  merchant <- findMerchantByShortId merchantShortId
  merchantOpCityId <- CQMOC.getMerchantOpCityId merchant (Just opCity)
  let cjmId = ID.cast @Dashboard.Common.CohortJourneyMapping @DCJM.CohortJourneyMapping req.cohortJourneyMappingId
  cjm <- QCJMExtra.findCohortJourneyMappingById cjmId >>= fromMaybeM (InvalidRequest "Cohort journey mapping not found")
  journey <- QJourney.findById cjm.journeyId >>= fromMaybeM (InvalidRequest "Incentive journey not found")
  unless (journey.merchantOperatingCityId == ID.cast merchantOpCityId && journey.merchantId == ID.cast merchant.id) $
    throwError (InvalidRequest "Incentive journey does not belong to this merchant/city")
  let startDate = fromMaybe cjm.startDate req.startDate
      streakRange = fromMaybe cjm.streakRange req.streakRange
      streakEndRewardType = (toDomainRewardType <$> req.streakEndRewardType) <|> cjm.streakEndRewardType
      streakEndRewardValue = req.streakEndRewardValue <|> cjm.streakEndRewardValue
      streakEndRewardExpirationAt = req.streakEndRewardExpirationAt <|> cjm.streakEndRewardExpirationAt
  when (streakRange <= 0) $ throwError (InvalidRequest "streakRange must be > 0")
  case IJStreak.validateMappingStartDate (SLJourney.journeyTypeOrDefault journey.journeyType) startDate of
    Left err -> throwError (InvalidRequest err)
    Right () -> pure ()
  validateStreakEndRewardOnMapping streakEndRewardType streakEndRewardValue
  void $
    QCJMExtra.updateCohortJourneyMappingFields
      cjm{DCJM.startDate = startDate,
          DCJM.streakRange = streakRange,
          DCJM.streakEndRewardType = streakEndRewardType,
          DCJM.streakEndRewardValue = streakEndRewardValue,
          DCJM.streakEndRewardExpirationAt = streakEndRewardExpirationAt
         }
  pure Success

postIncentiveJourneyAssign ::
  ID.ShortId DM.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  Common.AssignUserToIncentiveJourneyReq ->
  Environment.Flow APISuccess
postIncentiveJourneyAssign merchantShortId opCity req = do
  merchant <- findMerchantByShortId merchantShortId
  merchantOpCityId <- CQMOC.getMerchantOpCityId merchant (Just opCity)
  let cjmId = ID.cast @Dashboard.Common.CohortJourneyMapping @DCJM.CohortJourneyMapping req.cohortJourneyMappingId
      personId = ID.cast @Dashboard.Common.Person @DIJC.Person req.personId
  cjm <- QCJMExtra.findCohortJourneyMappingById cjmId >>= fromMaybeM (InvalidRequest "Cohort journey mapping not found")
  journey <- QJourney.findById cjm.journeyId >>= fromMaybeM (InvalidRequest "Incentive journey not found")
  unless (journey.merchantOperatingCityId == ID.cast merchantOpCityId && journey.merchantId == ID.cast merchant.id) $
    throwError (InvalidRequest "Incentive journey does not belong to this merchant/city")
  void $ QUCMExtra.upsertUserCohortMapping personId cjmId req.isTestGroup
  pure Success

deleteIncentiveJourneyUnassign ::
  ID.ShortId DM.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  Common.UnassignUserFromIncentiveJourneyReq ->
  Environment.Flow APISuccess
deleteIncentiveJourneyUnassign merchantShortId opCity req = do
  merchant <- findMerchantByShortId merchantShortId
  merchantOpCityId <- CQMOC.getMerchantOpCityId merchant (Just opCity)
  let cjmId = ID.cast @Dashboard.Common.CohortJourneyMapping @DCJM.CohortJourneyMapping req.cohortJourneyMappingId
      personId = ID.cast @Dashboard.Common.Person @DIJC.Person req.personId
  cjm <- QCJMExtra.findCohortJourneyMappingById cjmId >>= fromMaybeM (InvalidRequest "Cohort journey mapping not found")
  journey <- QJourney.findById cjm.journeyId >>= fromMaybeM (InvalidRequest "Incentive journey not found")
  unless (journey.merchantOperatingCityId == ID.cast merchantOpCityId && journey.merchantId == ID.cast merchant.id) $
    throwError (InvalidRequest "Incentive journey does not belong to this merchant/city")
  QUCMExtra.deleteUserCohortMapping personId cjmId
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
      journeyType = toApiJourneyType <$> (journey.journeyType <|> Just DIJ.Daily),
      enabled = journey.enabled,
      maxWaiveOffCount = journey.maxWaiveOffCount,
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
      conditionOperator = toApiConditionOperator (fromMaybe DIJM.GTE milestone.conditionOperator),
      conditionValue = milestone.conditionValue,
      areaType = toApiAreaType <$> milestone.areaType,
      specialLocationIds = milestone.specialLocationIds,
      vehicleCategory = milestone.vehicleCategory,
      serviceTierType = milestone.serviceTierType,
      rewardType = toApiRewardType milestone.rewardType,
      rewardValue = milestone.rewardValue,
      rewardExpirationAt = milestone.rewardExpirationAt,
      timeBounds = milestone.timeBounds,
      createdAt = milestone.createdAt,
      updatedAt = milestone.updatedAt
    }

validateArea :: Maybe DIJM.MilestoneAreaType -> Maybe [Text] -> Environment.Flow ()
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

validateMilestoneCondition :: DIJM.IncentiveJourneyMilestone -> Environment.Flow ()
validateMilestoneCondition milestone =
  validateArea milestone.areaType milestone.specialLocationIds

-- Same rules as driver dashboard. Rider payout is stubbed today; Coins configs are still allowed.
validateMilestoneReward :: DIJ.IncentiveJourney -> DIJM.IncentiveJourneyMilestone -> Environment.Flow ()
validateMilestoneReward _journey milestone =
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
  Maybe DIJC.MilestoneRewardType ->
  Maybe Int ->
  Environment.Flow ()
validateStreakEndRewardOnMapping mbRewardType mbRewardValue =
  case mbRewardType of
    Nothing -> pure ()
    Just DIJC.Coins ->
      case mbRewardValue of
        Just coins | coins > 0 -> pure ()
        _ -> throwError (InvalidRequest "Streak-end Coins reward requires streakEndRewardValue > 0")
    Just DIJC.NoReward -> pure ()
    Just other -> throwError (InvalidRequest $ show other <> " streak-end reward is not supported yet")

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
