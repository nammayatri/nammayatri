module Domain.Action.Dashboard.Management.IncentiveJourney
  ( getIncentiveJourneyList,
    postIncentiveJourneyCreate,
    putIncentiveJourneyUpdate,
    getIncentiveJourneyMilestoneList,
    postIncentiveJourneyMilestoneCreate,
    putIncentiveJourneyMilestoneUpdate,
  )
where

import qualified API.Types.ProviderPlatform.Management.IncentiveJourney as Common
import qualified Dashboard.Common
import Data.List (sortOn)
import qualified Domain.Types.Coins.CoinsConfig as DCoinsConfig
import qualified Domain.Types.IncentiveJourney as DIJ
import qualified Domain.Types.IncentiveJourneyMilestone as DIJM
import qualified Domain.Types.Merchant
import qualified Environment
import EulerHS.Prelude hiding (id, sortOn)
import Kernel.Types.APISuccess (APISuccess (Success))
import qualified Kernel.Types.Beckn.Context
import Kernel.Types.Error (GenericError (InvalidRequest))
import qualified Kernel.Types.Id as ID
import Kernel.Utils.Common
import SharedLogic.Merchant (findMerchantByShortId)
import qualified Storage.CachedQueries.IncentiveJourney as CQJourney
import qualified Storage.CachedQueries.IncentiveJourneyMilestone as CQMilestone
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import qualified Storage.Queries.IncentiveJourney as QJourney
import qualified Storage.Queries.IncentiveJourneyMilestone as QMilestone

getIncentiveJourneyList ::
  ID.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  Maybe Int ->
  Maybe Int ->
  Maybe Bool ->
  Maybe Text ->
  Environment.Flow Common.IncentiveJourneyListRes
getIncentiveJourneyList merchantShortId opCity mbLimit mbOffset mbEnabled mbDriverTag = do
  merchant <- findMerchantByShortId merchantShortId
  merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing merchant (Just opCity)
  -- Filter first (enabled / driverTag), then paginate so limit/offset stay correct.
  journeys <-
    case mbEnabled of
      Just True -> CQJourney.findEnabledByMerchantOperatingCityId merchantOpCityId
      Just False -> filter (not . (.enabled)) <$> CQJourney.findByMerchantOperatingCityId merchantOpCityId
      Nothing -> CQJourney.findByMerchantOperatingCityId merchantOpCityId
  let filtered =
        case mbDriverTag of
          Nothing -> journeys
          Just tag -> filter (\j -> j.driverTag == tag) journeys
      limitVal = fromMaybe 20 mbLimit
      offsetVal = fromMaybe 0 mbOffset
      page = take limitVal . drop offsetVal $ filtered
  pure Common.IncentiveJourneyListRes {journeys = map toJourneyListItem page}

postIncentiveJourneyCreate ::
  ID.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  Common.CreateIncentiveJourneyReq ->
  Environment.Flow Common.CreateIncentiveJourneyRes
postIncentiveJourneyCreate merchantShortId opCity req = do
  merchant <- findMerchantByShortId merchantShortId
  merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing merchant (Just opCity)
  when (req.endDate < req.startDate) $
    throwError (InvalidRequest "endDate must be >= startDate")
  now <- getCurrentTime
  journeyId <- generateGUID
  let journey =
        DIJ.IncentiveJourney
          { id = journeyId,
            merchantId = merchant.id,
            merchantOperatingCityId = merchantOpCityId,
            name = req.name,
            description = req.description,
            driverTag = req.driverTag,
            journeyType = Just (toDomainJourneyType req.journeyType),
            timeBounds = req.timeBounds,
            startDate = req.startDate,
            endDate = req.endDate,
            vehicleCategory = req.vehicleCategory,
            enabled = req.enabled,
            createdAt = now,
            updatedAt = now
          }
  QJourney.create journey
  CQJourney.clearCache journey
  pure Common.CreateIncentiveJourneyRes {journeyId = ID.cast journeyId}

putIncentiveJourneyUpdate ::
  ID.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  Common.UpdateIncentiveJourneyReq ->
  Environment.Flow APISuccess
putIncentiveJourneyUpdate merchantShortId opCity req = do
  merchant <- findMerchantByShortId merchantShortId
  merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing merchant (Just opCity)
  let journeyId = ID.cast @Dashboard.Common.IncentiveJourney @DIJ.IncentiveJourney req.journeyId
  journey <- QJourney.findById journeyId >>= fromMaybeM (InvalidRequest "Incentive journey not found")
  unless (journey.merchantOperatingCityId == merchantOpCityId && journey.merchantId == merchant.id) $
    throwError (InvalidRequest "Incentive journey does not belong to this merchant/city")
  let startDate = fromMaybe journey.startDate req.startDate
      endDate = fromMaybe journey.endDate req.endDate
  when (endDate < startDate) $
    throwError (InvalidRequest "endDate must be >= startDate")
  let updated =
        journey
          { DIJ.name = fromMaybe journey.name req.name,
            DIJ.description = maybe journey.description Just req.description,
            DIJ.driverTag = fromMaybe journey.driverTag req.driverTag,
            DIJ.journeyType = maybe journey.journeyType (Just . toDomainJourneyType) req.journeyType,
            DIJ.timeBounds = maybe journey.timeBounds Just req.timeBounds,
            DIJ.startDate = startDate,
            DIJ.endDate = endDate,
            DIJ.vehicleCategory = maybe journey.vehicleCategory Just req.vehicleCategory,
            DIJ.enabled = fromMaybe journey.enabled req.enabled
          }
  QJourney.updateByPrimaryKey updated
  CQJourney.clearCache updated
  pure Success

getIncentiveJourneyMilestoneList ::
  ID.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  Maybe Int ->
  Maybe Int ->
  ID.Id Dashboard.Common.IncentiveJourney ->
  Environment.Flow Common.IncentiveJourneyMilestoneListRes
getIncentiveJourneyMilestoneList merchantShortId opCity mbLimit mbOffset dashboardJourneyId = do
  merchant <- findMerchantByShortId merchantShortId
  merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing merchant (Just opCity)
  let journeyId = ID.cast @Dashboard.Common.IncentiveJourney @DIJ.IncentiveJourney dashboardJourneyId
  journey <- QJourney.findById journeyId >>= fromMaybeM (InvalidRequest "Incentive journey not found")
  unless (journey.merchantOperatingCityId == merchantOpCityId && journey.merchantId == merchant.id) $
    throwError (InvalidRequest "Incentive journey does not belong to this merchant/city")
  milestones <- CQMilestone.findByJourneyId journeyId
  let limitVal = fromMaybe 20 mbLimit
      offsetVal = fromMaybe 0 mbOffset
      page = take limitVal . drop offsetVal $ sortOn (.order) milestones
  pure Common.IncentiveJourneyMilestoneListRes {milestones = map toMilestoneListItem page}

postIncentiveJourneyMilestoneCreate ::
  ID.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  Common.CreateIncentiveJourneyMilestoneReq ->
  Environment.Flow Common.CreateIncentiveJourneyMilestoneRes
postIncentiveJourneyMilestoneCreate merchantShortId opCity req = do
  merchant <- findMerchantByShortId merchantShortId
  merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing merchant (Just opCity)
  let journeyId = ID.cast @Dashboard.Common.IncentiveJourney @DIJ.IncentiveJourney req.journeyId
  journey <- QJourney.findById journeyId >>= fromMaybeM (InvalidRequest "Incentive journey not found")
  unless (journey.merchantOperatingCityId == merchantOpCityId && journey.merchantId == merchant.id) $
    throwError (InvalidRequest "Incentive journey does not belong to this merchant/city")
  when (req.conditionValue < 0) $
    throwError (InvalidRequest "conditionValue must be >= 0")
  now <- getCurrentTime
  milestoneId <- generateGUID
  let milestone =
        DIJM.IncentiveJourneyMilestone
          { id = milestoneId,
            journeyId = journeyId,
            description = req.description,
            order = req.order,
            conditionType = toDomainConditionType req.conditionType,
            conditionOperator = Just (toDomainConditionOperator req.conditionOperator),
            conditionValue = req.conditionValue,
            pickupSpecialLocationIds = req.pickupSpecialLocationIds,
            dropSpecialLocationIds = req.dropSpecialLocationIds,
            rewardType = toDomainRewardType req.rewardType,
            rewardConfigId = ID.cast @Dashboard.Common.CoinsConfig @DCoinsConfig.CoinsConfig <$> req.rewardConfigId,
            rewardValue = req.rewardValue,
            createdAt = now,
            updatedAt = now,
            merchantId = Just merchant.id,
            merchantOperatingCityId = Just merchantOpCityId
          }
  validateMilestoneCondition milestone
  QMilestone.create milestone
  CQMilestone.clearCacheByJourneyId journeyId
  pure Common.CreateIncentiveJourneyMilestoneRes {milestoneId = ID.cast milestoneId}

putIncentiveJourneyMilestoneUpdate ::
  ID.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  Common.UpdateIncentiveJourneyMilestoneReq ->
  Environment.Flow APISuccess
putIncentiveJourneyMilestoneUpdate merchantShortId opCity req = do
  merchant <- findMerchantByShortId merchantShortId
  merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing merchant (Just opCity)
  let milestoneId = ID.cast @Dashboard.Common.IncentiveJourneyMilestone @DIJM.IncentiveJourneyMilestone req.milestoneId
  milestone <- QMilestone.findById milestoneId >>= fromMaybeM (InvalidRequest "Incentive journey milestone not found")
  journey <- QJourney.findById milestone.journeyId >>= fromMaybeM (InvalidRequest "Incentive journey not found")
  unless (journey.merchantOperatingCityId == merchantOpCityId && journey.merchantId == merchant.id) $
    throwError (InvalidRequest "Incentive journey does not belong to this merchant/city")
  whenJust req.conditionValue $ \v ->
    when (v < 0) $ throwError (InvalidRequest "conditionValue must be >= 0")
  let updated =
        milestone
          { DIJM.description = maybe milestone.description Just req.description,
            DIJM.order = fromMaybe milestone.order req.order,
            DIJM.conditionType = maybe milestone.conditionType toDomainConditionType req.conditionType,
            DIJM.conditionOperator = maybe milestone.conditionOperator (Just . toDomainConditionOperator) req.conditionOperator,
            DIJM.conditionValue = fromMaybe milestone.conditionValue req.conditionValue,
            DIJM.pickupSpecialLocationIds = maybe milestone.pickupSpecialLocationIds Just req.pickupSpecialLocationIds,
            DIJM.dropSpecialLocationIds = maybe milestone.dropSpecialLocationIds Just req.dropSpecialLocationIds,
            DIJM.rewardType = maybe milestone.rewardType toDomainRewardType req.rewardType,
            DIJM.rewardConfigId =
              maybe
                milestone.rewardConfigId
                (Just . ID.cast @Dashboard.Common.CoinsConfig @DCoinsConfig.CoinsConfig)
                req.rewardConfigId,
            DIJM.rewardValue = maybe milestone.rewardValue Just req.rewardValue
          }
  validateMilestoneCondition updated
  QMilestone.updateByPrimaryKey updated
  CQMilestone.clearCacheByJourneyId milestone.journeyId
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
      driverTag = journey.driverTag,
      journeyType = toApiJourneyType <$> (journey.journeyType <|> Just DIJ.Daily),
      timeBounds = journey.timeBounds,
      startDate = journey.startDate,
      endDate = journey.endDate,
      vehicleCategory = journey.vehicleCategory,
      enabled = journey.enabled,
      createdAt = journey.createdAt,
      updatedAt = journey.updatedAt
    }

toMilestoneListItem :: DIJM.IncentiveJourneyMilestone -> Common.IncentiveJourneyMilestoneListItem
toMilestoneListItem milestone =
  Common.IncentiveJourneyMilestoneListItem
    { milestoneId = ID.cast milestone.id,
      journeyId = ID.cast milestone.journeyId,
      description = milestone.description,
      order = milestone.order,
      conditionType = toApiConditionType milestone.conditionType,
      conditionOperator = toApiConditionOperator (fromMaybe DIJM.GTE milestone.conditionOperator),
      conditionValue = milestone.conditionValue,
      pickupSpecialLocationIds = milestone.pickupSpecialLocationIds,
      dropSpecialLocationIds = milestone.dropSpecialLocationIds,
      rewardType = toApiRewardType milestone.rewardType,
      rewardConfigId = ID.cast @DCoinsConfig.CoinsConfig @Dashboard.Common.CoinsConfig <$> milestone.rewardConfigId,
      rewardValue = milestone.rewardValue,
      createdAt = milestone.createdAt,
      updatedAt = milestone.updatedAt
    }

validateLocationFilters :: Maybe [Text] -> Maybe [Text] -> Environment.Flow ()
validateLocationFilters mbPickupSpecialLocationIds mbDropSpecialLocationIds = do
  when (maybe False null mbPickupSpecialLocationIds) $
    throwError (InvalidRequest "pickupSpecialLocationIds must be non-empty when provided")
  when (maybe False null mbDropSpecialLocationIds) $
    throwError (InvalidRequest "dropSpecialLocationIds must be non-empty when provided")

validateMilestoneCondition :: DIJM.IncentiveJourneyMilestone -> Environment.Flow ()
validateMilestoneCondition milestone = do
  validateLocationFilters milestone.pickupSpecialLocationIds milestone.dropSpecialLocationIds
  let conditionOperator = fromMaybe DIJM.GTE milestone.conditionOperator
      requireContainsOperator =
        unless (conditionOperator == DIJM.CT) $
          throwError (InvalidRequest "Special-location conditions require CT operator")
      requirePickupLocations =
        when (maybe True null milestone.pickupSpecialLocationIds) $
          throwError (InvalidRequest "Pickup special-location condition requires pickupSpecialLocationIds")
      requireDropLocations =
        when (maybe True null milestone.dropSpecialLocationIds) $
          throwError (InvalidRequest "Drop special-location condition requires dropSpecialLocationIds")
      requirePositiveRideCount =
        when (milestone.conditionValue <= 0) $
          throwError (InvalidRequest "Special-location conditionValue must be greater than 0")
  case milestone.conditionType of
    DIJM.PickupSpecialLocation -> requireContainsOperator >> requirePickupLocations >> requirePositiveRideCount
    DIJM.DropSpecialLocation -> requireContainsOperator >> requireDropLocations >> requirePositiveRideCount
    DIJM.PickupDropSpecialLocation -> requireContainsOperator >> requirePickupLocations >> requireDropLocations >> requirePositiveRideCount
    _ ->
      when (conditionOperator == DIJM.CT) $
        throwError (InvalidRequest "CT operator is only valid for special-location conditions")

toDomainJourneyType :: Common.IncentiveJourneyType -> DIJ.IncentiveJourneyType
toDomainJourneyType = \case
  Common.Daily -> DIJ.Daily
  Common.Weekly -> DIJ.Weekly

toApiJourneyType :: DIJ.IncentiveJourneyType -> Common.IncentiveJourneyType
toApiJourneyType = \case
  DIJ.Daily -> Common.Daily
  DIJ.Weekly -> Common.Weekly

toDomainConditionType :: Common.MilestoneConditionType -> DIJM.MilestoneConditionType
toDomainConditionType = \case
  Common.RideCompleted -> DIJM.RideCompleted
  Common.Earnings -> DIJM.Earnings
  Common.Distance -> DIJM.Distance
  Common.RideDuration -> DIJM.RideDuration
  Common.PickupSpecialLocation -> DIJM.PickupSpecialLocation
  Common.DropSpecialLocation -> DIJM.DropSpecialLocation
  Common.PickupDropSpecialLocation -> DIJM.PickupDropSpecialLocation

toApiConditionType :: DIJM.MilestoneConditionType -> Common.MilestoneConditionType
toApiConditionType = \case
  DIJM.RideCompleted -> Common.RideCompleted
  DIJM.Earnings -> Common.Earnings
  DIJM.Distance -> Common.Distance
  DIJM.RideDuration -> Common.RideDuration
  DIJM.PickupSpecialLocation -> Common.PickupSpecialLocation
  DIJM.DropSpecialLocation -> Common.DropSpecialLocation
  DIJM.PickupDropSpecialLocation -> Common.PickupDropSpecialLocation

toDomainConditionOperator :: Common.MilestoneConditionOperator -> DIJM.MilestoneConditionOperator
toDomainConditionOperator = \case
  Common.GTE -> DIJM.GTE
  Common.GT -> DIJM.GT
  Common.EQ -> DIJM.EQ
  Common.LTE -> DIJM.LTE
  Common.LT -> DIJM.LT
  Common.CT -> DIJM.CT

toApiConditionOperator :: DIJM.MilestoneConditionOperator -> Common.MilestoneConditionOperator
toApiConditionOperator = \case
  DIJM.GTE -> Common.GTE
  DIJM.GT -> Common.GT
  DIJM.EQ -> Common.EQ
  DIJM.LTE -> Common.LTE
  DIJM.LT -> Common.LT
  DIJM.CT -> Common.CT

toDomainRewardType :: Common.MilestoneRewardType -> DIJM.MilestoneRewardType
toDomainRewardType = \case
  Common.Coins -> DIJM.Coins
  Common.Cash -> DIJM.Cash
  Common.Coupons -> DIJM.Coupons

toApiRewardType :: DIJM.MilestoneRewardType -> Common.MilestoneRewardType
toApiRewardType = \case
  DIJM.Coins -> Common.Coins
  DIJM.Cash -> Common.Cash
  DIJM.Coupons -> Common.Coupons
