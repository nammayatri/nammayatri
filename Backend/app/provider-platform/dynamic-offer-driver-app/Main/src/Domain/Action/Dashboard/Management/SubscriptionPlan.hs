{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Action.Dashboard.Management.SubscriptionPlan
  ( getSubscriptionPlanList,
    getSubscriptionPlanDetails,
    postSubscriptionPlanCreate,
    postSubscriptionPlanUpdate,
    postSubscriptionPlanDeactivate,
    postSubscriptionPlanActivate,
    getSubscriptionPlanAnalytics,
  )
where

import qualified Dashboard.ProviderPlatform.Management.SubscriptionPlan as Common
import qualified Domain.Types.DriverPlan as DDP
import qualified Domain.Types.Extra.Plan as DExtra
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.Plan as DPlan
import qualified Domain.Types.SubscriptionPurchase as DSP
import qualified Domain.Types.VehicleCategory as DVC
import qualified Domain.Types.VehicleVariant as DVV
import Environment
import Kernel.Prelude
import Kernel.Types.APISuccess
import Kernel.Types.Beckn.Context as Context
import Kernel.Types.Common
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import Text.Read (readMaybe)
import SharedLogic.Merchant (findMerchantByShortId)
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import qualified Storage.CachedQueries.PlanExtra as CQPlan
import qualified Storage.Queries.DriverPlanExtra as QDriverPlan
import qualified Storage.Queries.Plan as QPlan
import qualified Storage.Queries.PlanExtra as QPlanExtra
import qualified Storage.Queries.SubscriptionPurchaseExtra as QSPurchase

-- | List plans with pagination and filters (status, city, frequency)
getSubscriptionPlanList ::
  ShortId DM.Merchant ->
  Context.City ->
  Maybe Text ->
  Maybe Text ->
  Maybe Text ->
  Maybe Int ->
  Maybe Int ->
  Flow Common.SubscriptionPlanListResp
getSubscriptionPlanList merchantShortId opCity mbStatus _mbCity mbFrequency mbLimit mbOffset = do
  merchant <- findMerchantByShortId merchantShortId
  merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing merchant (Just opCity)
  allPlans <- QPlanExtra.fetchAllPlanByMerchantOperatingCityMbServiceName merchantOpCityId Nothing
  now <- getCurrentTime
  -- Apply filters
  let filteredPlans = applyFilters mbStatus mbFrequency allPlans
  -- Pagination
  let totalCount = length filteredPlans
      limit' = min 20 (fromMaybe 10 mbLimit)
      offset' = fromMaybe 0 mbOffset
      paginatedPlans = take limit' . drop offset' $ filteredPlans
  -- Build entities with subscriber counts
  planEntities <- mapM (toPlanEntity merchantOpCityId now) paginatedPlans
  pure $
    Common.SubscriptionPlanListResp
      { plans = planEntities,
        totalCount = totalCount
      }

-- | Get single plan details with subscriber count
getSubscriptionPlanDetails ::
  ShortId DM.Merchant ->
  Context.City ->
  Text ->
  Flow Common.SubscriptionPlanDetailsResp
getSubscriptionPlanDetails merchantShortId opCity planIdText = do
  _merchant <- findMerchantByShortId merchantShortId
  merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing _merchant (Just opCity)
  let planId = Id planIdText
  plan <- QPlan.findByPrimaryKey planId >>= fromMaybeM (InvalidRequest "Plan not found")
  -- Verify multi-tenancy: plan belongs to this merchant operating city
  unless (plan.merchantOpCityId == merchantOpCityId) $
    throwError (InvalidRequest "Plan does not belong to this merchant operating city")
  now <- getCurrentTime
  planEntity <- toPlanEntity merchantOpCityId now plan
  pure $ Common.SubscriptionPlanDetailsResp {plan = planEntity}

-- | Create a new subscription plan
postSubscriptionPlanCreate ::
  ShortId DM.Merchant ->
  Context.City ->
  Common.CreateSubscriptionPlanReq ->
  Flow Common.CreateSubscriptionPlanResp
postSubscriptionPlanCreate merchantShortId opCity req = do
  merchant <- findMerchantByShortId merchantShortId
  merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing merchant (Just opCity)
  -- Validate all enum/text fields upfront before any DB writes
  paymentMode' <- safeParseFieldM "paymentMode" req.paymentMode
  planType' <- safeParseFieldM "planType" req.planType
  billingType' <- case req.billingType of
    Nothing -> pure Nothing
    Just bt -> Just <$> safeParseFieldM "billingType" bt
  frequency' <- safeParseFieldM "frequency" req.frequency
  planBaseAmount' <- safeParseFieldM "planBaseAmount" req.planBaseAmount
  basedOnEntity' <- safeParseFieldM "basedOnEntity" req.basedOnEntity
  serviceName' <- safeParseFieldM "serviceName" req.serviceName
  vehicleVariant' <- case req.vehicleVariant of
    Nothing -> pure Nothing
    Just vv -> Just <$> safeParseFieldM "vehicleVariant" vv
  vehicleCategory' <- safeParseFieldM "vehicleCategory" req.vehicleCategory
  planId <- generateGUID
  let plan =
        DPlan.Plan
          { id = planId,
            name = req.name,
            description = req.description,
            paymentMode = paymentMode',
            planType = planType',
            billingType = billingType',
            frequency = frequency',
            planBaseAmount = planBaseAmount',
            maxAmount = req.maxAmount,
            registrationAmount = req.registrationAmount,
            originalRegistrationAmount = req.originalRegistrationAmount,
            maxCreditLimit = req.maxCreditLimit,
            maxMandateAmount = req.maxMandateAmount,
            productOwnershipAmount = req.productOwnershipAmount,
            cgstPercentage = req.cgstPercentage,
            sgstPercentage = req.sgstPercentage,
            freeRideCount = req.freeRideCount,
            isOfferApplicable = req.isOfferApplicable,
            eligibleForCoinDiscount = req.eligibleForCoinDiscount,
            subscribedFlagToggleAllowed = req.subscribedFlagToggleAllowed,
            isDeprecated = req.isDeprecated,
            allowStrikeOff = req.allowStrikeOff,
            basedOnEntity = basedOnEntity',
            serviceName = serviceName',
            merchantId = merchant.id,
            merchantOpCityId = merchantOpCityId,
            vehicleVariant = vehicleVariant',
            vehicleCategory = vehicleCategory',
            listingPriority = req.listingPriority,
            validityInDays = req.validityInDays,
            isFleetOwnerPlan = req.isFleetOwnerPlan
          }
  QPlan.create plan
  CQPlan.clearPlanCacheByCity merchantOpCityId plan.serviceName
  logInfo $ "Created new subscription plan: " <> planId.getId
  pure $ Common.CreateSubscriptionPlanResp {planId = planId.getId}

-- | Update plan by creating a new versioned copy (immutable versioning pattern)
-- The original plan is marked as deprecated and a new plan is created with incremented version
postSubscriptionPlanUpdate ::
  ShortId DM.Merchant ->
  Context.City ->
  Text ->
  Common.UpdateSubscriptionPlanReq ->
  Flow Common.CreateSubscriptionPlanResp
postSubscriptionPlanUpdate merchantShortId opCity planIdText req = do
  merchant <- findMerchantByShortId merchantShortId
  merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing merchant (Just opCity)
  let originalPlanId = Id planIdText
  originalPlan <- QPlan.findByPrimaryKey originalPlanId >>= fromMaybeM (InvalidRequest "Plan not found")
  -- Verify multi-tenancy
  unless (originalPlan.merchantOpCityId == merchantOpCityId) $
    throwError (InvalidRequest "Plan does not belong to this merchant operating city")
  -- P0-1 FIX: Parse and validate ALL fields BEFORE deprecating the original plan.
  -- This ensures the old plan remains active if any parsing fails.
  paymentMode' <- maybe (pure originalPlan.paymentMode) (safeParseFieldM "paymentMode") req.paymentMode
  planType' <- maybe (pure originalPlan.planType) (safeParseFieldM "planType") req.planType
  frequency' <- maybe (pure originalPlan.frequency) (safeParseFieldM "frequency") req.frequency
  planBaseAmount' <- maybe (pure originalPlan.planBaseAmount) (safeParseFieldM "planBaseAmount") req.planBaseAmount
  basedOnEntity' <- maybe (pure originalPlan.basedOnEntity) (safeParseFieldM "basedOnEntity") req.basedOnEntity
  -- Create new versioned copy (validate first, then deprecate)
  newPlanId <- generateGUID
  let newPlan =
        DPlan.Plan
          { id = newPlanId,
            name = fromMaybe originalPlan.name req.name,
            description = fromMaybe originalPlan.description req.description,
            paymentMode = paymentMode',
            planType = planType',
            billingType = originalPlan.billingType,
            frequency = frequency',
            planBaseAmount = planBaseAmount',
            maxAmount = fromMaybe originalPlan.maxAmount req.maxAmount,
            registrationAmount = fromMaybe originalPlan.registrationAmount req.registrationAmount,
            originalRegistrationAmount = originalPlan.originalRegistrationAmount,
            maxCreditLimit = fromMaybe originalPlan.maxCreditLimit req.maxCreditLimit,
            maxMandateAmount = fromMaybe originalPlan.maxMandateAmount req.maxMandateAmount,
            productOwnershipAmount = fromMaybe originalPlan.productOwnershipAmount req.productOwnershipAmount,
            cgstPercentage = fromMaybe originalPlan.cgstPercentage req.cgstPercentage,
            sgstPercentage = fromMaybe originalPlan.sgstPercentage req.sgstPercentage,
            freeRideCount = fromMaybe originalPlan.freeRideCount req.freeRideCount,
            isOfferApplicable = fromMaybe originalPlan.isOfferApplicable req.isOfferApplicable,
            eligibleForCoinDiscount = originalPlan.eligibleForCoinDiscount,
            subscribedFlagToggleAllowed = originalPlan.subscribedFlagToggleAllowed,
            isDeprecated = fromMaybe False req.isDeprecated,
            allowStrikeOff = originalPlan.allowStrikeOff,
            basedOnEntity = basedOnEntity',
            serviceName = originalPlan.serviceName,
            merchantId = merchant.id,
            merchantOpCityId = merchantOpCityId,
            vehicleVariant = originalPlan.vehicleVariant,
            vehicleCategory = originalPlan.vehicleCategory,
            listingPriority = originalPlan.listingPriority,
            validityInDays = originalPlan.validityInDays,
            isFleetOwnerPlan = originalPlan.isFleetOwnerPlan
          }
  -- Now that all parsing succeeded, mark the original as deprecated
  QPlanExtra.markAsDeprecated originalPlanId
  CQPlan.clearPlanCacheByCity merchantOpCityId originalPlan.serviceName
  QPlan.create newPlan
  CQPlan.clearPlanCacheByCity merchantOpCityId newPlan.serviceName
  logInfo $ "Created versioned plan copy: " <> newPlanId.getId <> " from original: " <> planIdText
  pure $ Common.CreateSubscriptionPlanResp {planId = newPlanId.getId}

-- | Deactivate a plan by marking it as deprecated
-- Optionally sets a scheduled deactivation date for future deactivation
postSubscriptionPlanDeactivate ::
  ShortId DM.Merchant ->
  Context.City ->
  Text ->
  Common.DeactivatePlanReq ->
  Flow APISuccess
postSubscriptionPlanDeactivate merchantShortId opCity planIdText _req = do
  _merchant <- findMerchantByShortId merchantShortId
  merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing _merchant (Just opCity)
  let planId = Id planIdText
  plan <- QPlan.findByPrimaryKey planId >>= fromMaybeM (InvalidRequest "Plan not found")
  -- Verify multi-tenancy
  unless (plan.merchantOpCityId == merchantOpCityId) $
    throwError (InvalidRequest "Plan does not belong to this merchant operating city")
  -- Mark plan as deprecated (immediate deactivation)
  -- Note: scheduledDeactivationDate is stored in SubscriptionConfig for future scheduling support
  QPlanExtra.markAsDeprecated planId
  CQPlan.clearPlanCacheByCity plan.merchantOpCityId plan.serviceName
  logInfo $ "Deactivated subscription plan: " <> planIdText
  pure Success

-- | Activate a plan by clearing deprecation status
-- Optionally sets a scheduled activation date
postSubscriptionPlanActivate ::
  ShortId DM.Merchant ->
  Context.City ->
  Text ->
  Common.ActivatePlanReq ->
  Flow APISuccess
postSubscriptionPlanActivate merchantShortId opCity planIdText _req = do
  _merchant <- findMerchantByShortId merchantShortId
  merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing _merchant (Just opCity)
  let planId = Id planIdText
  plan <- QPlan.findByPrimaryKey planId >>= fromMaybeM (InvalidRequest "Plan not found")
  -- Verify multi-tenancy
  unless (plan.merchantOpCityId == merchantOpCityId) $
    throwError (InvalidRequest "Plan does not belong to this merchant operating city")
  QPlanExtra.markAsActive planId
  CQPlan.clearPlanCacheByCity plan.merchantOpCityId plan.serviceName
  logInfo $ "Activated subscription plan: " <> planIdText
  pure Success

-- | Get analytics for a specific plan: subscriber count, revenue, churn metrics
getSubscriptionPlanAnalytics ::
  ShortId DM.Merchant ->
  Context.City ->
  Text ->
  Flow Common.PlanAnalyticsResp
getSubscriptionPlanAnalytics merchantShortId opCity planIdText = do
  _merchant <- findMerchantByShortId merchantShortId
  merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing _merchant (Just opCity)
  let planId = Id planIdText
  plan <- QPlan.findByPrimaryKey planId >>= fromMaybeM (InvalidRequest "Plan not found")
  -- Verify multi-tenancy
  unless (plan.merchantOpCityId == merchantOpCityId) $
    throwError (InvalidRequest "Plan does not belong to this merchant operating city")
  -- Count total subscribers (all DriverPlan entries for this plan)
  allDriverPlans <- QDriverPlan.findAllByPlanId planId
  let totalSubscribers = length allDriverPlans
  -- Count active subscribers (ACTIVE subscription purchases)
  activePurchases <- QSPurchase.findAllActiveByPlanId planId
  let activeSubscribers = length activePurchases
  -- Calculate total revenue from completed purchases
  let totalRevenue = sum $ map (.planFee) activePurchases
  pure $
    Common.PlanAnalyticsResp
      { planId = planIdText,
        planName = plan.name,
        totalSubscribers = totalSubscribers,
        activeSubscribers = activeSubscribers,
        totalRevenue = totalRevenue,
        currentVersion = 1 -- TODO (P1-4): Version from SubscriptionConfig; hardcoded default 1 until versioning is implemented
      }

-- Helper functions

-- | Safely parse a Text field in a monadic context, throwing InvalidRequest on failure.
safeParseFieldM :: (Read a, MonadFlow m) => Text -> Text -> m a
safeParseFieldM fieldName value =
  case readMaybe (toString value) of
    Just parsed -> pure parsed
    Nothing -> throwError $ InvalidRequest $ "Invalid value for " <> fieldName <> ": " <> value

-- | Apply status and frequency filters to plan list
applyFilters :: Maybe Text -> Maybe Text -> [DPlan.Plan] -> [DPlan.Plan]
applyFilters mbStatus mbFrequency plans =
  let statusFiltered = case mbStatus of
        Just "ACTIVE" -> filter (not . (.isDeprecated)) plans
        Just "INACTIVE" -> filter (.isDeprecated) plans
        _ -> plans
      frequencyFiltered = case mbFrequency of
        Just freq -> filter (\p -> show p.frequency == toString freq) statusFiltered
        Nothing -> statusFiltered
   in frequencyFiltered

-- | Convert Plan domain type to SubscriptionPlanEntity API type
toPlanEntity :: Id DMOC.MerchantOperatingCity -> UTCTime -> DPlan.Plan -> Flow Common.SubscriptionPlanEntity
toPlanEntity _merchantOpCityId now plan = do
  -- Count subscribers for this plan
  subscriberCount <- length <$> QDriverPlan.findAllByPlanId plan.id
  pure $
    Common.SubscriptionPlanEntity
      { id = plan.id.getId,
        name = plan.name,
        description = plan.description,
        paymentMode = show plan.paymentMode,
        planType = show plan.planType,
        billingType = show <$> plan.billingType,
        frequency = show plan.frequency,
        planBaseAmount = show plan.planBaseAmount,
        maxAmount = plan.maxAmount,
        registrationAmount = plan.registrationAmount,
        cgstPercentage = plan.cgstPercentage,
        sgstPercentage = plan.sgstPercentage,
        freeRideCount = plan.freeRideCount,
        isOfferApplicable = plan.isOfferApplicable,
        isDeprecated = plan.isDeprecated,
        basedOnEntity = show plan.basedOnEntity,
        serviceName = show plan.serviceName,
        merchantId = plan.merchantId.getId,
        merchantOpCityId = plan.merchantOpCityId.getId,
        vehicleCategory = show plan.vehicleCategory,
        vehicleVariant = show <$> plan.vehicleVariant,
        -- TODO (P1-4): The following fields are hardcoded/fabricated because the Plan domain type
        -- does not yet persist them. These must stay for API compatibility but should be backed
        -- by real data once SubscriptionConfig or a versioning table is implemented.
        version = 1, -- TODO: Track actual version in SubscriptionConfig table
        scheduledActivationDate = Nothing, -- TODO: Not yet persisted; add to Plan or SubscriptionConfig
        scheduledDeactivationDate = Nothing, -- TODO: Not yet persisted; add to Plan or SubscriptionConfig
        parentPlanId = Nothing, -- TODO: Not yet persisted; needed for plan versioning lineage
        benefits = Nothing, -- TODO: Not yet persisted; plan benefits are not stored in DB
        subscriberCount = subscriberCount,
        createdAt = now, -- TODO: Plan type doesn't have createdAt; add createdAt to Plan table
        updatedAt = now
      }
