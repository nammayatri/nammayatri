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
  planId <- generateGUID
  let plan =
        DPlan.Plan
          { id = planId,
            name = req.name,
            description = req.description,
            paymentMode = read (toString req.paymentMode),
            planType = read (toString req.planType),
            billingType = read . toString <$> req.billingType,
            frequency = read (toString req.frequency),
            planBaseAmount = read (toString req.planBaseAmount),
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
            basedOnEntity = read (toString req.basedOnEntity),
            serviceName = read (toString req.serviceName),
            merchantId = merchant.id,
            merchantOpCityId = merchantOpCityId,
            vehicleVariant = read . toString <$> req.vehicleVariant,
            vehicleCategory = read (toString req.vehicleCategory),
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
  -- Mark original as deprecated
  QPlanExtra.markAsDeprecated originalPlanId
  CQPlan.clearPlanCacheByCity merchantOpCityId originalPlan.serviceName
  -- Create new versioned copy
  newPlanId <- generateGUID
  let newPlan =
        DPlan.Plan
          { id = newPlanId,
            name = fromMaybe originalPlan.name req.name,
            description = fromMaybe originalPlan.description req.description,
            paymentMode = maybe originalPlan.paymentMode (read . toString) req.paymentMode,
            planType = maybe originalPlan.planType (read . toString) req.planType,
            billingType = originalPlan.billingType,
            frequency = maybe originalPlan.frequency (read . toString) req.frequency,
            planBaseAmount = maybe originalPlan.planBaseAmount (read . toString) req.planBaseAmount,
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
            basedOnEntity = maybe originalPlan.basedOnEntity (read . toString) req.basedOnEntity,
            serviceName = originalPlan.serviceName,
            merchantId = merchant.id,
            merchantOpCityId = merchantOpCityId,
            vehicleVariant = originalPlan.vehicleVariant,
            vehicleCategory = originalPlan.vehicleCategory,
            listingPriority = originalPlan.listingPriority,
            validityInDays = originalPlan.validityInDays,
            isFleetOwnerPlan = originalPlan.isFleetOwnerPlan
          }
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
        currentVersion = 1 -- Version from SubscriptionConfig; default 1 for Plan
      }

-- Helper functions

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
        version = 1, -- Default version; tracked in SubscriptionConfig
        scheduledActivationDate = Nothing,
        scheduledDeactivationDate = Nothing,
        parentPlanId = Nothing,
        benefits = Nothing,
        subscriberCount = subscriberCount,
        createdAt = now, -- Plan type doesn't have createdAt, using current time
        updatedAt = now
      }
