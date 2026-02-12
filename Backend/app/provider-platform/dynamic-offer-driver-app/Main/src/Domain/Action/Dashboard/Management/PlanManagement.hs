{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Action.Dashboard.Management.PlanManagement
  ( postPlanManagementCreate,
    getPlanManagementPlan,
    postPlanManagementDeletePlan,
    getPlanManagementListPlans,
  )
where

import qualified API.Types.ProviderPlatform.Management.Endpoints.PlanManagement as Common
import qualified Domain.Types.Extra.Plan as DExtra
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.Plan as DPlan
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
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import qualified Storage.Queries.Plan as QPlan
import qualified Storage.Queries.PlanExtra as QPlanExtra
import Tools.Auth (merchantAccessCheck)

postPlanManagementCreate ::
  ShortId DM.Merchant ->
  Context.City ->
  Common.CreatePlanReq ->
  Flow Common.CreatePlanResp
postPlanManagementCreate _merchantShortId _opCity req = do
  planId <- generateGUID
  let plan =
        DPlan.Plan
          { id = planId,
            name = req.name,
            description = req.description,
            paymentMode = castPaymentMode req.paymentMode,
            planType = castPlanType req.planType,
            billingType = castBillingType <$> req.billingType,
            frequency = castFrequency req.frequency,
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
            basedOnEntity = castBasedOnEntity req.basedOnEntity,
            serviceName = read (toString req.serviceName),
            merchantId = Id req.merchantId,
            merchantOpCityId = Id req.merchantOpCityId,
            vehicleVariant = read . toString <$> req.vehicleVariant,
            vehicleCategory = read (toString req.vehicleCategory),
            listingPriority = req.listingPriority,
            validityInDays = req.validityInDays,
            isFleetOwnerPlan = req.isFleetOwnerPlan
          }
  QPlan.create plan
  pure $ Common.CreatePlanResp {planId = planId.getId}

getPlanManagementPlan ::
  ShortId DM.Merchant ->
  Context.City ->
  Text ->
  Flow Common.PlanAPIEntity
getPlanManagementPlan _merchantShortId _opCity planIdText = do
  let planId = Id planIdText
  plan <- QPlan.findByPrimaryKey planId >>= fromMaybeM (InvalidRequest "Plan not found")
  pure $ toPlanAPIEntity plan

postPlanManagementDeletePlan ::
  ShortId DM.Merchant ->
  Context.City ->
  Text ->
  Flow APISuccess
postPlanManagementDeletePlan _merchantShortId _opCity planIdText = do
  let planId = Id planIdText
  _ <- QPlan.findByPrimaryKey planId >>= fromMaybeM (InvalidRequest "Plan not found")
  QPlanExtra.markAsDeprecated planId
  pure Success

getPlanManagementListPlans ::
  ShortId DM.Merchant ->
  Context.City ->
  Maybe Text ->
  Flow Common.ListPlansResp
getPlanManagementListPlans merchantShortId opCity mbServiceNameText = do
  merchant <- findMerchantByShortId merchantShortId
  merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing merchant (Just opCity)
  let mbServiceName = read . toString <$> mbServiceNameText
  allPlans <- QPlanExtra.fetchAllPlanByMerchantOperatingCityMbServiceName merchantOpCityId mbServiceName
  let activePlans = filter (not . (.isDeprecated)) allPlans
  pure $ Common.ListPlansResp {plans = map toPlanAPIEntity activePlans}

-- Conversion helpers

toPlanAPIEntity :: DPlan.Plan -> Common.PlanAPIEntity
toPlanAPIEntity plan =
  Common.PlanAPIEntity
    { id = plan.id.getId,
      name = plan.name,
      description = plan.description,
      paymentMode = toPlanPaymentMode plan.paymentMode,
      planType = toPlanType plan.planType,
      billingType = toPlanBillingType <$> plan.billingType,
      frequency = toPlanFrequency plan.frequency,
      planBaseAmount = show plan.planBaseAmount,
      maxAmount = plan.maxAmount,
      registrationAmount = plan.registrationAmount,
      originalRegistrationAmount = plan.originalRegistrationAmount,
      maxCreditLimit = plan.maxCreditLimit,
      maxMandateAmount = plan.maxMandateAmount,
      productOwnershipAmount = plan.productOwnershipAmount,
      cgstPercentage = plan.cgstPercentage,
      sgstPercentage = plan.sgstPercentage,
      freeRideCount = plan.freeRideCount,
      isOfferApplicable = plan.isOfferApplicable,
      eligibleForCoinDiscount = plan.eligibleForCoinDiscount,
      subscribedFlagToggleAllowed = plan.subscribedFlagToggleAllowed,
      isDeprecated = plan.isDeprecated,
      allowStrikeOff = plan.allowStrikeOff,
      basedOnEntity = toPlanBasedOnEntity plan.basedOnEntity,
      serviceName = show plan.serviceName,
      merchantId = plan.merchantId.getId,
      merchantOpCityId = plan.merchantOpCityId.getId,
      vehicleVariant = show <$> plan.vehicleVariant,
      vehicleCategory = show plan.vehicleCategory,
      listingPriority = plan.listingPriority,
      validityInDays = plan.validityInDays,
      isFleetOwnerPlan = plan.isFleetOwnerPlan
    }

castPaymentMode :: Common.PlanPaymentMode -> DPlan.PaymentMode
castPaymentMode Common.MANUAL = DPlan.MANUAL
castPaymentMode Common.AUTOPAY = DPlan.AUTOPAY

toPlanPaymentMode :: DPlan.PaymentMode -> Common.PlanPaymentMode
toPlanPaymentMode DPlan.MANUAL = Common.MANUAL
toPlanPaymentMode DPlan.AUTOPAY = Common.AUTOPAY

castPlanType :: Common.PlanType -> DPlan.PlanType
castPlanType Common.DEFAULT = DPlan.DEFAULT
castPlanType Common.SUBSCRIPTION = DPlan.SUBSCRIPTION

toPlanType :: DPlan.PlanType -> Common.PlanType
toPlanType DPlan.DEFAULT = Common.DEFAULT
toPlanType DPlan.SUBSCRIPTION = Common.SUBSCRIPTION

castBillingType :: Common.PlanBillingType -> DPlan.BillingType
castBillingType Common.PREPAID = DPlan.PREPAID
castBillingType Common.POSTPAID = DPlan.POSTPAID

toPlanBillingType :: DPlan.BillingType -> Common.PlanBillingType
toPlanBillingType DPlan.PREPAID = Common.PREPAID
toPlanBillingType DPlan.POSTPAID = Common.POSTPAID

castFrequency :: Common.PlanFrequency -> DPlan.Frequency
castFrequency Common.DAILY = DPlan.DAILY
castFrequency Common.WEEKLY = DPlan.WEEKLY
castFrequency Common.MONTHLY = DPlan.MONTHLY
castFrequency Common.FLEXIBLE = DPlan.FLEXIBLE

toPlanFrequency :: DPlan.Frequency -> Common.PlanFrequency
toPlanFrequency DPlan.DAILY = Common.DAILY
toPlanFrequency DPlan.WEEKLY = Common.WEEKLY
toPlanFrequency DPlan.MONTHLY = Common.MONTHLY
toPlanFrequency DPlan.FLEXIBLE = Common.FLEXIBLE

castBasedOnEntity :: Common.PlanBasedOnEntity -> DPlan.BasedOnEntity
castBasedOnEntity Common.RIDE = DPlan.RIDE
castBasedOnEntity Common.NONE = DPlan.NONE
castBasedOnEntity Common.VEHICLE = DPlan.VEHICLE
castBasedOnEntity Common.VEHICLE_AND_RIDE = DPlan.VEHICLE_AND_RIDE

toPlanBasedOnEntity :: DPlan.BasedOnEntity -> Common.PlanBasedOnEntity
toPlanBasedOnEntity DPlan.RIDE = Common.RIDE
toPlanBasedOnEntity DPlan.NONE = Common.NONE
toPlanBasedOnEntity DPlan.VEHICLE = Common.VEHICLE
toPlanBasedOnEntity DPlan.VEHICLE_AND_RIDE = Common.VEHICLE_AND_RIDE
