{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.OrphanInstances.Plan where

import qualified Domain.Types.Plan
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Storage.Beam.Plan as Beam
import qualified Storage.Queries.Transformers.Plan

instance FromTType' Beam.Plan Domain.Types.Plan.Plan where
  fromTType' (Beam.PlanT {..}) = do
    vehicleCategory' <- Storage.Queries.Transformers.Plan.getCategoryFromSubscriptionConfig vehicleCategory merchantOpCityId serviceName
    pure $
      Just
        Domain.Types.Plan.Plan
          { allowStrikeOff = Kernel.Prelude.fromMaybe True allowStrikeOff,
            basedOnEntity = basedOnEntity,
            billingType = billingType,
            cgstPercentage = cgstPercentage,
            description = description,
            eligibleForCoinDiscount = eligibleForCoinDiscount,
            freeRideCount = freeRideCount,
            frequency = frequency,
            id = Kernel.Types.Id.Id id,
            isDeprecated = isDeprecated,
            isFleetOwnerPlan = isFleetOwnerPlan,
            isOfferApplicable = isOfferApplicable,
            listingPriority = listingPriority,
            maxAmount = maxAmount,
            maxCreditLimit = maxCreditLimit,
            maxMandateAmount = maxMandateAmount,
            merchantId = Kernel.Types.Id.Id merchantId,
            merchantOpCityId = Kernel.Types.Id.Id merchantOpCityId,
            name = name,
            paymentMode = paymentMode,
            planBaseAmount = planBaseAmount,
            planType = planType,
            productOwnershipAmount = Kernel.Prelude.fromMaybe 0 productOwnershipAmount,
            registrationAmount = registrationAmount,
            serviceName = serviceName,
            sgstPercentage = sgstPercentage,
            subscribedFlagToggleAllowed = subscribedFlagToggleAllowed,
            validityInDays = validityInDays,
            vehicleCategory = vehicleCategory',
            vehicleVariant = vehicleVariant
          }

instance ToTType' Beam.Plan Domain.Types.Plan.Plan where
  toTType' (Domain.Types.Plan.Plan {..}) = do
    Beam.PlanT
      { Beam.allowStrikeOff = Kernel.Prelude.Just allowStrikeOff,
        Beam.basedOnEntity = basedOnEntity,
        Beam.billingType = billingType,
        Beam.cgstPercentage = cgstPercentage,
        Beam.description = description,
        Beam.eligibleForCoinDiscount = eligibleForCoinDiscount,
        Beam.freeRideCount = freeRideCount,
        Beam.frequency = frequency,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.isDeprecated = isDeprecated,
        Beam.isFleetOwnerPlan = isFleetOwnerPlan,
        Beam.isOfferApplicable = isOfferApplicable,
        Beam.listingPriority = listingPriority,
        Beam.maxAmount = maxAmount,
        Beam.maxCreditLimit = maxCreditLimit,
        Beam.maxMandateAmount = maxMandateAmount,
        Beam.merchantId = Kernel.Types.Id.getId merchantId,
        Beam.merchantOpCityId = Kernel.Types.Id.getId merchantOpCityId,
        Beam.name = name,
        Beam.paymentMode = paymentMode,
        Beam.planBaseAmount = planBaseAmount,
        Beam.planType = planType,
        Beam.productOwnershipAmount = Kernel.Prelude.Just productOwnershipAmount,
        Beam.registrationAmount = registrationAmount,
        Beam.serviceName = serviceName,
        Beam.sgstPercentage = sgstPercentage,
        Beam.subscribedFlagToggleAllowed = subscribedFlagToggleAllowed,
        Beam.validityInDays = validityInDays,
        Beam.vehicleCategory = Kernel.Prelude.Just vehicleCategory,
        Beam.vehicleVariant = vehicleVariant
      }
