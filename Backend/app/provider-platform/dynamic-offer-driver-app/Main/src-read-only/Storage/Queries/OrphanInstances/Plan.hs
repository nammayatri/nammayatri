{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.OrphanInstances.Plan where

import qualified Domain.Types.Plan
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (KvDbFlow, fromMaybeM, getCurrentTime)
import qualified Storage.Beam.Plan as Beam

instance FromTType' Beam.Plan Domain.Types.Plan.Plan where
  fromTType' (Beam.PlanT {..}) = do
    pure $
      Just
        Domain.Types.Plan.Plan
          { basedOnEntity = basedOnEntity,
            cgstPercentage = cgstPercentage,
            description = description,
            eligibleForCoinDiscount = eligibleForCoinDiscount,
            freeRideCount = freeRideCount,
            frequency = frequency,
            id = Kernel.Types.Id.Id id,
            isDeprecated = isDeprecated,
            isOfferApplicable = isOfferApplicable,
            maxAmount = maxAmount,
            maxCreditLimit = maxCreditLimit,
            maxMandateAmount = maxMandateAmount,
            merchantId = Kernel.Types.Id.Id merchantId,
            merchantOpCityId = Kernel.Types.Id.Id merchantOpCityId,
            name = name,
            paymentMode = paymentMode,
            planBaseAmount = planBaseAmount,
            planType = planType,
            registrationAmount = registrationAmount,
            serviceName = serviceName,
            sgstPercentage = sgstPercentage,
            subscribedFlagToggleAllowed = subscribedFlagToggleAllowed,
            merchantOperatingCityId = Kernel.Types.Id.Id <$> merchantOperatingCityId
          }

instance ToTType' Beam.Plan Domain.Types.Plan.Plan where
  toTType' (Domain.Types.Plan.Plan {..}) = do
    Beam.PlanT
      { Beam.basedOnEntity = basedOnEntity,
        Beam.cgstPercentage = cgstPercentage,
        Beam.description = description,
        Beam.eligibleForCoinDiscount = eligibleForCoinDiscount,
        Beam.freeRideCount = freeRideCount,
        Beam.frequency = frequency,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.isDeprecated = isDeprecated,
        Beam.isOfferApplicable = isOfferApplicable,
        Beam.maxAmount = maxAmount,
        Beam.maxCreditLimit = maxCreditLimit,
        Beam.maxMandateAmount = maxMandateAmount,
        Beam.merchantId = Kernel.Types.Id.getId merchantId,
        Beam.merchantOpCityId = Kernel.Types.Id.getId merchantOpCityId,
        Beam.name = name,
        Beam.paymentMode = paymentMode,
        Beam.planBaseAmount = planBaseAmount,
        Beam.planType = planType,
        Beam.registrationAmount = registrationAmount,
        Beam.serviceName = serviceName,
        Beam.sgstPercentage = sgstPercentage,
        Beam.subscribedFlagToggleAllowed = subscribedFlagToggleAllowed,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId <$> merchantOperatingCityId
      }
