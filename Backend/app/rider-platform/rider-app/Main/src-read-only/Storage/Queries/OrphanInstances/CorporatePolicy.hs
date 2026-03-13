{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.OrphanInstances.CorporatePolicy where

import qualified Domain.Types.CorporatePolicy
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Storage.Beam.CorporatePolicy as Beam

instance FromTType' Beam.CorporatePolicy Domain.Types.CorporatePolicy.CorporatePolicy where
  fromTType' (Beam.CorporatePolicyT {..}) = do
    pure $
      Just
        Domain.Types.CorporatePolicy.CorporatePolicy
          { id = Kernel.Types.Id.Id id,
            corporateEntityId = Kernel.Types.Id.Id corporateEntityId,
            name = name,
            policyType = Kernel.Prelude.fromMaybe Domain.Types.CorporatePolicy.RIDE_BUDGET (Kernel.Prelude.readMaybe (Kernel.Prelude.toString policyType)),
            maxFarePerTrip = Kernel.Prelude.realToFrac <$> maxFarePerTrip,
            maxMonthlyBudgetPerEmployee = Kernel.Prelude.realToFrac <$> maxMonthlyBudgetPerEmployee,
            allowedServiceTiers = allowedServiceTiers,
            requiresApproval = requiresApproval,
            nightShiftSafetyEnabled = nightShiftSafetyEnabled,
            womenSafetyRulesEnabled = womenSafetyRulesEnabled,
            surgeCap = surgeCap,
            isActive = isActive,
            createdAt = createdAt,
            updatedAt = updatedAt
          }

instance ToTType' Beam.CorporatePolicy Domain.Types.CorporatePolicy.CorporatePolicy where
  toTType' (Domain.Types.CorporatePolicy.CorporatePolicy {..}) = do
    Beam.CorporatePolicyT
      { Beam.id = Kernel.Types.Id.getId id,
        Beam.corporateEntityId = Kernel.Types.Id.getId corporateEntityId,
        Beam.name = name,
        Beam.policyType = Kernel.Prelude.show policyType,
        Beam.maxFarePerTrip = Kernel.Prelude.realToFrac <$> maxFarePerTrip,
        Beam.maxMonthlyBudgetPerEmployee = Kernel.Prelude.realToFrac <$> maxMonthlyBudgetPerEmployee,
        Beam.allowedServiceTiers = allowedServiceTiers,
        Beam.requiresApproval = requiresApproval,
        Beam.nightShiftSafetyEnabled = nightShiftSafetyEnabled,
        Beam.womenSafetyRulesEnabled = womenSafetyRulesEnabled,
        Beam.surgeCap = surgeCap,
        Beam.isActive = isActive,
        Beam.createdAt = createdAt,
        Beam.updatedAt = updatedAt
      }
