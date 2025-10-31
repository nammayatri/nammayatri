{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.OrphanInstances.Pass where

import qualified Domain.Types.Pass
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Storage.Beam.Pass as Beam

instance FromTType' Beam.Pass Domain.Types.Pass.Pass where
  fromTType' (Beam.PassT {..}) = do
    pure $
      Just
        Domain.Types.Pass.Pass
          { amount = amount,
            applicableVehicleServiceTiers = applicableVehicleServiceTiers,
            autoApply = autoApply,
            benefit = benefit,
            benefitDescription = benefitDescription,
            code = code,
            documentsRequired = documentsRequired,
            enable = enable,
            id = Kernel.Types.Id.Id id,
            maxValidDays = maxValidDays,
            maxValidTrips = maxValidTrips,
            merchantId = Kernel.Types.Id.Id merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id merchantOperatingCityId,
            name = name,
            order = order,
            passTypeId = Kernel.Types.Id.Id passTypeId,
            purchaseEligibilityJsonLogic = purchaseEligibilityJsonLogic,
            redeemEligibilityJsonLogic = redeemEligibilityJsonLogic,
            createdAt = createdAt,
            updatedAt = updatedAt
          }

instance ToTType' Beam.Pass Domain.Types.Pass.Pass where
  toTType' (Domain.Types.Pass.Pass {..}) = do
    Beam.PassT
      { Beam.amount = amount,
        Beam.applicableVehicleServiceTiers = applicableVehicleServiceTiers,
        Beam.autoApply = autoApply,
        Beam.benefit = benefit,
        Beam.benefitDescription = benefitDescription,
        Beam.code = code,
        Beam.documentsRequired = documentsRequired,
        Beam.enable = enable,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.maxValidDays = maxValidDays,
        Beam.maxValidTrips = maxValidTrips,
        Beam.merchantId = Kernel.Types.Id.getId merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId merchantOperatingCityId,
        Beam.name = name,
        Beam.order = order,
        Beam.passTypeId = Kernel.Types.Id.getId passTypeId,
        Beam.purchaseEligibilityJsonLogic = purchaseEligibilityJsonLogic,
        Beam.redeemEligibilityJsonLogic = redeemEligibilityJsonLogic,
        Beam.createdAt = createdAt,
        Beam.updatedAt = updatedAt
      }
