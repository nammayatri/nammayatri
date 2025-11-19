{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.OrphanInstances.Pass where

import qualified Domain.Types.Pass
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
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
            description = description,
            documentsRequired = documentsRequired,
            enable = enable,
            id = Kernel.Types.Id.Id id,
            maxAmount = Kernel.Prelude.fromMaybe amount maxAmount,
            maxValidDays = maxValidDays,
            maxValidTrips = maxValidTrips,
            merchantId = Kernel.Types.Id.Id merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id merchantOperatingCityId,
            minAmount = Kernel.Prelude.fromMaybe amount minAmount,
            name = name,
            order = order,
            passTypeCode = passTypeCode,
            passTypeId = Kernel.Types.Id.Id passTypeId,
            purchaseEligibilityJsonLogic = purchaseEligibilityJsonLogic,
            redeemEligibilityJsonLogic = redeemEligibilityJsonLogic,
            verificationValidity = Kernel.Prelude.fromMaybe 9000 verificationValidity,
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
        Beam.description = description,
        Beam.documentsRequired = documentsRequired,
        Beam.enable = enable,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.maxAmount = Just maxAmount,
        Beam.maxValidDays = maxValidDays,
        Beam.maxValidTrips = maxValidTrips,
        Beam.merchantId = Kernel.Types.Id.getId merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId merchantOperatingCityId,
        Beam.minAmount = Just minAmount,
        Beam.name = name,
        Beam.order = order,
        Beam.passTypeCode = passTypeCode,
        Beam.passTypeId = Kernel.Types.Id.getId passTypeId,
        Beam.purchaseEligibilityJsonLogic = purchaseEligibilityJsonLogic,
        Beam.redeemEligibilityJsonLogic = redeemEligibilityJsonLogic,
        Beam.verificationValidity = Kernel.Prelude.Just verificationValidity,
        Beam.createdAt = createdAt,
        Beam.updatedAt = updatedAt
      }
