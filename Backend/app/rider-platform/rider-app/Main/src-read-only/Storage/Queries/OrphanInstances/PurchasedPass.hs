{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.OrphanInstances.PurchasedPass where

import qualified Domain.Types.PurchasedPass
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Storage.Beam.PurchasedPass as Beam

instance FromTType' Beam.PurchasedPass Domain.Types.PurchasedPass.PurchasedPass where
  fromTType' (Beam.PurchasedPassT {..}) = do
    pure $
      Just
        Domain.Types.PurchasedPass.PurchasedPass
          { applicableVehicleServiceTiers = applicableVehicleServiceTiers,
            benefitDescription = benefitDescription,
            benefitType = benefitType,
            benefitValue = benefitValue,
            deviceId = deviceId,
            deviceSwitchCount = fromMaybe 0 deviceSwitchCount,
            endDate = endDate,
            id = Kernel.Types.Id.Id id,
            maxAmount = Kernel.Prelude.fromMaybe passAmount maxAmount,
            maxValidDays = maxValidDays,
            maxValidTrips = maxValidTrips,
            merchantId = Kernel.Types.Id.Id merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id merchantOperatingCityId,
            minAmount = Kernel.Prelude.fromMaybe passAmount minAmount,
            passAmount = passAmount,
            passCode = passCode,
            passDescription = passDescription,
            passName = passName,
            passNumber = passNumber,
            passTypeCode = passTypeCode,
            passTypeId = Kernel.Types.Id.Id passTypeId,
            personId = Kernel.Types.Id.Id personId,
            startDate = startDate,
            status = status,
            usedTripCount = usedTripCount,
            verificationValidity = Kernel.Prelude.fromMaybe 9000 verificationValidity,
            createdAt = createdAt,
            updatedAt = updatedAt
          }

instance ToTType' Beam.PurchasedPass Domain.Types.PurchasedPass.PurchasedPass where
  toTType' (Domain.Types.PurchasedPass.PurchasedPass {..}) = do
    Beam.PurchasedPassT
      { Beam.applicableVehicleServiceTiers = applicableVehicleServiceTiers,
        Beam.benefitDescription = benefitDescription,
        Beam.benefitType = benefitType,
        Beam.benefitValue = benefitValue,
        Beam.deviceId = deviceId,
        Beam.deviceSwitchCount = Just deviceSwitchCount,
        Beam.endDate = endDate,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.maxAmount = Just maxAmount,
        Beam.maxValidDays = maxValidDays,
        Beam.maxValidTrips = maxValidTrips,
        Beam.merchantId = Kernel.Types.Id.getId merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId merchantOperatingCityId,
        Beam.minAmount = Just minAmount,
        Beam.passAmount = passAmount,
        Beam.passCode = passCode,
        Beam.passDescription = passDescription,
        Beam.passName = passName,
        Beam.passNumber = passNumber,
        Beam.passTypeCode = passTypeCode,
        Beam.passTypeId = Kernel.Types.Id.getId passTypeId,
        Beam.personId = Kernel.Types.Id.getId personId,
        Beam.startDate = startDate,
        Beam.status = status,
        Beam.usedTripCount = usedTripCount,
        Beam.verificationValidity = Just verificationValidity,
        Beam.createdAt = createdAt,
        Beam.updatedAt = updatedAt
      }
