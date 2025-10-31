{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.OrphanInstances.PurchasedPass where

import qualified Domain.Types.PurchasedPass
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
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
            endDate = endDate,
            id = Kernel.Types.Id.Id id,
            maxValidDays = maxValidDays,
            maxValidTrips = maxValidTrips,
            merchantId = Kernel.Types.Id.Id merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id merchantOperatingCityId,
            passAmount = passAmount,
            passCode = passCode,
            passName = passName,
            passNumber = passNumber,
            passTypeId = Kernel.Types.Id.Id passTypeId,
            personId = Kernel.Types.Id.Id personId,
            startDate = startDate,
            status = status,
            usedTripCount = usedTripCount,
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
        Beam.endDate = endDate,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.maxValidDays = maxValidDays,
        Beam.maxValidTrips = maxValidTrips,
        Beam.merchantId = Kernel.Types.Id.getId merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId merchantOperatingCityId,
        Beam.passAmount = passAmount,
        Beam.passCode = passCode,
        Beam.passName = passName,
        Beam.passNumber = passNumber,
        Beam.passTypeId = Kernel.Types.Id.getId passTypeId,
        Beam.personId = Kernel.Types.Id.getId personId,
        Beam.startDate = startDate,
        Beam.status = status,
        Beam.usedTripCount = usedTripCount,
        Beam.createdAt = createdAt,
        Beam.updatedAt = updatedAt
      }
