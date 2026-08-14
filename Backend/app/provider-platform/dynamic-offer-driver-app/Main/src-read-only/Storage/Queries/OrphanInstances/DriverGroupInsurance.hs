{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.OrphanInstances.DriverGroupInsurance where

import qualified Domain.Types.DriverGroupInsurance
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Storage.Beam.DriverGroupInsurance as Beam

instance FromTType' Beam.DriverGroupInsurance Domain.Types.DriverGroupInsurance.DriverGroupInsurance where
  fromTType' (Beam.DriverGroupInsuranceT {..}) = do
    pure $
      Just
        Domain.Types.DriverGroupInsurance.DriverGroupInsurance
          { age = age,
            createdAt = createdAt,
            dob = dob,
            driverId = Kernel.Types.Id.Id driverId,
            enabledAt = enabledAt,
            fullName = fullName,
            gender = gender,
            id = Kernel.Types.Id.Id id,
            insuranceType = insuranceType,
            lastExportedAt = lastExportedAt,
            merchantId = Kernel.Types.Id.Id merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id merchantOperatingCityId,
            mobile = mobile,
            nomineeDob = nomineeDob,
            nomineeName = nomineeName,
            nomineeRelationship = nomineeRelationship,
            secondBotCheckAt = secondBotCheckAt,
            status = status,
            updatedAt = updatedAt
          }

instance ToTType' Beam.DriverGroupInsurance Domain.Types.DriverGroupInsurance.DriverGroupInsurance where
  toTType' (Domain.Types.DriverGroupInsurance.DriverGroupInsurance {..}) = do
    Beam.DriverGroupInsuranceT
      { Beam.age = age,
        Beam.createdAt = createdAt,
        Beam.dob = dob,
        Beam.driverId = Kernel.Types.Id.getId driverId,
        Beam.enabledAt = enabledAt,
        Beam.fullName = fullName,
        Beam.gender = gender,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.insuranceType = insuranceType,
        Beam.lastExportedAt = lastExportedAt,
        Beam.merchantId = Kernel.Types.Id.getId merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId merchantOperatingCityId,
        Beam.mobile = mobile,
        Beam.nomineeDob = nomineeDob,
        Beam.nomineeName = nomineeName,
        Beam.nomineeRelationship = nomineeRelationship,
        Beam.secondBotCheckAt = secondBotCheckAt,
        Beam.status = status,
        Beam.updatedAt = updatedAt
      }
