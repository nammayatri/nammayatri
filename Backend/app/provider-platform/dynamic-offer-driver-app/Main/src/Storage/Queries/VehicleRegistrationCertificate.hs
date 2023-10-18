{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Queries.VehicleRegistrationCertificate where

import Domain.Types.DriverOnboarding.VehicleRegistrationCertificate
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Id
import qualified Storage.Beam.DriverOnboarding.VehicleRegistrationCertificate as BeamVRC

instance FromTType' BeamVRC.VehicleRegistrationCertificate VehicleRegistrationCertificate where
  fromTType' BeamVRC.VehicleRegistrationCertificateT {..} = do
    pure $
      Just
        VehicleRegistrationCertificate
          { id = Id id,
            documentImageId = Id documentImageId,
            certificateNumber = EncryptedHashed (Encrypted certificateNumberEncrypted) certificateNumberHash,
            fitnessExpiry = fitnessExpiry,
            permitExpiry = permitExpiry,
            pucExpiry = pucExpiry,
            insuranceValidity = insuranceValidity,
            vehicleClass = vehicleClass,
            vehicleVariant = vehicleVariant,
            vehicleManufacturer = vehicleManufacturer,
            vehicleCapacity = vehicleCapacity,
            vehicleModel = vehicleModel,
            vehicleColor = vehicleColor,
            vehicleEnergyType = vehicleEnergyType,
            verificationStatus = verificationStatus,
            failedRules = failedRules,
            fleetOwnerId = fleetOwnerId,
            createdAt = createdAt,
            updatedAt = updatedAt
          }

instance ToTType' BeamVRC.VehicleRegistrationCertificate VehicleRegistrationCertificate where
  toTType' VehicleRegistrationCertificate {..} = do
    BeamVRC.VehicleRegistrationCertificateT
      { BeamVRC.id = getId id,
        BeamVRC.documentImageId = getId documentImageId,
        BeamVRC.certificateNumberEncrypted = unEncrypted certificateNumber.encrypted,
        BeamVRC.certificateNumberHash = certificateNumber.hash,
        BeamVRC.fitnessExpiry = fitnessExpiry,
        BeamVRC.permitExpiry = permitExpiry,
        BeamVRC.pucExpiry = pucExpiry,
        BeamVRC.insuranceValidity = insuranceValidity,
        BeamVRC.vehicleClass = vehicleClass,
        BeamVRC.vehicleVariant = vehicleVariant,
        BeamVRC.vehicleManufacturer = vehicleManufacturer,
        BeamVRC.vehicleCapacity = vehicleCapacity,
        BeamVRC.vehicleModel = vehicleModel,
        BeamVRC.vehicleColor = vehicleColor,
        BeamVRC.vehicleEnergyType = vehicleEnergyType,
        BeamVRC.verificationStatus = verificationStatus,
        BeamVRC.failedRules = failedRules,
        BeamVRC.fleetOwnerId = fleetOwnerId,
        BeamVRC.createdAt = createdAt,
        BeamVRC.updatedAt = updatedAt
      }
