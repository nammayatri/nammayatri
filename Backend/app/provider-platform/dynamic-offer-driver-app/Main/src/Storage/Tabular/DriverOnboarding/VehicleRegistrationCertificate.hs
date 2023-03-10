{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Tabular.DriverOnboarding.VehicleRegistrationCertificate where

import qualified Domain.Types.DriverOnboarding.IdfyVerification as Domain
import qualified Domain.Types.DriverOnboarding.VehicleRegistrationCertificate as Domain
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Storage.Esqueleto
import Kernel.Types.Id
import Storage.Tabular.DriverOnboarding.Image (ImageTId)

derivePersistField "Domain.VerificationStatus"

mkPersist
  defaultSqlSettings
  [defaultQQ|
    VehicleRegistrationCertificateT sql=vehicle_registration_certificate
      id Text
      documentImageId ImageTId
      certificateNumberEncrypted Text
      certificateNumberHash DbHash
      fitnessExpiry UTCTime
      permitExpiry UTCTime Maybe
      pucExpiry UTCTime Maybe
      insuranceValidity  UTCTime Maybe
      vehicleClass Text Maybe
      vehicleManufacturer Text Maybe
      vehicleCapacity Int Maybe
      vehicleModel Text Maybe
      vehicleColor Text Maybe
      vehicleEnergyType Text Maybe
      verificationStatus Domain.VerificationStatus
      failedRules (PostgresList Text)
      createdAt UTCTime
      updatedAt UTCTime
      UniqueVehicleRegistrationCertificateRCId certificateNumberHash fitnessExpiry
      Primary id
      deriving Generic
    |]

instance TEntityKey VehicleRegistrationCertificateT where
  type DomainKey VehicleRegistrationCertificateT = Id Domain.VehicleRegistrationCertificate
  fromKey (VehicleRegistrationCertificateTKey _id) = Id _id
  toKey (Id id) = VehicleRegistrationCertificateTKey id

instance FromTType VehicleRegistrationCertificateT Domain.VehicleRegistrationCertificate where
  fromTType VehicleRegistrationCertificateT {..} = do
    return $
      Domain.VehicleRegistrationCertificate
        { id = Id id,
          documentImageId = fromKey documentImageId,
          certificateNumber = EncryptedHashed (Encrypted certificateNumberEncrypted) certificateNumberHash,
          failedRules = unPostgresList failedRules,
          ..
        }

instance ToTType VehicleRegistrationCertificateT Domain.VehicleRegistrationCertificate where
  toTType Domain.VehicleRegistrationCertificate {..} =
    VehicleRegistrationCertificateT
      { id = getId id,
        documentImageId = toKey documentImageId,
        certificateNumberEncrypted = certificateNumber & unEncrypted . (.encrypted),
        certificateNumberHash = certificateNumber & (.hash),
        failedRules = PostgresList failedRules,
        ..
      }
