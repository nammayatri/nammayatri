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

module Storage.Tabular.DriverOnboarding.DriverLicense where

import qualified Domain.Types.DriverOnboarding.DriverLicense as Domain
import qualified Domain.Types.DriverOnboarding.IdfyVerification as Domain
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Storage.Esqueleto
import Kernel.Types.Id
import Storage.Tabular.DriverOnboarding.Image (ImageTId)
import Storage.Tabular.Person (PersonTId)

derivePersistField "Domain.VerificationStatus"

mkPersist
  defaultSqlSettings
  [defaultQQ|
    DriverLicenseT sql=driver_license
      id Text
      driverId PersonTId
      documentImageId1 ImageTId
      documentImageId2 ImageTId Maybe
      driverDob UTCTime Maybe
      driverName Text Maybe
      licenseNumberEncrypted Text
      licenseNumberHash DbHash
      licenseExpiry UTCTime
      classOfVehicles (PostgresList Text)
      failedRules (PostgresList Text)
      verificationStatus Domain.VerificationStatus
      consent Bool
      consentTimestamp UTCTime
      createdAt UTCTime
      updatedAt UTCTime
      UniqueDriverLicenseNumber licenseNumberHash
      Primary id
      deriving Generic
    |]

instance TEntityKey DriverLicenseT where
  type DomainKey DriverLicenseT = Id Domain.DriverLicense
  fromKey (DriverLicenseTKey _id) = Id _id
  toKey (Id id) = DriverLicenseTKey id

instance FromTType DriverLicenseT Domain.DriverLicense where
  fromTType DriverLicenseT {..} = do
    return $
      Domain.DriverLicense
        { id = Id id,
          driverId = fromKey driverId,
          documentImageId1 = fromKey documentImageId1,
          documentImageId2 = fromKey <$> documentImageId2,
          licenseNumber = EncryptedHashed (Encrypted licenseNumberEncrypted) licenseNumberHash,
          classOfVehicles = unPostgresList classOfVehicles,
          failedRules = unPostgresList failedRules,
          ..
        }

instance ToTType DriverLicenseT Domain.DriverLicense where
  toTType Domain.DriverLicense {..} =
    DriverLicenseT
      { id = getId id,
        driverId = toKey driverId,
        documentImageId1 = toKey documentImageId1,
        documentImageId2 = toKey <$> documentImageId2,
        licenseNumberEncrypted = licenseNumber & unEncrypted . (.encrypted),
        licenseNumberHash = licenseNumber & (.hash),
        classOfVehicles = PostgresList classOfVehicles,
        failedRules = PostgresList failedRules,
        ..
      }
