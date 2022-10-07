{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Tabular.DriverOnboarding.DriverLicense where

import Beckn.External.Encryption
import Beckn.Prelude
import Beckn.Storage.Esqueleto
import Beckn.Types.Id
import qualified Domain.Types.DriverOnboarding.DriverLicense as Domain
import qualified Domain.Types.DriverOnboarding.IdfyVerification as Domain
import qualified Idfy.Types as Idfy
import Storage.Tabular.DriverOnboarding.Image (ImageTId)
import Storage.Tabular.Person (PersonTId)

derivePersistField "Domain.VerificationStatus"
derivePersistField "Idfy.ClassOfVehicle"

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
      classOfVehicles (PostgresList Idfy.ClassOfVehicle)
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

instance TType DriverLicenseT Domain.DriverLicense where
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
