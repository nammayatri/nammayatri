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
import qualified Data.ByteString as BS
import qualified Domain.Types.DriverOnboarding.DriverLicense as Domain
import qualified Domain.Types.DriverOnboarding.IdfyVerification as Domain
import qualified Idfy.Types as Idfy
import Storage.Tabular.Person (PersonTId)

derivePersistField "Domain.VerificationStatus"
derivePersistField "Idfy.ClassOfVehicle"

mkPersist
  defaultSqlSettings
  [defaultQQ|
    DriverLicenseT sql=driver_license
      id Text
      driverId PersonTId
      driverDob UTCTime Maybe
      licenseNumber Text
      licenseExpiry UTCTime
      classOfVehicles (PostgresList Idfy.ClassOfVehicle)
      failedRules (PostgresList Text)
      verificationStatus Domain.VerificationStatus
      consent Bool
      consentTimestamp UTCTime
      createdAt UTCTime
      updatedAt UTCTime
      UniqueDriverLicenseNumber licenseNumber
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
          licenseNumber = EncryptedHashed (Encrypted licenseNumber) (DbHash BS.empty),
          classOfVehicles = unPostgresList classOfVehicles,
          failedRules = unPostgresList failedRules,
          ..
        }
  toTType Domain.DriverLicense {..} =
    DriverLicenseT
      { id = getId id,
        driverId = toKey driverId,
        licenseNumber = licenseNumber & unEncrypted . (.encrypted),
        classOfVehicles = PostgresList classOfVehicles,
        failedRules = PostgresList failedRules,
        ..
      }
