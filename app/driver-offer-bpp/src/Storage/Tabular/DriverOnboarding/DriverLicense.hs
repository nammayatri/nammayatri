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
import qualified Domain.Types.DriverOnboarding.ClassOfVehicle as Domain
import qualified Domain.Types.DriverOnboarding.DriverLicense as Domain
import Storage.Tabular.Person (PersonTId)

derivePersistField "Domain.VerificationStatus"
derivePersistField "Domain.ClassOfVehicle"

mkPersist
  defaultSqlSettings
  [defaultQQ|
    DriverLicenseT sql=driver_license
      id Text
      driverId PersonTId
      driverDob UTCTime Maybe
      licenseNumber Text
      licenseStart UTCTime Maybe
      licenseExpiry UTCTime Maybe
      classOfVehicles (PostgresList Domain.ClassOfVehicle)
      idfyResponseDump Text Maybe
      idfyRequestId Text Maybe
      verificationStatus Domain.VerificationStatus
      version Int
      active Bool
      consent Bool
      consentTimestamp UTCTime
      createdAt UTCTime
      updatedAt UTCTime
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
          ..
        }
  toTType Domain.DriverLicense {..} =
    DriverLicenseT
      { id = getId id,
        driverId = toKey driverId,
        licenseNumber = licenseNumber & unEncrypted . (.encrypted),
        classOfVehicles = PostgresList classOfVehicles,
        ..
      }
