{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Tabular.Driveronboarding.DriverDrivingLicense where
import Beckn.Prelude
import Beckn.Storage.Esqueleto
import Beckn.Types.Id
import Storage.Tabular.Person (PersonTId)
import qualified Domain.Types.Driveronboarding.DriverDrivingLicense as Domain1
import qualified Domain.Types.Driveronboarding.VehicleRegistrationCert as Domain2

-- import Beckn.Types.Id

derivePersistField "Domain1.Verification1"
derivePersistField "Domain2.VehicleClass"

mkPersist
  defaultSqlSettings
  [defaultQQ|
  DriverDrivingLicenseT sql = DriverDriving_License
      id Text
      driverId PersonTId
      driverLicenseNumber Text Maybe
      driverLicenseStart UTCTime Maybe
      driverLicenseStatus Domain1.Verification1
      driverVerificationStatus Domain1.Verification1 Maybe
      driverLicenseExpiry UTCTime Maybe
      classOfVehicle [Domain2.VehicleClass]
      request_id Text
      createdAt UTCTime
      updatedAt UTCTime
      Primary id
      deriving Generic
    |]
 

instance TEntityKey DriverDrivingLicenseT where
  type DomainKey DriverDrivingLicenseT = Id Domain1.DriverDrivingLicense
  fromKey (DriverDrivingLicenseTKey _id) = Id _id
  toKey (Id id) = DriverDrivingLicenseTKey id

instance TType DriverDrivingLicenseT Domain1.DriverDrivingLicense where
  fromTType DriverDrivingLicenseT {..} = do
    return $
      Domain1.DriverDrivingLicense
        { id = Id id,
          driverId = fromKey driverId,
          ..
        }
   
  toTType Domain1.DriverDrivingLicense {..} =
    DriverDrivingLicenseT
      { id = getId id,
        driverId = toKey driverId,
        ..
      }





