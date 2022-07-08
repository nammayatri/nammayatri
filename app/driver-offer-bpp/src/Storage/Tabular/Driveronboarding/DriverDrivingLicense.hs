{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Tabular.Driveronboarding.DriverDrivingLicense where
import Beckn.Prelude
import Beckn.Storage.Esqueleto
import qualified Domain.Types.Driveronboarding.VehicleRegistrationCert as Domain hiding (driverId, id)
import Beckn.Types.Id
import Storage.Tabular.Person (PersonTId)
import Domain.Types.Driveronboarding.DriverDrivingLicense (DLStatus)
import qualified Domain.Types.Driveronboarding.DriverDrivingLicense as Domain
-- import Beckn.Types.Id
derivePersistField "Domain.DLStatus"
derivePersistField "Domain.VehicleClass"
derivePersistField "Domain.DriverLicenseStatus"

mkPersist
  defaultSqlSettings
  [defaultQQ|
  DriverDrivingLicenseT sql = DriverDriving_License
      id Text
      driverId PersonTId
      driverLicenseNumber Text Maybe
      driverLicenseStart UTCTime Maybe
      driverLicenseStatus DLStatus
      driverVerificationStatus Domain.DriverLicenseStatus Maybe
      driverLicenseExpiry UTCTime Maybe
      classOfVehicle [Domain.VehicleClass]
      request_id Text
      createdAt UTCTime
      updatedAt UTCTime
      Primary id
      deriving Generic
    |]
 

instance TEntityKey DriverDrivingLicenseT where
  type DomainKey DriverDrivingLicenseT = Id Domain.DriverDrivingLicense
  fromKey (DriverDrivingLicenseTKey _id) = Id _id
  toKey (Id id) = DriverDrivingLicenseTKey id

instance TType DriverDrivingLicenseT Domain.DriverDrivingLicense where
  fromTType DriverDrivingLicenseT {..} = do
    return $
      Domain.DriverDrivingLicense
        { id = Id id,
          driverId = fromKey driverId,
          ..
        }
   
  toTType Domain.DriverDrivingLicense {..} =
    DriverDrivingLicenseT
      { id = getId id,
        driverId = toKey driverId,
        ..
      }





