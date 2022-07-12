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
import qualified Domain.Types.Driveronboarding.DriverDrivingLicense as Domain
import Domain.Types.Driveronboarding.VehicleRegistrationCert (IdfyStatus(..), COV (..))
-- import Beckn.Types.Id

derivePersistField "IdfyStatus"
derivePersistField "COV"

mkPersist
  defaultSqlSettings
  [defaultQQ|
  DriverDrivingLicenseT sql = _driver_driving_license_t
      id Text
      driverId PersonTId
      driverLicenseNumber Text Maybe
      driverLicenseStart UTCTime Maybe
      driverLicenseStatus IdfyStatus
      driverVerificationStatus IdfyStatus Maybe
      driverLicenseExpiry UTCTime Maybe
      classOfVehicle [COV] Maybe
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