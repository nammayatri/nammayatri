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
import Domain.Types.Driveronboarding.VehicleRegistrationCert (IdfyStatus(..), COV (..), VerificationStatus (..))
import Beckn.External.Encryption
import qualified Data.ByteString as BS
-- import Beckn.Types.Id

derivePersistField "IdfyStatus"
derivePersistField "VerificationStatus"
derivePersistField "COV"

mkPersist
  defaultSqlSettings
  [defaultQQ|
  DriverDrivingLicenseT sql = _driver_driving_license_t
      id Text
      driverId PersonTId
      driverDob UTCTime Maybe
      driverLicenseNumber Text Maybe
      driverLicenseStart UTCTime Maybe
      driverLicenseExpiry UTCTime Maybe
      classOfVehicle [COV] Maybe
      idfyStatus IdfyStatus
      verificationStatus VerificationStatus
      driverVerificationStatus VerificationStatus
      request_id Text
      consent Bool
      consentTimestamp UTCTime
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
          driverLicenseNumber = EncryptedHashed <$> (Encrypted <$> driverLicenseNumber) <*> Just (DbHash BS.empty),
          ..
        }
  toTType Domain.DriverDrivingLicense {..} =
    DriverDrivingLicenseT
      { id = getId id,
        driverId = toKey driverId,
        driverLicenseNumber = driverLicenseNumber <&> unEncrypted . (.encrypted),
        ..
      }