{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Domain.Types.Driveronboarding.DriverDrivingLicense where
import Beckn.Prelude
import Beckn.Types.Id
import Domain.Types.Person (Person)
import Domain.Types.Driveronboarding.VehicleRegistrationCert
import Beckn.External.Encryption
import Beckn.Utils.Common

data DriverDrivingLicenseE e = DriverDrivingLicense {
    id :: Id DriverDrivingLicense,
    driverId :: Id Person,
    driverLicenseNumber :: Maybe (EncryptedHashedField e Text),
    driverLicenseStart :: Maybe UTCTime,
    driverLicenseStatus :: IdfyStatus,
    driverVerificationStatus :: Maybe IdfyStatus,
    driverLicenseExpiry :: Maybe UTCTime,
    classOfVehicle :: Maybe [COV], -- to be changed
    request_id :: Text,
    createdAt :: UTCTime,
    updatedAt :: UTCTime
}
    deriving (Generic)

type DriverDrivingLicense = DriverDrivingLicenseE 'AsEncrypted
type DecryptedDriverDrivingLicense = DriverDrivingLicenseE 'AsUnencrypted
deriving instance Show DecryptedDriverDrivingLicense

instance EncryptedItem DriverDrivingLicense where
  type Unencrypted DriverDrivingLicense = (DecryptedDriverDrivingLicense, HashSalt)
  encryptItem (DriverDrivingLicense {..}, salt) = do
    driverLicenseNumber_ <- encryptItem $ (,salt) <$> driverLicenseNumber
    return DriverDrivingLicense {driverLicenseNumber = driverLicenseNumber_, ..}
  decryptItem DriverDrivingLicense {..} = do
    driverLicenseNumber_ <- fmap fst <$> decryptItem driverLicenseNumber
    return (DriverDrivingLicense {driverLicenseNumber = driverLicenseNumber_, ..}, "")

instance EncryptedItem' DriverDrivingLicense where
  type UnencryptedItem DriverDrivingLicense = DecryptedDriverDrivingLicense
  toUnencrypted a salt = (a, salt)
  fromUnencrypted a = fst a



data DriverDrivingLicenseAPIEntity = DriverDrivingLicenseAPIEntity
  { 
    id :: Id DriverDrivingLicense,
    driverId :: Id Person,
    driverLicenseNumber :: Maybe Text,
    driverLicenseStart :: Maybe UTCTime,
    driverLicenseStatus :: IdfyStatus,
    driverVerificationStatus :: Maybe IdfyStatus,
    driverLicenseExpiry :: Maybe UTCTime,
    classOfVehicle :: Maybe [COV],
    request_id :: Text
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)

makePersonAPIEntity :: DecryptedDriverDrivingLicense -> DriverDrivingLicenseAPIEntity
makePersonAPIEntity DriverDrivingLicense {..} =
  DriverDrivingLicenseAPIEntity
    { driverLicenseNumber = maskText <$> driverLicenseNumber,
      ..
    }
    