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
    driverDob :: Maybe UTCTime,
    driverLicenseNumber :: Maybe (EncryptedHashedField e Text), -- remove Maybe Data Type
    driverLicenseStart :: Maybe UTCTime,
    driverLicenseExpiry :: Maybe UTCTime,
    classOfVehicle :: Maybe [COV], -- to be changed
    idfyStatus :: IdfyStatus,
    verificationStatus :: VerificationStatus,
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
    idfyStatus :: IdfyStatus,
    verificationStatus :: VerificationStatus,
    driverLicenseExpiry :: Maybe UTCTime,
    classOfVehicle :: Maybe [COV],
    request_id :: Text
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)

makeDrivingLicenseEntity :: DecryptedDriverDrivingLicense -> DriverDrivingLicenseAPIEntity
makeDrivingLicenseEntity DriverDrivingLicense {..} =
  DriverDrivingLicenseAPIEntity
    { driverLicenseNumber = maskText <$> driverLicenseNumber,
      ..
    }
    