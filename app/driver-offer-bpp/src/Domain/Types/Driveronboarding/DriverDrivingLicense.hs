{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Domain.Types.Driveronboarding.DriverDrivingLicense where

import Beckn.External.Encryption
import Beckn.Prelude
import Beckn.Types.Id
import Domain.Types.Driveronboarding.VehicleRegistrationCert
import Domain.Types.Person (Person)

data DriverDrivingLicenseE e = DriverDrivingLicense
  { id :: Id DriverDrivingLicense,
    driverId :: Id Person,
    driverDob :: Maybe UTCTime,
    driverLicenseNumber :: Maybe (EncryptedHashedField e Text), -- remove Maybe Data Type
    driverLicenseStart :: Maybe UTCTime,
    driverLicenseExpiry :: Maybe UTCTime,
    classOfVehicle :: Maybe [COV],
    idfyStatus :: IdfyStatus,
    verificationRespDump :: Text,
    verificationStatus :: VerificationStatus,
    idfyRequestId :: Text,
    dlImage1S3Path :: Text,
    dlImage2S3Path :: Text,
    createdAt :: UTCTime,
    updatedAt :: UTCTime,
    consent :: Bool,
    consentTimestamp :: UTCTime
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
