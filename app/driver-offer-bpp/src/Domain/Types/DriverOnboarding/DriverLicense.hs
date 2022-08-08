{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Domain.Types.DriverOnboarding.DriverLicense where

import Beckn.External.Encryption
import Beckn.Prelude
import Beckn.Types.Id
import Domain.Types.DriverOnboarding.ClassOfVehicle
import Domain.Types.Person (Person)

data DriverLicenseE e = DriverLicense
  { id :: Id DriverLicense,
    driverId :: Id Person,
    driverDob :: Maybe UTCTime,
    licenseNumber :: EncryptedHashedField e Text,
    licenseStart :: Maybe UTCTime,
    licenseExpiry :: Maybe UTCTime,
    classOfVehicles :: [ClassOfVehicle],
    idfyResponseDump :: Maybe Text,
    idfyRequestId :: Maybe Text,
    verificationStatus :: VerificationStatus,
    imageS3Path :: Text,
    createdAt :: UTCTime,
    updatedAt :: UTCTime,
    verificationTryCount :: Int,
    consent :: Bool,
    consentTimestamp :: UTCTime
  }
  deriving (Generic)

type DriverLicense = DriverLicenseE 'AsEncrypted

type DecryptedDriverLicense = DriverLicenseE 'AsUnencrypted

instance EncryptedItem DriverLicense where
  type Unencrypted DriverLicense = (DecryptedDriverLicense, HashSalt)
  encryptItem (DriverLicense {..}, salt) = do
    licenseNumber_ <- encryptItem $ (,salt) licenseNumber
    return DriverLicense {licenseNumber = licenseNumber_, ..}
  decryptItem DriverLicense {..} = do
    licenseNumber_ <- fst <$> decryptItem licenseNumber
    return (DriverLicense {licenseNumber = licenseNumber_, ..}, "")

instance EncryptedItem' DriverLicense where
  type UnencryptedItem DriverLicense = DecryptedDriverLicense
  toUnencrypted a salt = (a, salt)
  fromUnencrypted a = fst a
