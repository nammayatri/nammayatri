{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Domain.Types.DriverOnboarding.VehicleRegistrationCertificate where

import Beckn.External.Encryption
import Beckn.Prelude
import Beckn.Types.Id
import Domain.Types.DriverOnboarding.ClassOfVehicle
import Domain.Types.Person (Person)

data VehicleRegistrationCertificateE e = VehicleRegistrationCertificate
  { id :: Id VehicleRegistrationCertificate,
    driverId :: Id Person,
    certificateNumber :: EncryptedHashedField e Text,
    fitnessExpiry :: Maybe UTCTime,
    permitNumber :: Maybe Text,
    permitStart :: Maybe UTCTime,
    permitExpiry :: Maybe UTCTime,
    pucExpiry :: Maybe UTCTime,
    vehicleClass :: Maybe ClassOfVehicle,
    vehicleColor :: Maybe Text,
    vehicleManufacturer :: Maybe Text,
    vehicleModel :: Maybe Text,
    insuranceValidity :: Maybe UTCTime,
    idfyRequestId :: Maybe Text,
    idfyResponseDump :: Maybe Text,
    verificationStatus :: VerificationStatus,
    version :: Int,
    active :: Bool,
    createdAt :: UTCTime,
    updatedAt :: UTCTime,
    consent :: Bool,
    consentTimestamp :: UTCTime
  }
  deriving (Generic)

type VehicleRegistrationCertificate = VehicleRegistrationCertificateE 'AsEncrypted

type DecryptedVehicleRegistrationCertificate = VehicleRegistrationCertificateE 'AsUnencrypted

deriving instance Show DecryptedVehicleRegistrationCertificate

instance EncryptedItem VehicleRegistrationCertificate where
  type Unencrypted VehicleRegistrationCertificate = (DecryptedVehicleRegistrationCertificate, HashSalt)
  encryptItem (VehicleRegistrationCertificate {..}, salt) = do
    certificateNumber_ <- encryptItem $ (,salt) certificateNumber
    return VehicleRegistrationCertificate {certificateNumber = certificateNumber_, ..}
  decryptItem VehicleRegistrationCertificate {..} = do
    certificateNumber_ <- fst <$> decryptItem certificateNumber
    return (VehicleRegistrationCertificate {certificateNumber = certificateNumber_, ..}, "")

instance EncryptedItem' VehicleRegistrationCertificate where
  type UnencryptedItem VehicleRegistrationCertificate = DecryptedVehicleRegistrationCertificate
  toUnencrypted a salt = (a, salt)
  fromUnencrypted a = fst a
