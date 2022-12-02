{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Domain.Types.DriverOnboarding.VehicleRegistrationCertificate where

import Beckn.External.Encryption
import Beckn.Prelude
import Beckn.Types.Id
import Domain.Types.DriverOnboarding.IdfyVerification
import qualified Domain.Types.DriverOnboarding.Image as Image

data VehicleRegistrationCertificateE e = VehicleRegistrationCertificate
  { id :: Id VehicleRegistrationCertificate,
    documentImageId :: Id Image.Image,
    certificateNumber :: EncryptedHashedField e Text,
    fitnessExpiry :: UTCTime,
    permitExpiry :: Maybe UTCTime,
    pucExpiry :: Maybe UTCTime,
    insuranceValidity :: Maybe UTCTime,
    vehicleClass :: Maybe Text,
    failedRules :: [Text],
    vehicleManufacturer :: Maybe Text,
    vehicleCapacity :: Maybe Int,
    vehicleModel :: Maybe Text,
    vehicleColor :: Maybe Text,
    vehicleEnergyType :: Maybe Text,
    verificationStatus :: VerificationStatus,
    createdAt :: UTCTime,
    updatedAt :: UTCTime
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
