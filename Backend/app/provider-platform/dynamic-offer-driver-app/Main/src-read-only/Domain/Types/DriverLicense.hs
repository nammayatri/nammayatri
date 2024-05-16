{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.DriverLicense where

import Data.Aeson
import qualified Domain.Types.IdfyVerification
import qualified Domain.Types.Image
import qualified Domain.Types.Merchant
import qualified Domain.Types.Person
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Types.Id
import qualified Tools.Beam.UtilsTH

data DriverLicenseE e = DriverLicense
  { classOfVehicles :: [Kernel.Prelude.Text],
    consent :: Kernel.Prelude.Bool,
    consentTimestamp :: Kernel.Prelude.UTCTime,
    documentImageId1 :: Kernel.Types.Id.Id Domain.Types.Image.Image,
    documentImageId2 :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Image.Image),
    driverDob :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    driverId :: Kernel.Types.Id.Id Domain.Types.Person.Person,
    driverName :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    failedRules :: [Kernel.Prelude.Text],
    id :: Kernel.Types.Id.Id Domain.Types.DriverLicense.DriverLicense,
    licenseExpiry :: Kernel.Prelude.UTCTime,
    licenseNumber :: Kernel.External.Encryption.EncryptedHashedField e Kernel.Prelude.Text,
    verificationStatus :: Domain.Types.IdfyVerification.VerificationStatus,
    merchantId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Merchant.Merchant),
    createdAt :: Kernel.Prelude.UTCTime,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic)

type DriverLicense = DriverLicenseE 'AsEncrypted

type DecryptedDriverLicense = DriverLicenseE 'AsUnencrypted

instance EncryptedItem DriverLicense where
  type Unencrypted DriverLicense = (DecryptedDriverLicense, HashSalt)
  encryptItem (entity, salt) = do
    licenseNumber_ <- encryptItem (licenseNumber entity, salt)
    pure
      DriverLicense
        { classOfVehicles = classOfVehicles entity,
          consent = consent entity,
          consentTimestamp = consentTimestamp entity,
          documentImageId1 = documentImageId1 entity,
          documentImageId2 = documentImageId2 entity,
          driverDob = driverDob entity,
          driverId = driverId entity,
          driverName = driverName entity,
          failedRules = failedRules entity,
          id = id entity,
          licenseExpiry = licenseExpiry entity,
          licenseNumber = licenseNumber_,
          verificationStatus = verificationStatus entity,
          merchantId = merchantId entity,
          createdAt = createdAt entity,
          updatedAt = updatedAt entity
        }
  decryptItem entity = do
    licenseNumber_ <- fst <$> decryptItem (licenseNumber entity)
    pure
      ( DriverLicense
          { classOfVehicles = classOfVehicles entity,
            consent = consent entity,
            consentTimestamp = consentTimestamp entity,
            documentImageId1 = documentImageId1 entity,
            documentImageId2 = documentImageId2 entity,
            driverDob = driverDob entity,
            driverId = driverId entity,
            driverName = driverName entity,
            failedRules = failedRules entity,
            id = id entity,
            licenseExpiry = licenseExpiry entity,
            licenseNumber = licenseNumber_,
            verificationStatus = verificationStatus entity,
            merchantId = merchantId entity,
            createdAt = createdAt entity,
            updatedAt = updatedAt entity
          },
        ""
      )

instance EncryptedItem' DriverLicense where
  type UnencryptedItem DriverLicense = DecryptedDriverLicense
  toUnencrypted a salt = (a, salt)
  fromUnencrypted = fst
