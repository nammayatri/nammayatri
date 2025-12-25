{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.BusinessLicense where

import Data.Aeson
import qualified Domain.Types.Image
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.Person
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Types.Documents
import qualified Kernel.Types.Id
import qualified Tools.Beam.UtilsTH

data BusinessLicenseE e = BusinessLicense
  { documentImageId :: Kernel.Types.Id.Id Domain.Types.Image.Image,
    driverId :: Kernel.Types.Id.Id Domain.Types.Person.Person,
    id :: Kernel.Types.Id.Id Domain.Types.BusinessLicense.BusinessLicense,
    licenseExpiry :: Kernel.Prelude.UTCTime,
    licenseNumber :: Kernel.External.Encryption.EncryptedHashedField e Kernel.Prelude.Text,
    verificationStatus :: Kernel.Types.Documents.VerificationStatus,
    merchantId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Merchant.Merchant),
    merchantOperatingCityId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity),
    createdAt :: Kernel.Prelude.UTCTime,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic)

type BusinessLicense = BusinessLicenseE ('AsEncrypted)

type DecryptedBusinessLicense = BusinessLicenseE ('AsUnencrypted)

instance EncryptedItem BusinessLicense where
  type Unencrypted BusinessLicense = (DecryptedBusinessLicense, HashSalt)
  encryptItem (entity, salt) = do
    licenseNumber_ <- encryptItem (licenseNumber entity, salt)
    pure
      BusinessLicense
        { documentImageId = documentImageId entity,
          driverId = driverId entity,
          id = id entity,
          licenseExpiry = licenseExpiry entity,
          licenseNumber = licenseNumber_,
          verificationStatus = verificationStatus entity,
          merchantId = merchantId entity,
          merchantOperatingCityId = merchantOperatingCityId entity,
          createdAt = createdAt entity,
          updatedAt = updatedAt entity
        }
  decryptItem entity = do
    licenseNumber_ <- fst <$> decryptItem (licenseNumber entity)
    pure
      ( BusinessLicense
          { documentImageId = documentImageId entity,
            driverId = driverId entity,
            id = id entity,
            licenseExpiry = licenseExpiry entity,
            licenseNumber = licenseNumber_,
            verificationStatus = verificationStatus entity,
            merchantId = merchantId entity,
            merchantOperatingCityId = merchantOperatingCityId entity,
            createdAt = createdAt entity,
            updatedAt = updatedAt entity
          },
        ""
      )

instance EncryptedItem' BusinessLicense where
  type UnencryptedItem BusinessLicense = DecryptedBusinessLicense
  toUnencrypted a salt = (a, salt)
  fromUnencrypted = fst
