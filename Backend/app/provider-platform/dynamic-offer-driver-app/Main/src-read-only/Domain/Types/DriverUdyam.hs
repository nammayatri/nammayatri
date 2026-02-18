{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.DriverUdyam where

import Data.Aeson
import qualified Domain.Types.DriverPanCard
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.Person
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Types.Documents
import qualified Kernel.Types.Id
import qualified Tools.Beam.UtilsTH

data DriverUdyamE e = DriverUdyam
  { driverId :: Kernel.Types.Id.Id Domain.Types.Person.Person,
    enterpriseName :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    enterpriseType :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    id :: Kernel.Types.Id.Id Domain.Types.DriverUdyam.DriverUdyam,
    merchantOperatingCityId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity),
    udyamNumber :: Kernel.External.Encryption.EncryptedHashedField e Kernel.Prelude.Text,
    verificationStatus :: Kernel.Types.Documents.VerificationStatus,
    verifiedBy :: Kernel.Prelude.Maybe Domain.Types.DriverPanCard.VerifiedBy,
    merchantId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Merchant.Merchant),
    createdAt :: Kernel.Prelude.UTCTime,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic)

type DriverUdyam = DriverUdyamE ('AsEncrypted)

type DecryptedDriverUdyam = DriverUdyamE ('AsUnencrypted)

instance EncryptedItem DriverUdyam where
  type Unencrypted DriverUdyam = (DecryptedDriverUdyam, HashSalt)
  encryptItem (entity, salt) = do
    udyamNumber_ <- encryptItem (udyamNumber entity, salt)
    pure
      DriverUdyam
        { driverId = driverId entity,
          enterpriseName = enterpriseName entity,
          enterpriseType = enterpriseType entity,
          id = id entity,
          merchantOperatingCityId = merchantOperatingCityId entity,
          udyamNumber = udyamNumber_,
          verificationStatus = verificationStatus entity,
          verifiedBy = verifiedBy entity,
          merchantId = merchantId entity,
          createdAt = createdAt entity,
          updatedAt = updatedAt entity
        }
  decryptItem entity = do
    udyamNumber_ <- fst <$> decryptItem (udyamNumber entity)
    pure
      ( DriverUdyam
          { driverId = driverId entity,
            enterpriseName = enterpriseName entity,
            enterpriseType = enterpriseType entity,
            id = id entity,
            merchantOperatingCityId = merchantOperatingCityId entity,
            udyamNumber = udyamNumber_,
            verificationStatus = verificationStatus entity,
            verifiedBy = verifiedBy entity,
            merchantId = merchantId entity,
            createdAt = createdAt entity,
            updatedAt = updatedAt entity
          },
        ""
      )

instance EncryptedItem' DriverUdyam where
  type UnencryptedItem DriverUdyam = DecryptedDriverUdyam
  toUnencrypted a salt = (a, salt)
  fromUnencrypted = fst
