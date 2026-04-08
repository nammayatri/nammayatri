{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.AadhaarCard where

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

data AadhaarCardE e = AadhaarCard
  { aadhaarBackImageId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Image.Image),
    aadhaarFrontImageId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Image.Image),
    aadhaarNumber :: Kernel.Prelude.Maybe (Kernel.External.Encryption.EncryptedHashedField e Kernel.Prelude.Text),
    address :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    consent :: Kernel.Prelude.Bool,
    consentTimestamp :: Kernel.Prelude.UTCTime,
    createdAt :: Kernel.Prelude.UTCTime,
    dateOfBirth :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    driverGender :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    driverId :: Kernel.Types.Id.Id Domain.Types.Person.Person,
    driverImage :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    driverImagePath :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    maskedAadhaarNumber :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    merchantId :: Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
    merchantOperatingCityId :: Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity,
    nameOnCard :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    updatedAt :: Kernel.Prelude.UTCTime,
    verificationStatus :: Kernel.Types.Documents.VerificationStatus
  }
  deriving (Generic)

type AadhaarCard = AadhaarCardE 'AsEncrypted

type DecryptedAadhaarCard = AadhaarCardE 'AsUnencrypted

instance EncryptedItem AadhaarCard where
  type Unencrypted AadhaarCard = (DecryptedAadhaarCard, HashSalt)
  encryptItem (entity, salt) = do
    aadhaarNumber_ <- encryptItem $ (,salt) <$> aadhaarNumber entity
    pure
      AadhaarCard
        { aadhaarBackImageId = aadhaarBackImageId entity,
          aadhaarFrontImageId = aadhaarFrontImageId entity,
          aadhaarNumber = aadhaarNumber_,
          address = address entity,
          consent = consent entity,
          consentTimestamp = consentTimestamp entity,
          createdAt = createdAt entity,
          dateOfBirth = dateOfBirth entity,
          driverGender = driverGender entity,
          driverId = driverId entity,
          driverImage = driverImage entity,
          driverImagePath = driverImagePath entity,
          maskedAadhaarNumber = maskedAadhaarNumber entity,
          merchantId = merchantId entity,
          merchantOperatingCityId = merchantOperatingCityId entity,
          nameOnCard = nameOnCard entity,
          updatedAt = updatedAt entity,
          verificationStatus = verificationStatus entity
        }
  decryptItem entity = do
    aadhaarNumber_ <- fmap fst <$> decryptItem (aadhaarNumber entity)
    pure
      ( AadhaarCard
          { aadhaarBackImageId = aadhaarBackImageId entity,
            aadhaarFrontImageId = aadhaarFrontImageId entity,
            aadhaarNumber = aadhaarNumber_,
            address = address entity,
            consent = consent entity,
            consentTimestamp = consentTimestamp entity,
            createdAt = createdAt entity,
            dateOfBirth = dateOfBirth entity,
            driverGender = driverGender entity,
            driverId = driverId entity,
            driverImage = driverImage entity,
            driverImagePath = driverImagePath entity,
            maskedAadhaarNumber = maskedAadhaarNumber entity,
            merchantId = merchantId entity,
            merchantOperatingCityId = merchantOperatingCityId entity,
            nameOnCard = nameOnCard entity,
            updatedAt = updatedAt entity,
            verificationStatus = verificationStatus entity
          },
        ""
      )

instance EncryptedItem' AadhaarCard where
  type UnencryptedItem AadhaarCard = DecryptedAadhaarCard
  toUnencrypted a salt = (a, salt)
  fromUnencrypted = fst
