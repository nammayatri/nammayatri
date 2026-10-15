{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.DocumentVersionMetadata where

import Data.Aeson
import qualified Domain.Types.DocumentVerificationConfig
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.Person
import qualified Domain.Types.VehicleVariant
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Types.Id
import qualified Tools.Beam.UtilsTH

data DocumentVersionMetadataE e = DocumentVersionMetadata
  { documentNumber :: Kernel.External.Encryption.EncryptedHashedField e Kernel.Prelude.Text,
    documentType :: Domain.Types.DocumentVerificationConfig.DocumentType,
    documentVersionId :: Kernel.Prelude.Text,
    metadata :: Kernel.Prelude.Maybe Domain.Types.DocumentVersionMetadata.VersionMetadata,
    personId :: Kernel.Types.Id.Id Domain.Types.Person.Person,
    merchantId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Merchant.Merchant),
    merchantOperatingCityId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity),
    createdAt :: Kernel.Prelude.UTCTime,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic)

type DocumentVersionMetadata = DocumentVersionMetadataE ('AsEncrypted)

type DecryptedDocumentVersionMetadata = DocumentVersionMetadataE ('AsUnencrypted)

instance EncryptedItem DocumentVersionMetadata where
  type Unencrypted DocumentVersionMetadata = (DecryptedDocumentVersionMetadata, HashSalt)
  encryptItem (entity, salt) = do
    documentNumber_ <- encryptItem (documentNumber entity, salt)
    pure
      DocumentVersionMetadata
        { documentNumber = documentNumber_,
          documentType = documentType entity,
          documentVersionId = documentVersionId entity,
          metadata = metadata entity,
          personId = personId entity,
          merchantId = merchantId entity,
          merchantOperatingCityId = merchantOperatingCityId entity,
          createdAt = createdAt entity,
          updatedAt = updatedAt entity
        }
  decryptItem entity = do
    documentNumber_ <- fst <$> decryptItem (documentNumber entity)
    pure
      ( DocumentVersionMetadata
          { documentNumber = documentNumber_,
            documentType = documentType entity,
            documentVersionId = documentVersionId entity,
            metadata = metadata entity,
            personId = personId entity,
            merchantId = merchantId entity,
            merchantOperatingCityId = merchantOperatingCityId entity,
            createdAt = createdAt entity,
            updatedAt = updatedAt entity
          },
        ""
      )

instance EncryptedItem' DocumentVersionMetadata where
  type UnencryptedItem DocumentVersionMetadata = DecryptedDocumentVersionMetadata
  toUnencrypted a salt = (a, salt)
  fromUnencrypted = fst

data DLVersionMetadata = DLVersionMetadata {dateOfExpiry :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime, driverDateOfBirth :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime}
  deriving (Generic, (FromJSON), (ToJSON), (Eq), (Show))

data RCVersionMetadata = RCVersionMetadata
  { fitnessExpiry :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    vehicleColor :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    vehicleManufacturer :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    vehicleModel :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    vehicleModelYear :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    vehicleVariant :: Kernel.Prelude.Maybe Domain.Types.VehicleVariant.VehicleVariant
  }
  deriving (Generic, (FromJSON), (ToJSON), (Eq), (Show))

data VersionMetadata
  = DLMetadata Domain.Types.DocumentVersionMetadata.DLVersionMetadata
  | RCMetadata Domain.Types.DocumentVersionMetadata.RCVersionMetadata
  deriving (Generic, (FromJSON), (ToJSON), (Eq), (Show))
