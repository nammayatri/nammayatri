{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.IdfyVerification where

import Data.Aeson
import qualified Domain.Types.DocumentVerificationConfig
import qualified Domain.Types.Image
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.Person
import qualified Domain.Types.VehicleCategory
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Types.Id
import qualified Tools.Beam.UtilsTH

data IdfyVerificationE e = IdfyVerification
  { airConditioned :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    docType :: Domain.Types.DocumentVerificationConfig.DocumentType,
    documentImageId1 :: Kernel.Types.Id.Id Domain.Types.Image.Image,
    documentImageId2 :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Image.Image),
    documentNumber :: Kernel.External.Encryption.EncryptedHashedField e Kernel.Prelude.Text,
    driverDateOfBirth :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    driverId :: Kernel.Types.Id.Id Domain.Types.Person.Person,
    id :: Kernel.Types.Id.Id Domain.Types.IdfyVerification.IdfyVerification,
    idfyResponse :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    imageExtractionValidation :: Domain.Types.IdfyVerification.ImageExtractionValidation,
    issueDateOnDoc :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    nameOnCard :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    oxygen :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    requestId :: Kernel.Prelude.Text,
    retryCount :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    status :: Kernel.Prelude.Text,
    vehicleCategory :: Kernel.Prelude.Maybe Domain.Types.VehicleCategory.VehicleCategory,
    ventilator :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    merchantId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Merchant.Merchant),
    merchantOperatingCityId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity),
    createdAt :: Kernel.Prelude.UTCTime,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic)

type IdfyVerification = IdfyVerificationE 'AsEncrypted

type DecryptedIdfyVerification = IdfyVerificationE 'AsUnencrypted

instance EncryptedItem IdfyVerification where
  type Unencrypted IdfyVerification = (DecryptedIdfyVerification, HashSalt)
  encryptItem (entity, salt) = do
    documentNumber_ <- encryptItem (documentNumber entity, salt)
    pure
      IdfyVerification
        { airConditioned = airConditioned entity,
          docType = docType entity,
          documentImageId1 = documentImageId1 entity,
          documentImageId2 = documentImageId2 entity,
          documentNumber = documentNumber_,
          driverDateOfBirth = driverDateOfBirth entity,
          driverId = driverId entity,
          id = id entity,
          idfyResponse = idfyResponse entity,
          imageExtractionValidation = imageExtractionValidation entity,
          issueDateOnDoc = issueDateOnDoc entity,
          nameOnCard = nameOnCard entity,
          oxygen = oxygen entity,
          requestId = requestId entity,
          retryCount = retryCount entity,
          status = status entity,
          vehicleCategory = vehicleCategory entity,
          ventilator = ventilator entity,
          merchantId = merchantId entity,
          merchantOperatingCityId = merchantOperatingCityId entity,
          createdAt = createdAt entity,
          updatedAt = updatedAt entity
        }
  decryptItem entity = do
    documentNumber_ <- fst <$> decryptItem (documentNumber entity)
    pure
      ( IdfyVerification
          { airConditioned = airConditioned entity,
            docType = docType entity,
            documentImageId1 = documentImageId1 entity,
            documentImageId2 = documentImageId2 entity,
            documentNumber = documentNumber_,
            driverDateOfBirth = driverDateOfBirth entity,
            driverId = driverId entity,
            id = id entity,
            idfyResponse = idfyResponse entity,
            imageExtractionValidation = imageExtractionValidation entity,
            issueDateOnDoc = issueDateOnDoc entity,
            nameOnCard = nameOnCard entity,
            oxygen = oxygen entity,
            requestId = requestId entity,
            retryCount = retryCount entity,
            status = status entity,
            vehicleCategory = vehicleCategory entity,
            ventilator = ventilator entity,
            merchantId = merchantId entity,
            merchantOperatingCityId = merchantOperatingCityId entity,
            createdAt = createdAt entity,
            updatedAt = updatedAt entity
          },
        ""
      )

instance EncryptedItem' IdfyVerification where
  type UnencryptedItem IdfyVerification = DecryptedIdfyVerification
  toUnencrypted a salt = (a, salt)
  fromUnencrypted = fst

data ImageExtractionValidation = Success | Skipped | Failed deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema)

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList ''ImageExtractionValidation)
