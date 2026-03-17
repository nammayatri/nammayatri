{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.MorthVerification where

import Data.Aeson
import qualified Domain.Types.DocumentVerificationConfig
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.Person
import qualified Domain.Types.VehicleCategory
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Types.Id
import qualified Tools.Beam.UtilsTH

data MorthVerificationE e = MorthVerification
  { docType :: Domain.Types.DocumentVerificationConfig.DocumentType,
    documentNumber :: Kernel.External.Encryption.EncryptedHashedField e Kernel.Prelude.Text,
    driverDateOfBirth :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    driverId :: Kernel.Types.Id.Id Domain.Types.Person.Person,
    id :: Kernel.Types.Id.Id Domain.Types.MorthVerification.MorthVerification,
    issueDateOnDoc :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    message :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    morthResponse :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    requestId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    status :: Kernel.Prelude.Text,
    statusCode :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    vehicleCategory :: Kernel.Prelude.Maybe Domain.Types.VehicleCategory.VehicleCategory,
    merchantId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Merchant.Merchant),
    merchantOperatingCityId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity),
    createdAt :: Kernel.Prelude.UTCTime,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic)

type MorthVerification = MorthVerificationE 'AsEncrypted

type DecryptedMorthVerification = MorthVerificationE 'AsUnencrypted

instance EncryptedItem MorthVerification where
  type Unencrypted MorthVerification = (DecryptedMorthVerification, HashSalt)
  encryptItem (entity, salt) = do
    documentNumber_ <- encryptItem (documentNumber entity, salt)
    pure
      MorthVerification
        { docType = docType entity,
          documentNumber = documentNumber_,
          driverDateOfBirth = driverDateOfBirth entity,
          driverId = driverId entity,
          id = id entity,
          issueDateOnDoc = issueDateOnDoc entity,
          message = message entity,
          morthResponse = morthResponse entity,
          requestId = requestId entity,
          status = status entity,
          statusCode = statusCode entity,
          vehicleCategory = vehicleCategory entity,
          merchantId = merchantId entity,
          merchantOperatingCityId = merchantOperatingCityId entity,
          createdAt = createdAt entity,
          updatedAt = updatedAt entity
        }
  decryptItem entity = do
    documentNumber_ <- fst <$> decryptItem (documentNumber entity)
    pure
      ( MorthVerification
          { docType = docType entity,
            documentNumber = documentNumber_,
            driverDateOfBirth = driverDateOfBirth entity,
            driverId = driverId entity,
            id = id entity,
            issueDateOnDoc = issueDateOnDoc entity,
            message = message entity,
            morthResponse = morthResponse entity,
            requestId = requestId entity,
            status = status entity,
            statusCode = statusCode entity,
            vehicleCategory = vehicleCategory entity,
            merchantId = merchantId entity,
            merchantOperatingCityId = merchantOperatingCityId entity,
            createdAt = createdAt entity,
            updatedAt = updatedAt entity
          },
        ""
      )

instance EncryptedItem' MorthVerification where
  type UnencryptedItem MorthVerification = DecryptedMorthVerification
  toUnencrypted a salt = (a, salt)
  fromUnencrypted = fst
