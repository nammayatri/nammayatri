{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.DriverGstin where

import Data.Aeson
import qualified Domain.Types.DriverPanCard
import qualified Domain.Types.Image
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.Person
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Types.Documents
import qualified Kernel.Types.Id
import qualified Tools.Beam.UtilsTH

data DriverGstinE e = DriverGstin
  { address :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    constitutionOfBusiness :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    dateOfLiability :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    documentImageId1 :: Kernel.Types.Id.Id Domain.Types.Image.Image,
    documentImageId2 :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Image.Image),
    driverId :: Kernel.Types.Id.Id Domain.Types.Person.Person,
    driverName :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    gstin :: Kernel.External.Encryption.EncryptedHashedField e Kernel.Prelude.Text,
    id :: Kernel.Types.Id.Id Domain.Types.DriverGstin.DriverGstin,
    isProvisional :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    isStrictlyVerified :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    legalName :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    merchantOperatingCityId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity),
    panNumber :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    tradeName :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    typeOfRegistration :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    validFrom :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    validUpto :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    verificationStatus :: Kernel.Types.Documents.VerificationStatus,
    verifiedBy :: Kernel.Prelude.Maybe Domain.Types.DriverPanCard.VerifiedBy,
    merchantId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Merchant.Merchant),
    createdAt :: Kernel.Prelude.UTCTime,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic)

type DriverGstin = DriverGstinE ('AsEncrypted)

type DecryptedDriverGstin = DriverGstinE ('AsUnencrypted)

instance EncryptedItem DriverGstin where
  type Unencrypted DriverGstin = (DecryptedDriverGstin, HashSalt)
  encryptItem (entity, salt) = do
    gstin_ <- encryptItem (gstin entity, salt)
    pure
      DriverGstin
        { address = address entity,
          constitutionOfBusiness = constitutionOfBusiness entity,
          dateOfLiability = dateOfLiability entity,
          documentImageId1 = documentImageId1 entity,
          documentImageId2 = documentImageId2 entity,
          driverId = driverId entity,
          driverName = driverName entity,
          gstin = gstin_,
          id = id entity,
          isProvisional = isProvisional entity,
          isStrictlyVerified = isStrictlyVerified entity,
          legalName = legalName entity,
          merchantOperatingCityId = merchantOperatingCityId entity,
          panNumber = panNumber entity,
          tradeName = tradeName entity,
          typeOfRegistration = typeOfRegistration entity,
          validFrom = validFrom entity,
          validUpto = validUpto entity,
          verificationStatus = verificationStatus entity,
          verifiedBy = verifiedBy entity,
          merchantId = merchantId entity,
          createdAt = createdAt entity,
          updatedAt = updatedAt entity
        }
  decryptItem entity = do
    gstin_ <- fst <$> decryptItem (gstin entity)
    pure
      ( DriverGstin
          { address = address entity,
            constitutionOfBusiness = constitutionOfBusiness entity,
            dateOfLiability = dateOfLiability entity,
            documentImageId1 = documentImageId1 entity,
            documentImageId2 = documentImageId2 entity,
            driverId = driverId entity,
            driverName = driverName entity,
            gstin = gstin_,
            id = id entity,
            isProvisional = isProvisional entity,
            isStrictlyVerified = isStrictlyVerified entity,
            legalName = legalName entity,
            merchantOperatingCityId = merchantOperatingCityId entity,
            panNumber = panNumber entity,
            tradeName = tradeName entity,
            typeOfRegistration = typeOfRegistration entity,
            validFrom = validFrom entity,
            validUpto = validUpto entity,
            verificationStatus = verificationStatus entity,
            verifiedBy = verifiedBy entity,
            merchantId = merchantId entity,
            createdAt = createdAt entity,
            updatedAt = updatedAt entity
          },
        ""
      )

instance EncryptedItem' DriverGstin where
  type UnencryptedItem DriverGstin = DecryptedDriverGstin
  toUnencrypted a salt = (a, salt)
  fromUnencrypted = fst
