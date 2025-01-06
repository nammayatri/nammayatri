{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.VehiclePUC where

import Data.Aeson
import qualified Domain.Types.Image
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.Person
import qualified Domain.Types.VehicleRegistrationCertificate
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Types.Documents
import qualified Kernel.Types.Id
import qualified Tools.Beam.UtilsTH

data VehiclePUCE e = VehiclePUC
  { documentImageId :: Kernel.Types.Id.Id Domain.Types.Image.Image,
    driverId :: Kernel.Types.Id.Id Domain.Types.Person.Person,
    id :: Kernel.Types.Id.Id Domain.Types.VehiclePUC.VehiclePUC,
    pucExpiry :: Kernel.Prelude.UTCTime,
    pucNumber :: Kernel.Prelude.Maybe (Kernel.External.Encryption.EncryptedHashedField e Kernel.Prelude.Text),
    rcId :: Kernel.Types.Id.Id Domain.Types.VehicleRegistrationCertificate.VehicleRegistrationCertificate,
    testDate :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    verificationStatus :: Kernel.Types.Documents.VerificationStatus,
    merchantId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Merchant.Merchant),
    merchantOperatingCityId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity),
    createdAt :: Kernel.Prelude.UTCTime,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic)

type VehiclePUC = VehiclePUCE 'AsEncrypted

type DecryptedVehiclePUC = VehiclePUCE 'AsUnencrypted

instance EncryptedItem VehiclePUC where
  type Unencrypted VehiclePUC = (DecryptedVehiclePUC, HashSalt)
  encryptItem (entity, salt) = do
    pucNumber_ <- encryptItem $ (,salt) <$> pucNumber entity
    pure
      VehiclePUC
        { documentImageId = documentImageId entity,
          driverId = driverId entity,
          id = id entity,
          pucExpiry = pucExpiry entity,
          pucNumber = pucNumber_,
          rcId = rcId entity,
          testDate = testDate entity,
          verificationStatus = verificationStatus entity,
          merchantId = merchantId entity,
          merchantOperatingCityId = merchantOperatingCityId entity,
          createdAt = createdAt entity,
          updatedAt = updatedAt entity
        }
  decryptItem entity = do
    pucNumber_ <- fmap fst <$> decryptItem (pucNumber entity)
    pure
      ( VehiclePUC
          { documentImageId = documentImageId entity,
            driverId = driverId entity,
            id = id entity,
            pucExpiry = pucExpiry entity,
            pucNumber = pucNumber_,
            rcId = rcId entity,
            testDate = testDate entity,
            verificationStatus = verificationStatus entity,
            merchantId = merchantId entity,
            merchantOperatingCityId = merchantOperatingCityId entity,
            createdAt = createdAt entity,
            updatedAt = updatedAt entity
          },
        ""
      )

instance EncryptedItem' VehiclePUC where
  type UnencryptedItem VehiclePUC = DecryptedVehiclePUC
  toUnencrypted a salt = (a, salt)
  fromUnencrypted = fst
