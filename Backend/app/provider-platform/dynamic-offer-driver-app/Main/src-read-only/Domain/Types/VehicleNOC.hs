{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.VehicleNOC where

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

data VehicleNOCE e = VehicleNOC
  { documentImageId :: Kernel.Types.Id.Id Domain.Types.Image.Image,
    driverId :: Kernel.Types.Id.Id Domain.Types.Person.Person,
    id :: Kernel.Types.Id.Id Domain.Types.VehicleNOC.VehicleNOC,
    nocExpiry :: Kernel.Prelude.UTCTime,
    nocNumber :: Kernel.External.Encryption.EncryptedHashedField e Kernel.Prelude.Text,
    rcId :: Kernel.Types.Id.Id Domain.Types.VehicleRegistrationCertificate.VehicleRegistrationCertificate,
    verificationStatus :: Kernel.Types.Documents.VerificationStatus,
    merchantId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Merchant.Merchant),
    merchantOperatingCityId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity),
    createdAt :: Kernel.Prelude.UTCTime,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic)

type VehicleNOC = VehicleNOCE ('AsEncrypted)

type DecryptedVehicleNOC = VehicleNOCE ('AsUnencrypted)

instance EncryptedItem VehicleNOC where
  type Unencrypted VehicleNOC = (DecryptedVehicleNOC, HashSalt)
  encryptItem (entity, salt) = do
    nocNumber_ <- encryptItem (nocNumber entity, salt)
    pure
      VehicleNOC
        { documentImageId = documentImageId entity,
          driverId = driverId entity,
          id = id entity,
          nocExpiry = nocExpiry entity,
          nocNumber = nocNumber_,
          rcId = rcId entity,
          verificationStatus = verificationStatus entity,
          merchantId = merchantId entity,
          merchantOperatingCityId = merchantOperatingCityId entity,
          createdAt = createdAt entity,
          updatedAt = updatedAt entity
        }
  decryptItem entity = do
    nocNumber_ <- fst <$> decryptItem (nocNumber entity)
    pure
      ( VehicleNOC
          { documentImageId = documentImageId entity,
            driverId = driverId entity,
            id = id entity,
            nocExpiry = nocExpiry entity,
            nocNumber = nocNumber_,
            rcId = rcId entity,
            verificationStatus = verificationStatus entity,
            merchantId = merchantId entity,
            merchantOperatingCityId = merchantOperatingCityId entity,
            createdAt = createdAt entity,
            updatedAt = updatedAt entity
          },
        ""
      )

instance EncryptedItem' VehicleNOC where
  type UnencryptedItem VehicleNOC = DecryptedVehicleNOC
  toUnencrypted a salt = (a, salt)
  fromUnencrypted = fst
