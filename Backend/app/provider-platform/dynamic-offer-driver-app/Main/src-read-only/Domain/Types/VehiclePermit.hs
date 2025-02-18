{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.VehiclePermit where

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

data VehiclePermitE e = VehiclePermit
  { documentImageId :: Kernel.Types.Id.Id Domain.Types.Image.Image,
    driverId :: Kernel.Types.Id.Id Domain.Types.Person.Person,
    id :: Kernel.Types.Id.Id Domain.Types.VehiclePermit.VehiclePermit,
    issueDate :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    nameOfPermitHolder :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    permitExpiry :: Kernel.Prelude.UTCTime,
    permitNumber :: Kernel.External.Encryption.EncryptedHashedField e Kernel.Prelude.Text,
    purposeOfJourney :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    rcId :: Kernel.Types.Id.Id Domain.Types.VehicleRegistrationCertificate.VehicleRegistrationCertificate,
    regionCovered :: Kernel.Prelude.Text,
    verificationStatus :: Kernel.Types.Documents.VerificationStatus,
    merchantId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Merchant.Merchant),
    merchantOperatingCityId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity),
    createdAt :: Kernel.Prelude.UTCTime,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic)

type VehiclePermit = VehiclePermitE 'AsEncrypted

type DecryptedVehiclePermit = VehiclePermitE 'AsUnencrypted

instance EncryptedItem VehiclePermit where
  type Unencrypted VehiclePermit = (DecryptedVehiclePermit, HashSalt)
  encryptItem (entity, salt) = do
    permitNumber_ <- encryptItem (permitNumber entity, salt)
    pure
      VehiclePermit
        { documentImageId = documentImageId entity,
          driverId = driverId entity,
          id = id entity,
          issueDate = issueDate entity,
          nameOfPermitHolder = nameOfPermitHolder entity,
          permitExpiry = permitExpiry entity,
          permitNumber = permitNumber_,
          purposeOfJourney = purposeOfJourney entity,
          rcId = rcId entity,
          regionCovered = regionCovered entity,
          verificationStatus = verificationStatus entity,
          merchantId = merchantId entity,
          merchantOperatingCityId = merchantOperatingCityId entity,
          createdAt = createdAt entity,
          updatedAt = updatedAt entity
        }
  decryptItem entity = do
    permitNumber_ <- fst <$> decryptItem (permitNumber entity)
    pure
      ( VehiclePermit
          { documentImageId = documentImageId entity,
            driverId = driverId entity,
            id = id entity,
            issueDate = issueDate entity,
            nameOfPermitHolder = nameOfPermitHolder entity,
            permitExpiry = permitExpiry entity,
            permitNumber = permitNumber_,
            purposeOfJourney = purposeOfJourney entity,
            rcId = rcId entity,
            regionCovered = regionCovered entity,
            verificationStatus = verificationStatus entity,
            merchantId = merchantId entity,
            merchantOperatingCityId = merchantOperatingCityId entity,
            createdAt = createdAt entity,
            updatedAt = updatedAt entity
          },
        ""
      )

instance EncryptedItem' VehiclePermit where
  type UnencryptedItem VehiclePermit = DecryptedVehiclePermit
  toUnencrypted a salt = (a, salt)
  fromUnencrypted = fst
