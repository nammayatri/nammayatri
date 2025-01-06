{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.VehicleFitnessCertificate where

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

data VehicleFitnessCertificateE e = VehicleFitnessCertificate
  { applicationNumber :: Kernel.External.Encryption.EncryptedHashedField e Kernel.Prelude.Text,
    categoryOfVehicle :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    documentImageId :: Kernel.Types.Id.Id Domain.Types.Image.Image,
    driverId :: Kernel.Types.Id.Id Domain.Types.Person.Person,
    fitnessExpiry :: Kernel.Prelude.UTCTime,
    id :: Kernel.Types.Id.Id Domain.Types.VehicleFitnessCertificate.VehicleFitnessCertificate,
    inspectingAuthority :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    inspectingOn :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    nextInspectionDate :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    rcId :: Kernel.Types.Id.Id Domain.Types.VehicleRegistrationCertificate.VehicleRegistrationCertificate,
    receiptDate :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    verificationStatus :: Kernel.Types.Documents.VerificationStatus,
    merchantId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Merchant.Merchant),
    merchantOperatingCityId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity),
    createdAt :: Kernel.Prelude.UTCTime,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic)

type VehicleFitnessCertificate = VehicleFitnessCertificateE 'AsEncrypted

type DecryptedVehicleFitnessCertificate = VehicleFitnessCertificateE 'AsUnencrypted

instance EncryptedItem VehicleFitnessCertificate where
  type Unencrypted VehicleFitnessCertificate = (DecryptedVehicleFitnessCertificate, HashSalt)
  encryptItem (entity, salt) = do
    applicationNumber_ <- encryptItem (applicationNumber entity, salt)
    pure
      VehicleFitnessCertificate
        { applicationNumber = applicationNumber_,
          categoryOfVehicle = categoryOfVehicle entity,
          documentImageId = documentImageId entity,
          driverId = driverId entity,
          fitnessExpiry = fitnessExpiry entity,
          id = id entity,
          inspectingAuthority = inspectingAuthority entity,
          inspectingOn = inspectingOn entity,
          nextInspectionDate = nextInspectionDate entity,
          rcId = rcId entity,
          receiptDate = receiptDate entity,
          verificationStatus = verificationStatus entity,
          merchantId = merchantId entity,
          merchantOperatingCityId = merchantOperatingCityId entity,
          createdAt = createdAt entity,
          updatedAt = updatedAt entity
        }
  decryptItem entity = do
    applicationNumber_ <- fst <$> decryptItem (applicationNumber entity)
    pure
      ( VehicleFitnessCertificate
          { applicationNumber = applicationNumber_,
            categoryOfVehicle = categoryOfVehicle entity,
            documentImageId = documentImageId entity,
            driverId = driverId entity,
            fitnessExpiry = fitnessExpiry entity,
            id = id entity,
            inspectingAuthority = inspectingAuthority entity,
            inspectingOn = inspectingOn entity,
            nextInspectionDate = nextInspectionDate entity,
            rcId = rcId entity,
            receiptDate = receiptDate entity,
            verificationStatus = verificationStatus entity,
            merchantId = merchantId entity,
            merchantOperatingCityId = merchantOperatingCityId entity,
            createdAt = createdAt entity,
            updatedAt = updatedAt entity
          },
        ""
      )

instance EncryptedItem' VehicleFitnessCertificate where
  type UnencryptedItem VehicleFitnessCertificate = DecryptedVehicleFitnessCertificate
  toUnencrypted a salt = (a, salt)
  fromUnencrypted = fst
