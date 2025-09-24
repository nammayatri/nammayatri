{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.VehicleInsurance where

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

data VehicleInsuranceE e = VehicleInsurance
  { documentImageId :: Kernel.Types.Id.Id Domain.Types.Image.Image,
    driverId :: Kernel.Types.Id.Id Domain.Types.Person.Person,
    id :: Kernel.Types.Id.Id Domain.Types.VehicleInsurance.VehicleInsurance,
    insuredName :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    issueDate :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    limitsOfLiability :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    policyExpiry :: Kernel.Prelude.UTCTime,
    policyNumber :: Kernel.External.Encryption.EncryptedHashedField e Kernel.Prelude.Text,
    policyProvider :: Kernel.Prelude.Text,
    rcId :: Kernel.Types.Id.Id Domain.Types.VehicleRegistrationCertificate.VehicleRegistrationCertificate,
    rejectReason :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    verificationStatus :: Kernel.Types.Documents.VerificationStatus,
    merchantId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Merchant.Merchant),
    merchantOperatingCityId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity),
    createdAt :: Kernel.Prelude.UTCTime,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic)

type VehicleInsurance = VehicleInsuranceE ('AsEncrypted)

type DecryptedVehicleInsurance = VehicleInsuranceE ('AsUnencrypted)

instance EncryptedItem VehicleInsurance where
  type Unencrypted VehicleInsurance = (DecryptedVehicleInsurance, HashSalt)
  encryptItem (entity, salt) = do
    policyNumber_ <- encryptItem (policyNumber entity, salt)
    pure
      VehicleInsurance
        { documentImageId = documentImageId entity,
          driverId = driverId entity,
          id = id entity,
          insuredName = insuredName entity,
          issueDate = issueDate entity,
          limitsOfLiability = limitsOfLiability entity,
          policyExpiry = policyExpiry entity,
          policyNumber = policyNumber_,
          policyProvider = policyProvider entity,
          rcId = rcId entity,
          rejectReason = rejectReason entity,
          verificationStatus = verificationStatus entity,
          merchantId = merchantId entity,
          merchantOperatingCityId = merchantOperatingCityId entity,
          createdAt = createdAt entity,
          updatedAt = updatedAt entity
        }
  decryptItem entity = do
    policyNumber_ <- fst <$> decryptItem (policyNumber entity)
    pure
      ( VehicleInsurance
          { documentImageId = documentImageId entity,
            driverId = driverId entity,
            id = id entity,
            insuredName = insuredName entity,
            issueDate = issueDate entity,
            limitsOfLiability = limitsOfLiability entity,
            policyExpiry = policyExpiry entity,
            policyNumber = policyNumber_,
            policyProvider = policyProvider entity,
            rcId = rcId entity,
            rejectReason = rejectReason entity,
            verificationStatus = verificationStatus entity,
            merchantId = merchantId entity,
            merchantOperatingCityId = merchantOperatingCityId entity,
            createdAt = createdAt entity,
            updatedAt = updatedAt entity
          },
        ""
      )

instance EncryptedItem' VehicleInsurance where
  type UnencryptedItem VehicleInsurance = DecryptedVehicleInsurance
  toUnencrypted a salt = (a, salt)
  fromUnencrypted = fst
