{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.UnserviceableCityLead where

import Data.Aeson
import qualified Domain.Types.Merchant
import qualified Domain.Types.VehicleCategory
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Types.Id
import qualified Tools.Beam.UtilsTH

data UnserviceableCityLeadE e = UnserviceableCityLead
  { createdAt :: Kernel.Prelude.UTCTime,
    id :: Kernel.Types.Id.Id Domain.Types.UnserviceableCityLead.UnserviceableCityLead,
    lat :: Kernel.Prelude.Maybe Kernel.Prelude.Double,
    lon :: Kernel.Prelude.Maybe Kernel.Prelude.Double,
    merchantId :: Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
    mobileCountryCode :: Kernel.Prelude.Text,
    mobileNumber :: Kernel.External.Encryption.EncryptedHashedField e Kernel.Prelude.Text,
    name :: Kernel.Prelude.Text,
    updatedAt :: Kernel.Prelude.UTCTime,
    vehicleCategory :: Domain.Types.VehicleCategory.VehicleCategory
  }
  deriving (Generic)

type UnserviceableCityLead = UnserviceableCityLeadE 'AsEncrypted

type DecryptedUnserviceableCityLead = UnserviceableCityLeadE 'AsUnencrypted

instance EncryptedItem UnserviceableCityLead where
  type Unencrypted UnserviceableCityLead = (DecryptedUnserviceableCityLead, HashSalt)
  encryptItem (entity, salt) = do
    mobileNumber_ <- encryptItem (mobileNumber entity, salt)
    pure
      UnserviceableCityLead
        { createdAt = createdAt entity,
          id = id entity,
          lat = lat entity,
          lon = lon entity,
          merchantId = merchantId entity,
          mobileCountryCode = mobileCountryCode entity,
          mobileNumber = mobileNumber_,
          name = name entity,
          updatedAt = updatedAt entity,
          vehicleCategory = vehicleCategory entity
        }
  decryptItem entity = do
    mobileNumber_ <- fst <$> decryptItem (mobileNumber entity)
    pure
      ( UnserviceableCityLead
          { createdAt = createdAt entity,
            id = id entity,
            lat = lat entity,
            lon = lon entity,
            merchantId = merchantId entity,
            mobileCountryCode = mobileCountryCode entity,
            mobileNumber = mobileNumber_,
            name = name entity,
            updatedAt = updatedAt entity,
            vehicleCategory = vehicleCategory entity
          },
        ""
      )

instance EncryptedItem' UnserviceableCityLead where
  type UnencryptedItem UnserviceableCityLead = DecryptedUnserviceableCityLead
  toUnencrypted a salt = (a, salt)
  fromUnencrypted = fst
