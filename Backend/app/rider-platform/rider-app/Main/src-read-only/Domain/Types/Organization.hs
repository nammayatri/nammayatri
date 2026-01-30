{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.Organization where

import Data.Aeson
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Types.Id
import qualified Tools.Beam.UtilsTH

data OrganizationE e = Organization
  { contactName :: Kernel.Prelude.Text,
    contactPhoneNumber :: Kernel.Prelude.Maybe (Kernel.External.Encryption.EncryptedHashedField e Kernel.Prelude.Text),
    contactRole :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    createdAt :: Kernel.Prelude.UTCTime,
    id :: Kernel.Types.Id.Id Domain.Types.Organization.Organization,
    merchantId :: Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
    merchantOperatingCityId :: Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity,
    organizationAddress :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    organizationName :: Kernel.Prelude.Text,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic)

type Organization = OrganizationE ('AsEncrypted)

type DecryptedOrganization = OrganizationE ('AsUnencrypted)

instance EncryptedItem Organization where
  type Unencrypted Organization = (DecryptedOrganization, HashSalt)
  encryptItem (entity, salt) = do
    contactPhoneNumber_ <- encryptItem $ (,salt) <$> contactPhoneNumber entity
    pure
      Organization
        { contactName = contactName entity,
          contactPhoneNumber = contactPhoneNumber_,
          contactRole = contactRole entity,
          createdAt = createdAt entity,
          id = id entity,
          merchantId = merchantId entity,
          merchantOperatingCityId = merchantOperatingCityId entity,
          organizationAddress = organizationAddress entity,
          organizationName = organizationName entity,
          updatedAt = updatedAt entity
        }
  decryptItem entity = do
    contactPhoneNumber_ <- fmap fst <$> decryptItem (contactPhoneNumber entity)
    pure
      ( Organization
          { contactName = contactName entity,
            contactPhoneNumber = contactPhoneNumber_,
            contactRole = contactRole entity,
            createdAt = createdAt entity,
            id = id entity,
            merchantId = merchantId entity,
            merchantOperatingCityId = merchantOperatingCityId entity,
            organizationAddress = organizationAddress entity,
            organizationName = organizationName entity,
            updatedAt = updatedAt entity
          },
        ""
      )

instance EncryptedItem' Organization where
  type UnencryptedItem Organization = DecryptedOrganization
  toUnencrypted a salt = (a, salt)
  fromUnencrypted = fst
