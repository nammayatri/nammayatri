{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.PartnerOrganization where

import Data.Aeson
import qualified Domain.Types.Merchant
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Types.Id
import qualified Tools.Beam.UtilsTH

data PartnerOrganizationE e = PartnerOrganization
  { apiKey :: Kernel.External.Encryption.EncryptedHashedField e Kernel.Prelude.Text,
    createdAt :: Kernel.Prelude.UTCTime,
    merchantId :: Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
    name :: Kernel.Prelude.Text,
    orgId :: Kernel.Types.Id.Id Domain.Types.PartnerOrganization.PartnerOrganization,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic)

type PartnerOrganization = PartnerOrganizationE 'AsEncrypted

type DecryptedPartnerOrganization = PartnerOrganizationE 'AsUnencrypted

instance EncryptedItem PartnerOrganization where
  type Unencrypted PartnerOrganization = (DecryptedPartnerOrganization, HashSalt)
  encryptItem (entity, salt) = do
    apiKey_ <- encryptItem (apiKey entity, salt)
    pure PartnerOrganization {apiKey = apiKey_, createdAt = createdAt entity, merchantId = merchantId entity, name = name entity, orgId = orgId entity, updatedAt = updatedAt entity}
  decryptItem entity = do
    apiKey_ <- fst <$> decryptItem (apiKey entity)
    pure (PartnerOrganization {apiKey = apiKey_, createdAt = createdAt entity, merchantId = merchantId entity, name = name entity, orgId = orgId entity, updatedAt = updatedAt entity}, "")

instance EncryptedItem' PartnerOrganization where
  type UnencryptedItem PartnerOrganization = DecryptedPartnerOrganization
  toUnencrypted a salt = (a, salt)
  fromUnencrypted = fst

data PartnerOrgTransaction = PartnerOrgTransaction {} deriving (Generic, Show, ToJSON, FromJSON, ToSchema)
