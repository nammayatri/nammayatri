{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.PartnerOrganization where

import Data.Aeson
import qualified Domain.Types.Merchant
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Types.Id
import qualified Tools.Beam.UtilsTH

data PartnerOrganizationE e = PartnerOrganization
  { orgId :: Kernel.Types.Id.Id Domain.Types.PartnerOrganization.PartnerOrganization,
    name :: Kernel.Prelude.Text,
    apiKey :: Kernel.External.Encryption.EncryptedHashedField e Kernel.Prelude.Text,
    merchantId :: Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
    createdAt :: Kernel.Prelude.UTCTime,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic)

type PartnerOrganization = PartnerOrganizationE 'AsEncrypted

type DecryptedPartnerOrganization = PartnerOrganizationE 'AsUnencrypted

instance EncryptedItem PartnerOrganization where
  type Unencrypted PartnerOrganization = (DecryptedPartnerOrganization, HashSalt)
  encryptItem (entity, salt) = do
    apiKey_ <- encryptItem (apiKey entity, salt)
    pure PartnerOrganization {orgId = orgId entity, name = name entity, apiKey = apiKey_, merchantId = merchantId entity, createdAt = createdAt entity, updatedAt = updatedAt entity}
  decryptItem entity = do
    apiKey_ <- fst <$> decryptItem (apiKey entity)
    pure (PartnerOrganization {orgId = orgId entity, name = name entity, apiKey = apiKey_, merchantId = merchantId entity, createdAt = createdAt entity, updatedAt = updatedAt entity}, "")

instance EncryptedItem' PartnerOrganization where
  type UnencryptedItem PartnerOrganization = DecryptedPartnerOrganization
  toUnencrypted a salt = (a, salt)
  fromUnencrypted = fst

data PartnerOrgTransaction = PartnerOrgTransaction {} deriving (Generic, Show, ToJSON, FromJSON, ToSchema)
