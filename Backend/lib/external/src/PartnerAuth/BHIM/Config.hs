module PartnerAuth.BHIM.Config where

import Kernel.External.Encryption
import Kernel.Prelude

-- | BHIM provider configuration, stored per merchant/op-city in
-- MerchantServiceConfig.config_json. The AES key is encrypted at rest
-- (EncryptedField) and decrypted only at call time.
data BhimCfg = BhimCfg
  { baseUrl :: BaseUrl,
    partnerId :: Text,
    aesKey :: EncryptedField 'AsEncrypted Text
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)
