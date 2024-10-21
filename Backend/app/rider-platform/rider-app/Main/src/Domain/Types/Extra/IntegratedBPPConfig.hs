{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.Extra.IntegratedBPPConfig where

import Data.Aeson
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Base64

data EBIXConfig = EBIXConfig
  { agentId :: Text,
    username :: Text,
    password :: EncryptedField 'AsEncrypted Text,
    networkHostUrl :: BaseUrl
  }
  deriving stock (Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

data CUMTAConfig = CUMTAConfig
  { cipherKey :: Base64
  }
  deriving stock (Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

data CMRLConfig = CMRLConfig
  { networkHostUrl :: BaseUrl,
    username :: Text,
    password :: EncryptedField 'AsEncrypted Text
  }
  deriving stock (Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)
