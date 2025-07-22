{-# LANGUAGE ApplicativeDo #-}

module Domain.Types.Extra.IntegratedBPPConfig where

import Data.Aeson
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Base64
import Kernel.Types.Time

data EBIXConfig = EBIXConfig
  { agentId :: Text,
    username :: Text,
    password :: EncryptedField 'AsEncrypted Text,
    networkHostUrl :: BaseUrl
  }
  deriving stock (Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

data DIRECTConfig = DIRECTConfig
  { cipherKey :: Base64,
    qrRefreshTtl :: Maybe Seconds
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

data ONDCBecknConfig = ONDCBecknConfig
  { networkHostUrl :: Maybe BaseUrl,
    networkId :: Maybe Text,
    multiInitAllowed :: Maybe Bool,
    fareCachingAllowed :: Maybe Bool,
    singleTicketForMultiplePassengers :: Maybe Bool
  }
  deriving stock (Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

data CRISConfig = CRISConfig
  { baseUrl :: BaseUrl,
    consumerKey :: EncryptedField 'AsEncrypted Text,
    consumerSecret :: EncryptedField 'AsEncrypted Text,
    decryptionKey :: EncryptedField 'AsEncrypted Text,
    clientSecret :: EncryptedField 'AsEncrypted Text,
    encryptionKey :: EncryptedField 'AsEncrypted Text,
    appCode :: Text,
    tpAccountId :: Int,
    sourceZone :: Text,
    ticketType :: Text,
    agentDataDecryptionKey :: EncryptedField 'AsEncrypted Text,
    utsDataKey :: EncryptedField 'AsEncrypted Text
  }
  deriving stock (Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)
