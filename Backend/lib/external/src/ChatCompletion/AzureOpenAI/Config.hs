module ChatCompletion.AzureOpenAI.Config where

import Kernel.External.Encryption
import Kernel.Prelude

data AzureOpenAICfg = AzureOpenAICfg
  { azureOpenAIChatCompletionUrl :: BaseUrl,
    azureApiKey :: EncryptedField 'AsEncrypted Text,
    azureApiVersion :: Text
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)
