{-# LANGUAGE DerivingStrategies #-}

module ChatCompletion.AzureOpenAI.Config where

import Kernel.External.Encryption
import Kernel.Prelude

data AzureOpenAICfg = AzureOpenAICfg
  { azureOpenAIChatCompletionUrl :: BaseUrl,
    apiKey :: EncryptedField 'AsEncrypted Text,
    apiVersion :: Text
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)
