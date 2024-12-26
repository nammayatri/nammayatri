module ChatCompletion.Gemini.Config where

import Kernel.External.Encryption
import Kernel.Prelude

data GeminiCfg = GeminiCfg
  { geminiChatCompletionUrl :: BaseUrl,
    geminiApiKey :: EncryptedField 'AsEncrypted Text
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)
