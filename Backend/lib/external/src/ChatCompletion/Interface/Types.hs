{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module ChatCompletion.Interface.Types where

import ChatCompletion.AzureOpenAI.Config as AzureOpenAI
import ChatCompletion.Gemini.Config as Gemini
import Data.Aeson.Types
import Kernel.Prelude

data LLMChatCompletionServiceConfig = AzureOpenAI AzureOpenAI.AzureOpenAICfg | Gemini Gemini.GeminiCfg
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

newtype GeneralChatCompletionReq = GeneralChatCompletionReq
  { genMessages :: [GeneralChatCompletionMessage]
  }
  deriving (Show, Generic, ToJSON, FromJSON)

data GeneralChatCompletionMessage = GeneralChatCompletionMessage
  { genRole :: Text,
    genContent :: Text
  }
  deriving (Show, Generic, ToJSON, FromJSON)

newtype GeneralChatCompletionResp = GeneralChatCompletionResp
  { genMessage :: GeneralChatCompletionMessage
  }
  deriving (Show, Generic, ToJSON, FromJSON)
