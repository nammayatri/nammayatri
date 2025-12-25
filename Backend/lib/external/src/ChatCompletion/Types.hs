{-# OPTIONS_GHC -Wno-orphans #-}

module ChatCompletion.Types where

import Kernel.Beam.Lib.UtilsTH
import Kernel.Prelude

data LLMChatCompletionService = AzureOpenAI | Gemini deriving (Show, Read, Eq, Ord, Generic, ToJSON, FromJSON, ToSchema, ToParamSchema)

availableChatCompletionServices :: [LLMChatCompletionService]
availableChatCompletionServices = [AzureOpenAI, Gemini]

$(mkBeamInstancesForEnumAndList ''LLMChatCompletionService)
