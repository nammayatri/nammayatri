{-# LANGUAGE DerivingStrategies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module ChatCompletion.Types (availableChatCompletionServices) where

import Kernel.Prelude

data ChatCompletionServices = AzureOpenAI | Gemini deriving (Show, Read, Eq, Ord, Generic, ToJSON, FromJSON, ToSchema)

availableChatCompletionServices :: [ChatCompletionServices]
availableChatCompletionServices = [AzureOpenAI, Gemini]
