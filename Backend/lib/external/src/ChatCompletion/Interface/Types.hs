{-# LANGUAGE DerivingStrategies #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module ChatCompletion.Interface.Types where

import ChatCompletion.AzureOpenAI.Config as AzureOpenAI
import Data.Aeson.Types
import Kernel.Prelude

data ChatCompletionServiceConfig = AzureOpenAI AzureOpenAI.AzureOpenAICfg | Gemini
  deriving (Show, Eq, Generic, FromJSON, ToJSON)
