{-# OPTIONS_GHC -Wno-incomplete-record-updates #-}

module Kernel.External.MultiModal.Types where

import Kernel.Prelude

data MultiModalService
  = GoogleTransit
  | OTPTransit
  deriving (Eq, Show, Generic, Read, Ord)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

data MultiModalServiceConfig
  = GoogleTransitConfig
      { baseUrl :: BaseUrl,
        apiKey :: Maybe Text
      }
  | OTPTransitConfig
      { baseUrl :: BaseUrl
      }
  deriving (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

moduleName :: MultiModalService -> Text
moduleName = \case
  GoogleTransit -> "GoogleTransit"
  OTPTransit -> "OTPTransit"
