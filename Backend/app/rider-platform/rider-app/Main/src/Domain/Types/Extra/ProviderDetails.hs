{-# LANGUAGE ApplicativeDo #-}

module Domain.Types.Extra.ProviderDetails where

import Data.Aeson
import Kernel.Prelude

data ONDCProviderDetails = ONDCProviderDetails
  { subscriberId :: Text,
    uniqueKeyId :: Text
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)
