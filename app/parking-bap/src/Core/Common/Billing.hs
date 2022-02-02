module Core.Common.Billing where

import Beckn.Prelude

data Billing = Billing
  { phone :: Text,
    name :: Text
  }
  deriving (Generic, Show)
  deriving anyclass (FromJSON, ToJSON, ToSchema)
