module Core.Confirm.Billing where

import Beckn.Prelude

data Billing = Billing
  { phone :: Text,
    requestorName :: Text
  }
  deriving (Generic, Show)
  deriving anyclass (FromJSON, ToJSON)
