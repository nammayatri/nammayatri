module Core.Confirm.Billing where

import Beckn.Prelude

newtype Billing = Billing
  { phone :: Text
  }
  deriving (Generic, Show)
  deriving anyclass (FromJSON, ToJSON)
