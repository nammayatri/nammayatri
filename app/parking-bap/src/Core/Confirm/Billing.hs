module Core.Confirm.Billing where

import Beckn.Prelude

data Billing = Billing
  { name :: Text,
    phone :: Text
  }
  deriving (Generic, FromJSON, ToJSON)