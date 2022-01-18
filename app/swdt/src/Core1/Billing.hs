module Core1.Billing where

import Beckn.Prelude

newtype Billing = Billing
  { name :: Text
  }
  deriving (Generic, ToJSON, FromJSON, Show)
