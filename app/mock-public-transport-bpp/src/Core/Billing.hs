module Core.Billing where

import Data.Aeson
import Relude

newtype Billing = Billing
  { name :: Text
  }
  deriving (Generic, ToJSON, FromJSON, Show)
