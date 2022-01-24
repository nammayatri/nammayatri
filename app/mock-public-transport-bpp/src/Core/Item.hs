module Core.Item where

import Data.Aeson
import Relude

newtype Quantity = Quantity
  { count :: Int
  }
  deriving (Generic, Show, ToJSON, FromJSON)
