module Core.OnConfirm.Price where

import Beckn.Prelude

data Price = Price
  { currency :: Text,
    value :: Int
  }
  deriving (Generic, FromJSON)