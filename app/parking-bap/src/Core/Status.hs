module Core.Status where

import Beckn.Prelude

newtype StatusMessage = StatusMessage
  { order :: Order
  }
  deriving (Generic, FromJSON, ToJSON)

newtype Order = Order
  { id :: Text
  }
  deriving (Generic, FromJSON, ToJSON)