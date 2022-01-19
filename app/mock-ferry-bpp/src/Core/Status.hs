module Core.Status where

import Beckn.Prelude

newtype StatusMessage = StatusMessage
  { order :: Order
  }
  deriving (Generic, Show, ToJSON, FromJSON)

newtype Order = Order
  { id :: Text
  }
  deriving (Generic, Show, ToJSON, FromJSON)
