module Core.Confirm.Item where

import Beckn.Prelude

data Item = Item
  { id :: Text,
    quantity :: Quantity
  }
  deriving (Generic, ToJSON)

newtype Quantity = Quantity
  { count :: Int
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON)