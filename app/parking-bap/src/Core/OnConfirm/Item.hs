module Core.OnConfirm.Item where

import Beckn.Prelude
import Core.OnConfirm.Price

data Item = Item
  { id :: Text,
    price :: Price,
    quantity :: Quantity
  }
  deriving (Generic, FromJSON)

newtype Quantity = Quantity
  { count :: Int
  }
  deriving stock (Generic)
  deriving anyclass (FromJSON)