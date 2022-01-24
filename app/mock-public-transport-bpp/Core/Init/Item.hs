module Core.Init.Item where

import Beckn.Prelude
import Core.Item

data Item = Item
  { id :: Text,
    fulfillment_id :: Text,
    quantity :: Quantity
  }
  deriving (Generic, Show, ToJSON, FromJSON)
