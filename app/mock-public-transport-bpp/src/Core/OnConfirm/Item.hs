module Core.OnConfirm.Item where

import Beckn.Prelude
import Core.Descriptor
import Core.OnConfirm.Quantity

data Item = Item
  { id :: Text,
    fulfillment_id :: Text,
    descriptor :: DescriptorCode,
    quantity :: Quantity
  }
  deriving (Generic, Show, ToJSON, FromJSON)
