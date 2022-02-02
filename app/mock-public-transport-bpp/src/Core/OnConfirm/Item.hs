module Core.OnConfirm.Item where

import Core.Descriptor
import Core.Quantity
import Beckn.Prelude

data Item = Item
  { id :: Text,
    fulfillment_id :: Text,
    descriptor :: DescriptorCode,
    quantity :: Quantity
  }
  deriving (Generic, Show, ToJSON, FromJSON)


