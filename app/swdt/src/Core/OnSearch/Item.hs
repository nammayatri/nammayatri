module Core.OnSearch.Item where

import Beckn.Prelude
import Core.Descriptor
import Core.Price

data Item = Item
  { id :: Text,
    fulfillment_id :: Text,
    descriptor :: DescriptorId,
    price :: Price
  }
  deriving (Generic, Show, ToJSON, FromJSON)
