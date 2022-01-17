module Core.Item where

import Beckn.Prelude
import Core.OnConfirm.Price (Price)
import Core.OnSearch.Descriptor
import Core.OnSearch.ItemQuantity

-- 'Maybe' fields are fields, that is present in request, but we do not use it anyhow
data Item = Item
  { id :: Text,
    descriptor :: Descriptor,
    price :: Price,
    category_id :: Maybe Text,
    location_id :: Text,
    matched :: Maybe Bool,
    quantity :: ItemQuantity
  }
  deriving (Generic, FromJSON, ToJSON, Show, ToSchema)
