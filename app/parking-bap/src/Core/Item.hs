module Core.Item where

import Beckn.Prelude
import Beckn.Types.Core.Migration.Descriptor (Descriptor)
import Beckn.Types.Core.Migration.ItemQuantity (ItemQuantity)
import Core.OnConfirm.Price (Price)

-- 'Maybe' fields are fields, that is present in request, but we do not use it anyhow
data Item = Item
  { id :: Maybe Text,
    descriptor :: Descriptor,
    price :: Price,
    category_id :: Maybe Text,
    location_id :: Text,
    matched :: Maybe Bool,
    quantity :: ItemQuantity
  }
  deriving (Generic, FromJSON, ToJSON, Show)
