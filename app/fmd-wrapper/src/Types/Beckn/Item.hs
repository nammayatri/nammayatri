module Types.Beckn.Item where

import EulerHS.Prelude hiding (id)
import Types.Beckn.Descriptor (Descriptor)
import Types.Beckn.Price (Price)

data Item = Item
  { id :: Text,
    descriptor :: Descriptor,
    price :: Price,
    category_id :: Text
  }
  deriving (Generic, FromJSON, ToJSON, Show)
