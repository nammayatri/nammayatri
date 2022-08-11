module Beckn.Types.Core.Metro.Search.Item where

import Beckn.Types.Core.Metro.Search.Descriptor (Descriptor)
import Beckn.Types.Core.Metro.Search.Price (Price)
import Beckn.Types.Core.Metro.Search.Tags (Tags)
import Beckn.Types.Core.Metro.Search.Time (Time)
-- import Data.OpenApi (ToSchema)
import EulerHS.Prelude hiding (id)

data Item = Item
  { id :: Maybe Text,
    parent_item_id :: Maybe Text,
    descriptor :: Maybe Descriptor,
    price :: Maybe Price,
    category_id :: Maybe Text,
    location_id :: Maybe Text,
    time :: Maybe Time,
    matched :: Maybe Bool,
    related :: Maybe Bool,
    recommended :: Maybe Bool,
    tags :: Maybe Tags
  }
  deriving (Generic, FromJSON, ToJSON, Show)

-- , ToSchema)
