module Beckn.Types.Core.Migration.Item where

import Beckn.Types.Core.Migration.Descriptor (Descriptor)
import Beckn.Types.Core.Migration.Price (Price)
import Beckn.Types.Core.Migration.Tags (Tags)
import Beckn.Types.Core.Migration.Time (Time)
import Data.OpenApi (ToSchema)
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
  deriving (Generic, FromJSON, ToJSON, Show, ToSchema)
