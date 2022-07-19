module Beckn.Types.Core.Metro.Search.Category (Category (..)) where

import Beckn.Types.Core.Metro.Search.Descriptor (Descriptor)
import Beckn.Types.Core.Metro.Search.Tags (Tags)
import Beckn.Types.Core.Metro.Search.Time (Time)
import Data.OpenApi (ToSchema)
import EulerHS.Prelude hiding (id)

data Category = Category
  { id :: Maybe Text,
    parent_category_id :: Maybe Text,
    descriptor :: Maybe Descriptor,
    time :: Maybe Time,
    tags :: Maybe Tags
  }
  deriving (Generic, FromJSON, ToJSON, Show, Eq, ToSchema)
