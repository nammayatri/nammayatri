module Beckn.Types.Core.Migration.Category (Category (..)) where

import Beckn.Types.Core.Migration.Descriptor (Descriptor)
import Beckn.Types.Core.Migration.Tags (Tags)
import Beckn.Types.Core.Migration.Time (Time)
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
