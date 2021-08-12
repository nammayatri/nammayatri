module Beckn.Types.Core.Category where

import Beckn.Types.Core.Descriptor
import Beckn.Types.Core.Tag
import Beckn.Utils.Example
import Data.OpenApi (ToSchema)
import Data.Text
import EulerHS.Prelude hiding (id)

data Category = Category
  { id :: Text,
    parent_category_id :: Maybe Text,
    descriptor :: Descriptor,
    tags :: [Tag]
  }
  deriving (Generic, FromJSON, ToJSON, Show, Eq, ToSchema)

instance Example Category where
  example =
    Category
      { id = idExample,
        parent_category_id = Just idExample,
        descriptor = example,
        tags = example
      }
