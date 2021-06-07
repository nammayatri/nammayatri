module Beckn.Types.Core.Category where

import Beckn.Types.Core.Descriptor
import Beckn.Types.Core.Tag
import Beckn.Utils.Example
import Beckn.Utils.JSON
import Data.Text
import EulerHS.Prelude hiding (id)

data Category = Category
  { id :: Text,
    parent_category_id :: Maybe Text,
    descriptor :: Descriptor,
    tags :: [Tag]
  }
  deriving (Generic, Show, Eq)

instance FromJSON Category where
  parseJSON = genericParseJSON stripPrefixUnderscoreIfAny

instance ToJSON Category where
  toJSON = genericToJSON stripPrefixUnderscoreIfAny

instance Example Category where
  example =
    Category
      { id = idExample,
        parent_category_id = Just idExample,
        descriptor = example,
        tags = example
      }
