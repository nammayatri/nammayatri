module Beckn.Types.Core.Category where

import Beckn.Types.Core.Descriptor
import Beckn.Types.Core.Tag
import Beckn.Utils.Common
import Data.Text
import EulerHS.Prelude

data Category = Category
  { _id :: Text,
    _parent_category_id :: Maybe Text,
    _descriptor :: Descriptor,
    _tags :: [Tag]
  }
  deriving (Generic, Show)

instance FromJSON Category where
  parseJSON = genericParseJSON stripLensPrefixOptions

instance ToJSON Category where
  toJSON = genericToJSON stripAllLensPrefixOptions

instance Example Category where
  example =
    Category
      { _id = idExample,
        _parent_category_id = Just idExample,
        _descriptor = example,
        _tags = example
      }
