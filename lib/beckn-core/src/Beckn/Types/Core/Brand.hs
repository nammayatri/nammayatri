module Beckn.Types.Core.Brand where

import Beckn.Types.Core.Descriptor
import Beckn.Utils.Example
import Data.Text
import EulerHS.Prelude

data Brand = Brand
  { _id :: Text,
    _parent_brand_id :: Maybe Text,
    _descriptor :: Descriptor
  }
  deriving (Generic, Show)

instance FromJSON Brand where
  parseJSON = genericParseJSON stripLensPrefixOptions

instance ToJSON Brand where
  toJSON = genericToJSON stripAllLensPrefixOptions

instance Example Brand where
  example =
    Brand
      { _id = idExample,
        _parent_brand_id = Just idExample,
        _descriptor = example
      }
