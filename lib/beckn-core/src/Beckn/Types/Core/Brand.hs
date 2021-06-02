module Beckn.Types.Core.Brand where

import Beckn.Types.Core.Descriptor
import Beckn.Utils.Example
import Beckn.Utils.JSON
import Data.Text
import EulerHS.Prelude

data Brand = Brand
  { id :: Text,
    parent_brand_id :: Maybe Text,
    descriptor :: Descriptor
  }
  deriving (Generic, Show)

instance FromJSON Brand where
  parseJSON = genericParseJSON stripPrefixUnderscoreIfAny

instance ToJSON Brand where
  toJSON = genericToJSON stripPrefixUnderscoreIfAny

instance Example Brand where
  example =
    Brand
      { id = idExample,
        parent_brand_id = Just idExample,
        descriptor = example
      }
