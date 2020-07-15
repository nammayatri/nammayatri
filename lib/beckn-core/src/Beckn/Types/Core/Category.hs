module Beckn.Types.Core.Category where

import Beckn.Utils.Common
import Data.Text
import EulerHS.Prelude

data Category = Category
  { _id :: Text,
    _subcategories :: [Category]
  }
  deriving (Generic, Show)

instance FromJSON Category where
  parseJSON = genericParseJSON stripAllLensPrefixOptions

instance ToJSON Category where
  toJSON = genericToJSON stripAllLensPrefixOptions

instance Example Category where
  example =
    Category
      { _id = idExample,
        _subcategories = []
      }
