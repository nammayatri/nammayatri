module Beckn.Types.Core.Catalog where

import Beckn.Types.Core.Category
import Beckn.Types.Core.Item
import Data.Text
import EulerHS.Prelude

data Catalog = Catalog
  { _category_tree :: Category,
    _items :: [Item]
  }
  deriving (Generic, Show)

instance FromJSON Catalog where
  parseJSON = genericParseJSON stripAllLensPrefixOptions

instance ToJSON Catalog where
  toJSON = genericToJSON stripLensPrefixOptions
