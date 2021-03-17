module Beckn.Types.FMD.Catalog where

import Beckn.Types.Core.Brand
import Beckn.Types.Core.Category
import Beckn.Types.Core.Item
import Beckn.Types.Core.Model
import Beckn.Types.Core.Offer
import Beckn.Utils.Example
import Data.Text
import Data.Time (UTCTime)
import EulerHS.Prelude

data Catalog = Catalog
  { _id :: Text,
    _categories :: [Category],
    _brands :: [Brand],
    _models :: [Model],
    _ttl :: UTCTime,
    _items :: [Item],
    _offers :: [Offer],
    _package_categories :: [Category]
  }
  deriving (Generic, Show)

instance FromJSON Catalog where
  parseJSON = genericParseJSON stripAllLensPrefixOptions

instance ToJSON Catalog where
  toJSON = genericToJSON stripLensPrefixOptions

instance Example Catalog where
  example =
    Catalog
      { _id = idExample,
        _categories = example,
        _brands = example,
        _models = example,
        _ttl = example,
        _items = example,
        _offers = example,
        _package_categories = example
      }
