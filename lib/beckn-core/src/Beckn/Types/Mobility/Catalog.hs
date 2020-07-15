module Beckn.Types.Mobility.Catalog where

import Beckn.Types.Core.Brand
import Beckn.Types.Core.Category
import Beckn.Types.Core.Item
import Beckn.Types.Mobility.FareProduct
import Beckn.Utils.Common
import Data.Text
import Data.Time.LocalTime
import EulerHS.Prelude

data Catalog = Catalog
  { _id :: Maybe Text,
    _categories :: [Category],
    _brands :: [Brand],
    _exp :: Maybe LocalTime,
    _items :: [Item],
    _fare_products :: [FareProduct]
  }
  deriving (Generic, Show)

instance FromJSON Catalog where
  parseJSON = genericParseJSON stripAllLensPrefixOptions

instance ToJSON Catalog where
  toJSON = genericToJSON stripLensPrefixOptions

instance Example Catalog where
  example =
    Catalog
      { _id = Just idExample,
        _categories = example,
        _brands = example,
        _exp = example,
        _items = example,
        _fare_products = example
      }
