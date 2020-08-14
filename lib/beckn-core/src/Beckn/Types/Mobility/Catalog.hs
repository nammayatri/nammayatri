module Beckn.Types.Mobility.Catalog where

import Beckn.Types.Core.Brand
import Beckn.Types.Core.Category
import Beckn.Types.Core.Item
import Beckn.Types.Core.Model
import Beckn.Types.Core.Offer
import Beckn.Types.Mobility.FareProduct
import Beckn.Utils.Common
import Data.Text
import Data.Time
import EulerHS.Prelude

data Catalog = Catalog
  { _id :: Text,
    _categories :: [Category],
    _brands :: [Brand],
    _models :: [Model],
    _ttl :: Maybe UTCTime,
    _items :: [Item],
    _offers :: [Offer],
    -- Mobility specific
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
      { _id = idExample,
        _categories = example,
        _brands = example,
        _models = example,
        _ttl = example,
        _items = example,
        _offers = example,
        _fare_products = example
      }
