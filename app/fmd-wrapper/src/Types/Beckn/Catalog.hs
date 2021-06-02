module Types.Beckn.Catalog where

import Beckn.Utils.Example
import Beckn.Utils.JSON
import Data.Text
import Data.Time (UTCTime)
import EulerHS.Prelude
import Types.Beckn.Brand
import Types.Beckn.Category
import Types.Beckn.Item
import Types.Beckn.Model
import Types.Beckn.Offer

data Catalog = Catalog
  { id :: Text,
    categories :: [Category],
    brands :: [Brand],
    models :: [Model],
    ttl :: UTCTime,
    items :: [Item],
    offers :: [Offer],
    package_categories :: [Category]
  }
  deriving (Generic, Show)

instance FromJSON Catalog where
  parseJSON = genericParseJSON stripPrefixUnderscoreIfAny

instance ToJSON Catalog where
  toJSON = genericToJSON stripPrefixUnderscoreIfAny

instance Example Catalog where
  example =
    Catalog
      { id = idExample,
        categories = example,
        brands = example,
        models = example,
        ttl = example,
        items = example,
        offers = example,
        package_categories = example
      }
