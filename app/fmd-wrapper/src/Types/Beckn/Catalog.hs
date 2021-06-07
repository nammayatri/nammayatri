module Types.Beckn.Catalog where

import Beckn.Utils.Example
import Beckn.Utils.JSON
import Data.Text
import Data.Time (UTCTime)
import EulerHS.Prelude hiding (id)
import Types.Beckn.Brand hiding (id)
import Types.Beckn.Category hiding (id)
import Types.Beckn.Item hiding (id)
import Types.Beckn.Model hiding (id)
import Types.Beckn.Offer hiding (id)

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
