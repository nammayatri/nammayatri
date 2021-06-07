module Types.Beckn.FmdItem where

import Beckn.Utils.Example
import Beckn.Utils.JSON
import EulerHS.Prelude hiding (id)
import Types.Beckn.Descriptor
import Types.Beckn.Duration
import Types.Beckn.Price
import Types.Beckn.Tag

data Item = Item
  { id :: Maybe Text,
    parent_item_id :: Maybe Text,
    descriptor :: Maybe Descriptor,
    price :: Maybe Price,
    model_id :: Maybe Text,
    category_id :: Maybe Text,
    package_category_id :: Maybe Text,
    brand_id :: Maybe Text,
    promotional :: Maybe Bool,
    ttl :: Maybe Duration,
    tags :: Maybe [Tag],
    fragile :: Maybe Bool
  }
  deriving (Generic, Show)

instance FromJSON Item where
  parseJSON = genericParseJSON stripPrefixUnderscoreIfAny

instance ToJSON Item where
  toJSON = genericToJSON stripPrefixUnderscoreIfAny

instance Example Item where
  example =
    Item
      { id = Just idExample,
        parent_item_id = Nothing,
        descriptor = example,
        price = example,
        model_id = Nothing,
        category_id = Just idExample,
        package_category_id = Just idExample,
        brand_id = Nothing,
        promotional = Nothing,
        ttl = Nothing,
        tags = example,
        fragile = Nothing
      }
