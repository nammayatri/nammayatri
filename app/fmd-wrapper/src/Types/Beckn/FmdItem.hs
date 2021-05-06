module Types.Beckn.FmdItem where

import Beckn.Utils.Example
import EulerHS.Prelude
import Types.Beckn.Descriptor
import Types.Beckn.Duration
import Types.Beckn.Price
import Types.Beckn.Tag

data Item = Item
  { _id :: Maybe Text,
    _parent_item_id :: Maybe Text,
    _descriptor :: Maybe Descriptor,
    _price :: Maybe Price,
    _model_id :: Maybe Text,
    _category_id :: Maybe Text,
    _package_category_id :: Maybe Text,
    _brand_id :: Maybe Text,
    _promotional :: Maybe Bool,
    _ttl :: Maybe Duration,
    _tags :: Maybe [Tag],
    _fragile :: Maybe Bool
  }
  deriving (Generic, Show)

instance FromJSON Item where
  parseJSON = genericParseJSON stripAllLensPrefixOptions

instance ToJSON Item where
  toJSON = genericToJSON stripAllLensPrefixOptions

instance Example Item where
  example =
    Item
      { _id = Just idExample,
        _parent_item_id = Nothing,
        _descriptor = example,
        _price = example,
        _model_id = Nothing,
        _category_id = Just idExample,
        _package_category_id = Just idExample,
        _brand_id = Nothing,
        _promotional = Nothing,
        _ttl = Nothing,
        _tags = example,
        _fragile = Nothing
      }
