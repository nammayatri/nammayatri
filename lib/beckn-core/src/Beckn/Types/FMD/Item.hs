module Beckn.Types.FMD.Item where

import Beckn.Types.Core.Descriptor
import Beckn.Types.Core.Duration
import Beckn.Types.Core.Price
import Beckn.Types.Core.Tag
import Beckn.Utils.Common
import EulerHS.Prelude

data Item = Item
  { _id :: Maybe Text,
    _parent_item_id :: Maybe Text,
    _descriptor :: Maybe Descriptor,
    _price :: Maybe Price,
    _model_id :: Maybe Text,
    _category_id :: Maybe Text,
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
        _brand_id = Nothing,
        _promotional = Nothing,
        _ttl = Nothing,
        _tags = example,
        _fragile = Nothing
      }
