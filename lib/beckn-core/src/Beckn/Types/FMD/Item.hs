module Beckn.Types.FMD.Item where

import Beckn.Types.Core.Descriptor
import Beckn.Types.Core.Price
import Beckn.Types.Core.Tag
import Beckn.Utils.Common
import EulerHS.Prelude

data Item = Item
  { _id :: Text,
    _parent_item_id :: Maybe Text,
    _descriptor :: Descriptor,
    _price :: Price,
    _model_id :: Maybe Text,
    _category_id :: Text,
    _brand_id :: Maybe Text,
    _promotional :: Bool,
    _ttl :: Maybe Integer,
    _tags :: [Tag],
    _fragile :: Bool
  }
  deriving (Generic, Show)

instance FromJSON Item where
  parseJSON = genericParseJSON stripAllLensPrefixOptions

instance ToJSON Item where
  toJSON = genericToJSON stripAllLensPrefixOptions

instance Example Item where
  example =
    Item
      { _id = idExample,
        _parent_item_id = Nothing,
        _descriptor = example,
        _price = example,
        _model_id = Nothing,
        _category_id = idExample,
        _brand_id = Nothing,
        _promotional = False,
        _ttl = Nothing,
        _tags = example,
        _fragile = True
      }
