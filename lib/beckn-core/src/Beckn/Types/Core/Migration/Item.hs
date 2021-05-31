module Beckn.Types.Core.Migration.Item where

import Beckn.Types.Core.Migration.Descriptor (Descriptor)
import Beckn.Types.Core.Migration.Price (Price)
import Beckn.Types.Core.Migration.Tags (Tags)
import Beckn.Types.Core.Migration.Time (Time)
import EulerHS.Prelude

data Item = Item
  { _id :: Maybe Text,
    _parent_item_id :: Maybe Text,
    _descriptor :: Maybe Descriptor,
    _price :: Maybe Price,
    _category_id :: Maybe Text,
    _location_id :: Maybe Text,
    _time :: Maybe Time,
    _matched :: Maybe Bool,
    _related :: Maybe Bool,
    _recommended :: Maybe Bool,
    _tags :: Maybe Tags
  }
  deriving (Generic, Show)

instance FromJSON Item where
  parseJSON = genericParseJSON stripAllLensPrefixOptions

instance ToJSON Item where
  toJSON = genericToJSON stripAllLensPrefixOptions
