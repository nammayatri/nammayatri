module Beckn.Types.Core.Item where

import Beckn.Types.Core.Image
import Beckn.Types.Core.Policy
import Beckn.Types.Core.Price
import Data.Text
import EulerHS.Prelude

data Item = Item
  { _id :: Text,
    _name :: Text,
    _description :: Text,
    _image :: Maybe Image,
    _price :: Price,
    _primary :: Bool,
    _selected :: Bool,
    _quantity :: Integer,
    _policy :: Maybe Policy,
    _category_id :: Text,
    _tags :: [Text]
  }
  deriving (Generic, Show)

instance FromJSON Item where
  parseJSON = genericParseJSON stripAllLensPrefixOptions

instance ToJSON Item where
  toJSON = genericToJSON stripAllLensPrefixOptions
