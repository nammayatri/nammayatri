module Beckn.Types.Mobility.FareProduct where

import Beckn.Types.App
import Beckn.Types.Core.Item
import Beckn.Types.Core.Policy
import Beckn.Utils.Common
import Data.Text
import EulerHS.Prelude

data FareProduct = FareProduct
  { _id :: Text,
    _fare_media :: Text,
    _name :: Text,
    _fare_policy :: Policy,
    _applies_to_items :: [Item]
  }
  deriving (Generic, Show)

instance FromJSON FareProduct where
  parseJSON = genericParseJSON stripAllLensPrefixOptions

instance ToJSON FareProduct where
  toJSON = genericToJSON stripLensPrefixOptions

instance Example FareProduct where
  example =
    FareProduct
      { _id = idExample,
        _fare_media = "",
        _name = "Christmas discount",
        _fare_policy = example,
        _applies_to_items = example
      }
