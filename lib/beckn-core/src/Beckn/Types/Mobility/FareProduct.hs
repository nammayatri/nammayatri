module Beckn.Types.Mobility.FareProduct where

import Beckn.Types.Core.Item
import Beckn.Types.Core.Policy
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
